*          DATA SET SPLNK22    AT LEVEL 067 AS OF 11/06/18                      
*PHASE T21E22B  for the runners                                                 
***********************************************************************         
* *** NOTE ***                                                                  
* 2011-11-17   WHOA                                                             
*    Instead of using checksum for entire buyline, i'm using a few              
*       fields that product only cares to point out (see PGETSUM)               
*                                                                               
* 2011-11-14   WHOA                                                             
*    CHANGED THE PCVERSION FOR THE P&G FLAGS TO 1.4.0.0                         
*                                                                               
* HWON - I MERGED ALL THE PINERGY CODE TO KWAN/WHOA LATEST VERSION              
*        AS OF 11/9/2011                                                        
***********************************************************************         
*=TOMBSTONE============================================================         
* ---- ------- --- ---------- -----------------------------------------         
* WHO  DDMMMYR LVL JIRA       DESCRIPTION                                       
* ---- ------- --- ---------- -----------------------------------------         
* HWON 07NOV18 067 SPEC-21994 SKIP AUDIENCE POST SPILL DEMOS VALUES             
*======================================================================         
SPLNK22  TITLE '- Canadian Spot Server Downloads/Sundry Uploads'                
                                                                                
         GBLB  &FAST_ASSEMBLY                                                   
&FAST_ASSEMBLY SETB 1                                                           
                                                                                
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,                                                 +        
               SERVERTYPE=TSTSPOT,                                     +        
               WORKERKEY=SPCD,                                         +        
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
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,                                          +        
               B#LP_D,LP_D,                                            +        
               B#TWAD,TWAD,                                            +        
               B#AGYREC,AGYHDR,                                        +        
               B#CLTREC,CLTHDR,                                        +        
               B#PRDREC,PRDRECD,                                       +        
               B#ESTREC,ESTHDR,                                        +        
               B#MKTREC,MKTREC,                                        +        
               B#CAMREC,CMPRECD,                                       +        
               B#CSEREC,CSERECD,                                       +        
               B#CSLREC,CSLRECD,                                       +        
               B#DCMREC,DMPRECD,                                       +        
               B#CORREC,CORRECD,                                       +        
               B#DDMREC,DDMRECD,                                       +        
               B#DCTREC,DCTRECD,                                       +        
               B#STAREC,STARECD,                                       +        
               B#SDRREC,SDRRECD,                                       +        
               B#CTIREC,CTIREC),                                       +        
               BLOCKS2=(B#BUYREC,BUYREC,                               +        
               B#GOLREC,GOALRECD,                                      +        
               B#SVRDEF,SVRDEF,                                        +        
               B#SPDREC,SDEFRECD,                                      +        
               B#NDFREC,NDEFRECD,                                      +        
               B#EQUREC,EQUHDR,                                        +        
               B#SHWREC,NPGMRECD,                                      +        
               B#DOVREC,DOVRECD,                                       +        
               B#ADDREC,ADDRECD,                                       +        
               B#STAEST,STAESTD,                                       +        
               B#DPRREC,DPRRECD)                                                
         EJECT                                                                  
CODE     NMOD1 0,**SL22**,RR=RE                                                 
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK parameter block)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   INIT02                                                           
         L     R9,LP_ABLK1         On-line - root provides WORKD/SAVED          
         L     R8,LP_ABLK2         in LP_BLK1 and LP_BLK2                       
         J     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(global w/s)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R8,WORKD+(((WORKL+7)/8)*8)                                       
         USING SAVED,R8            R8=A(save w/s)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    Extract DDLINK/RUNNER calling mode           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ABUFFRIN,RBUFFRIN                                                
                                                                                
         ST    R5,ALP              Save A(LP_D)                                 
         ST    RE,SRVRRELO         Save program relocation factor               
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         DROP  R6,R7                                                            
                                                                                
         USING STABLKD,WORK                                                     
         USING STAPACKD,WORK                                                    
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    Test first for run                           
         JNE   PRCWRK                                                           
                                                                                
         L     R0,ATIA                                                          
         ST    R0,ANMSTAB          Set A(network market/station array)          
         AHI   R0,NMSTMAX1*NMSTABL                                              
         ST    R0,AFLMMSK          Set A(films sent mask)                       
         LHI   R0,NMSTMAX1         Maximum on-line entries                      
         STH   R0,NMSTMAX                                                       
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNSTR02            No                                           
                                                                                
         LHI   R0,NMSTMAX2         Maximum off-line entries                     
         STH   R0,NMSTMAX                                                       
         MHI   R0,NMSTABL          R0=amount of storage required                
         GETMAIN R,LV=(0)          Acquire storage for NMSTAB                   
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ANMSTAB          Set A(network market/station array)          
         LA    R0,SAVED                                                         
         AHI   R0,FLMMSK-SAVED                                                  
         ST    R0,AFLMMSK          Set A(films sent mask)                       
                                                                                
         LHI   R0,20*ONEK                                                       
         GETMAIN R,LV=(0)          Acquire storage for spot array               
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ASDDTAB          Set A(spot array)                            
         AHI   R1,20*ONEK                                                       
         ST    R1,ASDDTBL          A(LAST ENTRY W/O CLOBBERING AFTER)           
                                                                                
         L     RF,ACOMFACS         LOAD FACILITIES OVERLAYS                     
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize server working storage            
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#CLTREC-1)*L'LP_BLKS)(AIOLAST-AIO2),AIO2              
         MVC   LP_BLKS+((B#AGYREC-1)*L'LP_BLKS)(L'LP_BLKS),AAGYREC              
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
         MVC   LP_BLKS+((B#LP_D-1)*L'LP_BLKS)(L'LP_BLKS),ALP                    
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    Test 'process work' mode                     
         JNE   RUNREQ                                                           
         XC    QVALUES(QVALUEL),QVALUES                                         
         XC    DVALUES(DVALUEL),DVALUES                                         
         XC    LASTS(LASTSL),LASTS                                              
         LA    R0,QVALS                                                         
         LHI   R1,L'QVALS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    RUNI,RUNI                                                        
         OI    LP_FLAG2,LP_FSTIA   Set save/restore TIA                         
*                                                                               
         TM    LP_FLAG2,LP_FMQIM   PROCESSING MQ MESSAGE?                       
         JZ    EXITY                                                            
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
                                                                                
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT                                                   
         MVC   GBYAGY,LP_AGY                                                    
         MVC   GBYCOMF,ACOMFACS                                                 
         GOTOR VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
         MVC   SAVE1OR2,GBY1OR2                                                 
                                                                                
         L     RE,LP_AWMP          Build default range elements in WMP          
         USING LW_D,RE                                                          
         MVI   LW_TYPE,LW_TALLQ    All values                                   
         STCM  RE,7,AALL                                                        
         AHI   RE,LW_LN1Q                                                       
         MVI   LW_TYPE,LW_TNZRQ    Non-zero values                              
         STCM  RE,7,ANZR                                                        
         AHI   RE,LW_LN1Q                                                       
         ST    RE,LP_AWMP                                                       
         MVI   LW_TYPE,LW_TSINQ    Single zero value                            
         MVI   LW_DATA1,0                                                       
         STCM  RE,7,AZER                                                        
         AHI   RE,LW_LN2Q                                                       
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
         XC    MAPI,MAPI           Initialize map indicators                    
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JNE   *+14                                                             
         MVC   MAPI,L'LP_QMAPN(RF)                                              
         J     *+10                                                             
         AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
                                                                                
         MVC   AGENCY,LP_AGY       Set agency alpha id                          
         MVC   AGENCYB,LP_AGYB     Set agency binary value                      
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNREQX                                                          
                                                                                
         L     RF,AMASTC           Set trace option                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,SPTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,STAFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,TRFDIR,(4,0),0                               
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
RUNREQX  GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY               Exit back to DDLINK                          
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Desktop initial download X'0200'                                    *         
***********************************************************************         
                                                                                
REQINI   LKREQ *,I#CDINID,OUTINI,NEXTREQ=REQCLT                                 
                                                                                
OUTINI   LKOUT H                                                                
                                                                                
INIAUP   LKOUT R,1                 Agency/user/profile values                   
Array    LKOUT C,1,(A,ARYAUP)                                                   
         LKOUT E                                                                
                                                                                
INIDEM   LKOUT R,2                 Demo conversion array                        
Array    LKOUT C,2,(A,ARYDCN)                                                   
         LKOUT E                                                                
                                                                                
INIMED   LKOUT R,3                 Media codes and names                        
Array    LKOUT C,3,(A,ARYMED1),FILTROUT=TSTN13020                               
Array    LKOUT C,3,(A,ARYMED2),FILTROUT=TST13020                                
         LKOUT E                                                                
                                                                                
INIPRV   LKOUT R,4                 Province array                               
Array    LKOUT C,4,(A,ARYPRV)                                                   
         LKOUT E                                                                
                                                                                
INISDR   LKOUT R,5                 Self defining record                         
PRout    LKOUT P,,GETSDR                                                        
Array    LKOUT C,5,(A,ARYSDR)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYAUP   LKOUT A,(D,B#AGYREC,AGYHDR),NEWEL=Y,NROWS=1,ROWWIDTH=0                 
                                                                                
AgyNm    LKOUT C,1,AGYNAME,CHAR                                                 
AgyAd    LKOUT C,2,AGYADDR,CHAR                                                 
PRout    LKOUT P,,GETUSN           Get origin name into work                    
UsrNm    LKOUT C,3,(D,B#WORKD,WORK),CHAR,LEN=L'CTORGNAM,ND=Y                    
BPOTO    LKOUT C,4,AGYPBOTO,CHAR,ND=Y                                           
BuyID    LKOUT C,5,AGYPBREQ,CHAR,ND=Y                                           
MgeMM    LKOUT C,6,AGYPMGMM,CHAR,ND=Y                                           
-SReq    LKOUT C,7,AGYPSAUT,CHAR,ND=Y                                           
By/Br    LKOUT C,8,AGYPBYBR,CHAR,ND=Y                                           
SOTOC    LKOUT C,9,AGYPSOCD,CHAR,ND=Y                                           
Array    LKOUT C,10,(A,ARYIDN)     ID title                                     
Alpha    LKOUT C,11,AGYKAGY,CHAR                                                
Array    LKOUT C,20,(A,ARYDST),PCVERSION=1.6.0.17  DEST NAME & ADDR             
Array    LKOUT C,25,(A,ARYORG),PCVERSION=1.6.0.17  ORIG NAME & ADDR             
                                                                                
         LKOUT E                                                                
                                                                                
ARYIDN   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,                           +        
               ROWID=(AGYIDEL,AGYIDELQ),ROWWIDTH=(V,AGYIDLN)                    
                                                                                
IDNam    LKOUT C,10,AGYTITLE,CHAR                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYDST   LKOUT A,(D,B#CTIREC,CTIDATA),EOT=EOR,                         +        
               ROWID=(CTDSTEL,CTDSTELQ),ROWWIDTH=(V,CTDSTLEN)                   
                                                                                
DName    LKOUT C,20,CTDSTNAM,CHAR                                               
DAddr    LKOUT C,21,CTDSTADD,CHAR                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYORG   LKOUT A,(D,B#CTIREC,CTIDATA),EOT=EOR,                         +        
               ROWID=(CTORGEL,CTORGELQ),ROWWIDTH=(V,CTORGLEN)                   
                                                                                
OName    LKOUT C,25,CTORGNAM,CHAR                                               
OAddr    LKOUT C,26,CTORGADD,CHAR                                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get User-ID name into WORK                                          *         
***********************************************************************         
                                                                                
GETUSN   XC    WORK(L'CTORGNAM),WORK                                            
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LP_USRID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO3'                            
         JNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         LA    R2,CTIDATA          R2=A(first element on id record)             
         USING CTORGD,R2                                                        
GETUSN02 CLI   CTORGEL,EOR         Test end of record                           
         JE    EXITY                                                            
         CLI   CTORGEL,CTORGELQ    Test origin detail element                   
         JNE   *+14                                                             
         MVC   WORK(L'CTORGNAM),CTORGNAM                                        
         J     EXITY                                                            
         LLC   R0,CTORGLEN         Bump to next element on record               
         AR    R2,R0                                                            
         J     GETUSN02                                                         
         DROP  R2                                                               
                                                                                
ARYDCN   LKOUT A,(R,GETDCN),NEWEL=R,EOT=EOR,ROWNAME=DEMTABD,           +        
               ROWWIDTH=DEMTABL                                                 
                                                                                
DemCd    LKOUT C,1,DEMTCOD,CHAR                                                 
DemNm    LKOUT C,2,DEMTNAM,CHAR                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Build demo code/name array in I/O area 6 for downloading            *         
***********************************************************************         
                                                                                
GETDCN   XC    DBLOCK,DBLOCK       Build array of demo codes & names            
         MVC   DBFILE,DEMOFILE                                                  
         MVI   DBSELMED,CANAGYQ    Set Canadian media                           
         MVC   DBCOMFCS,ACOMFACS                                                
                                                                                
         L     R2,AIO6             R2=A(demo array build area)                  
         ST    R2,LP_ADATA                                                      
         USING DEMTABD,R2                                                       
         LA    R3,DEMOMOD                                                       
GETDCN02 CLI   0(R3),0             Test end of modifier list                    
         JE    GETDCN06                                                         
         SR    R0,R0                                                            
GETDCN04 AHI   R0,1                Bump demo number                             
         CHI   R0,255              Test all categories processed                
         JNH   *+12                                                             
         AHI   R3,1                Yes - bump to next modifier                  
         J     GETDCN02                                                         
                                                                                
         MVI   WORK+0,0            Build demo look-up expression                
         MVC   WORK+1(1),0(R3)                                                  
         STC   R0,WORK+2                                                        
         GOTOR VDEMOCON,DMCB,WORK,(2,DEMTNAM),DBLOCK                            
         CLI   DEMTNNAM,C'*'       Test bad demo                                
         JE    GETDCN04                                                         
         CLI   DEMTNMOD,C' '       Test modifier is a space (imps)              
         JNE   *+14                                                             
         MVC   DEMTMOD(L'DEMTNNAM),DEMTNNAM                                     
         MVI   DEMTNNAM+L'DEMTNNAM-1,C' '                                       
         GOTOR (#EDTDCD,AEDTDCD),DMCB,WORK,0,DEMTCOD                            
                                                                                
         AHI   R2,DEMTABL          Bump to next output array entry              
         J     GETDCN04                                                         
                                                                                
GETDCN06 MVI   DEMTCOD,0           Set end of array                             
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
DEMTABD  DSECT ,                   ** Demo array **                             
DEMTCOD  DS    0CL(L'DEMTMOD+L'DEMTNUM)                                         
DEMTMOD  DS    C                   Demo modifier                                
DEMTNUM  DS    CL3                 Demo number                                  
DEMTNAM  DS    0CL(L'DEMTNMOD+L'DEMTNNAM)                                       
DEMTNMOD DS    C                   Demo modifier                                
DEMTNNAM DS    CL6                 Demo name                                    
DEMTABL  EQU   *-DEMTABD                                                        
SVRDEF   CSECT ,                                                                
                                                                                
ARYMED1  LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,                           +        
               ROWID=(AGYMEDEL,AGYMEDEQ),ROWWIDTH=(V,AGYMEDLN)                  
                                                                                
MedCd    LKOUT C,1,AGYMEDCD,CHAR                                                
MedNm    LKOUT C,2,AGYMEDEX,CHAR                                                
PRout    LKOUT P,AGYMEDCD,SETQMED                                               
Array    LKOUT C,3,(A,ARYREP)                                                   
Array    LKOUT C,4,(A,ARYSLN)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYMED2  LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,NEWEL=B,                   +        
               ROWID=(AGYMEDEL,AGYMEDEQ),ROWWIDTH=(V,AGYMEDLN)                  
                                                                                
MedCd    LKOUT C,1,AGYMEDCD,CHAR                                                
MedNm    LKOUT C,2,AGYMEDEX,CHAR                                                
PRout    LKOUT P,AGYMEDCD,SETQMED                                               
Array    LKOUT C,3,(A,ARYREP)                                                   
Array    LKOUT C,4,(A,ARYSLN)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
SETQMED  L     R1,LP_AINP          Set QMEDA/QMEDIA from input value            
         MVC   QMEDIA,0(R1)                                                     
         MVC   QMEDA,0(R1)                                                      
         BR    RE                                                               
                                                                                
ARYREP   LKOUT A,(R,NXTREP),ROWNAME=REPRECD                                     
                                                                                
RepCd    LKOUT C,3,REPKREP,CHAR                                                 
                                                                                
         LKOUT E                                                                
TSTN13020 OC    LP_VRSN,LP_VRSN    HAVE VERSION #?                              
          JZ    SETCCC                                                          
          CLC   LP_VRSN,V130020                                                 
          JL    EXITY                                                           
          J     EXITN                                                           
                                                                                
TST13020 OC    LP_VRSN,LP_VRSN     HAVE VERSION #?                              
         JZ    EXITY                                                            
         CLC   LP_VRSN,V130020                                                  
         JNL   EXITY                                                            
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Get rep records for initial download                                *         
***********************************************************************         
                                                                                
NXTREP   GOTOR (#NXTREC,ANXTREC),DMCB,REPKEYT,('B#REPREC',0),          +        
               ('$NXTRSTA',SAVED),0,0                                           
         JNE   EXITY                                                            
         J     MORE                Tell DDLINK to call again                    
                                                                                
ARYPRV   LKOUT A,(D,B#SVRDEF,PRVTAB),ROWWIDTH=L'PRVTAB,NROWS=PRVTABN            
                                                                                
PrvCd    LKOUT C,1,PRVTAB,CHAR                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Call DDLINK to get self-defining record                             *         
* CUSTOM OVERRIDE KEY:                                                          
*  + 0 - AGENCY ALPHA CODE                                                      
*  + 2 - ENVIRONMENT (X'FF'-UAT)                                                
*                                                                               
* NOTE:  AIO3 WILL BE CLOBBERED TO READ THE GENERIC SDR                         
*        AIO5 WILL BE CLOBBERED TO READ THE AGENCY SPECIFIC SDR                 
***********************************************************************         
                                                                                
GETSDR   GOTOR LP_AGSDR,DMCB,LP_D,ASDRREC,0                                     
*                                                                               
         L     RF,ACOMFACS         LOAD FACILITIES OVERLAYS                     
         L     RF,CRECUP-COMFACSD(RF)                                           
         ST    RF,VRECUP                                                        
*****                                                                           
* AGENCY ALPHA SDR OVERRIDE                                                     
*****                                                                           
         L     R2,AIO5                                                          
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
***************                                                                 
* CANADA DOES NOT COMPREHEND THE CONCEPT OF A PASSED ENVIRONMENT YET            
***************                                                                 
GTSDR10  DS    0H                                                               
*&&DO                                                                           
*****                                                                           
* ENVIROMENT SDR OVERRIDE                                                       
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
*&&                                                                             
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
GTSDRX   J     EXIT                                                             
**********************                                                          
* OVERWRITE VALUES IN GENERIC SDR THAT WE FOUND IN OUR SPECIFIC SDR             
**********************                                                          
SDROVRW  NTR1  LABEL=*                                                          
         L     R2,AIO5             LOOK AT THE AGENCY SPECIFIC SDR              
         OC    0(2,R2),0(R2)       MAKE SURE WE HAVE A RECORD                   
         JZ    SDROVX              OTHERWISE JUST USE GENERIC SDR               
         LA    R2,SDRRFRST                                                      
SDROV010 CLI   0(R2),0             ANY MORE ELEMENT IN AGY SPECIFIC?            
         JE    SDROVX                                                           
*                                                                               
         L     R6,ASDRREC                                                       
         LA    R6,SDRRFRST-SDRKEY(R6)  R6 = 1ST ELEMENT IN GENERIC              
SDROV020 CLI   0(R6),0             CAN'T FIND A MATCHING ELEM?                  
         JE    SDROV040            NO, JUST ADD IT TO GENERIC                   
         CLC   0(1,R6),0(R2)                                                    
         JE    SDROV030                                                         
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     SDROV020                                                         
*                                                                               
* DELETE THE ELEM IN GENERIC                                                    
SDROV030 GOTO1 VRECUP,DMCB,(X'FE',ASDRREC),0(R6),0(R6),=X'002A002007D0'         
*                                                                               
* ADD ELEM IN GENERIC                                                           
SDROV040 GOTO1 VRECUP,DMCB,(X'FE',ASDRREC),0(R2),0(R6),=X'002A002007D0'         
*                                                                               
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     SDROV010                                                         
SDROVX   J     EXIT                                                             
*                                                                               
ARYSDR   LKOUT A,(D,B#SDRREC,SDRRFRST),EOT=EOR,ROWWIDTH=(V,SDELEN),    +        
               ROWID=(SDELD,0)                                                  
                                                                                
Defvl    LKOUT C,255,SDELD,SDEL                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYSLN   LKOUT A,(R,NXTSLN),ROWNAME=SAVED                                       
                                                                                
SptLn    LKOUT C,4,SLNSLEN,LBIN                                                 
EqvLn    LKOUT C,5,SLNEQIV,LBIN                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get spot length equivalences                                        *         
***********************************************************************         
                                                                                
NXTSLN   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTSLN02                                                         
         GOTOR GETSLN              Get A(spot length array)                     
         MVI   SLNSLEN,0           Initialize spot length index                 
                                                                                
NXTSLN02 LLC   RF,SLNSLEN          Get previous spot length index               
                                                                                
NXTSLN04 AHI   RF,1                Bump spot length index                       
         CHI   RF,250                                                           
         JH    NOMORE                                                           
         LR    RE,RF                                                            
         MHI   RE,2                Multiply by width of array entry             
         A     RE,ASLNTAB          RE=A(spot length entry)                      
         CLI   1(RE),0             Test has an equivalent spot length           
         JE    NXTSLN04            No - ignore                                  
         STC   RF,SLNSLEN          Set spot length                              
         MVC   SLNEQIV,1(RE)       Set equivalent spot length                   
         LA    R0,SAVED                                                         
         ST    R0,LP_ADATA         Point to saved                               
         J     MORE                Tell DDLINK to call again                    
                                                                                
PRVTAB   DS    0CL2                ** Province array **                         
         DC    C'BCALSAMAONPQNBNSPENF'                                          
PRVTABN  EQU   (*-PRVTAB)/L'PRVTAB                                              
         EJECT                                                                  
***********************************************************************         
* Desktop client browse/single client download X'0201'                *         
***********************************************************************         
                                                                                
REQCLT   LKREQ H,I#CDCLTB,OUTCLT,NEXTREQ=REQMKT                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
                                                                                
         LKREQ E                                                                
                                                                                
OUTCLT   LKOUT H                                                                
                                                                                
CLTMUL   LKOUT R,1                 Multiple client browse                       
Array    LKOUT C,1,(A,ARYCLT),FILTROUT=TSTMCLT                                  
         LKOUT E                                                                
                                                                                
CLTSIN   LKOUT R,1                 Single client download                       
Array    LKOUT C,1,(A,ARYCLV),FILTROUT=TSTSCLT                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
TSTMCLT  OC    QACLT,QACLT         Test multiple client download                
         BR    RE                                                               
                                                                                
TSTSCLT  OC    QACLT,QACLT         Test single client download                  
         J     SETCCC                                                           
                                                                                
ARYCLT   LKOUT A,(R,NXTCLT),MULTIROW=Y,ROWNAME=CLTRECD                          
                                                                                
CltCd    LKOUT C,1,CKEYCLT,(U,#EDTCLT,$EDTCLT)                                  
CltNm    LKOUT C,2,CNAME,CHAR                                                   
CPool    LKOUT C,3,CPROF+0,CHAR,LEN=1,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get client records for client browse                                *         
***********************************************************************         
                                                                                
NXTCLT   GOTOR (#NXTREC,ANXTREC),DMCB,CLTKEYT,('B#CLTREC',0),          +        
               SAVED,0,('#LIMCLT',ALIMCLT)                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Desktop market browse X'0202'                                       *         
***********************************************************************         
                                                                                
REQMKT   LKREQ H,I#CDMKTB,OUTMKT,NEXTREQ=REQSSB                                 
                                                                                
MedCd    LKREQ F,1,(D,B#SAVED,QMEDIA),CHAR,TEXT=SP#MED,COL=*                    
                                                                                
         LKREQ E                                                                
                                                                                
OUTMKT   LKOUT H                                                                
                                                                                
MKTMKT   LKOUT R,1                 Market values                                
Array    LKOUT C,1,(A,ARYMKT)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYMKT   LKOUT A,(R,NXTMKT),MULTIROW=Y,ROWNAME=MKTREC                           
                                                                                
MktNo    LKOUT C,1,MKTKMKT,CHAR                                                 
MktNm    LKOUT C,2,MKTNAME,CHAR                                                 
Array    LKOUT C,3,(A,ARYMKA)      Market alpha list                            
RtgSv    LKOUT C,4,MKTRSVC,CHAR,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get market records for market browse                                *         
***********************************************************************         
                                                                                
NXTMKT   GOTOR (#NXTREC,ANXTREC),DMCB,MKTKEYT,('B#MKTREC',0),          +        
               ('$NXTRSTA',SAVED),0,0                                           
         J     EXITY                                                            
                                                                                
ARYMKA   LKOUT A,(D,B#MKTREC,MKTALST),NROWS=L'MKTALST/3,ROWWIDTH=3              
                                                                                
MktCd    LKOUT C,3,MKTALST,CHAR,LEN=3,ND=Y                                      
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop selective station browse X'0203'                            *         
***********************************************************************         
                                                                                
REQSSB   LKREQ H,I#CDSSTB,OUTSSB,NEXTREQ=REQNSB                                 
                                                                                
MedCd    LKREQ F,1,(D,B#WORKD,QMEDA),CHAR,TEXT=SP#MED,COL=*                     
CliCd    LKREQ F,2,(D,B#WORKD,QCLTA),CHAR,TEXT=SP#CLI,COL=*                     
MktNo    LKREQ F,3,(D,B#SAVED,QMARKET#),LBIN,TEXT=SP#MKT,COL=*                  
                                                                                
         LKREQ E                                                                
                                                                                
OUTSSB   LKOUT H                                                                
                                                                                
SSBSTA   LKOUT R,1                 Selective station values                     
Array    LKOUT C,1,(A,ARYSSB)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYSSB   LKOUT A,(R,NXTSSB),MULTIROW=Y,ROWNAME=STAREC                           
                                                                                
StaCd    LKOUT C,1,STAKCALL,CHAR                                                
MktNo    LKOUT C,2,SMKT,CHAR,FILTROUT=TSTMKT                                    
MktNm    LKOUT C,3,(D,B#SAVED,MARKETN),CHAR,ND=Y                                
NetTy    LKOUT C,4,SNETTYPE,CHAR,ND=Y                                           
VndrLk   LKOUT C,5,(D,B#SAVED,STAFLAG1),HDRO,FILTROUT=TSTSVLCK,        +        
               PCVERSION=1.3.0.60                                               
PnGType  LKOUT C,6,(D,B#SAVED,STAFLAG1),HDRO,FILTROUT=TSTSPNG,         +        
               PCVERSION=1.4.0.0                                                
                                                                                
         LKOUT E                                                                
                                                                                
TSTMKT   OC    QMARKET#,QMARKET#   Don't send market number if given            
         BR    RE                                                               
                                                                                
TSTSVLCK TM    STAFLAG1,SLOCK        X'04' - VENDOR LOCK?                       
         J     SETCCC                                                           
                                                                                
TSTSPNG  TM    STAFLAG1,STPG         X'01' - P&G Station?                       
         J     SETCCC                                                           
                                                                                
***********************************************************************         
* Get station records for selective station browse                    *         
***********************************************************************         
                                                                                
NXTSSB   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTSSB02                                                         
                                                                                
         MVC   QMEDIA,QMEDA        Set media code                               
         XC    MARKETA,MARKETA                                                  
                                                                                
         L     R0,AIO5             Clear market name reference array            
         LHI   R1,10000/8                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,QMARKET#       Test market filter given                     
         JZ    NXTSSB02                                                         
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MARKETA,DUB                                                      
                                                                                
NXTSSB02 LARL  R0,FLTSTA           R0=A(record filter routine)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,STAKEYT,('B#STAREC',0),          +        
               ('$NXTRSTA',SAVED),(R0)                                          
         JNE   EXITY                                                            
                                                                                
         L     R2,ASTAREC          R2=A(station record)                         
         XC    MARKETN,MARKETN                                                  
         MVC   STAFLAG1,SFLAG1-STARECD(R2)                                      
         PACK  DUB,SMKT-STARECD(,R2)                                            
         CVB   RE,DUB                                                           
         STH   RE,HALF             Half=binary market number                    
         SRDA  RE,3                                                             
         SRL   RF,32-3                                                          
         A     RE,AIO5                                                          
         LLC   RF,BITLIST(RF)                                                   
         BASR  R1,0                                                             
         TM    0(RE),0                                                          
         EX    RF,0(R1)            Test if name sent                            
         JNZ   EXITY                                                            
         BASR  R1,0                                                             
         OI    0(RE),0                                                          
         EX    RF,0(R1)            Set name sent                                
                                                                                
         GOTOR GETMKT,HALF         Read market record                           
         MVC   MARKETN,WORK        Set market name                              
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Desktop network station browse X'0204'                              *         
***********************************************************************         
                                                                                
REQNSB   LKREQ *,I#CDNSTB,OUTNSB,NEXTREQ=REQSVL                                 
                                                                                
OUTNSB   LKOUT H                                                                
                                                                                
NSBSTA   LKOUT R,1                 Network station values                       
Array    LKOUT C,1,(A,ARYNSB)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYNSB   LKOUT A,(R,NXTNSB),MULTIROW=Y,ROWNAME=ADDRREC                          
                                                                                
StaCd    LKOUT C,1,ADDKCALL,CHAR                                                
StaNm    LKOUT C,2,ANAME,CHAR                                                   
NetTy    LKOUT C,3,(D,B#SAVED,NETTYPE),CHAR,ND=Y                                
VndrLk   LKOUT C,4,(D,B#SAVED,NETFLAG1),HDRO,FILTROUT=TSTNVLCK,        +        
               PCVERSION=1.3.0.60                                               
PnGType  LKOUT C,5,(D,B#SAVED,NETFLAG1),HDRO,FILTROUT=TSTNPNG,         +        
               PCVERSION=1.4.0.0                                                
         LKOUT E                                                                
                                                                                
TSTNVLCK TM    NETFLAG1,SLOCK        X'04' - VENDOR LOCK?                       
         J     SETCCC                                                           
                                                                                
TSTNPNG  TM    NETFLAG1,STPG         X'01' - P&G Station?                       
         J     SETCCC                                                           
                                                                                
***********************************************************************         
* Get station records for network station browse                      *         
***********************************************************************         
                                                                                
NXTNSB   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTNSB02                                                         
         MVI   QMEDIA,TELMEDQ      Get TV media stations                        
         MVC   MARKETA,EZEROS      For network market                           
         XC    QCLTA,QCLTA         Client is irrelevent                         
                                                                                
NXTNSB02 LARL  R0,FLTSTA           R0=A(record filter routine)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,STAKEYT,('B#STAREC',0),          +        
               ('$NXTRSTA',SAVED),(R0)                                          
         JNE   EXITY                                                            
         L     R1,ASTAREC          Extract network type                         
         MVC   NETTYPE,SNETTYPE-STAREC(R1)                                      
         MVC   NETFLAG1,SFLAG1-STAREC(R1)                                       
                                                                                
         GOTOR GETADD              Get address record                           
         JNE   NXTNSB02            Drop this if no address                      
                                                                                
         MVC   LP_ADATA,AADDREC    Set A(address record) as output              
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Desktop station validation X'0205'                                  *         
***********************************************************************         
                                                                                
REQSVL   LKREQ H,I#CDSTAV,OUTSVL,NEXTREQ=REQEST                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),              +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
SCall    LKREQ F,3,(D,B#SAVED,QSTACALL),CHAR,TEXT=SP#STA,COL=*                  
EstNo    LKREQ F,4,(D,B#SAVED,QESTNO),LBIN,TEXT=SP#EST,COL=*                    
                                                                                
         LKREQ E                                                                
                                                                                
OUTSVL   LKOUT H                                                                
                                                                                
SVLSTA   LKOUT R,1                 Station/spill values                         
Array    LKOUT C,1,(A,ARYSVL)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYSVL   LKOUT A,(R,GETSVL),ROWNAME=STAREC                                      
                                                                                
MktNo    LKOUT C,1,SMKT,CHAR                                                    
StaNm    LKOUT C,2,(D,B#ADDREC,ANAME),CHAR                                      
GSTCd    LKOUT C,3,SGSTCODE,CHAR,ND=Y                                           
NetTy    LKOUT C,17,SNETTYPE,CHAR,ND=Y                                          
Array    LKOUT C,4,(A,ARYSVL1)     PST codes                                    
Call1    LKOUT C,5,SRS1CALL,CHAR,ND=Y                                           
Call2    LKOUT C,6,SRS2CALL,CHAR,ND=Y                                           
Array    LKOUT C,2,(A,ARYSVL2),FILTROUT=TSTSPILL                                
Array    LKOUT C,2,(A,ARYSVL3),FILTROUT=TSTCABLE                                
Array    LKOUT C,2,(A,ARYSVL4),FILTROUT=TSTNETWK                                
Array    LKOUT C,3,(A,ARYSVL5),FILTROUT=TSTNETWK                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get spill definition record for station validation                  *         
***********************************************************************         
                                                                                
GETSVL   ICM   RF,7,QAMED          Get media                                    
         JZ    NOMORE                                                           
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
         JNE   NOMORE                                                           
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NOMORE                                                           
         GOTOR (#EDTCLT,AEDTCLT),DMCB,QCLTX,,QCLTA                              
                                                                                
GETSVL02 LA    R2,IOKEY                                                         
         USING STARECD,R2          Read station record                          
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMEDA                                                    
         MVC   STAKCALL,QSTACALL                                                
*                                                                               
         CLI   QMEDA,RADMEDQ       Leave band alone for Radio                   
         JE    GETSVL03                                                         
*                                                                               
         MVI   STAKCALL+L'STAKCALL-1,TELMEDQ   Set default TV band              
         CLI   QMEDA,NTRMEDQ       Processing Ntwk-Radio?                       
         JNE   GETSVL03             No, for tv/n/c leave tv band                
         MVI   STAKCALL+L'STAKCALL-1,NTRMEDQ  Yes, set X band                   
*                                                                               
GETSVL03 MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLTA                                                    
         MVC   STAKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOSTAFIL+B#STAREC'                      
         JNE   NOMORE                                                           
         MVC   LP_ADATA,ASTAREC    Point to station record                      
                                                                                
         TM    RUN#NSTA,RUNSNSTA   Test doing network station look-up           
         JNZ   *+8                 Yes - don't need station name                
         GOTOR GETADD              Get station address record                   
                                                                                
         MVI   SVLFLAG,0           Set nothing found                            
         L     R2,ASTAREC                                                       
         USING STARECD,R2          R2=A(station record)                         
         CLC   SMKT,EZEROS         Test network/cable                           
         JE    GETSVL06                                                         
                                                                                
         LA    R2,IOKEY                                                         
         USING SDEFRECD,R2         Read spill definition record                 
         XC    SDEFKEY,SDEFKEY                                                  
         MVI   SDEFKTY,SDEFKTYQ                                                 
         MVI   SDEFKSB,SDEFKSBQ                                                 
         MVC   SDEFKAGY,AGENCY                                                  
         MVC   SDEFKSTA,QSTACALL                                                
         CLI   QMEDA,RADMEDQ       Test radio                                   
         JE    GETSVL04             Yes, leave band                             
*                                                                               
         MVI   SDEFKSTA+L'SDEFKSTA-1,NTRMEDQ                                    
         CLI   QMEDA,NTRMEDQ       Processing Ntwk-Radio?                       
         JE    GETSVL04             Yes, set X band                             
*                                                                               
         MVI   SDEFKSTA+L'SDEFKSTA-1,0   Otherwise, clear band                  
*                                                                               
GETSVL04 L     RF,ACLTREC                                                       
         MVC   SDEFKRSV,CPROF+3-CLTRECD(RF)                                     
         MVC   SDEFKCLT,QCLTX                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#SPDREC'                         
         JE    GETSVL05                                                         
         MVC   SDEFKEY,IOKEYSAV    Read for default record                      
         XC    SDEFKCLT,SDEFKCLT                                                
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   EXITY               No spill for this station                    
                                                                                
GETSVL05 GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#SPDREC'                        
         JNE   NOMORE                                                           
         OI    SVLFLAG,SVLFSPL     Set spill record found                       
         J     EXITY                                                            
                                                                                
GETSVL06 TM    RUN#NSTA,RUNSNSTA   Test doing network station look-up           
         JZ    *+6                                                              
         DC    H'0'                Yes - should not be here                     
                                                                                
         LA    R2,IOKEY                                                         
         USING NDEFRECD,R2         Read network/cable definition record         
         XC    NDEFKEY,NDEFKEY                                                  
         MVC   NDEFKTYP,=AL2(NDEFRECQ)                                          
         MVC   NDEFKAGY,AGENCY                                                  
         MVC   NDEFKNET,QSTACALL                                                
         MVC   NDEFKCLT,QCLTX                                                   
         MVC   NDEFKEST,QESTNO                                                  
                                                                                
GETSVL08 GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#NDFREC'                         
         JE    GETSVL10                                                         
                                                                                
         MVC   NDEFKEY,IOKEYSAV                                                 
         CLI   NDEFKEST,0          Test estimate in key                         
         JE    *+12                                                             
         MVI   NDEFKEST,0          Yes - clear and retry                        
         J     GETSVL08                                                         
         OC    NDEFKCLT,NDEFKCLT   Test client in key                           
         JZ    EXITY                                                            
         XC    NDEFKCLT,NDEFKCLT   Yes - clear and retry                        
         J     GETSVL08                                                         
                                                                                
GETSVL10 GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#NDFREC'                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ANDFREC                                                       
         LA    R2,NDEFEL                                                        
         USING NDEFEL02,R2         Locate cable/network element                 
GETSVL12 CLI   NDEFEL02,EOR                                                     
         JE    GETSVL14                                                         
         CLI   NDEFEL02,NDEFNELQ                                                
         JE    *+16                                                             
         LLC   R0,NDEFEL02+1                                                    
         AR    R2,R0                                                            
         J     GETSVL12                                                         
         CLI   NDEFNET,NDEFCABQ    Test cable                                   
         JNE   GETSVL14                                                         
         OI    SVLFLAG,SVLFCBL     Set cable definition found                   
         J     EXITY                                                            
                                                                                
GETSVL14 OI    SVLFLAG,SVLFNET     Set network definition found                 
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
ARYSVL1  LKOUT A,(D,B#STAREC,SPST),NROWS=L'SPST,ROWWIDTH=1                      
                                                                                
PSTCd    LKOUT C,4,SPST,CHAR,LEN=1                                              
                                                                                
         LKOUT E                                                                
                                                                                
TSTSPILL TM    SVLFLAG,SVLFSPL     Test if spill definition found               
         J     SETCCC                                                           
                                                                                
TSTCABLE TM    SVLFLAG,SVLFCBL     Test if cable definition found               
         J     SETCCC                                                           
                                                                                
TSTNETWK TM    SVLFLAG,SVLFNET     Test if network definition found             
         J     SETCCC                                                           
                                                                                
ARYSVL2  LKOUT A,(D,B#SPDREC,SDEFEL),NEWEL=B,EOT=EOR,                  +        
               ROWID=(SDEFEL05,SDEFELQ),ROWWIDTH=(V,SDEF5LEN)                   
                                                                                
AgyM#    LKOUT C,1,SDEFAMKT,LBIN                                                
RtgM#    LKOUT C,2,SDEFRMKT,LBIN                                                
Offst    LKOUT C,3,SDEFOSET,CBIN,ND=Y                                           
Indic    LKOUT C,4,SDEFCEX,HEXD,ND=Y                                            
BkTyp    LKOUT C,5,SDEFBKTY,CHAR,ND=Y                                           
Alpha    LKOUT C,6,SDEFALPH,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYSVL3  LKOUT A,(D,B#NDFREC,NDEFEL),EOT=EOR,                          +        
               ROWID=(NDEFEL01,NDEFELQ),ROWWIDTH=(V,NDEFEL01+1)                 
                                                                                
MktSf    LKOUT C,7,NDEFMSUF,CHAR,FILTROUT=TSTMKTZZ,SKIPCOLS=5                   
MktNo    LKOUT C,8,NDEFMNUM,LBIN                                                
MktPc    LKOUT C,9,NDEFPCT,LBIN,FILTROUT=TSTNETBY                               
MNotB    LKOUT C,9,NDEFPCT,HDRO,FILTROUT=TSTNETBN                               
MktAl    LKOUT C,10,NDEFAMKT,CHAR,ND=Y                                          
Ofset    LKOUT C,16,NDEFOSET,CBIN,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
TSTMKTZZ L     R1,LP_AINP          Set false if dummy market element            
         CLC   NDEFMSUF-NDEFEL01(,R1),=C'ZZZZ'                                  
         J     SETCCC                                                           
                                                                                
TSTNETBY L     R1,LP_AINP          Set true if station is bought                
         CLI   NDEFPCT-NDEFEL01(R1),FF                                          
         J     SETCCC                                                           
                                                                                
TSTNETBN L     R1,LP_AINP          Set true if station is not bought            
         CLI   NDEFPCT-NDEFEL01(R1),FF                                          
         BR    RE                                                               
                                                                                
ARYSVL4  LKOUT A,(D,B#NDFREC,NDEFEL),EOT=EOR,                          +        
               ROWID=(NDEFEL01,NDEFELQ),ROWWIDTH=(V,NDEFEL01+1)                 
                                                                                
PRout    LKOUT P,NDEFSTA,ADDNST    Add to network station list                  
SCall    LKOUT C,11,NDEFSTA,CHAR,FILTROUT=TSTSTAZZ,SKIPCOLS=6                   
SPcnt    LKOUT C,12,NDEFPCT,LBIN,FILTROUT=TSTNETBY                              
SNotB    LKOUT C,12,NDEFPCT,HDRO,FILTROUT=TSTNETBN                              
Ofset    LKOUT C,13,NDEFOSET,CBIN,ND=Y                                          
RCode    LKOUT C,14,NDEFRGN,CHAR,ND=Y                                           
PRout    LKOUT P,NDEFSTA,GETSMKT                                                
MktNo    LKOUT C,15,(D,B#WORKD,WORK),CHAR,LEN=L'SMKT,ND=Y                       
                                                                                
         LKOUT E                                                                
                                                                                
ADDNST   L     R1,LP_AINP                                                       
         CLC   0(L'NDEFSTA,R1),=C'ZZZZ'                                         
         BER   RE                                                               
         ICM   RF,7,DASTA          Add station to network station list          
         JNZ   ADDNST02                                                         
         ICM   RF,15,LP_AWMP       Acquire WMP storage first time               
         STCM  RF,7,DASTA                                                       
         XC    0(LW_LN2Q,RF),0(RF)                                              
         LA    R1,LW_LN2Q+(L'NDEFSTA*50)(RF)                                    
         ST    R1,LP_AWMP                                                       
                                                                                
ADDNST02 SR    R1,R1                                                            
         ICM   R1,3,LW_NUMN-LW_D(RF)                                            
         AHI   R1,1                                                             
         STCM  R1,3,LW_NUMN-LW_D(RF)                                            
         BCTR  R1,0                                                             
         MHI   R1,L'NDEFSTA                                                     
         LA    RF,LW_DATA2-LW_D(RF,R1)                                          
         L     R1,LP_AINP                                                       
         MVC   0(L'NDEFSTA,RF),0(R1)                                            
         BR    RE                                                               
                                                                                
TSTSTAZZ L     R1,LP_AINP          Set false if dummy station element           
         CLC   NDEFSTA-NDEFEL01(,R1),=C'ZZZZ'                                   
         J     SETCCC                                                           
                                                                                
GETSMKT  MVC   SVIOVALS,IOVALS     Get market number for station                
         XC    WORK(L'SMKT),WORK                                                
         LA    R2,IOKEY                                                         
         USING STARECD,R2          Read station record                          
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,STAKTYPQ                                                
         MVI   STAKMED,TELMEDQ                                                  
         L     R1,LP_AINP                                                       
         MVC   STAKCALL,0(R1) Careful, NDEFSTA is 4byte & STAKCALL is 5         
         CLI   QMEDA,RADMEDQ                                                    
         JE    *+8                                                              
         CLI   QMEDA,NTRMEDQ                                                    
         JE    *+8                                                              
         MVI   STAKCALL+L'STAKCALL-1,TELMEDQ                                    
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLTA                                                    
         MVC   STAKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOSTAFIL+B#STAREC'                      
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         JNE   EXITY                                                            
         L     R2,ASTAREC          Extract market number into WORK              
         MVC   WORK(L'SMKT),SMKT                                                
         J     EXITY                                                            
                                                                                
ARYSVL5  LKOUT A,(R,NXTNST),MULTIROW=Y                                          
                                                                                
Array    LKOUT C,3,(A,ARYSVL6)     Network station spill                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get next network station to look-up                                 *         
***********************************************************************         
                                                                                
NXTNST   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTNST02                                                         
         MVI   QMEDA,TELMEDQ                                                    
         OI    RUN#NSTA,RUNSNSTA   Set doing network station look-up            
         ICM   RF,7,DASTA                                                       
         JZ    NOMORE                                                           
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)                                            
         JZ    NOMORE                                                           
         STC   R0,DSTAIND          Set number of stations to look-up            
         AHI   RF,LW_LN2Q                                                       
         STCM  RF,7,DASTA          Point to first station in list               
                                                                                
NXTNST02 SR    RF,RF                                                            
         ICM   RF,1,DSTAIND                                                     
         JZ    NOMORE                                                           
         SHI   RF,1                Decrement station look-up counter            
         STC   RF,DSTAIND                                                       
         ICM   RF,7,DASTA                                                       
         MVC   QSTACALL(L'NDEFSTA),0(RF)                                        
         MVC   QSTACALL+L'QSTACALL-L'QMEDA(L'QMEDA),QMEDA                       
         AHI   RF,L'NDEFSTA                                                     
         STCM  RF,7,DASTA          Set A(next station to look-up)               
         ST    RF,LP_ADATA                                                      
         J     EXITY                                                            
                                                                                
ARYSVL6  LKOUT A,(R,GETSVL02),ROWNAME=STAREC                                    
                                                                                
SCall    LKOUT C,1,STAKCALL,CHAR                                                
Call1    LKOUT C,2,SRS1CALL,CHAR,ND=Y                                           
Call2    LKOUT C,3,SRS2CALL,CHAR,ND=Y                                           
GSTCd    LKOUT C,4,SGSTCODE,CHAR,ND=Y                                           
Array    LKOUT C,5,(A,ARYSVL7)     PST codes                                    
Array    LKOUT C,4,(A,ARYSVL2),FILTROUT=TSTSPILL                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYSVL7  LKOUT A,(D,B#STAREC,SPST),NROWS=L'SPST,ROWWIDTH=1                      
                                                                                
PSTCd    LKOUT C,5,SPST,CHAR,LEN=1                                              
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop estimate browse X'0206'                                     *         
***********************************************************************         
                                                                                
REQEST   LKREQ H,I#CDESTB,OUTEST,NEXTREQ=REQBUY                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
CliCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
StrDt    LKREQ F,3,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,4,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
                                                                                
         LKREQ E                                                                
                                                                                
OUTEST   LKOUT H                                                                
                                                                                
ESTEST   LKOUT R,1                 Estimate values                              
Array    LKOUT C,1,(A,ARYEST)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYEST   LKOUT A,(R,NXTEST),MULTIROW=Y,ROWNAME=ESTHDR                           
                                                                                
EstNo    LKOUT C,1,EKEYEST,LBIN                                                 
EstNm    LKOUT C,2,EDESC,CHAR                                                   
EstSD    LKOUT C,3,ESTART,EDAT                                                  
EstED    LKOUT C,4,EEND,EDAT                                                    
EstFL    LKOUT C,5,EPROF,CHAR,LEN=3,ND=Y                                        
OOWSD    LKOUT C,6,EOWSDAY,LBIN,ND=Y                                            
Daily    LKOUT C,7,EDAILY,HDRO,ND=Y                                             
DayPt    LKOUT C,8,EDAYMENU,CHAR,ND=Y                                           
RCtrl    LKOUT C,9,ERATE,CHAR,ND=Y                                              
RBook    LKOUT C,10,EBOOK,BMON,ND=Y                                             
SpLen    LKOUT C,11,ESLN,LBIN,ND=Y                                              
EstIn    LKOUT C,12,(D,B#SAVED,ESTINDS),HEXD,ND=Y                               
FrzMo    LKOUT C,13,(D,B#SAVED,ESTFRZMO),BMON,ND=Y                              
RepCd    LKOUT C,14,EREP,(U,#EDTREP,$EDTREP),ND=Y                               
C2Fac    LKOUT C,18,(D,B#SAVED,ESTC2FAC),SPAK,ND=Y                              
Array    LKOUT C,15,(A,ARYEST1)    Estimate demos                               
Array    LKOUT C,16,(A,ARYEST2)    User demo names                              
Array    LKOUT C,17,(A,ARYEST3)    Open products                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get estimate records for estimate browse and optionally build list  *         
* of daypart menu codes to download and list of estimates found (to   *         
* drive the buy estimate reading)                                     *         
***********************************************************************         
                                                                                
NXTEST   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTEST10                                                         
                                                                                
         MVI   BYTE2,C'F'          SET FIRST TIME                               
         XC    QESTSSTR,QESTSSTR                                                
         MVC   QESTEEND,EFFS                                                    
         OC    QESTEDAT,QESTEDAT   Test end date provided                       
         JNZ   *+10                                                             
         MVC   QESTEDAT,EFFS       No - set high values                         
                                                                                
         OC    QAEST,QAEST         Test estimate list provided                  
         JNZ   NXTEST10                                                         
         MVC   QAEST,ANZR          No - set default                             
                                                                                
NXTEST10 GOTOR (#NXTREC,ANXTREC),DMCB,ESTKEYT,('B#ESTREC',0),SAVED,0,0          
         JE    NXTEST20                                                         
         CLI   BYTE2,C'F'          FIRST TIME CALLING NEXTREC?                  
         JNE   EXITY                                                            
         J     QERROR         IF THIS CAUSES PROBLEMS, USE GETEST LOGIC         
                                                                                
NXTEST20 MVI   BYTE2,0                                                          
         L     R2,IOADDR                                                        
         USING ESTHDR,R2           R2=A(estimate record)                        
         XC    ESTV(ESTVL),ESTV                                                 
         TM    ECNTRL,X'08'        Test estimate locked                         
         JZ    *+8                                                              
         OI    EST#LOCK,ESTILOCK                                                
         MVC   ESTFRZMO,ELOCKYM    Set frozen month & flags                     
         NI    ESTFRZMO+1,FF-X'C0'                                              
         TM    ELOCKMON,X'80'                                                   
         JZ    *+8                                                              
         OI    EST#FPRI,ESTIFPRI                                                
         TM    ELOCKMON,X'40'                                                   
         JZ    *+8                                                              
         OI    EST#FSUB,ESTIFSUB                                                
         CLI   EDAILY,C'Y'                                                      
         JE    *+8                                                              
         MVI   EDAILY,0            Clear EDAILY if not 'Y'                      
*                                                                               
         OC    ECOST2,ECOST2       TEST COST2 DEFINED                           
         JZ    NXTEST30                                                         
         ZAP   ESTC2FAC,PZERO                                                   
         TM    ECOST2,X'80'        TEST INPUT BUT ZERO                          
         JNZ   NXTEST30                                                         
         ICM   R0,15,ECOST2                                                     
         CVD   R0,DUB                                                           
         ZAP   ESTC2FAC,DUB                                                     
                                                                                
NXTEST30 TM    MAP#DMNU,MAPSDMNU   Test daypart menu list required              
         JZ    NXTEST40                                                         
         GOTOR LP_AAWMP,DMCB,(L'EDAYMENU,EDAYMENU),DDPTIND,DDPTMAXQ,   +        
               LP_D                                                             
                                                                                
NXTEST40 TM    MAP#DEST,MAPSDEST   Test dynamic estimate list required          
         JZ    NXTEST50                                                         
         GOTOR LP_AAWMP,DMCB,(L'EKEYEST,EKEYEST),DESTIND,DESTMAXQ,LP_D          
                                                                                
NXTEST50 J     EXITY                                                            
         DROP  R2                                                               
                                                                                
ARYEST1  LKOUT A,(D,B#ESTREC,EDEMLST),NROWS=EDEMLSTN,                  +        
               ROWWIDTH=L'EDEMLIST                                              
                                                                                
DemCd    LKOUT C,15,EDEMLST,(U,#EDTDCD,$EDTDCD),LEN=L'EDEMLIST,ND=Y             
                                                                                
         LKOUT E                                                                
                                                                                
ARYEST2  LKOUT A,(D,B#ESTREC,EUSRNMS),NROWS=EUSRNMN,ROWWIDTH=L'EUSRNML          
                                                                                
UDemo    LKOUT C,16,EUSRNMS,CHAR,LEN=L'EUSRNML,ND=Y                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYEST3  LKOUT A,(R,NXTEPR),ROWNAME=EPKEY                                       
                                                                                
PrdCd    LKOUT C,17,EPKEYPRD,CHAR                                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get estimate records for estimate browse and optionally build list  *         
* of daypart menu codes to download and list of estimates found (to   *         
* drive the buy estimate reading)                                     *         
***********************************************************************         
                                                                                
*ETEST   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME IME                          
*        JNE   GETEST02                                                         
GETEST   MVI   FULL2,C'F'          SET FLAG FIRST TIME                          
         XC    QESTSSTR,QESTSSTR                                                
         MVC   QESTEEND,EFFS                                                    
         OC    QESTEDAT,QESTEDAT   Test end date provided                       
         JNZ   *+10                                                             
         MVC   QESTEDAT,EFFS       No - set high values                         
                                                                                
         OC    QAEST,QAEST         Test estimate list provided                  
         JNZ   GETEST02                                                         
         MVC   QAEST,ANZR          No - set default                             
                                                                                
GETEST02 GOTOR (#NXTREC,ANXTREC),DMCB,ESTKEYT,('B#ESTREC',0),SAVED,0,0          
         JE    GETEST04                                                         
         CLI   FULL2,C'F'          WAS IT FIRST TIME?                           
         JNE   EXITY                                                            
         MVC   DAEST,AZER                                                       
         J     EXITY                                                            
GETEST04 MVI   FULL2,0                                                          
         L     R2,IOADDR                                                        
         USING ESTHDR,R2           R2=A(estimate record)                        
         GOTOR LP_AAWMP,DMCB,(L'EKEYEST,EKEYEST),DESTIND,DESTMAXQ,LP_D          
         J     GETEST02                                                         
         DROP  R2                                                               
***********************************************************************         
* Get product pointers for current POL estimate passive pointer       *         
***********************************************************************         
                                                                                
         USING EPKEY,IOKEY                                                      
NXTEPR   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTEPR02                                                         
                                                                                
         MVC   SVIOVALS,IOVALS     Save current i/o values                      
         XC    EPKEYPRD,EPKEYPRD   Clear product (was 'POL')                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR'                                   
         J     NXTEPR04                                                         
                                                                                
NXTEPR02 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR'                                   
                                                                                
NXTEPR04 CLC   EPKEY(EPKEYPRD-EPKEY),IOKEYSAV                                   
         JNE   NXTEPR08                                                         
                                                                                
         TM    MAP#PRDS,MAPSPRDS   Test product download required               
         JZ    NXTEPR06                                                         
         GOTOR LP_AAWMP,DMCB,(L'EPKEYPRD,EPKEYPRD),DPRCIND,DPRDMAXQ,   +        
               LP_D                                                             
                                                                                
NXTEPR06 CLC   EPKEYPRD,POLPRD     Ignore 'POL' product                         
         JE    NXTEPR02                                                         
         LA    R1,IOKEY                                                         
         ST    R1,LP_ADATA         Point to estimate passive                    
         J     MORE                Tell DDLINK to call again                    
                                                                                
NXTEPR08 MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     NOMORE                                                           
         EJECT                                                                  
***********************************************************************         
* Desktop multiple buy download X'0207'                               *         
***********************************************************************         
                                                                                
REQBUY   LKREQ H,I#CDBUYD,OUTBUY,NEXTREQ=REQPIN                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
StrDt    LKREQ F,3,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,4,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
MktNo    LKREQ F,5,(I,B#SAVED,QMKTIND),LBIN,LIST=F,DEFAULT=Y,          +        
               OLEN=L'BUYKMKTN,TEXT=SP#MKT,COL=*                                
StaCd    LKREQ F,6,(I,B#SAVED,QSTAIND),(U,#VALSTA,$VALSTA),LIST=F,     +        
               DEFAULT=Y,OLEN=L'BUYKSTAC,TEXT=SP#STA,COL=*                      
EstNo    LKREQ F,7,(I,B#SAVED,QESTIND),LBIN,LIST=F,DEFAULT=NOT,RANGE=Y,+        
               OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                                 
OptOp    LKREQ F,8,(D,B#SAVED,QOPTOOPT),CHAR,TEXT=(*,OPTOLIT),COL=*             
ExclOp   LKREQ F,9,(D,B#SAVED,QEXCLOPT),CHAR,TEXT=(*,OPTOLIT),COL=*             
                                                                                
         LKREQ E                                                                
                                                                                
OUTBUY   LKOUT H                                                                
                                                                                
BUYCLT   LKOUT R,1                 Get client record                            
Array    LKOUT C,1,(A,ARYCLV)                                                   
         LKOUT E                                                                
                                                                                
BUYEST   LKOUT R,2                 Get estimate records                         
Array    LKOUT C,2,(A,ARYEST)      (uses estimate browse above)                 
         LKOUT E                                                                
                                                                                
BUYPRD   LKOUT R,3                 Get product records                          
Array    LKOUT C,3,(A,ARYPRD)                                                   
         LKOUT E                                                                
                                                                                
BUYDPT   LKOUT R,4                 Get daypart menus                            
Array    LKOUT C,4,(A,ARYDPT)                                                   
         LKOUT E                                                                
                                                                                
BUYNBY   LKOUT R,5                 Get network buy records                      
Array    LKOUT C,5,(A,ARYNBY),FILTROUT=TSTEXCL                                  
         LKOUT E                                                                
                                                                                
BUYCUT   LKOUT R,22                Get cutins for network buys                  
Array    LKOUT C,22,(A,ARYCUT),FILTROUT=TSTXCUT                                 
         LKOUT E                                                                
                                                                                
BUYSBY   LKOUT R,5                 Get selective buy records                    
Array    LKOUT C,5,(A,ARYSBY),FILTROUT=TSTEXCL                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
TSTXCUT  CLI   QEXCLOPT,C'Y'       Exclude rest of download?                    
         JE    SETCCC                                                           
TSTCUT   TM    RUN#CUTI,RUNSCUTI   Test if cutins required                      
         J     SETCCC                                                           
                                                                                
TSTEXCL  CLI   QEXCLOPT,C'Y'       Exclude rest of download?                    
         J     SETCCC                                                           
                                                                                
ARYCLV   LKOUT A,(R,GETCLV),ROWNAME=CLTRECD                                     
                                                                                
CltNm    LKOUT C,1,CNAME,CHAR                                                   
RtgSv    LKOUT C,2,CPROF+3,CHAR,LEN=1                                           
CPool    LKOUT C,3,CPROF+0,CHAR,LEN=1                                           
ACtrl    LKOUT C,4,CPROF+9,CHAR,LEN=1                                           
RCtrl    LKOUT C,5,CPROF+14,CHAR,LEN=1                                          
BuyID    LKOUT C,6,CEXTRA+2,CHAR,LEN=1                                          
FrzMo    LKOUT C,7,(D,B#SAVED,CLTFRZMO),BMON,ND=Y                               
CInds    LKOUT C,8,(D,B#SAVED,CLTINDS),HEXD,ND=Y                                
C2Fac    LKOUT C,9,(D,B#SAVED,CLTC2FAC),SPAK,ND=Y                               
                                                                                
Array    LKOUT C,21,(A,ARYEQU)     Equivalence values                           
Array    LKOUT C,23,(A,ARYPRF)     Profile values                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get client record and format client values for sending              *         
***********************************************************************         
                                                                                
GETCLV   ICM   RE,7,QAMED          Set media                                    
         JZ    *+14                                                             
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         J     GETCLV02                                                         
         CLI   QMEDX,0                                                          
         JE    NOMORE                                                           
GETCLV02 GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         MVC   QMEDIA,QMEDA                                                     
                                                                                
         ICM   RE,7,QACLT          Set client                                   
         JZ    *+14                                                             
         MVC   QCLTX,LW_DATA1-LW_D(RE)                                          
         J     GETCLV04                                                         
         OC    QCLTX,QCLTX                                                      
         JZ    NOMORE                                                           
GETCLV04 GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NOMORE                                                           
         GOTOR (#EDTCLT,AEDTCLT),DMCB,QCLTX,,QCLTA                              
                                                                                
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2          R2=A(client record)                          
         XC    CLTV(CLTVL),CLTV                                                 
         MVC   CLTFRZMO,CLOCKYM    Set frozen month                             
         NI    CLTFRZMO+1,FF-X'C0'                                              
         TM    COPT1,COP1COSQ      Set Cost2 required indicator                 
         JZ    *+8                                                              
         OI    CLT#C2RQ,CLTIC2RQ                                                
         TM    COPT2,COP2FRZ       Set frozen client indicator                  
         JZ    *+8                                                              
         OI    CLT#FRZN,CLTIFRZN                                                
         TM    CLOCKMON,X'80'      Set frozen month prior                       
         JZ    *+8                                                              
         OI    CLT#FPRI,CLTIFPRI                                                
         TM    CLOCKMON,X'40'      Set frozen month subsequent                  
         JZ    *+8                                                              
         OI    CLT#FSUB,CLTIFSUB                                                
         TM    COPT3,COP3COSQ      Set Cost2 optional indicator                 
         JZ    *+8                                                              
         OI    CLT#C2OP,CLTIC2OP                                                
         TM    COPT4,COP4TRD       Set Cost2 trade indicator                    
         JZ    *+8                                                              
         OI    CLT#C2TR,CLTIC2TR                                                
***                                                                             
         CLC   LP_VRSN,V140000     Is the version 1.4.0.0 or higher?            
         JL    GETCLV20            No                                           
         TM    COPT4,COP4PG        Yes, set P&G indicator                       
         JZ    *+8                                                              
         OI    CLT#PRGA,CLTIPRGA                                                
***                                                                             
GETCLV20 CLI   CEXTRA+5,YESQ       SET US SPILL INDICATOR                       
         JNE   *+8                                                              
         OI    CLT#USSP,CLTIUSSP                                                
***                                                                             
         CLC   LP_VRSN,V140000     IS THE VERSION 1.4.0.0 OR HIGHER?            
         JL    GETCLV23            NO                                           
         CLI   CEXTRA+8,C'P'       YES, GOAL REQUIRED IF SET TO C'P'            
         JE    GETCLV26                                                         
***                                                                             
GETCLV23 CLI   CEXTRA+8,YESQ       SET GOAL REQUIRED INDICATOR                  
         JNE   *+8                                                              
GETCLV26 OI    CLT#GLRQ,CLTIGLRQ                                                
*                                                                               
         CLI   CEXTRA+10,YESQ      Set out of week indicator                    
         JNE   *+8                                                              
         OI    CLT#OOWC,CLTIOOWC                                                
*                                                                               
         OC    CCOST2,CCOST2       TEST COST2 PRESENT                           
         JZ    GETCLV28                                                         
         ZAP   CLTC2FAC,PZERO                                                   
         TM    CCOST2,X'80'        TEST INPUT BUT ZERO                          
         JNZ   GETCLV28                                                         
         ICM   R0,15,CCOST2                                                     
         CVD   R0,DUB                                                           
         ZAP   CLTC2FAC,DUB                                                     
*                                                                               
GETCLV28 MVC   LP_ADATA,ACLTREC                                                 
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
ARYEQU   LKOUT A,(R,GETEQU)                                                     
                                                                                
Array    LKOUT C,21,(A,ARYEQU1)    Equivalence array                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read equivalency header record and build equivalency array for      *         
* sending                                                             *         
***********************************************************************         
                                                                                
GETEQU   LA    R2,IOKEY                                                         
         USING EQUHDR,R2                                                        
         XC    EQUKEY,EQUKEY                                                    
         MVI   EQUKTYPE,EQUKTYPQ                                                
         MVC   EQUKAGY,LP_AGY                                                   
         MVC   EQUKMED,QMEDA                                                    
         MVC   EQUKCLT(L'QCLTX),QCLTX                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#EQUREC'                         
         JE    GETEQU02                                                         
         MVC   EQUKEY,IOKEYSAV                                                  
         XC    EQUKCLT,EQUKCLT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#EQUREC'                         
         JE    GETEQU02                                                         
         MVC   EQUKEY,IOKEYSAV                                                  
         MVC   EQUKAGY,EZEROS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#EQUREC'                         
         JNE   NOMORE                                                           
                                                                                
GETEQU02 GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#EQUREC'                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AEQUREC          R2=A(equivalence record)                     
         LA    R0,DEQTAB                                                        
         ST    R0,LP_ADATA         Point to DEQTAB                              
         XC    DEQTAB(DEQTABL),DEQTAB                                           
         XC    CLTEQU30,CLTEQU30   Set no 30 second equivalence                 
         GOTOR GETSLN              Get A(spot length array)                     
         SR    RF,RF               RF=spot length index                         
                                                                                
GETEQU04 AHI   RF,1                Bump spot index number                       
         CHI   RF,250                                                           
         JH    EXITY                                                            
         LR    RE,RF                                                            
         MHI   RE,2                                                             
         A     RE,ASLNTAB                                                       
         CLM   RF,1,1(RE)          Test prime spot length                       
         JNE   GETEQU04                                                         
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)          R1=index to equivalency factor               
         LR    RE,R1                                                            
         LA    R1,EQUSECT1(R1)     R1=A(equivalency factor)                     
         SRL   RE,2                                                             
         MHI   RE,DEQTABW                                                       
         LA    RE,DEQTAB(RE)       RE=A(equivalency array entry)                
         USING DEQTAB,RE                                                        
         STC   RF,DEQTSLEN         Set spot length                              
         MVC   DEQTFACT,0(R1)      Set equivalency factor                       
         CHI   RF,SEC30Q           Test this is 30 second slot                  
         JNE   *+10                                                             
         MVC   CLTEQU30,0(R1)      Yes - set 30 second equivalence              
         J     GETEQU04                                                         
         DROP  R2,RE                                                            
                                                                                
ARYEQU1  LKOUT A,(*,DEQTAB),NROWS=DEQTABN,ROWNAME=DEQTAB,              +        
               ROWWIDTH=DEQTABW                                                 
                                                                                
SptLn    LKOUT C,21,DEQTSLEN,LBIN,ND=Y                                          
EqFac    LKOUT C,22,DEQTFACT,LBIN,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYPRF   LKOUT A,(D,B#SAVED,PROFVALS),ROWNAME=PROFVALS,NROWS=1,        +        
               ROWWIDTH=L'PROFVALS                                              
                                                                                
PRout    LKOUT P,(B#SVRDEF,PROFS00),GETPROF                                     
00/01    LKOUT C,0,PROFV01,CHAR,MAPMOD1=PROFVM#                                 
00/02    LKOUT C,0,PROFV02,CHAR,MAPMOD1=PROFVM#                                 
00/03    LKOUT C,0,PROFV03,UBIN,SENDCHAR=Y,MAPMOD1=PROFVM#                      
00/04    LKOUT C,0,PROFV04,UBIN,SENDCHAR=Y,MAPMOD1=PROFVM#                      
00/05    LKOUT C,0,PROFV05,CHAR,MAPMOD1=PROFVM#                                 
00/06    LKOUT C,0,PROFV06,UBIN,SENDCHAR=Y,MAPMOD1=PROFVM#                      
00/07    LKOUT C,0,PROFV07,UBIN,SENDCHAR=Y,MAPMOD1=PROFVM#                      
00/08    LKOUT C,0,PROFV08,UBIN,SENDCHAR=Y,MAPMOD1=PROFVM#                      
00/09    LKOUT C,0,PROFV09,UBIN,SENDCHAR=Y,MAPMOD1=PROFVM#                      
00/10    LKOUT C,0,(D,B#TWAD,SVS002DP),CHAR,MAPMOD1=PROFVM#                     
00/11    LKOUT C,0,PROFV11,CHAR,MAPMOD1=PROFVM#                                 
00/12    LKOUT C,0,PROFV12,CHAR,MAPMOD1=PROFVM#                                 
00/13    LKOUT C,0,PROFV13,CHAR,MAPMOD1=PROFVM#                                 
00/14    LKOUT C,0,PROFV14,CHAR,MAPMOD1=PROFVM#                                 
00/15    LKOUT C,0,PROFV15,CHAR,MAPMOD1=PROFVM#                                 
00/16    LKOUT C,0,PROFV16,CHAR,MAPMOD1=PROFVM#                                 
                                                                                
PRout    LKOUT P,(B#SVRDEF,PROFS1W),GETPROF                                     
Array    LKOUT C,0,(A,ARYPRF1)                                                  
                                                                                
PRout    LKOUT P,(B#SVRDEF,PROFSB0),GETPROF                                     
Array    LKOUT C,0,(A,ARYPRF1)                                                  
                                                                                
PRout    LKOUT P,(B#SVRDEF,PROFSST),GETPROF                                     
Array    LKOUT C,0,(A,ARYPRF1)                                                  
                                                                                
PRout    LKOUT P,(B#SVRDEF,PROFSAJ),GETPROF                                     
Array    LKOUT C,0,(A,ARYPRF1)                                                  
                                                                                
PRout    LKOUT P,(B#SVRDEF,PROFS00A),GETPROF                                    
Array    LKOUT C,0,(A,ARYPRF1)                                                  
                                                                                
PRout    LKOUT P,(B#SVRDEF,PROFSD0),GETPROF                                     
         LKOUT C,0,(A,ARYPRF1),PCVERSION=2.1.0.47                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYPRF1  LKOUT A,(D,B#SAVED,PROFVALS),ROWNAME=PROFVALS,NROWS=1                  
                                                                                
XX/01    LKOUT C,0,PROFV01,CHAR,MAPMOD1=PROFVM#                                 
XX/02    LKOUT C,0,PROFV02,CHAR,MAPMOD1=PROFVM#                                 
XX/03    LKOUT C,0,PROFV03,CHAR,MAPMOD1=PROFVM#                                 
XX/04    LKOUT C,0,PROFV04,CHAR,MAPMOD1=PROFVM#                                 
XX/05    LKOUT C,0,PROFV05,CHAR,MAPMOD1=PROFVM#                                 
XX/06    LKOUT C,0,PROFV06,CHAR,MAPMOD1=PROFVM#                                 
XX/07    LKOUT C,0,PROFV07,CHAR,MAPMOD1=PROFVM#                                 
XX/08    LKOUT C,0,PROFV08,CHAR,MAPMOD1=PROFVM#                                 
XX/09    LKOUT C,0,PROFV09,CHAR,MAPMOD1=PROFVM#                                 
XX/10    LKOUT C,0,PROFV10,CHAR,MAPMOD1=PROFVM#                                 
XX/11    LKOUT C,0,PROFV11,CHAR,MAPMOD1=PROFVM#                                 
XX/12    LKOUT C,0,PROFV12,CHAR,MAPMOD1=PROFVM#                                 
XX/13    LKOUT C,0,PROFV13,CHAR,MAPMOD1=PROFVM#                                 
XX/14    LKOUT C,0,PROFV14,CHAR,MAPMOD1=PROFVM#                                 
XX/15    LKOUT C,0,PROFV15,CHAR,MAPMOD1=PROFVM#                                 
XX/16    LKOUT C,0,PROFV16,CHAR,MAPMOD1=PROFVM#                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get profiles for client download                                    *         
***********************************************************************         
                                                                                
GETPROF  L     R1,LP_AINP          Point to profile key                         
         MVC   PROFVM#,4(R1)       Set map number                               
         GOTOR GETPRF,(R1)         Look-up profiles                             
         MVC   PROFVALS,WORK2      Move profiles to sending area                
         J     EXITN                                                            
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=PRDRECD                          
                                                                                
PrdCd    LKOUT C,1,PKEYPRD,CHAR                                                 
PrdNm    LKOUT C,2,PNAME,CHAR                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get product records for buy download                                *         
***********************************************************************         
                                                                                
NXTPRD   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRD02                                                         
         OC    DAPRC,DAPRC         Test product list given                      
         JNZ   NXTPRD02                                                         
         MVC   DAPRC,ANZR          No - set to read all                         
                                                                                
NXTPRD02 GOTOR (#NXTREC,ANXTREC),DMCB,PRDKEYT,('B#PRDREC',0),SAVED,0,0          
         J     EXITY                                                            
                                                                                
ARYDPT   LKOUT A,(R,NXTDPT),MULTIROW=Y                                          
                                                                                
DMenu    LKOUT C,1,(D,B#SAVED,DPTMENU),CHAR                                     
Array    LKOUT C,2,(A,ARYDPT1)     Daypart array                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get daypart menu records for buy download and build daypart array   *         
* for sending                                                         *         
***********************************************************************         
                                                                                
NXTDPT   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTDPT02                                                         
         OC    DADPT,DADPT         Test daypart list built                      
         JNZ   NXTDPT02                                                         
         MVC   DADPT,AALL          No - download all daypart menus              
                                                                                
NXTDPT02 GOTOR (#NXTREC,ANXTREC),DMCB,DPTKEYT,('B#DPTREC',0),SAVED,0,0          
         JNE   EXITY                                                            
                                                                                
         L     R2,ADPTREC                                                       
         USING DPTHDR,R2           R2=A(daypart menu record)                    
         MVC   DPTMENU,DPTKMENU    Extract daypart menu code                    
         LA    R3,DPTTAB           Build daypart array                          
         USING DPTTABD,R3          R3=A(daypart array)                          
         ST    R3,LP_ADATA         Set A(daypart array)                         
                                                                                
         LA    R4,DPTCODES         R4=A(daypart list)                           
NXTDPT04 XC    DPTTABD(DPTTABL),DPTTABD                                         
         CLI   0(R4),EOR           Test end of daypart list                     
         JE    EXITY                                                            
         MVC   DPTTALPH,0(R4)                                                   
         MVC   DPTTMSCD,1(R4)                                                   
         MVC   DPTTNAME,2(R4)                                                   
         TM    DPTTMSCD,X'F0'                                                   
         JZ    NXTDPT08                                                         
         MVC   BYTE1,DPTTMSCD                                                   
         NI    BYTE1,X'F0'                                                      
                                                                                
         LA    R1,DPTCODES         Look-up master daypart code in list          
NXTDPT06 CLI   0(R1),0                                                          
         JE    NXTDPT08                                                         
         MVC   BYTE2,1(R1)                                                      
         NI    BYTE2,X'F0'                                                      
         CLC   BYTE1,BYTE2         Match on master daypart number               
         JE    *+12                                                             
         AHI   R1,5                                                             
         J     NXTDPT06                                                         
         CR    R1,R4               Test pointing to itself                      
         JE    NXTDPT08                                                         
         MVC   DPTTMAST,2(R1)      No - set master daypart                      
                                                                                
NXTDPT08 AHI   R4,5                Bump to next daypart list entry              
         AHI   R3,DPTTABL          Bump to next daypart array entry             
         J     NXTDPT04                                                         
         DROP  R2,R3                                                            
                                                                                
DPTTABD  DSECT ,                   ** Daypart array **                          
DPTTALPH DS    C                   Daypart alpha code                           
DPTTNAME DS    CL3                 Daypart name                                 
DPTTMAST DS    CL3                 Master daypart name                          
DPTTMSCD DS    X                   Master/slave code                            
DPTTABL  EQU   *-DPTTABD                                                        
DPTTMAXN EQU   36                  Maximum N'array entries                      
SVRDEF   CSECT ,                                                                
                                                                                
ARYDPT1  LKOUT A,(D,B#SAVED,DPTTAB),EOT=EOR,ROWWIDTH=DPTTABL,          +        
               ROWNAME=DPTTABD                                                  
                                                                                
DCode    LKOUT C,2,DPTTALPH,CHAR                                                
DName    LKOUT C,3,DPTTNAME,CHAR                                                
DMast    LKOUT C,4,DPTTMAST,CHAR,ND=Y                                           
MDptN    LKOUT C,5,DPTTMSCD,HEXD                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYNBY   LKOUT A,(R,NXTNBY),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
Array    LKOUT C,1,(A,ARYBLD)      Buy line details                             
Array    LKOUT C,8,(A,ARYNWK1),FILTROUT=TSTNBY                                  
                                                                                
         LKOUT E                                                                
                                                                                
TSTNBY   L     R1,ABUYREC          Test for network buy record                  
         OC    BUYKMKTN-BUYREC(,R1),BUYKMKTN-BUYREC(R1)                         
         BR    RE                                                               
                                                                                
***********************************************************************         
* Get network buy records for buy download and handle network station *         
* download optimization                                               *         
***********************************************************************         
                                                                                
NXTNBY   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTNB050                                                         
                                                                                
         TM    MAP#SNDL,MAPSSNDL   Test single network buy download             
         JNZ   NXTNB010                                                         
         TM    MAP#SBDL,MAPSSBDL   Or single buy download                       
         JNZ   NXTNB010                                                         
         XC    BUYSDAT,BUYSDAT     Set buy start date                           
         OC    QESTSDAT,QESTSDAT                                                
         JZ    NXTNB005                                                         
         GOTOR VDATCON,DMCB,(2,QESTSDAT),(3,BUYSDAT)                            
                                                                                
NXTNB005 MVC   BUYEDAT,EFFS        Set buy end date                             
         CLC   QESTEDAT,EFFS                                                    
         JE    NXTNB010                                                         
         GOTOR VDATCON,DMCB,(2,QESTEDAT),(3,BUYEDAT)                            
                                                                                
NXTNB010 TM    LP_FLAG,LP_FRUNX    Test running other process on exit           
         JNZ   NXTNB020                                                         
         L     R0,AFLMMSK          Clear films sent mask                        
         LHI   R1,L'FLMMSK                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
NXTNB020 CLI   QMEDA,NETMEDQ       Test network media request                   
         JNE   NOMORE                                                           
*                                                                               
         TM    MAP#PIND,MAPSPIND   Pinergy buy download?                        
         JZ    NXTNB025                                                         
         MVI   QOPTOOPT,C'N'                                                    
*                                                                               
NXTNB025 CLC   DAEST,AZER          DID WE FIND ANY VALID ESTIAMTES?             
         JE    NOMORE               NO, JUST EXIT (SAVE THE I/O)                
*                                                                               
         LA    R0,BUYV             CLEAR BUY RELATED VALUES                     
         LHI   R1,BUYVL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR BUFFER,DMCB,('XPLBUFQ',TSAINI),('XPLKEYL',XPLRECL)               
                                                                                
         TM    MAP#MGAN,MAPSMGAN   Test makegood analysis                       
         JNZ   NXTNB030            Yes - no allocation records                  
         OI    RUN#CUTI,RUNSCUTI   Set cutin record required                    
         GOTOR BUFFER,DMCB,('ALLBUFQ',TSAINI),('ALLKEYL',ALLRECL)               
         XC    ALLREC(ALLKEYL),ALLREC                                           
         XC    XPLREC(XPLKEYL),XPLREC                                           
                                                                                
NXTNB030 TM    MAP#SNDL,MAPSSNDL   Test single network buy download             
         JNZ   *+12                                                             
         TM    MAP#SBDL,MAPSSBDL   or single buy download                       
         JZ    NXTNB040                                                         
         MVC   LP_ADATA,ABUYREC    Yes - we have the buy already                
         J     EXITY                                                            
                                                                                
NXTNB040 MVC   SV1OR2,SAVE1OR2                                                  
         CLC   QCLTA,=C'TBL'                                                    
         JNE   NXTNB050                                                         
         CLC   LP_AGY,=C'SJ'                                                    
         JNE   NXTNB050                                                         
         MVI   SV1OR2,2                                                         
                                                                                
NXTNB050 TM    MAP#SNDL,MAPSSNDL   Test single network buy download             
         JNZ   NOMORE                                                           
         TM    MAP#SBDL,MAPSSBDL   or single buy download                       
         JNZ   NOMORE              Yes - there is only one buy                  
                                                                                
NXTNB060 LARL  R0,FLTBYK           R0=A(directory filter routine)               
         LARL  RF,FLTBYR           RF=A(record filter routine)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,NBYKEYT,('B#BUYREC',0),SAVED,    +        
               (R0),(RF)                                                        
         JNE   EXITY               Exit if buy not found                        
         CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   EXITY                                                            
                                                                                
         NI    BUYFLAG,FF-(BUYFNTWF+BUYFNTWK)                                   
         L     RE,ABUYREC          Optimize network station sending             
         AHI   RE,BDELEM-BUYREC                                                 
         USING NTWKELEM,RE                                                      
                                                                                
NXTNB070 LLC   R0,NTWKLEN          Locate first network element                 
         AR    RE,R0                                                            
         CLI   NTWKCODE,EOR                                                     
         JE    EXITY                                                            
         CLI   NTWKCODE,NTWKCODQ                                                
         JNE   NXTNB070                                                         
         LR    RF,RC                                                            
                                                                                
NXTNB080 CLI   NTWKCODE,NTWKCODQ   Test network element                         
         JNE   NXTNB090                                                         
         MVC   0(12,RF),NTWKCODE   Yes - save in element stack                  
         LLC   R0,NTWKLEN                                                       
         AR    RF,R0               Bump by element length                       
                                                                                
NXTNB090 CLI   NTWKCODE,EOR        Test at end of record                        
         JE    *+16                                                             
         LLC   R0,NTWKLEN                                                       
         AR    RE,R0                                                            
         J     NXTNB080                                                         
         DROP  RE                                                               
                                                                                
         SR    RF,RC               RF=L'network elements on this record         
         CH    RF,SVNTWKLN         Test same length as saved network            
         JNE   NXTNB100                                                         
         ST    RF,DUB                                                           
         LA    R0,SVNTWKEL         Yes - test if same network content           
         LR    R1,RF                                                            
         LR    RE,RC                                                            
         CLCL  R0,RE                                                            
         JNE   *+12                                                             
         OI    BUYFLAG,BUYFNTWK    Yes - set network same as previous           
         J     EXITY                                                            
         L     RF,DUB                                                           
                                                                                
NXTNB100 STH   RF,SVNTWKLN         Save network definition if different         
         LA    R0,SVNTWKEL                                                      
         LR    R1,RF                                                            
         LR    RE,RC                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
                                                                                
ARYSBY   LKOUT A,(R,NXTSBY),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
PRout    LKOUT P,,GETCS2           Set cost2 if any                             
Array    LKOUT C,1,(A,ARYBLD)      Buy line details                             
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get selective buy records for buy download                          *         
***********************************************************************         
                                                                                
NXTSBY   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTSBY30                                                         
*                                                                               
         CLI   QMEDA,0             Test media is resolved                       
         JE    NOMORE                                                           
         TM    MAP#SNDL,MAPSSNDL   Test single network buy download             
         JNZ   *+12                                                             
         CLI   QMEDA,NETMEDQ       Test network media request                   
         JE    NOMORE                                                           
*                                                                               
         LA    R0,BUYV             Clear buy related values                     
         LHI   R1,BUYVL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         TM    MAP#SBDL,MAPSSBDL   or single buy download                       
         JNZ   NXTSBY05                                                         
         TM    MAP#SNDL,MAPSSNDL   Test single network buy download             
         JZ    NXTSBY10                                                         
NXTSBY05 MVC   LP_ADATA,ABUYREC    Yes - we have the buy already                
         J     EXITY                                                            
*                                                                               
NXTSBY10 MVC   SV1OR2,SAVE1OR2                                                  
         CLC   QCLTA,=C'TBL'                                                    
         JNE   NXTSBY20                                                         
         CLC   LP_AGY,=C'SJ'                                                    
         JNE   NXTSBY20                                                         
         MVI   SV1OR2,2                                                         
*                                                                               
NXTSBY20 TM    MAP#PIND,MAPSPIND   PINERGY BUY DOWNLOAD?                        
         JZ    NXTSBY30                                                         
         MVI   QOPTOOPT,C'N'                                                    
*                                                                               
NXTSBY30 TM    MAP#SNDL,MAPSSNDL   TEST SINGLE NETWORK BUY DOWNLOAD             
         JNZ   NOMORE                                                           
         TM    MAP#SBDL,MAPSSBDL   or single buy download                       
         JNZ   NOMORE              YES - THERE IS ONLY ONE BUY                  
*                                                                               
         CLC   DAEST,AZER          DID WE FIND ANY VALID ESTIAMTES?             
         JE    NOMORE               NO, JUST EXIT (SAVE THE I/O)                
*                                                                               
         LARL  R0,FLTBYK           R0=A(directory filter routine)               
         LARL  RF,FLTBYR           RF=A(record filter routine)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,SBYKEYT,('B#BUYREC',0),SAVED,    +        
               (R0),(RF)                                                        
*                                                                               
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* ROUTINE TO ESTABLISH BUY COST2                                                
*                                                                     *         
* NTRY:- R1=A(BUY RECORD)                                             *         
***********************************************************************         
                                                                                
GETCS2   DS    0H                                                               
         XC    BUYC2FAC,BUYC2FAC   CLEAR IT FIRST                               
         CLI   QMEDA,RADMEDQ       ONLY DO THIS FOR RADIO AND                   
         JE    GETC2005                                                         
         CLI   QMEDA,TELMEDQ       TV AND                                       
         JE    GETC2005                                                         
         CLI   QMEDA,NTRMEDQ       NETWORK RADIO!                               
         JNE   GETC2X                                                           
GETC2005 L     RF,ABUYREC                                                       
         USING BUYREC,RF                                                        
         LA    RF,BDELEM           LOCATE COST2 ELEMENTS ON BUY                 
         USING COS2ELEM,RF                                                      
         SR    R0,R0                                                            
GETC2010 CLI   COS2ELEM,EOR                                                     
         JE    GETC2X                                                           
         CLI   COS2ELEM,C2FCELQ                                                 
         JE    GETC2020                                                         
                                                                                
         LLC   R0,COS2ELEM+1                                                    
         AR    RF,R0                                                            
         J     GETC2010                                                         
                                                                                
GETC2020 MVC   BUYC2FAC,2(RF)      SET COST 2 FACTOR                            
                                                                                
GETC2X   J     SETEQ                                                            
         DROP  RF                                                               
         EJECT                                                                  
                                                                                
ARYBLD   LKOUT A,(D,B#BUYREC,BUYREC),NROWS=1,ROWWIDTH=4000                      
                                                                                
Markt    LKOUT C,1,BUYKMKTN,LBIN,FILTROUT=TSTBMKT                               
StNet    LKOUT C,2,BUYKSTAC,(R,EDTNET),FILTROUT=TSTBSTA                         
EstNo    LKOUT C,3,BUYKEST,LBIN,FILTROUT=TSTBEST                                
LinNo    LKOUT C,4,BUYKBUY,LBIN,LEN=2                                           
                                                                                
DskAd    LKOUT C,5,(D,B#WORKD,IODA),HEXD                                        
PRout    LKOUT P,(B#LP_D,ABUYREC),GETSUM                                        
PutCS    LKOUT C,6,(D,B#WORKD,FULL),HEXD                                        
                                                                                
StrDt    LKOUT C,10,BDSTART,BDAT                                                
EndDt    LKOUT C,11,BDEND,BDAT                                                  
NoWks    LKOUT C,12,BDWKS,LBIN                                                  
DtInp    LKOUT C,13,BDINPUT,CHAR,ND=Y                                           
WkInd    LKOUT C,14,BDWKIND,CHAR,ND=Y                                           
Rotat    LKOUT C,15,BDDAY,LBIN                                                  
NoSpt    LKOUT C,16,BDNOWK,LBIN                                                 
SecLn    LKOUT C,17,BDSEC,LBIN                                                  
PBSec    LKOUT C,18,BDTIME,LBIN,ND=Y                                            
PBPct    LKOUT C,19,BDCOSTP,LBIN,ND=Y                                           
DayPt    LKOUT C,20,BDDAYPT,CHAR,ND=Y                                           
STime    LKOUT C,21,BDTIMST,LBIN                                                
ETime    LKOUT C,22,BDTIMEND,LBIN                                               
ShwCd    LKOUT C,23,BDPROGRM,(R,EDTSHW),ND=Y                                    
PrGcd    LKOUT C,24,BDPROGRM,(R,EDTPRG)                                         
AdjCd    LKOUT C,25,BDPROGT,(R,EDTADJ),ND=Y                                     
SCost    LKOUT C,26,BDCOST,LBIN                                                 
CInd1    LKOUT C,27,BDCIND,HEXD,ND=Y                                            
XFRAg    LKOUT C,28,BDXFRAGY,LBIN,ND=Y                                          
TaxRt    LKOUT C,29,BDNTAX,LBIN,ND=Y                                            
BWhy3    LKOUT C,30,BDWHY3,HEXD,ND=Y                                            
RepCd    LKOUT C,31,BDREP,(U,#EDTREP,$EDTREP),ND=Y                              
LCDat    LKOUT C,32,BDCHG,BDAT,ND=Y                                             
BWhy1    LKOUT C,33,BDWHY,HEXD,ND=Y                                             
PurpC    LKOUT C,34,BDPURP,CHAR,ND=Y                                            
SEDay    LKOUT C,35,BDSEDAY,LBIN,ND=Y                                           
Canad    LKOUT C,36,BDCANAD,HEXD,ND=Y                                           
CInd2    LKOUT C,37,BDCIND2,HEXD,ND=Y                                           
BWhy2    LKOUT C,38,BDWHY2,HEXD,ND=Y                                            
Stat1    LKOUT C,39,BDSTAT,HEXD,ND=Y                                            
MGCod    LKOUT C,40,BDMGDATE,CHAR,ND=Y                                          
Stat3    LKOUT C,41,BDSTAT3,HEXD,ND=Y                                           
NwkRg    LKOUT C,42,BDNRGN,CHAR,ND=Y                                            
AdvAg    LKOUT C,44,BDADVAGY,LBIN,ND=Y                                          
Mast1    LKOUT C,45,BDMASPRD+0,(U,#EDTPRD,$EDTPRD),ND=Y                         
Mast2    LKOUT C,46,BDMASPRD+1,(U,#EDTPRD,$EDTPRD),ND=Y                         
Stat2    LKOUT C,47,BDSTAT2,HEXD,ND=Y                                           
Stat4    LKOUT C,48,BDSTAT4,HEXD,ND=Y                                           
Array    LKOUT C,50,(A,ARYCON)     Contract number                              
Array    LKOUT C,51,(A,ARYCOM)     Comments                                     
C2Fac    LKOUT C,56,(D,B#SAVED,BUYC2FAC),CBIN,ND=Y                              
                                                                                
Array    LKOUT C,6,(A,ARYSPT)      Spot details                                 
Array    LKOUT C,7,(A,ARYDEM)      Original demo values                         
Array    LKOUT C,10,(A,ARYPBO)     Post buy original demo values                
Array    LKOUT C,9,(A,ARYSPL)      Spill demos values                           
Array    LKOUT C,11,(A,ARYPBS)     Post buy spill demo values                   
                                                                                
DUMMY    LKOUT C,12,(D,B#WORKD,BYTE1),HEXD,FILTROUT=TSTNMGAN,          +        
               SKIPCOLS=10,ND=Y                                                 
Array    LKOUT C,12,(A,ARYTAX)     Canadian tax element                         
Array    LKOUT C,13,(A,ARYBUP)     Buy upload activity                          
Array    LKOUT C,14,(A,ARYBWS)     BWS transfer                                 
Array    LKOUT C,15,(A,ARYBTR)     AmFm/Dare trace activity                     
Array    LKOUT C,16,(A,ARYACT)     Activity values                              
Array    LKOUT C,17,(A,ARYBDA)     Dare makegood trace element                  
Array    LKOUT C,18,(A,ARYMFX)     Market fix                                   
Array    LKOUT C,19,(A,ARYSFX)     Station fix                                  
Array    LKOUT C,20,(A,ARYORB)     Orbit                                        
Array    LKOUT C,21,(A,ARYCLN)     Cloned record                                
                                                                                
         LKOUT E                                                                
                                                                                
TSTBMKT  CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   SETCCC                                                           
         L     R1,LP_AINP                                                       
         USING BUYREC,R1                                                        
         CLC   BUYMKTL,BUYKMKTN    Don't send market if unchanged               
         JE    SETCCC                                                           
         MVC   BUYMKTL,BUYKMKTN    Else save market and send                    
         J     SETCCC                                                           
         DROP  R1                                                               
                                                                                
TSTBSTA  CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   SETCCC                                                           
         L     R1,LP_AINP                                                       
         USING BUYREC,R1                                                        
         CLC   BUYSTAL,BUYKSTAC    Don't send network if unchanged              
         JE    SETCCC                                                           
         MVC   BUYSTAL,BUYKSTAC    Else save network and send                   
         J     SETCCC                                                           
         DROP  R1                                                               
                                                                                
TSTBEST  CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   SETCCC                                                           
         L     R1,LP_AINP                                                       
         USING BUYREC,R1                                                        
*&&DO                                                                           
         CLI   QMEDA,NETMEDQ       Network?                                     
         JNE   TSTBEST1            No                                           
         OC    BUYKMKTN,BUYKMKTN   Do we have a market number?                  
         JNZ   SETCCC              For selective ntwk, must send est            
*&&                                                                             
TSTBEST1 CLC   BUYESTL,BUYKEST     Don't send estimate if unchanged             
         JE    SETCCC                                                           
         MVC   BUYESTL,BUYKEST     Else save estimate and send                  
         J     SETCCC                                                           
         DROP  R1                                                               
                                                                                
TSTNMGAN MVI   BYTE1,0             Don't send dummy output field                
         TM    MAP#MGAN,MAPSMGAN   Set false if makegood analysis               
         BR    RE                                                               
                                                                                
ARYCON   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(IDELEM,IDELCODQ),  +        
               ROWWIDTH=(V,IDELLEN)                                             
                                                                                
ConNo    LKOUT C,50,IDCONNO,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYCOM   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(COMELEM,CMCODEQ),  +        
               ROWWIDTH=(V,CMLEN)                                               
                                                                                
Comnt    LKOUT C,50,CMDATA,CHAR,LEN=V,ND=Y,MAPMOD1=CMNUM                        
                                                                                
         LKOUT E                                                                
                                                                                
ARYSPT   LKOUT A,(R,GETSPT),NROWS=(B#SAVED,DNROWS),NEWEL=B,            +        
               ROWNAME=$SDD,ROWWIDTH=$SDDDL                                     
                                                                                
SptDt    LKOUT C,1,$SDDATE,CDAT,ND=Y                                            
SStat    LKOUT C,2,$SDSTAT,HEXD                                                 
SPrd1    LKOUT C,3,$SDPRD1#,(U,#EDTPRD,$EDTPRD),ND=Y                            
SLen1    LKOUT C,4,$SDLEN1,LBIN,ND=Y                                            
SPrd2    LKOUT C,5,$SDPRD2#,(U,#EDTPRD,$EDTPRD),ND=Y                            
SLen2    LKOUT C,6,$SDLEN2,LBIN,ND=Y                                            
SCost    LKOUT C,7,$SDCOST,LBIN,ND=Y                                            
SMgCd    LKOUT C,8,$SDMGCD,CHAR,ND=Y                                            
SClrD    LKOUT C,9,$SDCLRDT,CDAT,ND=Y                                           
SClr#    LKOUT C,10,$SDCLRSQ,LBIN,ND=Y                                          
SAffD    LKOUT C,11,$SDADATE,CDAT,ND=Y                                          
SAffT    LKOUT C,12,$SDATIME,LBIN,ND=Y                                          
SADay    LKOUT C,13,$SDADAY,LBIN,ND=Y                                           
SAF1#    LKOUT C,14,$SDAFLM1,LBIN,ND=Y                                          
SAF1C    LKOUT C,15,$SDAFLM1,(R,EDTFLM),ND=Y                                    
SAF2#    LKOUT C,16,$SDAFLM2,LBIN,ND=Y                                          
SAF2C    LKOUT C,17,$SDAFLM2,(R,EDTFLM),ND=Y                                    
SDFl#    LKOUT C,18,$SDDFLM,LBIN,ND=Y                                           
SDFlc    LKOUT C,19,$SDDFLM,(R,EDTFLM),ND=Y                                     
SDTag    LKOUT C,20,$SDDTAG,LBIN,ND=Y                                           
SDDat    LKOUT C,21,$SDDDATE,CDAT,ND=Y                                          
STF1#    LKOUT C,22,$SDTFLM1,LBIN,ND=Y                                          
STF1C    LKOUT C,23,$SDTFLM1,(R,EDTFLM),ND=Y                                    
STF2#    LKOUT C,24,$SDTFLM2,LBIN,ND=Y                                          
STF2C    LKOUT C,25,$SDTFLM2,(R,EDTFLM),ND=Y                                    
STPat    LKOUT C,26,$SDTPATT,LBIN,ND=Y                                          
SSpt#    LKOUT C,32,$SDSPT#,LBIN,ND=Y                                           
SSet#    LKOUT C,33,$SDSET#,LBIN,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Build spot array for sending                                        *         
***********************************************************************         
                                                                                
GETSPT   L     R2,ABUYREC                                                       
         USING BUYREC,R2           R2=A(buy record)                             
                                                                                
         XC    FULL,FULL           Extract cost for FLTMGD routine              
         MVC   FULL+(L'FULL-L'BDCOST),BDCOST                                    
                                                                                
         LA    R3,BDELEM                                                        
         USING REGELEM,R3          R3=A(current spot element)                   
         L     R4,ATIA             Point to TIA on-line                         
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    *+8                                                              
         L     R4,ASDDTAB          Point to getmain area off-line               
         USING $SDD,R4             R4=A(spot array)                             
         ST    R4,LP_ADATA                                                      
         SR    R6,R6               R6=reference entry pointer                   
         XC    DNROWS,DNROWS       Clear number of rows in array                
                                                                                
         XC    HALF2,HALF2         Initialize Spot counter                      
         TM    RUN#CUTI,RUNSCUTI   Test processing cutins                       
         JZ    GETSPT02                                                         
         LA    RE,ALLALLOC         Yes - intialize allocation values            
         LHI   RF,ALLOCL                                                        
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
                                                                                
GETSPT02 LLC   R0,RLEN             Bump to next element                         
         AR    R3,R0                                                            
         CLI   RCODE,0             Test end of record                           
         JE    GETSPT28                                                         
         CLI   RCODE,RCORGQ        Test for spot elements                       
         JL    GETSPT02                                                         
         CLI   RCODE,X'0D'                                                      
         JH    GETSPT02                                                         
                                                                                
         GOTOR FLTMGD,REGELEM      Apply makegood filter if required            
         JNE   GETSPT02                                                         
         LH    R0,HALF2            Incrmemnt Spot counter                       
         AHI   R0,1                                                             
*&&DO                                                                           
         CHI   R0,MAXALLOC         ARE WE BEYOND MAX ALLOCS?                    
         JNH   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         STH   R0,HALF2                                                         
                                                                                
         XC    $SDD($SDDDL),$SDD   Initialize array entry                       
                                                                                
         CLC   LP_QMAPN,CDPIND#    Pinergy buy download                         
         JNE   GETSPT03                                                         
         CLC   RDATE,QESTSDAT                                                   
         JL    GETSPT02                                                         
         CLC   RDATE,QESTEDAT                                                   
         JH    GETSPT02                                                         
         TM    RSTATUS,RSMINUSQ+RSMINSDQ   Minused or Missed?                   
         JNZ   GETSPT02                Yes, skip this spot for Pinergy          
*                                                                               
         LA    RE,REGELEM                                                       
         LLC   RF,RLEN                                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,12,HALF          CHECKSM HAS FOR EXAMPLE 1234ABCD             
         XR    RE,RE               TOP HALF=1234   LOWER HALF=ABCD              
         ICM   RE,1,HALF           ADD HOB OF TOP TO LOB OF LOWER               
         ICM   RE,2,HALF+1         ADD LOB OF TOP TO HOB OF LOWER               
         AR    R0,RE               SO  3412 + ABCD = DFDF                       
         STCM  R0,3,$SCKSUM                                                     
                                                                                
GETSPT03 CLI   RCODE,RCPOTOQ                                                    
         JNE   *+8                                                              
         OI    $SD#OTOS,$SDSOTOS   Set OTO status                               
         TM    RSTATUS,RSMINUSQ                                                 
         JZ    *+8                                                              
         OI    $SD#MNUS,$SDSMNUS   Set minused status                           
         TM    RSTATUS,RSMINSDQ                                                 
         JZ    *+8                                                              
         OI    $SD#MISD,$SDSMISD   Set missed status                            
         TM    RSTATUS,RSMKGDPQ                                                 
         JZ    *+8                                                              
         OI    $SD#MGPD,$SDSMGPD   Set makegood pending status                  
         TM    RSTATUS,RSMGONLQ                                                 
         JZ    *+8                                                              
         OI    $SD#MKGD,$SDSMKGD   Set madegood status                          
         TM    RSTATUS,RSHIATSQ                                                 
         JZ    *+8                                                              
         OI    $SD#HIAT,$SDSHIAT   Set hiatus status                            
         TM    RSTATUS,RSNOALLQ                                                 
         JZ    *+8                                                              
         OI    $SD#PRAL,$SDSPRAL   Set pre-allocated status                     
         TM    RSTATUS,RSB1PALQ                                                 
         JZ    *+8                                                              
         OI    $SD#B1PA,$SDSB1PA   Set brand 1 pays all status                  
         TM    RSTATUS,RSRATOVQ                                                 
         JZ    *+8                                                              
         OI    $SD#COVR,$SDSCOVR   Set rate override status                     
                                                                                
         MVC   $SDDATE,RDATE       Set spot date                                
         OC    RPAY,RPAY           Set clearance date                           
         JZ    *+8                                                              
         OI    $SD#PAID,$SDSPAID   Set cleared status                           
                                                                                
         MVC   $SDCOST,RPCOST      Set cost override                            
                                                                                
         CLI   RLEN,RLPOL1LQ       Test pol spot                                
         JL    GETSPT05                                                         
                                                                                
         OC    $SDPRD1#,RPPRD      Allocations                                  
         JZ    *+8                                                              
         OI    $SD#ALOC,$SDSALOC   Set spot is allocated                        
         CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   *+10                                                             
         CLC   RPTIME,BDSEC                                                     
         JE    *+10                                                             
         MVC   $SDLEN1,RPTIME                                                   
                                                                                
         TM    RPSTAT2,X'FF'       TEST MAKEGOOD CODE PRESENT                   
         JZ    GETSPT04                                                         
                                                                                
         LA    R6,ELEM                                                          
         USING MGABLKD,R6                                                       
         XC    0(MGALNQ,R6),0(R6)                                               
         MVI   MGAACT,MGAQTRNS                                                  
         MVC   MGAACOM,ACOMFACS    SET A(COMFACS)                               
         L     RF,ABUYREC          A(BUYREC)                                    
         ST    RF,MGAIO                                                         
         ST    R3,MGAELEM                                                       
                                                                                
         LHI   R0,QBLDMGN                                                       
         ICM   R0,B'1110',T00A                                                  
         GOTOR VCALLOV,DMCB,0,(R0),0                                            
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RF,0(R1)                                                         
         GOTO1 (RF),MGABLKD                                                     
         CLI   MGAERR,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   $SDMGCD,MGAENTRY+MGECODE-MGENTRYD                                
         DROP  R6                                                               
                                                                                
GETSPT04 CLI   RLEN,RLPOL2LQ                                                    
         JL    GETSPT05                                                         
         MVC   $SDPRD2#,RPALLOC2+(RPPRD-RPALLOC)                                
         CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   *+10                                                             
         CLC   RPTIME,RPALLOC2+(RPTIME-RPALLOC)                                 
         JE    GETSPT05                                                         
         MVC   $SDLEN2,RPALLOC2+(RPTIME-RPALLOC)                                
                                                                                
GETSPT05 TM    MAP#MGAN,MAPSMGAN   Test makegood analysis                       
         JNZ   GETSPT16                                                         
                                                                                
         CLI   RCODE,RCPOLOQ       Test pool spot                               
         JNL   *+10                                                             
         MVC   $SDCLRSQ,RPAYSEQ    Set clearance sequence number                
         CLI   RLEN,RLPOL1LQ                                                    
         JL    *+10                                                             
         MVC   $SDCLRSQ,RPPAYSEQ                                                
         MVC   $SDCLRDT,RPAY       Set clearance date                           
                                                                                
         LA    R1,REGELEM          Process spot related elements                
                                                                                
         TM    RUN#CUTI,RUNSCUTI   Test building cutin record                   
         JZ    GETSPT06                                                         
         SR    RE,RE                                                            
         LH    RE,HALF2            YES - UPDATE ALLOCATION ARRAY                
         MHI   RE,L'ALLALLOC                                                    
         LA    RE,ALLALLOC-L'ALLALLOC(RE)                                       
         MVC   0(L'$SDPRD1#,RE),$SDPRD1#                                        
         MVC   L'$SDPRD1#(L'$SDPRD2#,RE),$SDPRD2#                               
         CLI   $SDPRD2#,0          Test piggyback                               
         JE    GETSPT06                                                         
         CLC   $SDLEN1,$SDLEN2     Test even piggyback split                    
         JE    GETSPT06                                                         
         MVC   L'$SDPRD1#+L'$SDPRD2#(L'$SDLEN1,RE),$SDLEN1                      
         DROP  R3                                                               
                                                                                
         USING FLMELEM,R1                                                       
GETSPT06 LLC   R0,FLMLEN           Bump to next element                         
         AR    R1,R0                                                            
         CLI   FLMELEM,EOR         Test end of record                           
         JE    GETSPT16                                                         
         CLI   FLMELEM,FLMCODEQ    Test film element                            
         JE    GETSPT08                                                         
         CLI   FLMELEM,ACCODEQ     Test affidavit element                       
         JE    GETSPT10                                                         
         CLI   FLMELEM,DLTGELQ     Test dealer tag element                      
         JE    GETSPT12                                                         
         CLI   FLMELEM,TRACIDQ     Test traffic cml assign element              
         JE    GETSPT14                                                         
         J     GETSPT16                                                         
                                                                                
GETSPT08 DS    0H                                                               
****     MVC   $SDADAY,FLMDAY      Film data                                    
* We won't use FLMDAY until the I2 is fixed                                     
*     -AND-                                                                     
* ALL the film elements in ALL the buy records are fixed                        
*                                                                               
* In MOBCSTD-2018, we found that the I2 was not setting FLMDAY                  
* correctly, but changing this program is easier and would have less            
* impact production-wise                                                        
****                                                                            
*                                                                               
         OC    $SDADATE,$SDADATE   DO WE HAVE AN AFFID DATE?                    
         JZ    GETSPT06            NO, THEN SKIP THIS FILM                      
* NOTE: AFFID ELEM comes before FILM element so  $SDADATE would be set          
         LR    R0,R1               R1 = A(element), so back it up               
         GOTO1 VDATCON,DMCB,(2,$SDADATE),(0,WORK) Use affid date                
         GOTO1 VGETDAY,DMCB,WORK,WORK+10                                        
         CLI   DMCB,0              Invalid date?                                
         JE    *+2                 Yes                                          
         LLC   R1,DMCB             We now have its day number                   
         LA    RE,X'80'                                                         
         SRL   RE,0(R1)            Shift RE to get proper day bit on            
         STC   RE,$SDADAY          We'll use this instead of FLMDAY             
         LR    R1,R0               RESTORE A(ELEMENT)                           
*                                                                               
GETSPT09 MVC   $SDAFLM1,FLMNUM                                                  
         CLI   FLMLEN,FLMNUM+(L'FLMNUM*2)-FLMELEM                               
         JL    GETSPT06                                                         
         MVC   $SDAFLM2,FLMNUM+L'FLMNUM                                         
         J     GETSPT06                                                         
                                                                                
         USING AFFELEM,R1          Affidavit data                               
GETSPT10 MVC   $SDADATE,ADATE                                                   
         MVC   $SDATIME,ATIME                                                   
         J     GETSPT06                                                         
                                                                                
         USING DLTGID,R1           Dealer tag data                              
GETSPT12 MVC   $SDDFLM,DLTGCSQ+(L'DLTGCSQ-L'$SDDFLM)                            
         MVC   $SDDTAG,DLTGTAG                                                  
         CLI   DLTGLN,DLTLENQ                                                   
         JL    GETSPT06                                                         
         MVC   $SDDDATE,DLTGDTE                                                 
         J     GETSPT06                                                         
                                                                                
         USING TRACID,R1           Traffic commercial assign data               
GETSPT14 MVC   $SDTFLM1,TRACCSQ                                                 
         MVC   $SDTFLM2,TRACCSQ2                                                
         MVC   $SDTPATT,TRACREF                                                 
         J     GETSPT06                                                         
         DROP  R1                                                               
                                                                                
GETSPT16 CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   GETSPT20                                                         
                                                                                
         LTR   R6,R6               Test reference spot available                
         JZ    GETSPT18                                                         
R        USING $SDD,R6             Yes - test same as reference spot            
         CLC   R.$SDD($SDDUL+$SDAV2L),$SDD                                      
         JNE   GETSPT18                                                         
         LLC   R0,R.$SDSPT#        Bump spot replica count                      
         AHI   R0,1                                                             
         STC   R0,R.$SDSPT#                                                     
         J     GETSPT02                                                         
                                                                                
GETSPT18 LA    R6,$SDD             Point to new reference spot                  
                                                                                
GETSPT20 LH    R0,DNROWS           Bump number of rows                          
         AHI   R0,1                                                             
         STH   R0,DNROWS                                                        
         AHI   R4,$SDDDL           Bump to next array entry                     
                                                                                
         L     RF,ATIA                                                          
         AHI   RF,18*ONEK                                                       
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    GETSPT22                                                         
         L     RF,ASDDTBL          MAKE SURE AN ENTRY CAN FIT                   
GETSPT22 CR    R4,RF                                                            
         JNH   GETSPT02                                                         
         DC    H'0'                                                             
         DROP  R                                                                
                                                                                
***********************************************************************         
* Spot optimization phase I - data compression                        *         
***********************************************************************         
                                                                                
GETSPT28 CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   EXITY                                                            
                                                                                
         SR    R3,R3                                                            
         ICM   R3,3,DNROWS                                                      
         JZ    EXITY                                                            
         SHI   R3,1                No compression if single spot                
         JZ    EXITY                                                            
         L     R4,LP_ADATA                                                      
         LA    R6,$SDD+$SDDDL                                                   
C        USING $SDD,R6             Current entry                                
         MVC   ELEM($SDDDL),$SDD                                                
R        USING $SDD,ELEM           Reference entry                              
                                                                                
***********************************************************************         
* Optimize allocation values                                          *         
***********************************************************************         
                                                                                
GETSPT30 OC    C.$SDAV1($SDAV1L),C.$SDAV1                                       
         JNZ   *+14                                                             
         OC    C.$SDAV2($SDAV2L),C.$SDAV2                                       
         JZ    GETSPT34                                                         
         CLC   C.$SDAV1($SDAV1L),R.$SDAV1                                       
         JNE   *+14                                                             
         CLC   C.$SDAV2($SDAV2L),R.$SDAV2                                       
         JE    GETSPT32                                                         
         MVC   R.$SDAV1($SDAV1L),C.$SDAV1                                       
         MVC   R.$SDAV2($SDAV2L),C.$SDAV2                                       
         J     GETSPT34                                                         
                                                                                
GETSPT32 OI    C.$SD#ALOS,$SDSALOS Set allocation is the same                   
         XC    C.$SDAV1($SDAV1L),C.$SDAV1                                       
         XC    C.$SDAV2($SDAV2L),C.$SDAV2                                       
                                                                                
***********************************************************************         
* Optimize clearance values                                           *         
***********************************************************************         
                                                                                
GETSPT34 OC    C.$SDCLRV($SDCLRL),C.$SDCLRV                                     
         JZ    GETSPT38                                                         
         CLC   C.$SDCLRDT,R.$SDCLRDT                                            
         JE    *+14                                                             
         MVC   R.$SDCLRDT,C.$SDCLRDT                                            
         J     GETSPT36                                                         
         OC    R.$SDCLRDT,R.$SDCLRDT                                            
         JZ    GETSPT36                                                         
         XC    C.$SDCLRDT,C.$SDCLRDT                                            
         OI    C.$SD#CLRD,$SDSCLRD Set clearance date is the same               
                                                                                
GETSPT36 CLC   C.$SDCLRSQ,R.$SDCLRSQ                                            
         JE    *+14                                                             
         MVC   R.$SDCLRSQ,C.$SDCLRSQ                                            
         J     GETSPT38                                                         
         XC    C.$SDCLRSQ,C.$SDCLRSQ                                            
         OI    C.$SD#CLRS,$SDSCLRS Set clearance sequence is the same           
                                                                                
***********************************************************************         
* Optimize affidavit date                                             *         
***********************************************************************         
                                                                                
GETSPT38 OC    C.$SDADATE,C.$SDADATE                                            
         JZ    GETSPT40                                                         
         CLC   C.$SDADATE,R.$SDADATE                                            
         JE    *+14                                                             
         MVC   R.$SDADATE,C.$SDADATE                                            
         J     GETSPT40                                                         
         XC    C.$SDADATE,C.$SDADATE                                            
         OI    C.$SD#ADTS,$SDSADTS Set affidavit date is the same               
                                                                                
***********************************************************************         
* Optimize film code 1                                                *         
***********************************************************************         
                                                                                
GETSPT40 OC    C.$SDAFLM1,C.$SDAFLM1                                            
         JZ    GETSPT42                                                         
         CLC   C.$SDAFLM1,R.$SDAFLM1                                            
         JE    *+14                                                             
         MVC   R.$SDAFLM1,C.$SDAFLM1                                            
         J     GETSPT42                                                         
         XC    C.$SDAFLM1,C.$SDAFLM1                                            
         OI    C.$SD#AF1S,$SDSAF1S Set film code 1 is the same                  
                                                                                
***********************************************************************         
* Optimize film code 2                                                *         
***********************************************************************         
                                                                                
GETSPT42 OC    C.$SDAFLM2,C.$SDAFLM2                                            
         JZ    GETSPT44                                                         
         CLC   C.$SDAFLM2,R.$SDAFLM2                                            
         JE    *+14                                                             
         MVC   R.$SDAFLM2,C.$SDAFLM2                                            
         J     GETSPT44                                                         
         XC    C.$SDAFLM2,C.$SDAFLM2                                            
         OI    C.$SD#AF2S,$SDSAF2S Set film code 2 is the same                  
                                                                                
***********************************************************************         
* Optimize dealer tag values                                          *         
***********************************************************************         
                                                                                
GETSPT44 OC    C.$SDDLRV($SDDLRL),C.$SDDLRV                                     
         JZ    GETSPT46                                                         
         CLC   C.$SDDLRV($SDDLRL),R.$SDDLRV                                     
         JE    *+14                                                             
         MVC   R.$SDDLRV($SDDLRL),C.$SDDLRV                                     
         J     GETSPT46                                                         
         OI    C.$SD#DLRS,$SDSDLRS Set dealet tag is the same                   
         XC    C.$SDDLRV($SDDLRL),C.$SDDLRV                                     
                                                                                
***********************************************************************         
* Optimize traffic values                                             *         
***********************************************************************         
                                                                                
GETSPT46 OC    C.$SDTRFV($SDTRFL),C.$SDTRFV                                     
         JZ    GETSPT50                                                         
         CLC   C.$SDTRFV($SDTRFL),R.$SDTRFV                                     
         JE    *+14                                                             
         MVC   R.$SDTRFV($SDTRFL),C.$SDTRFV                                     
         J     GETSPT50                                                         
         OI    C.$SD#TRFS,$SDSTRFS Set traffic values the same                  
         XC    C.$SDTRFV($SDTRFL),C.$SDTRFV                                     
                                                                                
***********************************************************************         
* Optimize spot date                                                  *         
***********************************************************************         
                                                                                
GETSPT50 CLC   $SDDATE,C.$SDDATE   Test date same as previous                   
         JNE   *+14                                                             
         XC    C.$SDDATE,C.$SDDATE Yes - clear current date                     
         J     GETSPT54                                                         
                                                                                
         CLC   C.$SDDATE,R.$SDDATE Match spot to reference date                 
         JNE   *+14                                                             
         XC    C.$SDDATE,C.$SDDATE Spot in same week as reference spot          
         J     GETSPT54                                                         
                                                                                
         GOTOR VDATCON,DMCB,(2,R.$SDDATE),(0,WORK)                              
         GOTOR VADDAY,DMCB,WORK,WORK+6,7                                        
         GOTOR VDATCON,DMCB,(0,WORK+6),(2,R.$SDDATE)                            
         CLC   C.$SDDATE,R.$SDDATE Test current is last reference+7             
         JNE   GETSPT52                                                         
         XC    C.$SDDATE,C.$SDDATE                                              
         OI    C.$SD#ISEQ,$SDSISEQ Set spot in next week                        
         J     GETSPT54                                                         
                                                                                
GETSPT52 LR    R4,R6                                                            
         MVC   R.$SDDATE,$SDDATE   Set reference date                           
                                                                                
GETSPT54 AHI   R6,$SDDDL           Bump to next array entry                     
         JCT   R3,GETSPT30         Do for number of entries                     
                                                                                
***********************************************************************         
* Spot optimization phase II - spot set compression                   *         
***********************************************************************         
                                                                                
         LH    R3,DNROWS                                                        
         L     R4,LP_ADATA         Current entry                                
         LA    R6,$SDD+$SDDDL                                                   
R        USING $SDD,R6             Reference entry                              
                                                                                
GETSPT56 JCT   R3,*+8                                                           
         J     EXITY                                                            
                                                                                
         CLC   R.$SDD($SDSET#-$SDD),$SDD                                        
         JNE   GETSPT58                                                         
         LLC   R0,$SDSET#          Bump spot set replica count                  
         AHI   R0,1                                                             
         STC   R0,$SDSET#                                                       
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MHI   R1,$SDDDL                                                        
         LA    R0,R.$SDD                                                        
         LA    RE,R.$SDD+$SDDDL                                                 
         LR    RF,R1                                                            
         MVCL  R0,RE               Remove replica                               
         LH    R1,DNROWS                                                        
         SHI   R1,1                Decrement array count                        
         STH   R1,DNROWS                                                        
         J     GETSPT56                                                         
                                                                                
GETSPT58 LA    R4,R.$SDD           Reference spot becomes current               
         AHI   R6,$SDDDL           Point to next reference spot                 
         J     GETSPT56            Do for number of entries                     
         DROP  R2,R4,C,R                                                        
                                                                                
ARYDEM   LKOUT A,(D,B#BUYREC,BDELEM),NROWS=1,NEWEL=Y                            
                                                                                
Array    LKOUT C,1,(A,ARYDEM1)     Demo look-up values                          
Array    LKOUT C,5,(A,ARYDEM2)     Demo values                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYDEM1  LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(DLUELEM,DLUCODEQ), +        
               ROWWIDTH=(V,DLULEN)                                              
                                                                                
BType    LKOUT C,1,DLUBKTYP,CHAR,ND=Y                                           
AMark    LKOUT C,2,DLUBAMKT,CHAR,ND=Y                                           
StaOv    LKOUT C,3,DLUBSTOV,CHAR,ND=Y                                           
Flags    LKOUT C,4,DLUBFLGS,HEXD,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYDEM2  LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(NDELEM,NDCORGQ),   +        
               ROWWIDTH=(V,NDLEN)                                               
                                                                                
RBook    LKOUT C,5,NDBOOK,BMON,ND=Y                                             
Progm    LKOUT C,6,NDPROG,CHAR,ND=Y                                             
PRout    LKOUT P,NDCODE,SETDEMEL                                                
Array    LKOUT C,10,(A,ARYDEM3)    Demo category/value/indicators               
                                                                                
         LKOUT E                                                                
                                                                                
ARYDEM3  LKOUT A,(*,NDEMNO),ROWNAME=NDELEM,NROWS=*,ROWWIDTH=NDEMLNQ             
                                                                                
DemCd    LKOUT C,10,NDEMNO,(U,#EDTDCD,$EDTDCD),                        +        
               FILTROUT=TSTSPLAU,SKIPCOLS=4                                     
PROUT    LKOUT P,NDEMRAW,SETDEM                                                 
DemVl    LKOUT C,11,(D,B#WORKD,FULL),LBIN,ND=Y                                  
DemIn    LKOUT C,12,(D,B#WORKD,BYTE1),HEXD,ND=Y                                 
                                                                                
         LKOUT E                                                                
                                                                                
SETDEM   L     R1,LP_AINP              SET DEMO VALUE IN FULL                   
         MVC   FULL,0(R1)                                                       
         NI    FULL,FF-(NDEMMANQ+NDEM2DEC)                                      
         MVC   BYTE1,0(R1)             SET DEMO FLAG IN BYTE1                   
         NI    BYTE1,NDEMMANQ+NDEM2DEC                                          
*                                                                               
         TM    0(R1),NDEM2DEC      TEST DEMO TO 2 DECIMAL PLACES                
         BNZR  RE                                                               
         L     RF,LP_ATWA                                                       
         CLI   SVS002DP-TWAD(RF),YESQ                                           
         BNER  RE                                                               
         L     RF,FULL             CONVERT TO 2DEC                              
         MHI   RF,10                                                            
         ST    RF,FULL                                                          
         OI    BYTE1,NDEM2DEC      ADD 2-DEC FLAG                               
         BR    RE                                                               
                                                                                
TSTSPLAU CLI   BYTE4,NDCSPLQ       TEST DOING SPILL?                            
         JNE   EXITY                NO                                          
         L     R1,LP_AINP           YES                                         
         CLI   1(R1),C'I'          IF AUDIENCE/IMPRESSION, DON'T D/L            
         J     SETCCC                                                           
                                                                                
ARYNWK1  LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(NTWKELEM,NTWKCODQ),+        
               ROWWIDTH=(V,NTWKLEN),NEWEL=Y                                     
                                                                                
PRout    LKOUT P,NTWKELEM,GETNMS   Get market/station sequence numver           
PRout    LKOUT P,NTWKELEM,PUTCUT   Put cutin record to buffer                   
                                                                                
Array    LKOUT C,8,(A,ARYNWK2),FILTROUT=TSTNWKY                                 
Array    LKOUT C,8,(A,ARYNWK3),FILTROUT=TSTNWKN                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get market/station sequence number - set market/station fields for  *         
* sending if first occurrence                                         *         
***********************************************************************         
                                                                                
GETNMS   L     R2,LP_AINP                                                       
         USING NTWKELEM,R2         R2=A(network element)                        
                                                                                
         MVC   ELEM,NTWKELEM       Extract network element into elem            
         XC    NMSVALS(NMSVALL),NMSVALS                                         
                                                                                
         TM    MAP#SBDL,MAPSSBDL   Test single buy download                     
         JNZ   GETNMS04                                                         
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    GETNMS04                                                         
                                                                                
         LH    R0,DNMSTAB#         Get market/station sequence number           
         LH    RF,NMSTMAX          Get maximum number of array entries          
         GOTOR VBINSRCH,DMCB,(1,NTWKMKST),ANMSTAB,(R0),NMSTABL,        +        
               L'NMSTMKST,(RF)                                                  
         MVC   DNMSTAB#,10(R1)     Save number of array entries                 
         ICM   R3,7,1(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING NMSTABD,R3          R3=A(array entry)                            
         CLI   0(R1),0             Test entry added (first occurrence)          
         JNE   GETNMS02                                                         
         CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   GETNMS04                                                         
         MVC   NMSSEQ,NMSTSEQ      No - send sequence number only               
         J     EXITN                                                            
                                                                                
GETNMS02 MVC   NMSTSEQ,DNMSTAB#    Set sequence number in array entry           
         CLI   QOPTOOPT,0          Test optimizer off                           
         JNE   GETNMS04                                                         
         MVC   NMSSEQ,NMSTSEQ      Set sequence number for sending              
         DROP  R3                                                               
                                                                                
GETNMS04 MVC   NMSMKT,NTWKMKST     Set market for sending                       
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVI   STAPMED,NETMEDQ                                                  
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPSTA,NTWKMKST+L'BUYKMKTN                                      
         GOTOR VSTAPACK,STAPACKD                                                
         MVC   NMSSTA,STAPQSTA     Set station for sending                      
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
NMSTABD  DSECT ,                   ** Network market/station array **           
NMSTMKST DS    XL(L'NTWKMKST)      Market/station code                          
NMSTSEQ  DS    AL2                 Sequence number                              
NMSTABL  EQU   *-NMSTABD                                                        
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Put network buy allocation and exploded buy record to buffers       *         
***********************************************************************         
                                                                                
PUTCUT   L     R2,LP_AINP                                                       
         USING NTWKELEM,R2         R2=A(network element)                        
                                                                                
         TM    MAP#MGAN,MAPSMGAN   Test makegood analysis                       
         JNZ   PUTCUT02            Yes - need all exploded buys                 
                                                                                
         CLI   NTWKLEN,NTWKFLG-NTWKELEM                                         
         JNH   EXITN                                                            
         TM    NTWKFLG,NTWKFLG_CUTIN                                            
         JZ    EXITN               Exit if not a cutin                          
                                                                                
         CLC   ALLNBYDA,IODA       Test have put network allocation             
         JE    PUTCUT02                                                         
         MVC   ALLNBYDA,IODA       No - build & add allocation record           
*&&DO                                                                           
*** THIS CODE IS NO LONGER NEEDED SINCE ALLOC SITS ONTOP OF ALLALLOC            
         LA    R0,ALLALLOC                                                      
         LHI   R1,L'ALLALLOC                                                    
         LA    RE,ALLOC                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*&&                                                                             
         GOTOR BUFFER,DMCB,('ALLBUFQ',TSAADD)                                   
         JE    PUTCUT02                                                         
         DC    H'0'                                                             
                                                                                
PUTCUT02 MVC   XPLMKST,NTWKMKST    Set market/station                           
         MVC   XPLMSSEQ,NMSSEQ     Set market/station sequence number           
         L     RF,ABUYREC                                                       
         MVC   XPLEST,BUYKEST-BUYREC(RF)                                        
         MVC   XPLLIN,BUYKBUY-BUYREC(RF)                                        
         MVC   XPLNBYDA,IODA       Disk address of network buy record           
         GOTOR BUFFER,DMCB,('XPLBUFQ',TSAADD)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    RUN#XPLA,RUNSXPLA   Set have added exploded buy record           
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
ARYNWK2  LKOUT A,(D,B#SAVED,YES),NROWS=1                                        
                                                                                
SNTWK    LKOUT C,1,YES,HDRO                                                     
                                                                                
         LKOUT E                                                                
                                                                                
TSTNWKY  TM    BUYFLAG,BUYFNTWF    Test first network element done              
         BNZR  RE                                                               
         OI    BUYFLAG,BUYFNTWF    Set first network element done               
         TM    BUYFLAG,BUYFNTWK    Set true if network is the same              
         J     SETCCC                                                           
                                                                                
ARYNWK3  LKOUT A,(D,B#WORKD,ELEM),ROWID=(NTWKELEM,NTWKCODQ),           +        
               ROWWIDTH=(V,NTWKLEN),NROWS=1                                     
                                                                                
SeqNo    LKOUT C,2,(D,B#SAVED,NMSSEQ),LBIN,ND=Y                                 
Markt    LKOUT C,3,(D,B#SAVED,NMSMKT),LBIN,ND=Y                                 
Statn    LKOUT C,4,(D,B#SAVED,NMSSTA),CHAR,ND=Y                                 
MktPc    LKOUT C,5,NTWKSHR,LBIN,ND=Y                                            
Flags    LKOUT C,6,NTWKFLG,HEXD,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
TSTNWKN  TM    BUYFLAG,BUYFNTWK    Set true if network is different             
         BR    RE                                                               
                                                                                
ARYSPL   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(NDELEM,NDCSPLQ),   +        
               ROWWIDTH=(V,NDLEN),NEWEL=B                                       
                                                                                
BkTyp    LKOUT C,1,NDBKTYPE,CHAR,ND=Y                                           
MktAl    LKOUT C,2,NDMKTALF,CHAR,ND=Y                                           
SCall    LKOUT C,3,NDSTA,CHAR,ND=Y                                              
RBook    LKOUT C,5,NDBOOK,BMON,ND=Y                                             
AgyMk    LKOUT C,7,NDAGYMKT,LBIN,ND=Y                                           
RtgSv    LKOUT C,8,NDRTGSVC,CHAR,ND=Y                                           
RSvMk    LKOUT C,9,NDRSMKT,LBIN,ND=Y                                            
PRout    LKOUT P,NDCODE,SETDEMEL                                                
Array    LKOUT C,10,(A,ARYDEM3)    Demo category/value/indicators               
                                                                                
         LKOUT E                                                                
                                                                                
ARYPBO   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(PDELEM,PDELEMQ),   +        
               ROWWIDTH=(V,PDLEN),NEWEL=Y                                       
                                                                                
Array    LKOUT C,1,(A,ARYPBO2),FILTROUT=SETPBOD                                 
                                                                                
         LKOUT E                                                                
                                                                                
SETDEMEL L     R2,LP_AINP                                                       
         MVC   BYTE4,0(R2)         SAVE ORIG OR SPILL DEMO ELEM CODE            
         J     EXIT                                                             
*                                                                               
SETPBOD  L     R2,LP_AINP                                                       
         USING PDELEM,R2                                                        
         MVC   ELEM,PDEMO          Extract demo values                          
         SR    R0,R0                                                            
         ICM   R0,1,PDLEN                                                       
         LR    R3,R0                                                            
         SHI   R0,PDEMO-PDELEM                                                  
         SRDA  R0,32                                                            
         LHI   RF,L'PDEMO                                                       
         DR    R0,RF                                                            
         AR    R2,R3                                                            
SETBPOD2 SHI   R2,L'PDEMO                                                       
         OC    0(L'PDEMO,R2),0(R2)                                              
         JNZ   *+8                                                              
         JCT   R1,SETBPOD2                                                      
         STH   R1,DNROWS           Set number of demos                          
         LTR   R1,R1                                                            
         J     SETCCC                                                           
         DROP  R2                                                               
                                                                                
ARYPBO2  LKOUT A,(D,B#WORKD,ELEM),ROWWIDTH=L'PDEMO,                    +        
               NROWS=(B#SAVED,DNROWS)                                           
                                                                                
DemoV    LKOUT C,3,ELEM,LBIN,LEN=L'PDEMO,FILTROUT=SETPBOV                       
DemoF    LKOUT C,4,(D,B#WORKD,BYTE1),HDRO,ND=Y                                  
DEMOF    LKOUT C,5,(D,B#WORKD,BYTE2),HDRO,ND=Y                                  
                                                                                
         LKOUT E                                                                
                                                                                
SETPBOV  L     R1,LP_AINP                                                       
         MVI   BYTE1,0                                                          
         TM    0(R1),X'80'         Test demo override                           
         JZ    SETPBOV5                                                         
         MVI   BYTE1,YESQ                                                       
         NI    0(R1),FF-X'80'                                                   
                                                                                
SETPBOV5 MVI   BYTE2,0                                                          
         OC    LP_VRSN,LP_VRSN          HAVE VERSION #?                         
         JZ    SETPBOV6                                                         
         CLC   LP_VRSN,=AL1(2,0,0,026)   VERSION 2.0.0.26 OR HIGHER?            
         JL    SETPBOV9                 NO, DON'T ADJUST                        
                                                                                
SETPBOV6 ICM   RF,15,LP_ATWA                                                    
         JZ    *+2                                                              
         CLI   SVS002DP-TWAD(RF),YESQ                                           
         JNE   SETPBOV9                                                         
         MVI   BYTE2,YESQ                                                       
         TM    0(R1),X'40'         TEST DEMO TO 2 DECIMAL PLACES                
         JO    SETPBOV8             YES, SKIP ADJUSTMENT                        
         SR    RF,RF                                                            
         ICM   RF,7,0(R1)          NO, LETS ADJUST IT                           
         MHI   RF,10                                                            
         STCM  RF,7,0(R1)                                                       
SETPBOV8 NI    0(R1),FF-X'40'                                                   
SETPBOV9 CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
                                                                                
ARYPBS   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(SDELEM,SDELEMQ),   +        
               ROWWIDTH=(V,SDLEN),NEWEL=B                                       
                                                                                
AgyMk    LKOUT C,1,SDAGYMKT,LBIN,FILTROUT=TSTPBS,SKIPCOLS=3                     
RtgMk    LKOUT C,2,SDRSVMKT,LBIN                                                
Array    LKOUT C,3,(A,ARYPBO2),FILTROUT=SETPBSD                                 
                                                                                
         LKOUT E                                                                
                                                                                
         USING SDELEM,R2                                                        
TSTPBS   L     R2,LP_AINP          A(POST BUY SPILL ELEM X'23')                 
         L     R1,ABUYREC          A(BUY RECORD)                                
         AHI   R1,BDELEM-BUYREC                                                 
TSTPBS10 CLI   0(R1),0             EOR?                                         
         JE    SETNEQ                                                           
         CLI   0(R1),NDCSPLQ       FIND SPILL ELEM 'X03'?                       
         JNE   TSTPBS20             NO                                          
         CLC   SDAGYMKT,NDAGYMKT-NDELEM(R1) DOES MARKET MATCH?                  
         JE    TSTPBS30                      YES                                
TSTPBS20 LLC   RF,1(R1)            CHECK NEXT ELEM                              
         LA    R1,0(R1,RF)                                                      
         J     TSTPBS10                                                         
TSTPBS30 MVC   ELEM2,0(R1)         FOUND MATCH, SAVE IN ELEM2                   
         BR    RE                                                               
         DROP  R2                                                               
*                                                                               
         USING SDELEM,R2                                                        
SETPBSD  L     R2,LP_AINP                                                       
         LLC   R0,SDLEN            GET LENGTH                                   
         SHI   R0,SDEMO-SDELEM     SUBTRACT OVERHEAD                            
         SRDA  R0,32               SHIFT VALUE TO R1                            
         LHI   RF,L'SDEMO          LENGTH OF EACH SDEMO ENTRY                   
         DR    R0,RF               DIVIDE, R1 = # OF DEMOS                      
                                                                                
         LA    R4,ELEM2+NDEMNO-NDELEM                                           
         LA    R3,ELEM                                                          
SETPBSD2 CLI   1(R4),C'I'          IF AUDIENCE/IMPRESSION                       
         JE    SETPBSD4            THEN DON'T D/L                               
         MVC   0(L'SDEMO,R3),SDEMO SET THE RATING VALUE                         
         LA    R3,L'SDEMO(R3)      BUMP THE OUTPUT                              
SETPBSD4 LA    R4,NDEMLNQ(R4)      BUMP THE SPILL ELEM                          
         LA    R2,L'SDEMO(R2)      BUMP THE INPUT                               
         JCT   R1,SETPBSD2         CHECK NEXT                                   
         DROP  R2                                                               
*                                                                               
         LR    R0,R3               COPY R3 INTO R0                              
         LA    RF,ELEM                                                          
         SR    R0,RF               GET LENGTH OF OUTPUT                         
         SRDA  R0,32               AND SHIFT IT TO R1                           
         LHI   RF,L'SDEMO          GET LENGTH OF EACH SDEMO ENTRY               
         DR    R0,RF               AND DIVIDE, R1 = # OF DEMOS                  
SETPBSD8 SHI   R3,L'SDEMO                                                       
         OC    0(L'SDEMO,R3),0(R3) GET NUMBER OF DEMOS TO OUTPUT                
         JNZ   *+8                                                              
         JCT   R1,SETPBSD8                                                      
         STH   R1,DNROWS           SET NUMBER OF DEMOS                          
         LTR   R1,R1               TEST NUMBER OF DEMOS TO OUTPUT               
         J     SETCCC                                                           
                                                                                
ARYTAX   LKOUT A,(D,B#BUYREC,BDELEM),NROWS=1,NEWEL=Y                            
                                                                                
Array    LKOUT C,1,(A,ARYNTX)      Tax amount element                           
Array    LKOUT C,2,(A,ARYGST)      GST code                                     
Array    LKOUT C,3,(A,ARYPST)      PST code                                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYNTX   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(NTWKCTAX,NTWKCTXQ),+        
               ROWWIDTH=(V,NTWKCTAX+1)                                          
                                                                                
TaxAm    LKOUT C,1,NTWKCTAX+2,LBIN,LEN=4                                        
                                                                                
         LKOUT E                                                                
                                                                                
ARYGST   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(VATELEM,VATELQ),   +        
               ROWWIDTH=(V,VATELEM+1)                                           
                                                                                
GSTCd    LKOUT C,2,VATSTA,CHAR                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYPST   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,ROWID=(PSTELEM,PSTELQ),   +        
               ROWWIDTH=(V,PSTELEM+1)                                           
                                                                                
Array    LKOUT C,3,(A,ARYPST1),FILTROUT=SETPST                                  
                                                                                
         LKOUT E                                                                
                                                                                
SETPST   L     R1,LP_AINP          Build province/pst code list                 
         AHI   R1,PSTVALS-PSTELEM                                               
         LHI   R0,L'PSTVALS                                                     
         LHI   RF,1                RF=province index                            
         SR    R2,R2               R2=row count                                 
         LA    R3,WORK             R3=A(output list)                            
SETPST2  CLI   0(R1),C'A'                                                       
         JNH   SETPST4                                                          
         STC   RF,0(R3)                                                         
         MVC   1(1,R3),0(R1)                                                    
         AHI   R3,2                                                             
         AHI   R2,1                                                             
SETPST4  AHI   R1,1                Bump to next PST code                        
         AHI   RF,1                Bump province index                          
         JCT   R0,SETPST2          Do for number of provinces                   
         STH   R2,DNROWS                                                        
         LTR   R2,R2                                                            
         J     SETCCC                                                           
                                                                                
ARYPST1  LKOUT A,(D,B#WORKD,WORK),NROWS=(B#SAVED,DNROWS),ROWWIDTH=2             
                                                                                
Index    LKOUT C,3,WORK+0,LBIN,LEN=1                                            
PSTCd    LKOUT C,4,WORK+1,CHAR,LEN=1                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYBUP   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(BUPELEM,BUPCODEQ),ROWWIDTH=(V,BUPLEN)                     
                                                                                
UpDat    LKOUT C,1,BUPDAT,BDAT,ND=Y                                             
UpInd    LKOUT C,2,BUPIND,HEXD,ND=Y                                             
UpUID    LKOUT C,3,BUPUID,CHAR,ND=Y                                             
UpTim    LKOUT C,4,BUPTIME,HEXD,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYBWS   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(BWSELEM,BWSCODEQ),ROWWIDTH=(V,BWSLEN)                     
                                                                                
BWByr    LKOUT C,1,BWSBYR,CHAR,ND=Y                                             
BWCam    LKOUT C,2,BWSCAM,LBIN,ND=Y                                             
BWDat    LKOUT C,3,BWSDATE,BDAT,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYBTR   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(BTRCELEM,BTRCCODQ),ROWWIDTH=(V,BTRCLEN)                   
                                                                                
BTSeq    LKOUT C,1,BTRCSEQN,LBIN,FILTROUT=TSTSEQN,ND=Y                          
BTSeq    LKOUT C,1,BTRCSEQN,HEXD,FILTROUT=TSTSEQH,ND=Y                          
BTDat    LKOUT C,2,BTRCDATE,BDAT,ND=Y                                           
BTTim    LKOUT C,3,BTRCTIME,HEXD,ND=Y                                           
BTTrm    LKOUT C,4,BTRCLUID,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
TSTSEQN  L     R1,LP_AINP          Set CC=equal if sequence number              
         OC    BTRCSEQN-BTRCELEM(2,R1),BTRCSEQN-BTRCELEM(R1)                    
         BR    RE                                                               
                                                                                
TSTSEQH  L     R1,LP_AINP          Set CC=equal if hex sequence code            
         OC    BTRCSEQN-BTRCELEM(2,R1),BTRCSEQN-BTRCELEM(R1)                    
         J     SETCCC                                                           
                                                                                
ARYACT   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(ACTVELEM,ACTVELQ),ROWWIDTH=(V,ACTVELEM+1)                 
                                                                                
AdWho    LKOUT C,1,ACTVADD,(R,EDTPID),LEN=2,ND=Y                                
WhoNM    LKOUT C,2,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
AdDat    LKOUT C,3,ACTVADD+L'ACTVADD,BDAT,LEN=3,ND=Y                            
ChWho    LKOUT C,4,ACTVCHG,(R,EDTPID),LEN=2,ND=Y                                
WhoCh    LKOUT C,5,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
ChDat    LKOUT C,6,ACTVCHG+L'ACTVCHG,BDAT,LEN=3,ND=Y                            
                                                                                
         LKOUT E                                                                
                                                                                
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
                                                                                
ARYMFX   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(MFXELEM,MFXCODEQ),ROWWIDTH=(V,MFXLEN)                     
                                                                                
MktNo    LKOUT C,1,MFXMKT,LBIN,ND=Y                                             
ChDat    LKOUT C,2,MFXDATE,CDAT,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYSFX   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(SFXELEM,SFXCODEQ),ROWWIDTH=(V,SFXLEN)                     
                                                                                
SCall    LKOUT C,1,SFXSTA,(R,EDTSTA),ND=Y                                       
ChDat    LKOUT C,2,SFXDATE,CDAT,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYORB   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(ORBELEM,ORBELEMQ),ROWWIDTH=(V,ORBLEN)                     
                                                                                
OrbDy    LKOUT C,1,ORBDAY,LBIN                                                  
OrbTm    LKOUT C,2,ORBTIME,HEXD                                                 
OrdDs    LKOUT C,3,ORBDESC,CHAR                                                 
OrdDm    LKOUT C,4,ORBDEM,LBIN                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYCLN   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(CRELEM,CRCODEQ),ROWWIDTH=(V,CRLEN)                        
                                                                                
CFKey    LKOUT C,1,CRCLKEY,HEXD                                                 
CFAgy    LKOUT C,2,CRALPHA,CHAR                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCUT   LKOUT A,(R,NXTCUT),MULTIROW=Y,ROWNAME=XPLREC                           
                                                                                
MktSt    LKOUT C,1,XPLMSSEQ,LBIN,FILTROUT=TSTMBDL                               
MktNo    LKOUT C,10,XPLMKT,LBIN,FILTROUT=TSTSBDL                                
MktNo    LKOUT C,11,XPLSTA,(R,EDTSTA),FILTROUT=TSTSBDL                          
BuyDA    LKOUT C,2,XPLNBYDA,HEXD                                                
Array    LKOUT C,3,(A,ARYALL)      Exploded buy allocations                     
                                                                                
         LKOUT E                                                                
                                                                                
TSTMBDL  CLI   QOPTOOPT,0          Set false if optimizer is off                
         BNER  RE                                                               
         TM    MAP#MBDL,MAPSMBDL   Set true if multiple buy download            
         J     SETCCC                                                           
                                                                                
TSTSBDL  CLI   QOPTOOPT,0          Set true if optimization is off              
         JNE   SETCCC                                                           
         TM    MAP#MBDL,MAPSMBDL   Set true if single buy download              
         BR    RE                                                               
                                                                                
***********************************************************************         
* Read cutin records                                                  *         
***********************************************************************         
                                                                                
NXTCUT   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTCUT02                                                         
                                                                                
         XC    ALLREC(ALLKEYL),ALLREC                                           
         XC    XPLREC(XPLKEYL),XPLREC                                           
         GOTOR BUFFER,DMCB,('XPLBUFQ',TSARDH)                                   
         J     NXTCUT04                                                         
                                                                                
NXTCUT02 GOTOR BUFFER,DMCB,('XPLBUFQ',TSANXT)                                   
                                                                                
NXTCUT04 TM    BUFFRET,TSEEOF      Test all entries processed                   
         JNZ   NOMORE                                                           
                                                                                
         LA    R2,IOKEY                                                         
         USING BUYREC,R2           Read exploded buy record                     
         XC    BUYKEY,BUYKEY                                                    
         MVC   BUYKAM,QMEDX                                                     
         MVC   BUYKCLT,QCLTX                                                    
         MVI   BUYKPRD,FF                                                       
         MVC   BUYKMSTA,XPLMKST                                                 
         MVC   BUYKEST,XPLEST                                                   
         MVC   BUYKBUY+1(2),XPLLIN                                              
***TBL   MVI   BUYKBUY+2,1                                                      
         MVI   GBYACT,GBYREAD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#BUYREC'                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#BUYREC'                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R0,XPLREC                                                        
         ST    R0,LP_ADATA         Point to exploded buy record                 
                                                                                
         CLC   ALLNBYDA,XPLNBYDA   Test have network allocation                 
         JE    EXITY                                                            
         MVC   ALLNBYDA,XPLNBYDA                                                
         GOTOR BUFFER,DMCB,('ALLBUFQ',TSARDH)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
ARYALL   LKOUT A,(R,GETALL),NROWS=(B#SAVED,DNROWS),                    +        
               ROWNAME=$ALD,ROWWIDTH=$ALDL                                      
                                                                                
SeqNo    LKOUT C,3,$ALSEQ#,LBIN                                                 
APrd1    LKOUT C,4,$ALPRD1,(U,#EDTPRD,$EDTPRD),ND=Y                             
APrd2    LKOUT C,5,$ALPRD2,(U,#EDTPRD,$EDTPRD),ND=Y                             
ALen1    LKOUT C,6,$ALLEN1,LBIN,ND=Y                                            
RepFc    LKOUT C,7,$ALREP#,LBIN,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Process exploded buy record/build allocation array for sending      *         
***********************************************************************         
                                                                                
GETALL   L     R1,ABUYREC                                                       
         AHI   R1,BDELEM-BUYREC                                                 
         USING REGELEM,R1          R1=A(spot element)                           
         L     R2,AIO6                                                          
         ST    R2,LP_ADATA                                                      
         USING $ALD,R2             R2=A(output allocation array)                
         XC    $ALD($ALDL),$ALD                                                 
         LA    RF,ALLALLOC                                                      
N        USING $ALPRDS,RF          (N=network buy spot allocation)              
         SR    R3,R3               R3=spot sequence number                      
         XC    DNROWS,DNROWS       Initialize array count                       
         SR    RE,RE               Initialize work registers                    
                                                                                
GETALL02 LLC   R0,RLEN             Bump to next element                         
         AR    R1,R0                                                            
         CLI   RCODE,EOR           Test end of record                           
         JE    EXITY                                                            
         CLI   RCODE,RCORGQ        Test spot element                            
         JL    GETALL02                                                         
         CLI   RCODE,X'0D'                                                      
         JH    GETALL02                                                         
                                                                                
         AHI   R3,1                Bump spot sequence number                    
X        USING $ALPRDS,WORK        (X=exploded buy spot allocation)             
         XC    X.$ALPRDS($ALPRDL),X.$ALPRDS                                     
         CLI   RLEN,RLPOL1LQ       Set exploded buy spot allocation             
         JL    GETALL04                                                         
         MVC   X.$ALPRD1,RPPRD                                                  
         CLI   RLEN,RLPOL2LQ                                                    
         JL    GETALL04                                                         
         MVC   X.$ALPRD2,RPALLOC2+(RPPRD-RPALLOC)                               
         CLC   RPTIME,RPALLOC2+(RPTIME-RPALLOC)                                 
         JE    GETALL04                                                         
         MVC   X.$ALLEN1,RPTIME                                                 
                                                                                
GETALL04 CLC   X.$ALPRDS($ALPRDL),N.$ALPRDS                                     
         JE    GETALL08                                                         
                                                                                
         CLC   $ALPRDS($ALPRDL),X.$ALPRDS                                       
         JNE   GETALL06                                                         
         ICM   RE,1,$ALSEQ#                                                     
         JZ    GETALL06                                                         
         LLC   R0,$ALREP#                                                       
         AHI   R0,1                                                             
         AR    RE,R0                                                            
         CR    R3,RE               Test spot in sequence                        
         JNE   GETALL06                                                         
         STC   R0,$ALREP#          Yes - bump replication factor                
         J     GETALL08                                                         
                                                                                
GETALL06 OC    DNROWS,DNROWS       Test first array entry                       
         JZ    *+8                                                              
         AHI   R2,$ALDL            No - advance array pointer                   
         STC   R3,$ALSEQ#                                                       
         MVC   $ALPRDS($ALPRDL),X.$ALPRDS                                       
         MVI   $ALREP#,0                                                        
         LH    RE,DNROWS           Increment array count                        
         AHI   RE,1                                                             
         STH   RE,DNROWS                                                        
                                                                                
GETALL08 AHI   RF,$ALPRDL          Bump to next network spot allocation         
         J     GETALL02                                                         
         DROP  R1,R2,N,X                                                        
                                                                                
$ALD     DSECT ,                   ** Allocation array **                       
$ALSEQ#  DS    X                   Spot sequence number                         
$ALPRDS  DS    0X                  ** Allocated products **                     
$ALPRD1  DS    XL(L'RPPRD)         Allocated product 1                          
$ALPRD2  DS    XL(L'RPPRD)         Allocated product 2                          
$ALLEN1  DS    XL(L'RPTIME)        Allocated product 1 length                   
$ALPRDL  EQU   *-$ALPRDS                                                        
$ALREP#  DS    X                   Replication factor                           
$ALDL    EQU   *-$ALD                                                           
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* ADD NEW STATION TO LIST OF STATIONS FOR READ FOR ROUTES DOWNLOAD    *         
***********************************************************************         
                                                                                
ADDSTA   STM   RE,R1,12(RD)                                                     
                                                                                
         ICM   RE,7,DASTA          RE=A(STATION LIST WMP ENTRY)                 
         JZ    ADDSTAX                                                          
                                                                                
         USING LW_D,RE                                                          
         LA    RF,LW_DATA2         POINT TO STATION LIST                        
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET ENTRY COUNT SO FAR                       
         JZ    ADDSTA04                                                         
ADDSTA02 CLC   0(L'BUYKSTA,RF),0(R1)                                            
         JE    ADDSTAX             EXIT IF ALREADY HAVE THIS STATION            
         AHI   RF,L'BUYKSTA                                                     
         JCT   R0,ADDSTA02                                                      
         ICM   R0,3,LW_NUMN        ADD NEW ENTRY TO END OF LIST                 
                                                                                
ADDSTA04 AHI   R0,1                BUMP ENTRY COUNT                             
         STCM  R0,3,LW_NUMN                                                     
         MVC   0(L'BUYKSTA,RF),0(R1)   SAVE THE ENTRY                           
         XC    L'BUYKSTA(L'BUYKSTA,RF),L'BUYKSTA(RF)  AND CLEAR NEXT            
                                                                                
ADDSTAX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* Pinergy multiple buy download X'0217'                               *         
***********************************************************************         
                                                                                
REQPIN   LKREQ H,I#CDPIND,OUTPIN,NEXTREQ=REQSBD                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
StrDt    LKREQ F,3,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,4,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
MktNo    LKREQ F,5,(I,B#SAVED,QMKTIND),LBIN,LIST=F,DEFAULT=Y,          +        
               OLEN=L'BUYKMKTN,TEXT=SP#MKT,COL=*                                
StaCd    LKREQ F,6,(I,B#SAVED,QSTAIND),(U,#VALSTA,$VALSTA),LIST=F,     +        
               DEFAULT=Y,OLEN=L'BUYKSTAC,TEXT=SP#STA,COL=*                      
EstNo    LKREQ F,7,(I,B#SAVED,QESTIND),LBIN,LIST=F,DEFAULT=NOT,RANGE=Y,+        
               OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                                 
OptOp    LKREQ F,8,(D,B#SAVED,QOPTOOPT),CHAR,TEXT=(*,OPTOLIT),COL=*             
ExclOp   LKREQ F,9,(D,B#SAVED,QEXCLOPT),CHAR,TEXT=(*,OPTOLIT),COL=*             
                                                                                
         LKREQ E                                                                
                                                                                
OUTPIN   LKOUT H                                                                
                                                                                
PINNBY   LKOUT R,5                 Get network buy records                      
PRout    LKOUT P,,GETCLV           Get client record                            
PRout    LKOUT P,,GETEST                                                        
*rray    LKOUT C,3,(A,GETESTS)                                                  
PRout    LKOUT P,,MQEND            Turn OFF MQ outflag flag                     
Array    LKOUT C,5,(A,ARYPNBY)                                                  
         LKOUT E                                                                
                                                                                
PINSBY   LKOUT R,5                 Get selective buy records                    
Array    LKOUT C,5,(A,ARYPSBY)                                                  
         LKOUT E                                                                
                                                                                
PINSTA   LKOUT R,12                Get Station list                             
PRout    LKOUT P,,MQSTART          Turn ON MQ outflag flag                      
Array    LKOUT C,12,(A,ARYSTA)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
*                                                                               
*ETESTS  LKOUT A,(R,GETEST),MULTIROW=Y,ROWNAME=ESTHDR                           
*        LKOUT E                                                                
                                                                                
ARYPNBY  LKOUT A,(R,NXTNBY),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
Array    LKOUT C,1,(A,ARYPBLD)      Buy line details                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYPSBY  LKOUT A,(R,NXTSBY),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
Array    LKOUT C,1,(A,ARYPBLD)      Buy line details                            
                                                                                
         LKOUT E                                                                
                                                                                
ARYPBLD  LKOUT A,(D,B#BUYREC,BUYREC),NROWS=1,ROWWIDTH=4000                      
                                                                                
StNet    LKOUT C,2,BUYKSTAC,(R,EDTNET),FILTROUT=TSTBSTA                         
EstNo    LKOUT C,3,BUYKEST,LBIN,FILTROUT=TSTBEST                                
LinNo    LKOUT C,4,BUYKBUY,LBIN,LEN=2                                           
                                                                                
Rotat    LKOUT C,15,BDDAY,LBIN                                                  
SecLn    LKOUT C,17,BDSEC,LBIN                                                  
DayPt    LKOUT C,20,BDDAYPT,CHAR,ND=Y                                           
STime    LKOUT C,21,BDTIMST,(R,EDTTIM)                                          
ETime    LKOUT C,22,BDTIMEND,(R,EDTTIM)                                         
PrGcd    LKOUT C,24,BDPROGRM,(R,EDTPRG)                                         
AdjCd    LKOUT C,25,BDPROGT,(R,EDTADJ),ND=Y                                     
SCost    LKOUT C,26,BDCOST,LBIN                                                 
CInd1    LKOUT C,27,BDCIND,HEXD,ND=Y                                            
CInd2    LKOUT C,37,BDCIND2,HEXD,ND=Y                                           
Buykey   LKOUT C,56,BUYKEY,HEXD,ND=Y                                            
PRout    LKOUT P,(B#LP_D,ABUYREC),PGETSUM                                       
PutCS    LKOUT C,6,(D,B#SAVED,LINCKSUM),(R,HXOLCS)                              
                                                                                
Array    LKOUT C,6,(A,ARYPSPT)     Spot details                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYPSPT  LKOUT A,(R,GETSPT),NROWS=(B#SAVED,DNROWS),NEWEL=B,            +        
               ROWNAME=$SDD,ROWWIDTH=$SDDDL                                     
                                                                                
SptDt    LKOUT C,1,$SDDATE,CDAT,ND=Y                                            
SStat    LKOUT C,2,$SDSTAT,HEXD                                                 
SPrd1    LKOUT C,3,$SDPRD1#,(U,#EDTPRD,$EDTPRD),ND=Y                            
SLen1    LKOUT C,4,$SDLEN1,LBIN,ND=Y                                            
SPrd2    LKOUT C,5,$SDPRD2#,(U,#EDTPRD,$EDTPRD),ND=Y                            
SLen2    LKOUT C,6,$SDLEN2,LBIN,ND=Y                                            
**SCost    LKOUT C,7,$SDCOST,LBIN,ND=Y                                          
SCost    LKOUT C,7,$SDCOST,(R,EDTSCST),ND=Y                                     
SAffD    LKOUT C,11,$SDADATE,CDAT,ND=Y                                          
SAffT    LKOUT C,12,$SDATIME,LBIN,ND=Y                                          
SADay    LKOUT C,13,$SDADAY,LBIN,ND=Y                                           
PutSCS   LKOUT C,34,$SCKSUM,(R,HXOSCS)                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYSTA   LKOUT A,(R,GETSTAL),ROWNAME=WORKD                                      
Station  LKOUT C,1,FULL,(R,EDTSTA),ND=Y                                         
         LKOUT E                                                                
                                                                                
MQSTART  OI    LP_OFLAG,LP_OFFMQ                                                
         BR    RE                                                               
                                                                                
MQEND    NI    LP_OFLAG,255-LP_OFFMQ                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* BUILD STATION LIST                                                            
***********************************************************************         
                                                                                
GETSTAL  CLI   LP_RMODE,LP_RFRST                                                
         JNE   GSTAL020                                                         
         ST    R0,LP_ADATA         SET A(DUMMY OUTPUT) IN CASE OF EXIT          
                                                                                
         ICM   R1,7,DASTA          Point to disk address in wmp                 
         JZ    NOMORE                                                           
         MVC   DSTAIND,LW_NUMN+1-LW_D(R1)                                       
         AHI   R1,LW_LN2Q                                                       
         STCM  R1,7,DASTA                                                       
                                                                                
GSTAL020 SR    R0,R0                                                            
         ICM   R0,1,DSTAIND        All done?                                    
         JZ    NOMORE                                                           
         SHI   R0,1                Decrement index count                        
         STC   R0,DSTAIND                                                       
         SR    R1,R1                                                            
         ICM   R1,7,DASTA                                                       
         XC    FULL,FULL                                                        
         MVC   FULL(3),0(R1)                                                    
         AHI   R1,3                                                             
         STCM  R1,7,DASTA                                                       
                                                                                
         LA    R0,WORKD                                                         
         ST    R0,LP_ADATA         Point to saved                               
         J     MORE                                                             
                                                                                
***********************************************************************         
* Desktop single buy download X'0208'                                 *         
***********************************************************************         
                                                                                
REQSBD   LKREQ H,I#CDSBDL,OUTSBD,NEXTREQ=REQNBD                                 
                                                                                
DskAd    LKREQ F,1,(I,B#SAVED,QRDAIND),HEXD,LIST=F,OLEN=L'IODAOVER,    +        
               SORT=N,TEXT=SP#SDBDA,COL=*                                       
OptOp    LKREQ F,2,(D,B#SAVED,QOPTOOPT),CHAR,TEXT=(*,OPTOLIT),COL=*             
                                                                                
         LKREQ E                                                                
                                                                                
OUTSBD   LKOUT H                                                                
                                                                                
SBDBUY   LKOUT R,1                 Single buy download                          
Array    LKOUT C,1,(A,ARYSBD)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYSBD   LKOUT A,(R,NXTSBD),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
Array    LKOUT C,5,(A,ARYNBY)      Network buy processing                       
Array    LKOUT C,5,(A,ARYSBY)      Selective buy processing                     
Array    LKOUT C,22,(A,ARYCUT),FILTROUT=TSTCUT                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get buy (and client) records for single buy download                *         
***********************************************************************         
                                                                                
NXTSBD   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTSBD20                                                         
                                                                                
         ICM   R1,7,QARDA          Point to disk address in wmp                 
         JZ    NOMORE                                                           
         CLI   LW_TYPE-LW_D(R1),LW_TLSTQ                                        
         JE    *+16                                                             
         MVI   QRDAIND,1                                                        
         AHI   R1,LW_LN1Q                                                       
         J     NXTSBD10                                                         
         MVC   QRDAIND,LW_NUMN+1-LW_D(R1)                                       
         AHI   R1,LW_LN2Q                                                       
                                                                                
NXTSBD10 STCM  R1,7,QARDA                                                       
                                                                                
NXTSBD20 SR    R0,R0                                                            
         ICM   R0,1,QRDAIND        All done?                                    
         JZ    NOMORE                                                           
         SHI   R0,1                Decrement index count                        
         STC   R0,QRDAIND                                                       
         ICM   R1,7,QARDA                                                       
         MVC   IODAOVER,0(R1)      Set disk address of next buy to send         
         MVC   FULL,IODAOVER                                                    
         AHI   R1,L'IODAOVER                                                    
         STCM  R1,7,QARDA                                                       
**       MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#BUYREC'                        
         JNE   NXTSBD20                                                         
                                                                                
         L     R2,ABUYREC                                                       
         ST    R2,LP_ADATA                                                      
         USING BUYREC,R2           R2=A(buy record)                             
         MVC   QMEDX,BUYKAM        Set media                                    
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         MVC   QCLTX,BUYKCLT       Set client                                   
         L     RF,ACLTREC                                                       
         CLC   BUYKAM(BUYKPRD-BUYKEY),CKEYAM-CLTRECD(RF)                        
         JE    NXTSBD40                                                         
         GOTOR (#GETCLT,AGETCLT)   Get client record                            
         JNE   NXTSBD20                                                         
*                                                                               
NXTSBD40 DS    0H                                                               
         MVC   GBY1OR2,SAVE1OR2    NEED TO SET GBY1OR2, I CALL GETBUY           
         MVC   SV1OR2,SAVE1OR2                                                  
         CLC   QCLTX,=X'CC2B'      CLIENT TBL?                                  
         JNE   NXTSBD60                                                         
         CLC   LP_AGY,=C'SJ'                                                    
         JNE   NXTSBD60                                                         
         MVI   GBY1OR2,2                                                        
         MVI   SV1OR2,2                                                         
NXTSBD60 MVC   GBYIOA,ABUYREC                                                   
         MVI   GBYACT,GBYCONV                                                   
         GOTOR VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
***NOP   MVI   QOPTOOPT,C'N'       Turn Optimizer off                           
         J     EXIT                Send buy record                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Desktop single network buy download X'0209'                         *         
***********************************************************************         
                                                                                
REQNBD   LKREQ H,I#CDSNDL,OUTNBD,NEXTREQ=REQSHW                                 
                                                                                
DskAd    LKREQ F,1,(I,B#SAVED,QRDAIND),HEXD,LIST=F,OLEN=L'IODAOVER,    +        
               SORT=N,TEXT=SP#SDBDA,COL=*                                       
OptOp    LKREQ F,2,(D,B#SAVED,QOPTOOPT),CHAR,TEXT=(*,OPTOLIT),COL=*             
                                                                                
         LKREQ E                                                                
                                                                                
OUTNBD   LKOUT H                                                                
                                                                                
NBDBUY   LKOUT R,1                 Single network buy record                    
Array    LKOUT C,1,(A,ARYNBD)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYNBD   LKOUT A,(R,NXTNBD),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
Array    LKOUT C,5,(A,ARYNBY)      Network buy processing                       
Array    LKOUT C,5,(A,ARYNWK)      Exploded buy processing                      
Array    LKOUT C,99,(A,ARYNWX)     End of network buy                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get buy (and client) records for single network buy download        *         
***********************************************************************         
                                                                                
NXTNBD   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTNBD20                                                         
                                                                                
         ICM   R1,7,QARDA          Point to disk address in wmp                 
         JZ    NOMORE                                                           
         CLI   LW_TYPE-LW_D(R1),LW_TLSTQ                                        
         JE    *+16                                                             
         MVI   QRDAIND,1                                                        
         AHI   R1,LW_LN1Q                                                       
         J     NXTNBD10                                                         
         MVC   QRDAIND,LW_NUMN+1-LW_D(R1)                                       
         AHI   R1,LW_LN2Q                                                       
                                                                                
NXTNBD10 STCM  R1,7,QARDA                                                       
                                                                                
NXTNBD20 SR    R0,R0                                                            
         ICM   R0,1,QRDAIND        All done?                                    
         JZ    NOMORE                                                           
         SHI   R0,1                Decrement index count                        
         STC   R0,QRDAIND                                                       
         ICM   R1,7,QARDA                                                       
         MVC   IODAOVER,0(R1)      Set disk address of next buy to send         
         MVC   FULL,IODAOVER                                                    
         AHI   R1,L'IODAOVER                                                    
         STCM  R1,7,QARDA                                                       
***      MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#BUYREC'                        
         JNE   NXTNBD20                                                         
                                                                                
         L     R2,ABUYREC                                                       
         ST    R2,LP_ADATA                                                      
         USING BUYREC,R2           R2=A(buy record)                             
         MVC   QMEDX,BUYKAM        Set media                                    
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         MVC   QCLTX,BUYKCLT       Set client                                   
         L     RF,ACLTREC                                                       
         CLC   BUYKAM(BUYKPRD-BUYKEY),CKEYAM-CLTRECD(RF)                        
         JE    NXTNBD40                                                         
         GOTOR (#GETCLT,AGETCLT)   Get client record                            
         JNE   NXTNBD20                                                         
*                                                                               
NXTNBD40 DS    0H                                                               
         MVC   GBY1OR2,SAVE1OR2    NEED TO SET GBY1OR2, I CALL GETBUY           
         MVC   SV1OR2,SAVE1OR2                                                  
         CLC   QCLTX,=X'CC2B'      CLIENT TBL?                                  
         JNE   NXTNBD60                                                         
         CLC   LP_AGY,=C'SJ'                                                    
         JNE   NXTNBD60                                                         
         MVI   GBY1OR2,2                                                        
         MVI   SV1OR2,2                                                         
NXTNBD60 MVC   GBYIOA,ABUYREC                                                   
         MVI   GBYACT,GBYCONV                                                   
         GOTOR VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
         J     EXIT                Send buy record                              
         DROP  R2                                                               
                                                                                
ARYNWK   LKOUT A,(R,NXTNWK),MULTIROW=Y                                          
                                                                                
Array    LKOUT C,5,(A,ARYSBY)      Selective buy download                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get exploded buys for a network buy                                 *         
***********************************************************************         
                                                                                
NXTNWK   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTNWK04                                                         
                                                                                
         L     RE,ABUYREC                                                       
         SR    R1,R1                                                            
         ICM   R1,3,BUYRLEN-BUYREC(RE)                                          
         AHI   R1,1                                                             
         L     R0,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               Save network buy record                      
                                                                                
         L     R2,AIO3                                                          
         USING BUYREC,R2                                                        
         LA    R1,BDELEM           Locate first pointer element                 
         USING NTWKELEM,R1                                                      
NXTNWK02 LLC   R0,NTWKLEN          Bump to next element                         
         AR    R1,R0                                                            
         CLI   NTWKCODE,EOR        Test end of record                           
         JE    NOMORE                                                           
         CLI   NTWKCODE,NTWKCODQ   Test network element                         
         JNE   NXTNWK02                                                         
         STCM  R1,15,ANWKELEM      Save A(current network element)              
         J     NXTNWK06                                                         
                                                                                
NXTNWK04 ICM   R1,15,ANWKELEM      Point to last network element                
         SR    R0,R0                                                            
         J     NXTNWK02            And find next                                
                                                                                
NXTNWK06 LA    R2,IOKEY                                                         
         USING BUYREC,R2           Build exploded buy key                       
         L     RF,AIO3                                                          
         MVC   BUYKEY(BUYKBUY-BUYKEY),0(RF)                                     
         MVC   BUYKMSTA,NTWKMKST                                                
         MVI   BUYKBUY,0                                                        
         MVC   BUYKBUY+1(L'BUYKBUY-1),BUYKBUY-BUYKEY(RF)                        
         MVI   GBYACT,GBYREAD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#BUYREC'                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#BUYREC'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   LP_ADATA,ABUYREC    Point to buy record                          
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
ARYNWX   LKOUT A,(D,B#SAVED,YES),NEWEL=Y,NROWS=1                                
                                                                                
Dummy    LKOUT C,1,(D,B#SAVED,YES),HDRO                                         
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop ShowCode browse X'020A'                                     *         
***********************************************************************         
                                                                                
REQSHW   LKREQ H,I#CDSHWB,OUTSHW,NEXTREQ=REQGVP                                 
                                                                                
NetWk    LKREQ F,1,(I,B#SAVED,QSTAIND),CHAR,LIST=F,OLEN=L'NPGMKNET,    +        
               DEFAULT=Y,TEXT=SP#NTWRK,COL=*                                    
ShwCd    LKREQ F,2,(I,B#SAVED,QSHWIND),CHAR,LIST=F,OLEN=L'NPGMKID,     +        
               DEFAULT=Y,TEXT=SP#PRG,COL=*                                      
Rottn    LKREQ F,3,(D,B#SAVED,QROTDAY),LBIN,TEXT=(*,ROTDLIT),COL=*              
StrTm    LKREQ F,4,(D,B#SAVED,QSTRTIME),LBIN,TEXT=(*,STRTLIT),COL=*             
EndTm    LKREQ F,5,(D,B#SAVED,QENDTIME),LBIN,TEXT=(*,ENDTLIT),COL=*             
DayPt    LKREQ F,6,(I,B#SAVED,QDPTIND),CHAR,LIST=F,DEFAULT=Y,          +        
               OLEN=L'NPGMDPT,TEXT=SP#DAYPT,COL=*                               
KillD    LKREQ F,7,(D,B#SAVED,QKILLDAT),CDAT,TEXT=(*,KILLLIT),COL=*             
CltCd    LKREQ F,8,(D,B#WORKD,QCLTX),(R,VALNCL),TEXT=SP#CLI,COL=*               
SChar    LKREQ F,9,(I,B#SAVED,QSTRIND),(R,VALSTR),OLEN=L'NPGMPGM+1,    +        
               LIST=F,TEXT=(*,STRILIT),COL=*                                    
                                                                                
         LKREQ E                                                                
                                                                                
OUTSHW   LKOUT H                                                                
                                                                                
SHWSHW   LKOUT R,1                 Show definition                              
Array    LKOUT C,1,(A,ARYSHW)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYSHW   LKOUT A,(R,NXTSHW),MULTIROW=Y,ROWNAME=NPGMRECD                         
                                                                                
NetWk    LKOUT C,1,NPGMKNET,CHAR                                                
ShwCd    LKOUT C,2,NPGMKID,CHAR                                                 
ProgN    LKOUT C,3,NPGMPGM,CHAR                                                 
Rottn    LKOUT C,4,NPGMDAY,LBIN                                                 
StrTm    LKOUT C,5,NPGMSTR,LBIN                                                 
EndTm    LKOUT C,6,NPGMEND,LBIN                                                 
DayPt    LKOUT C,7,NPGMDPT,CHAR                                                 
KillD    LKOUT C,8,NPGMKDAT,CDAT,ND=Y                                           
OOWRD    LKOUT C,9,NPGMOOWR,LBIN,ND=Y                                           
DskAd    LKOUT C,10,(D,B#SAVED,SHOWDA),HEXD                                     
Array    LKOUT C,11,(A,ARYADT)                                                  
Array    LKOUT C,15,(A,ARYDOV),FILTROUT=TSTDOV                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYADT   LKOUT A,(D,B#SHWREC,NPGMEL),EOT=EOR,                          +        
               ROWID=(ACTVD,ACTVEL2Q),ROWWIDTH=(V,ACTVLEN)                      
                                                                                
AddDt    LKOUT C,11,ACTVADDT,BDAT.ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
TSTDOV   OC    QCLTX,QCLTX         Test client given on request                 
         J     SETCCC                                                           
                                                                                
***********************************************************************         
* Get ShowCode records for ShowCode browse                            *         
***********************************************************************         
                                                                                
NXTSHW   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTSHW04                                                         
                                                                                
         OC    QENDTIME,QENDTIME                                                
         JNZ   *+10                                                             
         MVC   QENDTIME,EFFS       Set default end time                         
                                                                                
         MVC   QSTRTIMX,QENDTIME                                                
         MVC   QENDTIMS,QSTRTIME                                                
         CLC   QSTRTIME,QENDTIME                                                
         JNH   NXTSHW02                                                         
         LHI   R0,2400                                                          
         STCM  R0,3,QSTRTIMX                                                    
         XC    QENDTIMS,QENDTIMS                                                
                                                                                
NXTSHW02 MVI   QMEDA,NETMEDQ       Resolve network media                        
         GOTOR (#VALMED,AVALMED),DMCB,QMEDA,,QMEDX                              
         JNE   NOMORE                                                           
                                                                                
         OC    QCLTX,QCLTX         Resolve client (if requested)                
         JZ    NXTSHW04                                                         
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NOMORE                                                           
                                                                                
NXTSHW04 GOTOR (#NXTREC,ANXTREC),DMCB,SHWKEYT,('B#SHWREC',0),SAVED,0,0          
         JNE   EXITY                                                            
         MVC   SHOWDA,IODA         Set disk address of ShowDef record           
         L     R2,ASHWREC                                                       
         USING NPGMRECD,R2         R2=A(ShowCode record)                        
                                                                                
         CLI   QROTDAY,0           Filter on rotation days                      
         JE    *+14                                                             
         CLC   QROTDAY,NPGMDAY                                                  
         JNE   NXTSHW04                                                         
                                                                                
         CLC   NPGMSTR,QSTRTIME    Filter on start time                         
         JL    *+14                                                             
         CLC   NPGMSTR,QSTRTIMX                                                 
         JNH   NXTSHW06                                                         
                                                                                
         CLC   NPGMEND,QENDTIMS    Filter on end time                           
         JL    NXTSHW04                                                         
         CLC   NPGMEND,QENDTIME                                                 
         JH    NXTSHW04                                                         
                                                                                
NXTSHW06 OC    NPGMKDAT,NPGMKDAT   Filter on kill date                          
         JZ    *+14                                                             
         CLC   NPGMKDAT,QKILLDAT   Yes - test program is dead                   
         JNH   NXTSHW04                                                         
                                                                                
         GOTOR LP_ASETK,DMCB,(1,DPTFLT),NPGMDPT,SAVED,('FF',LP_D)               
         JNE   NXTSHW04                                                         
                                                                                
         ICM   R3,7,QASTR          Filter on search strings                     
         JZ    NXTSHW12                                                         
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R3)                                            
         AHI   R3,LW_LN2Q                                                       
                                                                                
NXTSHW08 LLC   R1,0(R3)            Get search string length-1                   
         LHI   RF,L'NPGMPGM                                                     
         SR    RF,R1               Rf=search count                              
         LA    R4,NPGMPGM                                                       
                                                                                
NXTSHW10 BASR  RE,0                                                             
         CLC   0(0,R4),1(R3)       Test for string match                        
         EX    R1,0(RE)                                                         
         JE    NXTSHW12                                                         
         AHI   R4,1                                                             
         JCT   RF,NXTSHW10                                                      
                                                                                
         AHI   R3,L'NPGMPGM+1                                                   
         JCT   R0,NXTSHW08                                                      
         J     NXTSHW04            No matches - get next string                 
                                                                                
NXTSHW12 J     EXITY                                                            
         DROP  R2                                                               
                                                                                
ARYDOV   LKOUT A,(R,GETDOV),ROWNAME=DOVRECD                                     
                                                                                
CltCd    LKOUT C,15,DOVKCLT,(U,#EDTCLT,$EDTCLT),ND=Y                            
DskAd    LKOUT C,16,(D,B#SAVED,DOVRDA),HEXD                                     
                                                                                
Array    LKOUT C,17,(A,ARYDOV1)    DOVEL01 values                               
Array    LKOUT C,17,(A,ARYDOV3)    DOVEL02 values                               
Array    LKOUT C,2,(A,ARYDOV5)     DOVEL05 values                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get DemoDef/DemOver records for ShowCode browse                     *         
***********************************************************************         
                                                                                
GETDOV   MVC   SVIOVALS,IOVALS     Save current i/o values                      
                                                                                
         LA    R2,IOKEY                                                         
         USING DOVRECD,R2          R2=A(demo override record)                   
         XC    DOVKEY,DOVKEY                                                    
                                                                                
         L     R3,ASHWREC                                                       
         USING NPGMRECD,R3         R3=A(ShowCode record)                        
                                                                                
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVC   STAPAGY,LP_AGY                                                   
         MVI   STAPACT,C'P'                                                     
         MVI   STAPMED,NETMEDQ                                                  
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,EZEROS                                                  
         MVC   STAPQSTA,SPACES                                                  
         MVC   STAPQSTA(L'NPGMKNET),NPGMKNET                                    
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   DOVKTYP+0,DOVTYPQ   Read DemoDef/DemOver record                  
         MVI   DOVKTYP+1,DOVSUBQ                                                
         MVC   DOVKAGMD,QMEDX                                                   
         MVC   DOVKNET,STAPSTA                                                  
         MVC   DOVKCLT,QCLTX                                                    
         MVC   DOVKPGM,NPGMKID                                                  
         L     RF,ACLTREC                                                       
         MVC   DOVKRTS,CPROF+3-CLTRECD(RF)                                      
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#DOVREC'                         
         JE    GETDOV02                                                         
         MVC   DOVKEY,IOKEYSAV                                                  
         XC    DOVKCLT,DOVKCLT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#DOVREC'                         
         JE    GETDOV02                                                         
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     NOMORE                                                           
                                                                                
GETDOV02 GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#DOVREC'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   DOVRDA,IODA         Set disk address of DemOver record           
         MVC   LP_ADATA,ADOVREC    Set A(record)                                
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
ARYDOV1  LKOUT A,(D,B#DOVREC,DOVEL01),EOT=EOR,ROWID=(DOVEL01,DOVEL01Q),+        
               ROWWIDTH=(V,DOVEL01+1)                                           
                                                                                
PRout    LKOUT P,DOVEL01,SETHDEM   Set number of demos                          
BaseB    LKOUT C,17,DOVBBK,BMON,ND=Y                                            
TillB    LKOUT C,18,DOVUTBK,BMON,ND=y                                           
Array    LKOUT C,19,(A,ARYDOV2)    Demo codes and values                        
                                                                                
         LKOUT E                                                                
                                                                                
SETHDEM  L     R1,LP_AINP          Set actual number of DemOver demos           
         AHI   R1,DOVDLSTC-DOVEL01                                              
         LHI   R0,DOVDMAXQ                                                      
         SR    RF,RF                                                            
SETHDEM2 OC    0(L'DOVDEMO,R1),0(R1)                                            
         JZ    SETHDEM4                                                         
         AHI   R1,L'DOVDEMO                                                     
         AHI   RF,1                                                             
         JCT   R0,SETHDEM2                                                      
SETHDEM4 STCM  RF,3,DOVDEMOS                                                    
         LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
ARYDOV2  LKOUT A,(*,DOVDLSTC),ROWNAME=DOVEL01,ROWWIDTH=L'DOVDEMO,      +        
               NROWS=L'DOVDLSTC/L'DOVDEMO                                       
                                                                                
DemCd    LKOUT C,19,DOVDEMO,(U,#EDTDCD,$EDTDCD),LEN=L'DOVDEMO,ND=Y              
PRout    LKOUT P,DOVDEMO,SETDFLAG  Set demo has values indicator                
IFlag    LKOUT C,20,(D,B#WORKD,BYTE1),HDRO,ND=Y                                 
                                                                                
         LKOUT E                                                                
                                                                                
SETDFLAG L     R1,LP_AINP          R1=A(DOVDEMO)                                
         MVI   BYTE1,0                                                          
         TM    0(R1),DOVDVALQ      Test demo has values                         
         JZ    EXITN                                                            
         MVI   BYTE1,YESQ          Yes - send map code                          
         J     EXITN                                                            
                                                                                
ARYDOV3  LKOUT A,(D,B#DOVREC,DOVEL01),EOT=EOR,ROWID=(DOVEL02,DOVEL02Q),+        
               ROWWIDTH=(V,DOVEL02+1)                                           
                                                                                
Array    LKOUT C,21,(A,ARYDOV4)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYDOV4  LKOUT A,(*,DOVIMPC),ROWNAME=DOVEL02,ROWWIDTH=L'DOVIMPC,NROWS=*         
                                                                                
DemCd    LKOUT C,21,DOVIMPDC,(U,#EDTDCD,$EDTDCD)                                
DemVl    LKOUT C,22,DOVIMPVC,LBIN                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYDOV5  LKOUT A,(D,B#DOVREC,DOVEL01),EOT=EOR,ROWID=(DOVEL05,DOVEL05Q),+        
               ROWWIDTH=(V,DOVEL05+1),NEWEL=B                                   
                                                                                
PRout    LKOUT P,DOVEL05,SETDOV3   Set number of demos                          
Statn    LKOUT C,23,(D,B#SAVED,DOVSTAC),CHAR,ND=Y                               
Markt    LKOUT C,24,(D,B#SAVED,DOVMKT#),CHAR,ND=Y                               
Array    LKOUT C,25,(A,ARYDOV6)    Demo values                                  
                                                                                
         LKOUT E                                                                
                                                                                
SETDOV3  L     R2,LP_AINP                                                       
         USING DOVEL05,R2          R2=A(station demo element)                   
         MVC   ELEM,DOVDEMV                                                     
         LLC   R0,1(R2)                                                         
         SHI   R0,DOVDEMV-DOVEL05                                               
         SRDA  R0,32                                                            
         LHI   RF,L'DOVDEMV                                                     
         DR    R0,RF                                                            
         CLM   R1,3,DOVDEMOS                                                    
         JNH   *+8                                                              
         ICM   R1,3,DOVDEMOS                                                    
         STH   R1,DNROWS           Set mumber of demos in element               
         XC    DOVV(DOVL),DOVV                                                  
         SR    R0,R0                                                            
         ICM   R0,7,DOVSTA                                                      
         CHI   R0,9999             Test market or station                       
         JH    SETDOV32                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DOVMKT#,DUB                                                      
         J     EXITN                                                            
                                                                                
SETDOV32 XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,QMEDA                                                    
         MVC   STAPACOM,ACOMFACS                                                
         STCM  R0,7,STAPSTA                                                     
         GOTOR VSTAPACK,STAPACKD                                                
         MVC   DOVSTAC,STAPQSTA                                                 
         CLI   DOVSTAC+3,C' '      Deal with 3 character station codes          
         JNE   EXITN                                                            
         MVC   DOVSTAC+3(L'STAPQSTA+L'STAPQNET-3),STAPQSTA+4                    
         MVI   DOVSTAC+L'DOVSTAC-1,C' '                                         
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
ARYDOV6  LKOUT A,(D,B#WORKD,ELEM),ROWWIDTH=L'DOVDEMV,                  +        
               NROWS=(B#SAVED,DNROWS),ROWNAME=DOVDEMV                           
                                                                                
DemVl    LKOUT C,25,DOVDEMV,LBIN,FILTROUT=TSTDEMV                               
DemLU    LKOUT C,26,(D,B#WORKD,BYTE1),HDRO,ND=Y                                 
                                                                                
         LKOUT E                                                                
                                                                                
TSTDEMV  L     R1,LP_AINP          R1=A(demo value)                             
         MVI   BYTE1,YESQ                                                       
         OC    0(L'DOVDEMV,R1),0(R1)                                            
         JZ    SETCCC              Don't send zeros (look-up values)            
         MVI   BYTE1,0                                                          
         TM    0(R1),X'80'                                                      
         BZR   RE                                                               
         NI    0(R1),FF-X'80'      Turn off 'zero demo' flag                    
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Desktop GvP report X'020B'                                          *         
***********************************************************************         
                                                                                
REQGVP   LKREQ H,I#CDGVPR,OUTGVP,NEXTREQ=REQMGA                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
StrDt    LKREQ F,3,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,4,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
MktNo    LKREQ F,5,(I,B#SAVED,QMKTIND),LBIN,LIST=F,DEFAULT=NOT,        +        
               OLEN=L'GKEYMKT,TEXT=SP#MKT,COL=*                                 
TypFt    LKREQ F,6,(D,B#SAVED,QTYPFLT),LBIN,TEXT=(*,TYPFLIT),COL=*              
SlnFt    LKREQ F,7,(I,B#SAVED,QSLNIND),LBIN,LIST=F,DEFAULT=Y,          +        
               OLEN=L'GKEYSLN,TEXT=(*,SLNFLIT),COL=*                            
DPTFt    LKREQ F,8,(I,B#SAVED,QDPTIND),CHAR,LIST=F,DEFAULT=Y,          +        
               OLEN=L'GKEYDPT,TEXT=(*,DPTFLIT),COL=*                            
DemCd    LKREQ F,9,(I,B#SAVED,QDEMIND),(U,#VALDCD,$VALDCD),            +        
               LIST=(F,NOS),MAXLIST=GPWRDMAX,TEXT=(*,DEMOLIT),         +        
               OLEN=L'NDEMNO,COL=*                                              
TotOnly  LKREQ F,10,(D,B#SAVED,QTOTONLY),CHAR,TEXT=(*,TOTOLIT),COL=*            
OOWRF    LKREQ F,11,(D,B#SAVED,OOWRFLAG),CHAR,TEXT=(*,TOTOLIT),COL=*            
                                                                                
         LKREQ E                                                                
                                                                                
OUTGVP   LKOUT H                                                                
                                                                                
GVPEST   LKOUT R,1                 Get and send estimate records                
Array    LKOUT C,1,(A,ARYGPE)                                                   
         LKOUT E                                                                
                                                                                
GVPPRD   LKOUT R,2                 Get and send product records                 
Array    LKOUT C,2,(A,ARYPRD)                                                   
         LKOUT E                                                                
                                                                                
GVPEQU   LKOUT R,3                 GvP report                                   
PRout    LKOUT P,,GETEQU           Build equivalency array                      
PRout    LKOUT P,,GETGPG           Get and post goals                           
PRout    LKOUT P,,GETGPB           Get and post buys/spill                      
Array    LKOUT C,3,(A,ARYGVP)      Send GvP report records                      
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYGPE   LKOUT A,(R,NXTGPE),MULTIROW=Y,ROWNAME=ESTHDR                           
                                                                                
EstNo    LKOUT C,1,EKEYEST,LBIN                                                 
EstNm    LKOUT C,2,EDESC,CHAR                                                   
EstSD    LKOUT C,3,ESTART,EDAT                                                  
EstED    LKOUT C,4,EEND,EDAT                                                    
EstFL    LKOUT C,5,EPROF,CHAR,LEN=3,ND=Y                                        
DayPt    LKOUT C,8,EDAYMENU,CHAR,ND=Y                                           
Array    LKOUT C,15,(A,ARYEPD)     Product/demo array                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read client and estimates for GvP report - build a list of          *         
* estimates for goal/buy reading and list of estimate/product/target  *         
* demos for buy demo processing                                       *         
***********************************************************************         
                                                                                
NXTGPE   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTGPE02                                                         
                                                                                
         LA    R0,GVPV             Clear work values                            
         LHI   R1,GVPL                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR ADDEPD,DMCB,EFFS,EFFS,EFFS                                       
                                                                                
         ICM   RF,7,QAMED          Initialize media                             
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         MVC   QMEDIA,QMEDX                                                     
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
                                                                                
         ICM   RF,7,QACLT          Initialize client                            
         MVC   QCLTX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#GETCLT,AGETCLT)   Read client record                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   QAEST,AALL          Initialize estimate                          
                                                                                
         MVC   DLODATEE,EFFS       Set low date to high value                   
         XC    QESTSSTR,QESTSSTR                                                
         MVC   QESTEEND,EFFS                                                    
         OC    QESTEDAT,QESTEDAT   Test end date provided                       
         JNZ   NXTGPE02                                                         
         MVC   QESTEDAT,EFFS       No - set high value                          
                                                                                
NXTGPE02 LARL  R0,BLDEPD           R0=A(key filter routine)                     
         GOTOR (#NXTREC,ANXTREC),DMCB,ESTKEYT,('B#ESTREC',0),SAVED,    +        
               (R0),0                                                           
         JNE   EXITY                                                            
         L     R2,AESTREC                                                       
         USING ESTHDR,R2           R2=a(estimate record)                        
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'EKEYEST,EKEYEST),DESTIND,DESTMAXQ,LP_D          
         GOTOR ADDEPD,DMCB,EKEYEST,EFFS,EDEMLIST                                
                                                                                
         CLC   ESTART,DLODATEE     Test lowest start date                       
         JNL   *+10                                                             
         MVC   DLODATEE,ESTART                                                  
         CLC   EEND,DHIDATEE       Test highest end date                        
         JNH   *+10                                                             
         MVC   DHIDATEE,EEND                                                    
                                                                                
         L     R3,FULL             R3=A(next entry in product array)            
         USING $PDD,R3             Build POL product entry                      
         MVC   $PDPRDC,EKEYPRD                                                  
         MVC   $PDTARG,EDEMLIST                                                 
         AHI   R3,$PDDL                                                         
         MVI   $PDD,EOR            Set end of array                             
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Read product estimates for a particular POL estimate, build a list  *         
* of products/target demos to send, build a list of products to read  *         
* goals and buys for and build a list of products to send product     *         
* records for                                                         *         
***********************************************************************         
                                                                                
         USING EPKEY,IOKEY                                                      
BLDEPD   MVC   SVIOVALS,IOVALS     Save current i/o values                      
         XC    EPKEYPRD,EPKEYPRD   Clear product (was 'POL')                    
         L     R3,AIO6                                                          
         USING $PDD,R3             R3=A(product/demo array)                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO7'                               
         J     BLDEPD04                                                         
                                                                                
BLDEPD02 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO7'                               
                                                                                
BLDEPD04 CLC   EPKEY(EPKEYPRD-EPKEY),IOKEYSAV                                   
         JNE   BLDEPD06                                                         
         CLC   EPKEYPRD,POLPRD     Ignore POL product                           
         JE    BLDEPD02                                                         
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'EPKEYPRD,EPKEYPRD),DPRCIND,DPRDMAXQ,   +        
               LP_D                                                             
                                                                                
         MVC   IODAOVER,EPKDA                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO7'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO7                                                          
         USING ESTHDR,R2           R2=A(estimate record)                        
                                                                                
         MVC   $PDPRDC,EKEYPRD     Add entry to product/demo array              
         MVC   $PDTARG,EDEMLIST                                                 
         AHI   R3,$PDDL                                                         
                                                                                
         GOTOR (#VALPRD,AVALPRD),DMCB,EKEYPRD,,WORK                             
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR LP_AAWMP,DMCB,(L'GKEYPRD,WORK),DPR#IND,DPRDMAXQ,LP_D             
         GOTOR ADDEPD,DMCB,EKEYEST,WORK,EDEMLIST                                
         J     BLDEPD02                                                         
                                                                                
BLDEPD06 MVC   IOVALS(IOVALL),SVIOVALS                                          
         C     R3,AIO6             Test any non-pol products                    
         JE    EXITN               No - don't want this one                     
         ST    R3,FULL             Save address of this entry                   
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
$PDD     DSECT ,                   ** Product/Demo array **                     
$PDPRDC  DS    CL(L'EKEYPRD)       Product code                                 
$PDTARG  DS    XL(L'EDEMLIST)      Target demo                                  
$PDDL    EQU   *-$PDD                                                           
SVRDEF   CSECT ,                                                                
                                                                                
ARYEPD   LKOUT A,(I,B#WORKD,AIO6),ROWNAME=$PDD,ROWWIDTH=$PDDL,EOT=EOR           
                                                                                
PrdCd    LKOUT C,15,$PDPRDC,CHAR                                                
DemCd    LKOUT C,17,$PDTARG,(U,#EDTDCD,$EDTDCD)                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get goal records for GvP report                                     *         
***********************************************************************         
                                                                                
GETGPG   CLC   DLODATEE,EFFS       Test any estimates found                     
         JE    EXITY                                                            
                                                                                
         OC    QESTSDAT,QESTSDAT   Test start date given                        
         JZ    GETGPG02                                                         
         GOTOR VDATCON,DMCB,(2,QESTSDAT),(0,DLODATEE)                           
                                                                                
GETGPG02 CLC   QESTEDAT,EFFS       Test end date given                          
         JE    GETGPG04                                                         
         GOTOR VDATCON,DMCB,(2,QESTEDAT),(0,DHIDATEE)                           
                                                                                
GETGPG04 GOTOR VGETDAY,DMCB,DLODATEE,WORK                                       
         CLI   0(R1),1             Test low date is a monday                    
         JE    GETGPG06                                                         
         SR    R0,R0               No - get date of previous monday             
         ICM   R0,1,0(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   R0,1                                                             
         LNR   R0,R0                                                            
         GOTOR VADDAY,DMCB,DLODATEE,WORK,(R0)                                   
         MVC   DLODATEE,WORK       DLODATEE now contains monday date            
                                                                                
GETGPG06 GOTOR VDATCON,DMCB,DLODATEE,(2,DLODATEC)                               
         GOTOR VDATCON,DMCB,DHIDATEE,(2,DHIDATEC)                               
                                                                                
         ICM   R1,7,QADEM          Calculate length of GPWREC in R0             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R1)                                            
         MHI   R0,GPWRDLNQ         N'requested demos*length of entry            
         AHI   R0,GPWRDEMO-GPWRECD +key overhead                                
         GOTOR BUFFER,DMCB,('GPWBUFQ',TSAINI),('GPWRKEYL',(R0))                 
         MVI   LP_RMODE,LP_RFRST   Set mode for NXTREC routine                  
         CLI   QTYPFLT,0           Test type filter set                         
         JE    GETGPG08                                                         
         TM    QTYPFLT,1           Test want goal records                       
         JZ    EXITY                                                            
                                                                                
GETGPG08 LARL  R0,FLTGOK           A=(Key filter routine)                       
         GOTOR (#NXTREC,ANXTREC),DMCB,GOLKEYT,('B#GOLREC',0),SAVED,    X        
               (R0),0                                                           
         JE    GETGPG12                                                         
                                                                                
         TM    RUN#GOAL,RUNSGOAL   Test any goal records found                  
         JNZ   EXITY               Yes - exit                                   
         TM    RUN#SWAP,RUNSSWAP   Test have swapped media already              
         JNZ   EXITY               Yes - exit                                   
*                                                                               
*  If no goals under original media T/N/C, try alternate media                  
*  Don't do this for media R/X                                                  
*                                                                               
*&&DO                                                                           
         CLI   QMEDA,RADMEDQ       C'R' - RADIO REQUESTED?                      
         JE    GETGPG10                                                         
         CLI   QMEDA,NTRMEDQ       C'X' -  OR NTWK RADIO?                       
         JE    GETGPG10                                                         
*&&                                                                             
         CLI   QMEDA,TELMEDQ       Test TV media request                        
         JE    *+12                                                             
         CLI   QMEDA,NETMEDQ       Or network media request                     
         JNE   *+12                                                             
         MVI   QMEDA,COMMEDQ       Yes - try combined                           
         J     GETGPG10                                                         
                                                                                
         CLI   QMEDA,COMMEDQ       Test combined media request                  
         JNE   EXITY                                                            
         MVI   QMEDA,NETMEDQ       Yes - try network                            
                                                                                
GETGPG10 GOTOR (#VALMED,AVALMED),DMCB,QMEDA,,QMEDIA                             
         JNE   EXITY                                                            
         MVI   LP_RMODE,LP_RFRST                                                
         OI    RUN#SWAP,RUNSSWAP   Set have swapped media code                  
         J     GETGPG08                                                         
                                                                                
GETGPG12 OI    RUN#GOAL,RUNSGOAL   Set goal record found                        
                                                                                
         L     R2,AGOLREC                                                       
         USING GOALRECD,R2         R2=A(goal record)                            
                                                                                
         USING GPWRECD,GPWREC                                                   
         LA    R0,GPWRECD                                                       
         LHI   R1,GPWRRECL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   GPWRMKT,GKEYMKT     Build key of GvP work record                 
         XC    GPWRSTA(L'GPWRSTA+L'GPWRNET),GPWRSTA                             
         MVC   GPWREST,GKEYEST                                                  
         MVC   GPWRDPT,GKEYDPT                                                  
         MVC   GPWRSLN,GKEYSLN                                                  
         MVC   GPWRSEC,GKEYSEC                                                  
         MVC   GPWRPRD,GKEYPRD                                                  
         TM    GKEYAGY,GKEY2NPQ                                                 
         JNZ   *+10                                                             
         MVC   GPWRPIG,GKEYPRD2                                                 
         MVI   GPWRTYPE,GPWRTGOL   Set this is a goal record                    
                                                                                
         LA    R2,GDELEM                                                        
         USING GLEMENT,R2          R2=A(first goal record element)              
         SR    R0,R0                                                            
GETGPG14 CLI   GLCODE,EOR          Test end of record                           
         JE    GETGPG08                                                         
         CLI   GLCODE,GLCODEQ      Test weekly element                          
         JNE   GETGPG20                                                         
         CLC   GLWEEK,DLODATEC     Test week in request period                  
         JL    GETGPG20                                                         
         CLC   GLWEEK,DHIDATEC                                                  
         JH    GETGPG20                                                         
                                                                                
         MVC   GPWRWKSD,GLWEEK     Set week start date                          
         CLI   QTOTONLY,C'Y'       Test sending totals only                     
         JNE   *+10                                                             
         MVC   GPWRWKSD,DLODATEC                                                
                                                                                
         OC    GLGRP,GLGRP         Test grp...                                  
         JZ    *+14                                                             
         OC    GLBUDGET,GLBUDGET   ...or dollar value missing                   
         JNZ   GETGPG16                                                         
         GOTOR GETCPP,GLEMENT      Yes - attempt look-up of CPP guide           
                                                                                
GETGPG16 ICM   R1,15,GLGRP                                                      
         CVD   R1,DUB                                                           
         ZAP   GPWRGRPS,DUB        Set goal grps                                
         ICM   R1,15,GLBUDGET                                                   
         CVD   R1,DUB                                                           
         ZAP   GPWRDOLS,DUB        Set goal dollars                             
                                                                                
         L     R0,AIO8             Copy record to IO8                           
         LHI   R1,GPWRRECL                                                      
         LA    RE,GPWRECD                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR BUFFER,DMCB,('GPWBUFQ',TSARDH)                                   
         JE    GETGPG18                                                         
                                                                                
         LA    R0,GPWRECD          Record not found in buffer so                
         LHI   R1,GPWRRECL         Restore record from IO8 & add it             
         L     RE,AIO8                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR BUFFER,DMCB,('GPWBUFQ',TSAADD)                                   
         J     GETGPG20                                                         
                                                                                
GETGPG18 L     R1,AIO8                                                          
N        USING GPWRECD,R1                                                       
         AP    GPWRDOLS,N.GPWRDOLS                                              
         AP    GPWRGRPS,N.GPWRGRPS                                              
         DROP  N                                                                
         GOTOR BUFFER,DMCB,('GPWBUFQ',TSAPUT)                                   
         JE    GETGPG20                                                         
         DC    H'0'                                                             
                                                                                
GETGPG20 LLC   R0,GLEN             Bump to next element on record               
         AR    R2,R0                                                            
         J     GETGPG14                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Get buy records for GvP report                                      *         
***********************************************************************         
                                                                                
GETGPB   CLC   DLODATEE,EFFS       Test any estimates found                     
         JE    EXITY                                                            
         CLI   QTYPFLT,0           Test type filter set                         
         JE    *+12                                                             
         TM    QTYPFLT,2           Test want buy records                        
         JZ    EXITY                                                            
                                                                                
         ICM   RE,7,QAMED          Set media                                    
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   QASTA,AALL          Set to read all stations                     
****                                                                            
         CLI   OOWRFLAG,C'Y'       Are we out of week?                          
         JE    *+10                                                             
         MVC   QESTSDAT,DLODATEC   No, use the Monday aligned date              
         GOTOR VDATCON,DMCB,(2,QESTSDAT),(3,BUYSDAT)                            
****                                                                            
         MVC   QESTEDAT,DHIDATEC                                                
         GOTOR VDATCON,DMCB,(2,QESTEDAT),(3,BUYEDAT)                            
                                                                                
         LA    R2,GVPWEEKS         Build a array of week start dates            
         LHI   R0,GVPWEEKN-3       Maximum number of weeks                      
         LHI   R3,3                Number of entries in GVPWEEKS                
         XC    0(L'GVPWEEKS,R2),0(R2)                                           
         AHI   R2,L'GVPWEEKS                                                    
         MVC   0(L'GVPWEEKS,R2),QESTSDAT                                        
         AHI   R2,L'GVPWEEKS                                                    
****                                                                            
         CLI   OOWRFLAG,C'Y'       Are we out of week?                          
         JE    GETGPB1W                                                         
         MVC   WORK(L'DLODATEE),DLODATEE                                        
         J     GETGPB02                                                         
*                                                                               
GETGPB1W GOTOR VDATCON,DMCB,(2,QESTSDAT),(0,WORK)                               
*                                                                               
GETGPB02 GOTOR VADDAY,DMCB,WORK,WORK+6,7                                        
         MVC   WORK(6),WORK+6                                                   
         GOTOR VDATCON,DMCB,WORK,(2,(R2))                                       
         CLC   0(L'GVPWEEKS,R2),QESTEDAT                                        
         JH    GETGPB04                                                         
         AHI   R2,L'GVPWEEKS       Bump to next week                            
         AHI   R3,1                Bump number of array entries                 
         JCT   R0,GETGPB02                                                      
         DC    H'0'                Request period too large                     
GETGPB04 MVC   0(L'GVPWEEKS,R2),EFFS                                            
         STH   R3,GVPWEEK#         Save number of array entries                 
                                                                                
         MVI   LP_RMODE,LP_RFRST   Set mode for nxtrec routine                  
                                                                                
GETGPB06 GOTOR (#NXTREC,ANXTREC),DMCB,SBYKEYT,('B#BUYREC',0),SAVED,0,0          
         JNE   EXITY                                                            
                                                                                
         L     R2,ABUYREC                                                       
         USING BUYREC,R2           R2=A(buy record)                             
         CLC   BDSTART,BUYEDAT     Test buy period overlaps request             
         JH    GETGPB06                                                         
         CLC   BDEND,BUYSDAT                                                    
         JL    GETGPB06                                                         
                                                                                
         GOTOR LP_ASETK,DMCB,(1,DPTFLT),BDDAYPT,SAVED,('FF',LP_D)               
         JNE   GETGPB06                                                         
                                                                                
         GOTOR LP_ASETK,DMCB,(1,SECFLT),BDSEC,SAVED,('FF',LP_D)                 
         JNE   GETGPB06                                                         
                                                                                
         LA    R0,GPWREC                                                        
         LHI   R1,GPWRRECL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               Initialize GvP work record                   
         MVC   GPWRMKT,BUYKMKT                                                  
         MVC   GPWRSTA,BUYKSTA                                                  
         GOTOR GETNET,BUYREC       Establish network code                       
         MVC   GPWRNET,WORK                                                     
         MVC   GPWREST,BUYKEST                                                  
         MVC   GPWRDPT,BDDAYPT                                                  
         MVC   GPWRSEC,BDSEC                                                    
         MVI   GPWRTYPE,GPWRTPUR   Set this is a purchased record               
                                                                                
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST         R1=cost (dollars or pennies)                 
         TM    BDCIND,BDCMINSQ     Test minus rate                              
         JZ    *+6                                                              
         LNR   R0,R0                                                            
                                                                                
         MVC   WORK(L'BUYKAM),BUYKAM                                            
         NI    WORK,X'0F'                                                       
*** CHECK REMOVED SINCE NETWORK EXPLODED BUYS CAN NOW BE IN DOLLARS             
*&&DO                                                                           
         CLI   WORK,NETNUMQ        Test (exploded) network buy                  
         JE    GETGPB10            Yes - they are always in pennies             
*&&                                                                             
         TM    BDCIND2,BDCNBRDQ    For other media default is pennies           
         JZ    GETGPB10            Unless the indicator says not                
                                                                                
GETGPB08 MHI   R0,100              Convert to pennies                           
                                                                                
GETGPB10 CVD   R0,DUB1             Save buy cost                                
                                                                                
         LA    R3,BDELEM                                                        
         USING NDELEM,R3           R3=A(demo element)                           
         XC    GVPADEM,GVPADEM     Locate demo element on record                
         MVI   BYTE1,NDCORGQ                                                    
         CLC   BUYKMKT,IOKEY+(BUYKMKT-BUYKEY)                                   
         JE    GETGPB12                                                         
         MVC   GPWRSPL,IOKEY+(BUYKMKT-BUYKEY)                                   
         MVI   BYTE1,NDCSPLQ                                                    
         MVI   GPWRTYPE,GPWRTSPL   Set spill record                             
GETGPB12 LLC   R0,NDLEN            Bump to next element                         
         AR    R3,R0                                                            
         CLI   NDCODE,EOR          Test end of record                           
         JE    GETGPB14                                                         
         CLC   NDCODE,BYTE1        Match on element code                        
         JNE   GETGPB12                                                         
         CLI   NDCODE,NDCORGQ      Test original demo                           
         JE    *+14                                                             
         CLC   NDAGYMKT,IOKEY+(BUYKMKT-BUYKEY)                                  
         JNE   GETGPB12                                                         
         ST    R3,GVPADEM          Save A(demo element)                         
                                                                                
GETGPB14 LA    R3,BDELEM                                                        
         USING REGELEM,R3          R3=A(spot element)                           
GETGPB16 LLC   R0,RLEN             Bump to next element                         
         AR    R3,R0                                                            
         CLI   RCODE,EOR           Test end of record                           
         JE    GETGPB06                                                         
         CLI   RCODE,RCORGQ        Test for spot element                        
         JL    GETGPB16                                                         
         CLI   RCODE,X'0D'                                                      
         JH    GETGPB16                                                         
         TM    RSTATUS,RSMINUSQ+RSMINSDQ                                        
         JNZ   GETGPB16            Drop minus/missed spots                      
         CLC   RDATE,QESTSDAT      Test spot in request period                  
         JL    GETGPB16                                                         
         CLC   RDATE,QESTEDAT                                                   
         JH    GETGPB16                                                         
         CLI   QTOTONLY,C'Y'       Test sending totals only                     
         JNE   *+14                                                             
         MVC   GPWRWKSD,DLODATEC                                                
         J     GETGPB18                                                         
                                                                                
         LH    R0,GVPWEEK#                                                      
         GOTOR VBINSRCH,DMCB,(2,RDATE),GVPWEEKS,(R0),L'GVPWEEKS,       +        
               L'GVPWEEKS,(R0)                                                  
         ICM   R1,7,1(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   RDATE,0(R1)         Test Monday spot                             
         JE    *+8                                                              
         SHI   R1,L'GVPWEEKS       No - back up to previous Monday date         
         OC    0(L'GVPWEEKS,R1),0(R1)                                           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   GPWRWKSD,0(R1)      Set week start date                          
                                                                                
GETGPB18 MVC   GPWRSLN,GPWRSEC                                                  
         MVI   GPWRPRD,POLPRDQ                                                  
         MVI   GPWRPIG,0                                                        
         ZAP   GPWRDOLS,PZERO                                                   
         XC    GPWRDEM#,GPWRDEM#                                                
                                                                                
         CLI   RLEN,RLPOL1LQ       Test allocated                               
         JL    GETGPB20                                                         
         MVC   GPWRPRD,RPPRD                                                    
                                                                                
         CLI   RLEN,RLPOL2LQ       Test piggyback allocation                    
         JL    GETGPB20                                                         
         MVC   GPWRSLN,RPTIME                                                   
         MVC   GPWRPIG,RPALLOC2+(RPPRD-RPALLOC)                                 
                                                                                
GETGPB20 ICM   RE,15,GVPADEM                                                    
         JZ    GETGPB40                                                         
         LLC   R4,NDLEN-NDELEM(RE)                                              
         SHI   R4,NDEMNO-NDELEM                                                 
         JZ    GETGPB40                                                         
         SRL   R4,3                                                             
         LA    R7,NDEMNO-NDELEM(RE)                                             
         USING NDEMNO,R7                                                        
         LA    R6,GPWRDEMO                                                      
         USING GPWRDEMO,R6                                                      
                                                                                
GETGPB22 OC    NDEMRAW,NDEMRAW     Don't send zero demo values                  
         JZ    GETGPB38                                                         
                                                                                
         OC    QADEM,QADEM         Test demo list requested                     
         JZ    GETGPB26                                                         
         SR    R1,R1                                                            
         ICM   R1,7,QADEM                                                       
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R1)                                            
         AHI   R1,LW_LN2Q                                                       
GETGPB24 CLC   NDEMNO,0(R1)        Test this demo requested                     
         JE    GETGPB28                                                         
         AHI   R1,L'NDEMNO                                                      
         JCT   R0,GETGPB24                                                      
         J     GETGPB38                                                         
                                                                                
GETGPB26 CLI   NDEMNO+1,C'R'       Only interested in demos                     
         JE    GETGPB28                                                         
         CLI   NDEMNO+1,C'E'       and extended demos                           
         JNE   GETGPB38                                                         
                                                                                
GETGPB28 MVC   GPWRDCOD,NDEMNO     Set demo code                                
         MVC   FULL,NDEMRAW                                                     
         NI    FULL,FF-(NDEMMANQ+NDEM2DEC)                                      
                                                                                
         ICM   R1,15,FULL          R1=DEMO VALUE                                
         JZ    GETGPB34             If zero, don't bother chk dec.prec.         
                                                                                
         L     RE,LP_ATWA                                                       
         TM    NDEMRAW,NDEM2DEC    Test demo to 2 decimal places                
         JNZ   GETGPB32                                                         
         CLI   SVS002DP-TWAD(RE),YESQ  AGENCY USES 2-DECIMAL?                   
         JNE   GETGPB34                                                         
         MHI   R1,10                                                            
***NOP   OI    NDEMRAW,NDEM2DEC    Don't touch bitflag in record unless         
         J     GETGPB34             we adjust record value as well              
                                                                                
GETGPB32 CLI   SVS002DP-TWAD(RE),YESQ  AGENCY USES 2-DECIMAL?                   
         JE    GETGPB34                 Yes, no need to convert                 
         SR    R0,R0                    No, convert to 1-decimal                
         SLDA  R0,1                                                             
         LHI   RE,10                                                            
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
GETGPB34 SR    RE,RE                                                            
         ICM   RE,1,NDSVI                                                       
         JZ    GETGPB36                                                         
         SR    R0,R0               Apply hut adjustment                         
         SLDA  R0,1                                                             
         MR    R0,RE                                                            
         LHI   RE,100                                                           
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
GETGPB36 CVD   R1,DUB2                                                          
         CP    DUB2,PZERO          Test demo grps are zero                      
         JE    GETGPB38                                                         
         ZAP   GPWRDGRP,DUB2       Set demo grp value                           
         SR    R0,R0                                                            
         ICM   R0,3,GPWRDEM#       Bump number of demos                         
         AHI   R0,1                                                             
         STCM  R0,3,GPWRDEM#                                                    
         AHI   R6,GPWRDLNQ         Bump output record pointer                   
                                                                                
GETGPB38 AHI   R7,NDEMLNQ          Bump to next demo                            
         JCT   R4,GETGPB22         Do for number of demos                       
         DROP  R6,R7                                                            
                                                                                
GETGPB40 CLI   GPWRTYPE,GPWRTSPL   Test spill record                            
         JNE   GETGPB42                                                         
         OC    GPWRDEM#,GPWRDEM#   Test any spill demos to post                 
         JNZ   GETGPB46                                                         
         J     GETGPB16                                                         
                                                                                
GETGPB42 ZAP   GPWRDOLS,DUB1       Set dollars                                  
         TM    RSTATUS,RSRATOVQ    Test cost override                           
         JZ    GETGPB44                                                         
         SR    R1,R1                                                            
         ICM   R1,7,RPCOST                                                      
         CVD   R1,DUB                                                           
         ZAP   GPWRDOLS,DUB        Set cost override dollars                    
                                                                                
GETGPB44 DS    0H  8/17/10-NOOP SO STATION WITH NO GRP/$ ARE STILL D/L          
*&&DO                                                                           
GETGPB44 CP    GPWRDOLS,PZERO      Test have cost to post                       
         JNE   GETGPB46                                                         
         OC    GPWRDEM#,GPWRDEM#   Test have demos to post                      
         JZ    GETGPB16                                                         
*&&                                                                             
GETGPB46 L     R0,AIO8             Copy record to IO8                           
         LHI   R1,GPWRRECL                                                      
         LA    RE,GPWRECD                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR BUFFER,DMCB,('GPWBUFQ',TSARDH)                                   
         JE    GETGPB48                                                         
                                                                                
         LA    R0,GPWRECD          Record not found in buffer so                
         LHI   R1,GPWRRECL         Restore record from IO8 & add it             
         L     RE,AIO8                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR BUFFER,DMCB,('GPWBUFQ',TSAADD)                                   
         JE    GETGPB16                                                         
         DC    H'0'                                                             
                                                                                
GETGPB48 L     R6,AIO8             Add in values from record in IO8             
N        USING GPWRECD,R6                                                       
         AP    GPWRDOLS,N.GPWRDOLS Add in dollars                               
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,N.GPWRDEM#     R0=number of demo values                     
         JZ    GETGPB58                                                         
         LA    R6,N.GPWRDEMO                                                    
N        USING GPWRDEMO,R6         R6=A(demo code/demo grp list)                
                                                                                
GETGPB50 LA    RF,GPWRDEMO                                                      
         USING GPWRDEMO,RF                                                      
         SR    RE,RE               Locate demo entry in buffer record           
         ICM   RE,3,GPWRDEM#                                                    
         JZ    GETGPB54                                                         
                                                                                
GETGPB52 CLC   GPWRDCOD,N.GPWRDCOD Match demo code                              
         JNE   *+14                                                             
         AP    GPWRDGRP,N.GPWRDGRP Yes - add in demo grp value                  
         J     GETGPB56                                                         
         AHI   RF,GPWRDLNQ         Bump to next demo in list                    
         JCT   RE,GETGPB52         Do for number of demos                       
                                                                                
GETGPB54 ICM   RE,3,GPWRDEM#       Add new demo to list                         
         AHI   RE,1                                                             
         CHI   RE,GPWRDMAX         Unless at the maximum demos allowed          
         JH    GETGPB56                                                         
         STCM  RE,3,GPWRDEM#       Set new number of demos                      
         MVC   GPWRDCOD,N.GPWRDCOD Set demo code                                
         ZAP   GPWRDGRP,N.GPWRDGRP Set demo grps                                
         DROP  RF                                                               
                                                                                
GETGPB56 AHI   R6,GPWRDLNQ         Bump to next demo in new record              
         JCT   R0,GETGPB50         Do for number of demos                       
                                                                                
GETGPB58 GOTOR BUFFER,DMCB,('GPWBUFQ',TSAPUT)                                   
         JE    GETGPB16                                                         
         DC    H'0'                                                             
         DROP  R2,R3,N                                                          
                                                                                
GPWRECD  DSECT ,                   ** GvP buffer record **                      
                                                                                
GPWRKEY  DS    0X                  ** Buffer key **                             
GPWRMKT  DS    XL(L'GKEYMKT)       Market number                                
GPWRSTA  DS    XL(L'BUYKSTA)       Station code                                 
GPWRNET  DS    CL4                 Network code (exploded buy)                  
GPWREST  DS    XL(L'GKEYEST)       Estimate number                              
GPWRDPT  DS    XL(L'GKEYDPT)       Daypart code                                 
GPWRSLN  DS    XL(L'GKEYSLN)       Spot seconds length                          
GPWRSEC  DS    XL(L'GKEYSEC)       Total seconds length                         
GPWRPRD  DS    XL(L'GKEYPRD)       Product number                               
GPWRPIG  DS    XL(L'GKEYPRD2)      Piggyback product number                     
GPWRSPL  DS    XL(L'GKEYMKT)       Spill market                                 
GPWRTYPE DS    X                   ** Record type **                            
GPWRTGOL EQU   3                   Goal values                                  
GPWRTPUR EQU   4                   Purchased values                             
GPWRTSPL EQU   5                   Spill values                                 
GPWRWKSD DS    XL(L'GLWEEK)        Week start date                              
GPWRKEYL EQU   *-GPWRKEY                                                        
                                                                                
GPWRDOLS DS    PL8                 Goal dollars                                 
GPWRGRPS DS    PL8                 Goal grps                                    
                                                                                
         ORG   GPWRGRPS                                                         
GPWRDEM# DS    AL2                 Number of demos in GPRWDEMO                  
                                                                                
GPWRDEMO DS    0X                  ** Buy/Spill demo code/grps **               
GPWRDCOD DS    XL3                 Demo code                                    
GPWRDGRP DS    PL8                 Demo grps                                    
GPWRDLNQ EQU   *-GPWRDEMO                                                       
GPWRDMAX EQU   32                  Maximum number of demos supported            
         DS    (GPWRDMAX-1)XL(GPWRDLNQ)                                         
GPWRDEML EQU   *-GPWRDEMO          Length of demos                              
GPWRRECL EQU   *-GPWRECD                                                        
SVRDEF   CSECT ,                                                                
                                                                                
ARYGVP   LKOUT A,(R,NXTGVP),MULTIROW=Y,ROWNAME=GVPRECD                          
                                                                                
MapCd    LKOUT C,0,(D,B#WORKD,HALF),EMAP,FILTROUT=SETGVPM                       
MktNo    LKOUT C,1,GVPRMKT,LBIN,ND=Y                                            
EstNo    LKOUT C,2,GVPREST,LBIN,ND=Y                                            
DptCd    LKOUT C,3,GVPRDPT,CHAR,ND=Y                                            
SptLn    LKOUT C,4,GVPRSLN,LBIN,ND=Y                                            
TotLn    LKOUT C,5,GVPRSEC,LBIN,ND=Y                                            
PrdCd    LKOUT C,6,GVPRPRD,(U,#EDTPRD,$EDTPRD),ND=Y                             
PigCd    LKOUT C,7,GVPRPIG,(U,#EDTPRD,$EDTPRD),ND=Y                             
StaCd    LKOUT C,8,GVPRSTA,(R,EDTSTA),ND=Y                                      
NetCd    LKOUT C,9,GVPRNET,CHAR,ND=Y                                            
Spill    LKOUT C,10,GVPRSPL,LBIN,ND=Y                                           
Array    LKOUT C,16,(A,ARYGPG),FILTROUT=TSTGVPG                                 
Array    LKOUT C,6,(A,ARYGPB),FILTROUT=TSTGVPB                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get GvP report buffer records and format for sending                *         
***********************************************************************         
                                                                                
NXTGVP   L     R2,AIO3             R2=A(output record)                          
         ST    R2,LP_ADATA                                                      
         USING GVPRECD,R2          Build a new record                           
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTGVP02                                                         
                                                                                
         CLC   DLODATEE,EFFS       Test any estimates found                     
         JE    NXTGVPNM                                                         
                                                                                
         XC    GPWREC(GPWRKEYL),GPWREC                                          
         GOTOR BUFFER,DMCB,('GPWBUFQ',TSARDH)                                   
         J     NXTGVP04                                                         
                                                                                
NXTGVP02 TM    GPWFLAG,GPWFEOF     Test end of file last time                   
         JNZ   NXTGVPNM                                                         
         TM    GPWFLAG,GPWFBUF     Test we have a record buffered               
         JNZ   NXTGVP06                                                         
         GOTOR BUFFER,DMCB,('GPWBUFQ',TSANXT)                                   
                                                                                
NXTGVP04 TM    BUFFRET,TSEEOF      Test all entries processed                   
         JNZ   NXTGVP26                                                         
         TM    GPWFLAG,GPWFANY     Test first time                              
         JNZ   NXTGVP12                                                         
         OI    GPWFLAG,GPWFANY     Set not first time                           
                                                                                
NXTGVP06 NI    GPWFLAG,FF-GPWFBUF  Create new GvP record                        
         MVC   GVPRKEY(GVPRKEYL),GPWRKEY                                        
         MVC   GVPRWKSD,GPWRWKSD                                                
         ZAP   GVPRDOLS,GPWRDOLS                                                
         LHI   R0,1                                                             
         STH   R0,DNROWS                                                        
         LA    R0,GVPRROW                                                       
         ST    R0,GVPAROW          Set A(current row)                           
         MVC   GVPWEEK,GVPRWKSD    Save current week                            
         MVI   GVPRREP#,0          Set no replication                           
         ZAP   GVPDOLS,GVPRDOLS    Save current dollars                         
                                                                                
         CLI   GVPRTYPE,GVPRTGOL   Test goal record                             
         JNE   NXTGVP08                                                         
         ZAP   GVPRGRPS,GPWRGRPS   Set goal demo grps                           
         ZAP   GVPGRPS,GVPRGRPS    Save current grps                            
         J     NXTGVP02                                                         
                                                                                
NXTGVP08 SR    R1,R1               Move demos to GvP record for                 
         ICM   R1,3,GPWRDEM#       Buy/spill records                            
         STCM  R1,3,GVPRDEM#                                                    
         MHI   R1,GVPRDLNQ                                                      
         LA    R0,GVPRDEMO                                                      
         LA    RE,GPWRDEMO                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         ICM   R1,3,GPWRDEM#       Save current demo values                     
         STCM  R1,3,GVPDEM#                                                     
         MHI   R1,GVPRDLNQ                                                      
         LA    R0,GVPDEMO                                                       
         LA    RE,GPWRDEMO                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     NXTGVP02                                                         
                                                                                
NXTGVP12 CLC   GVPRKEY(GVPRKEYL),GPWRKEY                                        
         JE    NXTGVP14                                                         
         MVC   WORK(GVPRKEYL),GVPRKEY                                           
R        USING GVPRECD,GVPLKEY                                                  
         CLC   GVPRMKT,R.GVPRMKT   Optimize key values sending                  
         JNE   *+10                                                             
         XC    GVPRMKT,GVPRMKT                                                  
         CLC   GVPRSTA(L'GVPRSTA+L'GVPRNET),R.GVPRSTA                           
         JNE   *+10                                                             
         XC    GVPRSTA(L'GVPRSTA+L'GVPRNET),R.GVPRSTA                           
         CLC   GVPREST,R.GVPREST                                                
         JNE   *+10                                                             
         XC    GVPREST,GVPREST                                                  
         CLC   GVPRDPT,R.GVPRDPT                                                
         JNE   *+10                                                             
         XC    GVPRDPT,GVPRDPT                                                  
         CLC   GVPRSLN,R.GVPRSLN                                                
         JNE   *+10                                                             
         XC    GVPRSLN,GVPRSLN                                                  
         CLC   GVPRSEC,R.GVPRSEC                                                
         JNE   *+10                                                             
         XC    GVPRSEC,GVPRSEC                                                  
         CLC   GVPRPRD(L'GVPRPRD+L'GVPRPIG),R.GVPRPRD                           
         JNE   *+10                                                             
         XC    GVPRPRD(L'GVPRPRD+L'GVPRPIG),GVPRPRD                             
         MVC   GVPLKEY,WORK        Restore saved key                            
         XC    GVPDOLS(L'GVPDOLS+L'GVPGRPS),GVPDOLS                             
         XC    GVPDEM#,GVPDEM#                                                  
         OI    GPWFLAG,GPWFBUF                                                  
         J     EXITY                                                            
         DROP  R                                                                
                                                                                
NXTGVP14 L     R3,GVPAROW          Point to current row                         
         USING GVPRROW,R3                                                       
         CLC   GPWRDOLS,GVPDOLS                                                 
         JNE   NXTGVP20                                                         
         CLI   GVPRTYPE,GVPRTGOL   Test goal record                             
         JNE   NXTGVP16                                                         
         CLC   GPWRGRPS,GVPGRPS    Match demo                                   
         JNE   NXTGVP20                                                         
         J     NXTGVP18                                                         
                                                                                
NXTGVP16 CLC   GPWRDEM#,GVPDEM#    Buy/spill - match demos                      
         JNE   NXTGVP20                                                         
         SR    R1,R1                                                            
         ICM   R1,3,GVPDEM#                                                     
         MHI   R1,GPWRDLNQ                                                      
         LA    R0,GPWRDEMO                                                      
         LA    RE,GVPDEMO                                                       
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         JNE   NXTGVP20                                                         
                                                                                
NXTGVP18 GOTOR VDATCON,DMCB,(2,GVPWEEK),WORK                                    
         GOTOR VADDAY,DMCB,WORK,WORK+6,7                                        
         GOTOR VDATCON,DMCB,WORK+6,(2,GVPWEEK)                                  
         CLC   GPWRWKSD,GVPWEEK    Test week in sequence                        
         JNE   NXTGVP20                                                         
         LLC   R0,GVPRREP#         Bump replication factor                      
         AHI   R0,1                                                             
         STC   R0,GVPRREP#                                                      
         J     NXTGVP02                                                         
                                                                                
NXTGVP20 AHI   R3,GVPRROWL         Build a new week row                         
         ST    R3,GVPAROW                                                       
         MVC   GVPRWKSD,GPWRWKSD                                                
         ZAP   GVPRDOLS,GPWRDOLS                                                
         MVI   GVPRREP#,0                                                       
         CP    GVPRDOLS,GVPDOLS                                                 
         JNE   *+14                                                             
         XC    GVPRDOLS,GVPRDOLS                                                
         J     *+10                                                             
         ZAP   GVPDOLS,GVPRDOLS                                                 
         CLI   GVPRTYPE,GVPRTGOL   Test goal record                             
         JNE   NXTGVP22                                                         
         ZAP   GVPRGRPS,GPWRGRPS                                                
         CP    GVPRGRPS,GVPGRPS                                                 
         JNE   *+14                                                             
         XC    GVPRGRPS,GVPRGRPS                                                
         J     *+10                                                             
         ZAP   GVPGRPS,GVPRGRPS                                                 
         J     NXTGVP24                                                         
                                                                                
NXTGVP22 SR    R1,R1               Move demos to gvp record for                 
         ICM   R1,3,GPWRDEM#       Buy/spill records                            
         STCM  R1,3,GVPRDEM#                                                    
         MHI   R1,GVPRDLNQ                                                      
         LA    R0,GVPRDEMO                                                      
         LA    RE,GPWRDEMO                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         ICM   R1,3,GPWRDEM#       Save current demo values                     
         STCM  R1,3,GVPDEM#                                                     
         MHI   R1,GVPRDLNQ                                                      
         LA    R0,GVPDEMO                                                       
         LA    RE,GPWRDEMO                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
NXTGVP24 MVC   GVPWEEK,GVPRWKSD    Save current week start date                 
         LH    R0,DNROWS           Bump number of rows                          
         AHI   R0,1                                                             
         STH   R0,DNROWS                                                        
         J     NXTGVP02                                                         
         DROP  R3                                                               
                                                                                
NXTGVP26 TM    GPWFLAG,GPWFANY     Test any records found                       
         JZ    NXTGVPNM                                                         
         OI    GPWFLAG,GPWFEOF     Set end of file encountered                  
         J     EXITY                                                            
*                                                                               
NXTGVPNM DS    0H                  TSAFRE not needed as we reuse area           
****     GOTOR BUFFER,DMCB,('GPWBUFQ',TSAFRE)                                   
         J     NOMORE                                                           
         DROP  R2                                                               
                                                                                
GVPRECD  DSECT ,                   ** GvP buffer record **                      
                                                                                
GVPRKEY  DS    0X                  ** Record key **                             
GVPRMKT  DS    XL(L'GPWRMKT)       Market number                                
GVPRSTA  DS    XL(L'GPWRSTA)       Station code (purchased only)                
GVPRNET  DS    CL(L'GPWRNET)       Network code (exploded buy)                  
GVPREST  DS    XL(L'GPWREST)       Estimate number                              
GVPRDPT  DS    XL(L'GPWRDPT)       Daypart code                                 
GVPRSLN  DS    XL(L'GPWRSLN)       Spot seconds length                          
GVPRSEC  DS    XL(L'GPWRSEC)       Total seconds length                         
GVPRPRD  DS    XL(L'GPWRPRD)       Product number                               
GVPRPIG  DS    XL(L'GPWRPIG)       Piggyback product number                     
GVPRSPL  DS    XL(L'GKEYMKT)       Spill market                                 
GVPRTYPE DS    XL(L'GPWRTYPE)      ** Record type (goal/purchased) **           
GVPRTGOL EQU   GPWRTGOL            Goal values                                  
GVPRTPUR EQU   GPWRTPUR            Purchased values                             
GVPRTSPL EQU   GPWRTSPL            Spill values                                 
GVPRKEYL EQU   *-GVPRECD                                                        
                                                                                
GVPRROW  DS    0X                  ** Week row **                               
GVPRWKSD DS    XL(L'GPWRWKSD)      Week start date                              
GVPRREP# DS    X                   Replication factor                           
GVPRDOLS DS    PL(L'GPWRDOLS)      Goal dollars                                 
GVPRGRPS DS    PL(L'GPWRGRPS)      Goal grps                                    
                                                                                
         ORG   GVPRGRPS                                                         
GVPRDEM# DS    AL2                 Number of demos in GVPRDEMO                  
                                                                                
GVPRDEMO DS    0X                  ** Buy/Spill demo code/grps **               
GVPRDCOD DS    XL(L'GPWRDCOD)      Demo code                                    
GVPRDGRP DS    PL(L'GPWRDGRP)      Demo grps                                    
GVPRDLNQ EQU   *-GVPRDEMO                                                       
GVPRDMAX EQU   32                  Maximum number of demos supported            
         DS    (GVPRDMAX-1)XL(GVPRDLNQ)                                         
GVPRROWL EQU   *-GVPRROW                                                        
SVRDEF   CSECT ,                                                                
                                                                                
SETGVPM  L     R1,LP_AINP          set map code for GvP download record         
         LLC   R0,GVPRTYPE-GVPRECD(R1)                                          
         STH   R0,HALF                                                          
         BR    RE                                                               
                                                                                
***********************************************************************         
* GvP weekly values for Goal records                                  *         
***********************************************************************         
                                                                                
ARYGPG   LKOUT A,(*,GVPRROW),ROWNAME=GVPRECD,ROWWIDTH=GVPRROWL,        +        
               NROWS=(B#SAVED,DNROWS)                                           
                                                                                
WSDat    LKOUT C,16,GVPRWKSD,CDAT                                               
GGRPs    LKOUT C,17,GVPRGRPS,SPAK,ND=Y                                          
GDlrs    LKOUT C,18,GVPRDOLS,SPAK,ND=Y                                          
RepFc    LKOUT C,19,GVPRREP#,LBIN,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
TSTGVPG  L     R1,LP_AINP          Set CC=equal if goal record                  
         CLI   GVPRTYPE-GVPRECD(R1),GVPRTGOL                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* GvP weekly values for Buy/Spill records                             *         
***********************************************************************         
                                                                                
ARYGPB   LKOUT A,(*,GVPRROW),ROWNAME=GVPRECD,ROWWIDTH=GVPRROWL,        +        
               NROWS=(B#SAVED,DNROWS),NEWEL=B                                   
                                                                                
WSDat    LKOUT C,1,GVPRWKSD,CDAT                                                
GDlrs    LKOUT C,2,GVPRDOLS,SPAK,ND=Y                                           
RepFc    LKOUT C,3,GVPRREP#,LBIN,ND=Y                                           
Array    LKOUT C,4,(A,ARYGPD),FILTROUT=SETNDEM                                  
                                                                                
         LKOUT E                                                                
                                                                                
TSTGVPB  L     R1,LP_AINP          Set CC=equal if buy/spill record             
         CLI   GVPRTYPE-GVPRECD(R1),GVPRTPUR                                    
         BER   RE                                                               
         CLI   GVPRTYPE-GVPRECD(R1),GVPRTSPL                                    
         BR    RE                                                               
                                                                                
SETNDEM  L     R1,LP_AINP          Set number of demos in week row              
         MVC   HALF,GVPRDEM#-GVPRROW(R1)                                        
         BR    RE                                                               
                                                                                
ARYGPD   LKOUT A,(*,GVPRDEMO),ROWNAME=GVPRROW,ROWWIDTH=GVPRDLNQ,       +        
               NROWS=(B#WORKD,HALF)                                             
                                                                                
DemCd    LKOUT C,4,GVPRDCOD,(U,#EDTDCD,$EDTDCD)                                 
DemVl    LKOUT C,5,GVPRDGRP,SPAK,ND=Y                                           
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop makegood analysis X'020C'                                   *         
***********************************************************************         
                                                                                
REQMGA   LKREQ H,I#CDMGAN,OUTMGA,NEXTREQ=REQDOV                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
CLTCD    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
EstNo    LKREQ F,3,(I,B#SAVED,QESTIND),LBIN,LIST=F,DEFAULT=NOT,RANGE=Y,+        
               OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                                 
MktNo    LKREQ F,4,(I,B#SAVED,QMKTIND),LBIN,LIST=F,DEFAULT=Y,          +        
               OLEN=L'BUYKMKTN,TEXT=SP#MKT,COL=*                                
StNet    LKREQ F,5,(I,B#SAVED,QSTAIND),(U,#VALSTA,$VALSTA),DEFAULT=Y,  +        
               OLEN=L'BUYKSTAC,TEXT=SP#STA,LIST=NOD,COL=*                       
                                                                                
         LKREQ E                                                                
                                                                                
OUTMGA   LKOUT H                                                                
                                                                                
MGANBY   LKOUT R,5                 Get network buy records                      
PRout    LKOUT P,,INIMGA           Initialize for makegood analysis             
Array    LKOUT C,5,(A,ARYNBY)                                                   
         LKOUT E                                                                
                                                                                
MGAXPL   LKOUT R,22                Get exploded network buy records             
Array    LKOUT C,22,(A,ARYMGX)                                                  
         LKOUT E                                                                
                                                                                
MGASBY   LKOUT R,5                 Get selective buy records                    
Array    LKOUT C,5,(A,ARYSBY)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* Initialize run variables for makegood analysis                      *         
***********************************************************************         
                                                                                
INIMGA   MVC   DAEST,QAEST                                                      
         XC    QESTSDAT,QESTSDAT                                                
         MVC   QESTEDAT,EFFS                                                    
         ICM   RE,7,QAMED                                                       
         JZ    QERROR                                                           
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         MVC   QMEDIA,QMEDA                                                     
         ICM   RE,7,QACLT                                                       
         JZ    QERROR                                                           
         MVC   QCLTX,LW_DATA1-LW_D(RE)                                          
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   QERROR                                                           
         GOTOR (#EDTCLT,AEDTCLT),DMCB,QCLTX,,QCLTA                              
         J     NOMORE                                                           
                                                                                
ARYMGX   LKOUT A,(R,NXTMGX),MULTIROW=Y,ROWNAME=XPLREC                           
                                                                                
MktSt    LKOUT C,1,XPLMSSEQ,LBIN                                                
BuyDA    LKOUT C,2,XPLNBYDA,HEXD                                                
                                                                                
Array    LKOUT C,7,(A,ARYDEM)      Original demo values                         
Array    LKOUT C,10,(A,ARYPBO)     Post buy original demo values                
Array    LKOUT C,9,(A,ARYSPL)      Spill demos values                           
Array    LKOUT C,11,(A,ARYPBS)     Post buy spill demo values                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read and send exploded network buy records for makegood analysis    *         
***********************************************************************         
                                                                                
NXTMGX   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTMGX02                                                         
         TM    RUN#XPLA,RUNSXPLA   Test any exploded records added              
         JZ    NOMORE                                                           
                                                                                
         XC    XPLREC,XPLREC                                                    
         GOTOR BUFFER,DMCB,('XPLBUFQ',TSARDH)                                   
         J     NXTMGX04                                                         
                                                                                
NXTMGX02 GOTOR BUFFER,DMCB,('XPLBUFQ',TSANXT)                                   
                                                                                
NXTMGX04 TM    BUFFRET,TSEEOF      Test all entries processed                   
         JNZ   NOMORE                                                           
                                                                                
         LA    R2,IOKEY                                                         
         USING BUYREC,R2           Read exploded buy record                     
         XC    BUYKEY,BUYKEY                                                    
         MVC   BUYKAM,QMEDX                                                     
         MVC   BUYKCLT,QCLTX                                                    
         MVI   BUYKPRD,FF                                                       
         MVC   BUYKMSTA,XPLMKST                                                 
         MVC   BUYKEST,XPLEST                                                   
         MVC   BUYKBUY+1(2),XPLLIN                                              
***TBL   MVI   BUYKBUY+2,1                                                      
         MVI   GBYACT,GBYREAD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#BUYREC'                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#BUYREC'                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R0,XPLREC                                                        
         ST    R0,LP_ADATA         Point to exploded buy record                 
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Desktop ShowDef/DemoDef/DemOver record maintenance X'020D'          *         
***********************************************************************         
                                                                                
REQDOV   LKREQ H,I#CDSDDM,OUTSDD,NEXTREQ=REQPRB                                 
                                                                                
DovDA    LKREQ F,1,(D,B#SAVED,QDA),HEXD,TEXT=(*,DOVDLIT),COL=*                  
LiveS    LKREQ F,2,(D,B#SAVED,QLIVE),CHAR,TEXT=(*,LIVELIT),COL=*                
MedCd    LKREQ F,3,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,4,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
Netwk    LKREQ F,5,(I,B#SAVED,QSTAIND),CHAR,OLEN=L'NPGMKNET,           +        
               TEXT=SP#NTWRK,COL=*                                              
ShwCd    LKREQ F,6,(I,B#SAVED,QSHWIND),CHAR,OLEN=L'NPGMKID,            +        
               TEXT=SP#PRG,COL=*                                                
RtgSv    LKREQ F,7,(D,B#SAVED,QRTGSVC),CHAR,TEXT=(*,RTGSLIT),COL=*              
PgmNm    LKREQ F,8,(I,B#SAVED,QPNDIND),CHAR,OLEN=L'NPGMPGM,            +        
               TEXT=(*,PGMDLIT),COL=*                                           
Rottn    LKREQ F,9,(D,B#SAVED,QROTDAY),LBIN,TEXT=(*,ROTDLIT),COL=*              
OOWRD    LKREQ F,10,(D,B#SAVED,QOOWDAY),LBIN,TEXT=(*,OOWDLIT),COL=*             
StrTm    LKREQ F,11,(D,B#SAVED,QSTRTIME),LBIN,TEXT=(*,STRTLIT),COL=*            
EndTm    LKREQ F,12,(D,B#SAVED,QENDTIME),LBIN,TEXT=(*,ENDTLIT),COL=*            
DayPt    LKREQ F,13,(D,B#SAVED,QDAYPART),CHAR,TEXT=SP#DAYPT,COL=*               
KillD    LKREQ F,14,(D,B#SAVED,QKILLDAT),CDAT,TEXT=(*,KILLLIT),COL=*            
BBook    LKREQ F,15,(D,B#SAVED,QBBOOK),BMON,TEXT=(*,BBOKLIT),COL=*              
UBook    LKREQ F,16,(D,B#SAVED,QUTBOOK),BMON,TEXT=(*,UBOKLIT),COL=*             
                                                                                
DemCd    LKREQ F,17,(I,B#SAVED,QDEMIND),(U,#VALDCD,$VALDCD),ARRAY=S,   +        
               SORT=N,OLEN=L'DOVDEMO,TEXT=(*,DEMOLIT),COL=*                     
DemFl    LKREQ F,18,,HDRO,OLEN=1,TEXT=(*,DEMFLIT),ARRAY=E,COL=*                 
                                                                                
NADem    LKREQ F,19,(I,B#SAVED,QNADIND),(U,#VALDCD,$VALDCD),ARRAY=S,   +        
               SORT=N,OLEN=L'DOVIMPDC,TEXT=(*,NADDLIT),COL=*                    
NADVl    LKREQ F,20,,LBIN,OLEN=L'DOVIMPVC,TEXT=(*,NADVLIT),ARRAY=E              
                                                                                
StMkt    LKREQ F,21,(I,B#SAVED,QSMDIND),(R,VALSTM),ARRAY=S,            +        
               OLEN=L'DSDSTMK+L'DSDNETW,SORT=N,TEXT=(*,STMKLIT),COL=*           
DemVl    LKREQ F,22,,(R,VALDEM),OLEN=L'DSDDEMVL,ENTRIES=DSDDEMMX,      +        
               ECOUNT=Y,ARRAY=E,TEXT=(*,DEMVLIT),COL=*                          
                                                                                
         LKREQ E                                                                
                                                                                
OUTSDD   LKOUT H                                                                
                                                                                
SDDVAL   LKOUT R,1                                                              
PRout    LKOUT P,,UPLSDD                                                        
ShwCd    LKOUT C,1,(D,B#SAVED,SHOWCODE),CHAR,ND=Y                               
ShwDA    LKOUT C,2,(D,B#SAVED,SHOWDA),HEXD,ND=Y                                 
DovDA    LKOUT C,3,(D,B#SAVED,DOVRDA),HEXD,ND=Y                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* Upload ShowDef/DemoDef/DemOver records                              *         
***********************************************************************         
                                                                                
UPLSDD   XC    SHOWVALS(SHOWVALL),SHOWVALS                                      
                                                                                
         ICM   RE,7,QAMED          Extract media code                           
         JZ    *+2                                                              
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
                                                                                
         OC    QDA,QDA             Test change DemoDef/DemOver record           
         JZ    UPLSDD02            (no ShowCode maintenance)                    
         MVC   IODAOVER,QDA        Read existing DemoDef/DemOver record         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOFIL+B#DOVREC'                      
         JNE   *+2                                                              
         L     R2,ADOVREC                                                       
         USING DOVRECD,R2                                                       
*                                                                               
* MAKE SURE WE HAVE THE RIGHT RECORD                                            
*                                                                               
         CLC   DOVKEY(2),=X'0D17'                                               
         JNE   *+2                                                              
         MVC   SHOWCODE,DOVKPGM    Extract program code                         
         MVC   DOVDATE,DOVCDAT     Extract creation date                        
         MVC   IOKEYSAV,DOVKEY     Set key of DemoDef/DemOver record            
         J     UPLSDD20                                                         
*                                                                               
UPLSDD02 ICM   RE,7,QAPND          Test adding new ShowCode record              
         JZ    UPLSDD18            No                                           
                                                                                
         OC    QASHW,QASHW         Test show code provided                      
         JZ    UPLSDD04                                                         
         LA    R2,IOKEY            Yes - ensure it's not there                  
         USING NPGMRECD,R2                                                      
         XC    NPGMKEY,NPGMKEY                                                  
         MVI   NPGMKTYP+0,NPGMTYPQ                                              
         MVI   NPGMKTYP+1,NPGMSUBQ                                              
         MVC   NPGMKAGY,AGENCY                                                  
         ICM   RE,7,QASTA                                                       
         JZ    *+2                                                              
         MVC   NPGMKNET,LW_DATA1-LW_D(RE)                                       
         ICM   RE,7,QASHW                                                       
         MVC   NPGMKID,LW_DATA1-LW_D(RE)                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUPD+IODIR+B#SHWREC'                      
         JE    UPLERR1             Record found is an error                     
         TM    IOERR,IOEDEL                                                     
         JNZ   UPLERR1             and so is found and deleted                  
         J     UPLSDD16                                                         
                                                                                
UPLSDD04 LA    R2,IOKEY            Yes - ensure it's not there                  
         XC    NPGMKEY,NPGMKEY                                                  
         MVI   NPGMKTYP+0,NPGMTYPQ                                              
         MVI   NPGMKTYP+1,NPGMSUBQ                                              
         MVC   NPGMKAGY,AGENCY                                                  
         ICM   RE,7,QASTA                                                       
         JZ    *+2                                                              
         MVC   NPGMKNET,LW_DATA1-LW_D(RE)                                       
         ICM   RE,7,QAPND                                                       
         MVC   WORK1(L'NPGMPGM),LW_DATA1-LW_D(RE)                               
         XC    ELEM,ELEM                                                        
         LA    R1,WORK1                                                         
         LHI   R0,L'NPGMPGM                                                     
         LA    RE,WORK2                                                         
         SR    RF,RF                                                            
                                                                                
UPLSDD06 LLC   R4,0(R1)            Build list of unique characters              
         LA    R3,ALLOWCHR(R4)                                                  
         CLI   0(R3),0             Test this is an allowable character          
         JE    UPLSDD08            No                                           
         LA    R4,ELEM(R4)                                                      
         CLI   0(R4),0             Test already used                            
         JNE   UPLSDD08                                                         
         MVI   0(R4),1                                                          
         MVC   0(1,RE),0(R1)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
                                                                                
UPLSDD08 AHI   R1,1                                                             
         JCT   R0,UPLSDD06                                                      
         LTR   RF,RF               Test any good characters                     
         JZ    UPLERR2                                                          
         CHI   RF,1                                                             
         JH    UPLSDD10                                                         
         BCTR  RE,0                                                             
         MVC   1(1,RE),0(RE)       Replicate if only 1 character                
         AHI   RF,1                                                             
                                                                                
UPLSDD10 STC   RF,BYTE1                                                         
         MVI   BYTE2,0                                                          
         MVI   BYTE4,99            High sequence for non-live show              
         CLI   QLIVE,0                                                          
         JE    UPLSDD12                                                         
         MVI   BYTE4,9             High sequence for live shows                 
                                                                                
UPLSDD12 LLC   RE,BYTE2            Set show code prefix                         
         AHI   RE,1                                                             
         CLM   RE,1,BYTE1          Test all characters exhausted                
         JNL   UPLERR2                                                          
         STC   RE,BYTE2                                                         
         LA    RE,WORK2(RE)                                                     
         LA    RF,WORK1                                                         
         CLI   QLIVE,0             Test live show                               
         JE    *+12                                                             
         MVI   WORK1,C'='          Yes - prefix is =xx                          
         LA    RF,WORK1+1                                                       
         MVC   0(1,RF),WORK2                                                    
         MVC   1(1,RF),0(RE)                                                    
         MVI   BYTE3,0             Initialize sequence number                   
                                                                                
UPLSDD14 LLC   RE,BYTE3            Increment/set sequence number                
         AHI   RE,1                                                             
         CLM   RE,1,BYTE4                                                       
         JH    UPLSDD12                                                         
         STC   RE,BYTE3                                                         
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         CLI   QLIVE,0             Test live show                               
         JE    *+14                                                             
         UNPK  WORK1+3(1),DUB      Live sequence number 1-9                     
         J     *+10                                                             
         UNPK  WORK1+2(2),DUB      Non-live sequence number=01-99               
                                                                                
         MVC   NPGMKID,WORK1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUPD+IODIR+B#SHWREC'                      
         JE    UPLSDD14            Loop if found                                
         TM    IOERR,IOEDEL                                                     
         JNZ   UPLSDD14            Or found and deleted                         
         TM    IOERR,IOERNF        Must be not found                            
         JZ    *+2                                                              
                                                                                
UPLSDD16 L     R2,ASHWREC          Build and add ShowCode record                
         XC    NPGMKEY(256),NPGMKEY                                             
         MVC   NPGMKEY,IOKEYSAV                                                 
         MVC   NPGMAGYA,AGENCY                                                  
         MVI   NPGMEL01+0,1                                                     
         MVI   NPGMEL01+1,L'NPGMEL01                                            
         ICM   RE,7,QAPND                                                       
         MVC   NPGMPGM,LW_DATA1-LW_D(RE)                                        
         MVC   NPGMDAY,QROTDAY                                                  
         MVC   NPGMSTR,QSTRTIME                                                 
         MVC   NPGMEND,QENDTIME                                                 
         MVC   NPGMDPT,QDAYPART                                                 
         MVC   NPGMKDAT,QKILLDAT                                                
         MVC   NPGMOOWR,QOOWDAY                                                 
                                                                                
         LA    R3,NPGMEL01+L'NPGMEL01                                           
         USING ACTVD,R3            Build activity element                       
         MVI   ACTVEL,ACTVEL2Q                                                  
         MVI   ACTVLEN,ACTVLENQ                                                 
         GOTOR VDATCON,DMCB,(5,0),(3,ACTVADDT)                                  
         DROP  R3                                                               
                                                                                
         LHI   R0,NPGMEL01-NPGMRECD+L'NPGMEL01+ACTVLENQ+1                       
         STCM  R0,3,NPGMLEN                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOFIL+B#SHWREC'                        
         JNE   *+2                                                              
                                                                                
         MVC   SHOWCODE,NPGMKID    Extract show code                            
         MVC   SHOWDA,IODA         And disk address of ShowCode record          
                                                                                
UPLSDD18 OC    QADEM,QADEM         Test add DemoDef/DemOver record              
         JZ    UPLSDDX                                                          
                                                                                
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVC   STAPAGY,LP_AGY                                                   
         MVI   STAPACT,C'P'                                                     
         MVI   STAPMED,NETMEDQ                                                  
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,EZEROS                                                  
         MVC   STAPQSTA,SPACES                                                  
         ICM   RE,7,QASTA                                                       
         MVC   STAPQSTA(4),LW_DATA1-LW_D(RE)                                    
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JNE   *+2                                                              
                                                                                
         LA    R2,IOKEY                                                         
         USING DOVRECD,R2          Build key of DemOver record                  
         XC    DOVKEY,DOVKEY                                                    
         MVI   DOVKTYP+0,DOVTYPQ                                                
         MVI   DOVKTYP+1,DOVSUBQ                                                
         MVC   DOVKAGMD,QMEDX                                                   
         MVC   DOVKNET,STAPSTA                                                  
         ICM   RE,7,QACLT                                                       
         JZ    *+10                                                             
         MVC   DOVKCLT,LW_DATA1-LW_D(RE)                                        
         MVC   DOVKPGM,SHOWCODE                                                 
         ICM   RE,7,QASHW                                                       
         JZ    *+10                                                             
         MVC   DOVKPGM,LW_DATA1-LW_D(RE)                                        
         MVC   DOVKRTS,QRTGSVC                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUPD+IODIR+B#DOVREC'                      
         JE    *+2                 DemOver record cannot exist                  
                                                                                
UPLSDD20 L     R2,ADOVREC                                                       
         XC    DOVRECD(256),DOVRECD                                             
         MVC   DOVKEY,IOKEYSAV                                                  
         OI    DOVCNTL,DOVCACDQ                                                 
         MVC   DOVAGYA,AGENCY                                                   
                                                                                
         MVI   DOVEL01,DOVEL01Q    Build description element                    
         MVI   DOVEL01+1,DOV01LNQ                                               
         GOTOR VDATCON,DMCB,(5,0),(3,DOVCDAT)                                   
         MVC   DOVADAT,DOVCDAT                                                  
         OC    DOVDATE,DOVDATE                                                  
         JZ    *+10                                                             
         MVC   DOVCDAT,DOVDATE     Use original creation date if change         
         MVC   DOVBBK,QBBOOK                                                    
         MVC   DOVUTBK,QUTBOOK                                                  
                                                                                
         ICM   R1,7,QADEM          R1=A(demo list)                              
         JZ    *+2                                                              
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R1)                                            
         AHI   R1,LW_LN2Q          R1=A(input demo list)                        
         LA    RF,DOVDEMO          RF=A(output list)                            
                                                                                
         BASR  RE,0                Move demos to element                        
         MVC   0(L'DOVDEMO,RF),0(R1)                                            
         CLI   L'DOVDEMO(R1),0     Test demo has values                         
         JE    *+8                                                              
         OI    0(RF),DOVDVALQ                                                   
         AHI   R1,L'DOVDEMO+1                                                   
         AHI   RF,L'DOVDEMO                                                     
         BCTR  R0,RE                                                            
                                                                                
         LA    R3,DOVEL01+DOV01LNQ Point to next element on record              
                                                                                
         ICM   RE,7,QANAD                                                       
         JZ    UPLSDD22                                                         
         USING DOVEL02,R3          Build national audience imp element          
         MVI   DOVEL02,DOVEL02Q                                                 
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN-LW_D(RE)                                            
         MHI   RF,L'DOVIMPC        RF=length of imp data                        
         LA    R1,DOVIMPC-DOVEL02(RF)                                           
         STC   R1,DOVEL02+1        Set element length                           
         AHI   RE,LW_LN2Q          RE=A(input list)                             
         LA    R0,DOVIMPC          R0=A(output list)                            
         LR    R1,RF                                                            
         MVCL  R0,RE               Move imp data to element                     
         LR    R3,R0               Point to next element on record              
                                                                                
UPLSDD22 ICM   R4,7,QASMD          Point to station demos                       
         JZ    UPLSDD30                                                         
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R4)                                            
         AHI   R4,LW_LN2Q                                                       
         USING DSDD,R4             R4=A(station/market demo array)              
         USING DOVEL05,R3          R3=A(station/market demo element)            
                                                                                
UPLSDD24 MVI   DOVEL05,DOVEL05Q    Set element code                             
                                                                                
         TM    DSDSTMK,X'F0'       Test station or (spill) market               
         JNO   UPLSDD26                                                         
         PACK  DUB,DSDSTMK         Get market number into binary                
         CVB   RF,DUB                                                           
         STCM  RF,3,DOVMKT         Set (spill) market number                    
         J     UPLSDD28                                                         
                                                                                
UPLSDD26 XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVC   STAPAGY,LP_AGY                                                   
         MVI   STAPACT,C'P'                                                     
         MVI   STAPMED,NETMEDQ                                                  
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,EZEROS                                                  
         MVC   STAPQSTA,SPACES                                                  
         MVC   STAPQSTA(L'DSDSTMK),DSDSTMK                                      
         MVC   STAPQNET,DSDNETW                                                 
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JNE   *+2                                                              
         MVC   DOVSTA,STAPSTA      Set station code                             
                                                                                
UPLSDD28 LLC   R1,DSDDEMON         R1=number of demos in list                   
         MHI   R1,L'DSDDEMVL                                                    
         LA    RF,DOVDEMV-DOVEL05(R1)                                           
         STC   RF,DOVEL05+1        Set element length                           
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   DOVDEMV(0),DSDDEMVL                                              
         EX    R1,0(RE)                                                         
                                                                                
         AR    R3,RF               Point to next output element                 
         AHI   R4,DSDL             Point to next input array entry              
         JCT   R0,UPLSDD24         Do for number of entries                     
         DROP  R3,R4                                                            
                                                                                
UPLSDD30 MVI   0(R3),0             Set end of record                            
         AHI   R3,1                                                             
         LA    R0,DOVRECD                                                       
         SR    R3,R0                                                            
         STCM  R3,3,DOVLEN         Set record length                            
                                                                                
         OC    QDA,QDA             Test change DemoDef/DemOver record           
         JZ    UPLSDD32                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOFIL+B#DOVREC'                        
         JE    UPLSDDX                                                          
         DC    H'0'                                                             
                                                                                
UPLSDD32 GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOFIL+B#DOVREC'                        
         JNE   *+2                                                              
         MVC   DOVRDA,IODA         Set disk address of DemOver record           
                                                                                
UPLSDDX  J     EXITY                                                            
         DROP  R2                                                               
                                                                                
UPLERR1  LHI   R0,SE#SHWAU         Show code already exists                     
         J     UPLERRX                                                          
UPLERR2  LHI   R0,SE#CCSHW         Can't generate show code                     
                                                                                
UPLERRX  STCM  R0,3,LP_ERROR       Set error                                    
         J     EXITN                                                            
                                                                                
DSDD     DSECT ,                   ** Station/Market demo array **              
DSDSTMK  DS    CL4                 Station/market code                          
DSDNETW  DS    CL3                 Network                                      
DSDDEMON DS    X                   Number of demos in list                      
DSDDEMMX EQU   10                  Maximum number of demos in list              
DSDDEMVL DS    (DSDDEMMX)XL(L'DOVDEMV)                                          
DSDL     EQU   *-DSDD                                                           
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop product browse X'020F'                                      *         
***********************************************************************         
                                                                                
REQPRB   LKREQ H,I#CDPRDB,OUTPRB,NEXTREQ=REQCAB                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
                                                                                
         LKREQ E                                                                
                                                                                
OUTPRB   LKOUT H                                                                
                                                                                
PRBPRO   LKOUT R,1                 Get product records                          
Array    LKOUT C,1,(A,ARYPRD)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop Campaign Browse X'0280'                                     *         
***********************************************************************         
                                                                                
REQCAB   LKREQ H,I#COCAMB,OUTCAB,NEXTREQ=REQOCA                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
                                                                                
         LKREQ E                                                                
                                                                                
OUTCAB   LKOUT H                                                                
                                                                                
CABREC   LKOUT R,1                 Campaign browse                              
Array    LKOUT C,1,(A,ARYCAB)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCAB   LKOUT A,(R,NXTCAM),MULTIROW=Y,ROWNAME=CMPRECD                          
                                                                                
CamCo    LKOUT P,CMPKCAM,CAMCOMP                                                
CamNo    LKOUT C,1,(D,B#WORKD,FULL),LBIN                                        
Array    LKOUT C,2,(A,ARYDSC)                                                   
Array    LKOUT C,3,(A,ARYCID)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read campaign records                                               *         
***********************************************************************         
                                                                                
NXTCAM   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTCAM20                                                         
         ICM   RF,7,QAMED          GET MEDIA                                    
         JZ    NOMORE                                                           
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
         JNE   NOMORE                                                           
         MVC   WORK(1),QMEDA                                                    
*                                                                               
         CLI   QMEDA,RADMEDQ       C'R' - RADIO REQUESTED?                      
         JE    NXTCAM10                                                         
         CLI   QMEDA,NTRMEDQ       C'X' -  OR NTWK RADIO?                       
         JE    NXTCAM10                                                         
         MVI   WORK,COMMEDQ        Campaigns under combined media               
*                                                                               
NXTCAM10 GOTOR (#VALMED,AVALMED),DMCB,WORK,,QMEDIA                              
         JNE   NOMORE                                                           
         OC    QACAM,QACAM         Set campaign to all if not given             
         JNZ   NXTCAM20                                                         
         MVC   QACAM,AALL                                                       
                                                                                
NXTCAM20 GOTOR (#NXTREC,ANXTREC),DMCB,CAMKEYT,('B#CAMREC',0),          +        
               ('$NXTRXSP',SAVED),0,0                                           
         JNE   EXITY                                                            
         L     R1,ACAMREC          Extract & set campaign number                
         MVC   QCAMX,CMPKCAM-CMPRECD(R1)                                        
         J     EXITY                                                            
                                                                                
CONCOMP  DS    0H                  Complement contact number                    
ORDCOMP  DS    0H                  Complement order number                      
CAMCOMP  L     R1,LP_AINP          Complement campaign number                   
         MVC   FULL,0(R1)                                                       
         XC    FULL,EFFS                                                        
         BR    RE                                                               
                                                                                
ARYDSC   LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPDSELD,CMPDSELQ),ROWWIDTH=(V,CMPDSLEN)                  
                                                                                
CamDs    LKOUT C,2,CMPDSDES,CHAR,LEN=V,ND=Y                                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYCID   LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPIDELD,CMPIDELQ),ROWWIDTH=(V,CMPIDLEN)                  
                                                                                
CamSd    LKOUT C,3,CMPIDSDT,BDAT                                                
CamEd    LKOUT C,4,CMPIDEDT,BDAT                                                
CamSt    LKOUT C,5,CMPIDSTA,HEXD,ND=Y                                           
BType    LKOUT C,6,CMPIDTYP,CHAR,ND=Y                                           
StDOW    LKOUT C,7,CMPIDDAY,CHAR,ND=Y                                           
DptMn    LKOUT C,8,CMPIDDPT,CHAR,ND=Y                                           
SBook    LKOUT C,9,CMPIDSBK,BMON,ND=Y                                           
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop Campaign Open X'0281'                                       *         
***********************************************************************         
                                                                                
REQOCA   LKREQ H,I#COOCAM,OUTOCA,NEXTREQ=REQORB                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
CamNo    LKREQ F,3,(D,B#SAVED,QCAMX),(R,VALCAM),TEXT=(*,CAMNLIT),      +        
               COL=*                                                            
                                                                                
         LKREQ E                                                                
                                                                                
OUTOCA   LKOUT H                                                                
                                                                                
OCAREC   LKOUT R,1                 Open campaign                                
Array    LKOUT C,1,(A,ARYOCA)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYOCA   LKOUT A,(R,GETCAM),ROWNAME=CMPRECD                                     
                                                                                
Array    LKOUT C,1,(A,ARYCAM)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read a campaign record                                              *         
***********************************************************************         
                                                                                
GETCAM   ICM   RF,7,QAMED          GET REQUESTED MEDIA                          
         JZ    NOMORE                                                           
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
         JNE   NOMORE                                                           
*                                                                               
         CLI   QMEDA,RADMEDQ       C'R' - RADIO REQUESTED?                      
         JE    GETCAM10                                                         
         CLI   QMEDA,NTRMEDQ       C'X' -   OR NTWK RADIO?                      
         JE    GETCAM10                                                         
         MVI   QMEDA,COMMEDQ       CAMPAIGNS UNDER COMBINED MEDIA               
*                                                                               
GETCAM10 GOTOR (#VALMED,AVALMED),DMCB,QMEDA,,QMEDIA                             
         JNE   NOMORE                                                           
*                                                                               
         ICM   RE,7,QACLT                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   QCLTX,LW_DATA1-LW_D(RE)                                          
         USING CMPRECD,IOKEY                                                    
         XC    CMPKEY,CMPKEY                                                    
         MVI   CMPKTYPE,CMPKTYPQ                                                
         MVI   CMPKSTYP,CMPKSTYQ                                                
         MVC   CMPKAGMD,QMEDIA                                                  
         MVC   CMPKCLT,QCLTX                                                    
         MVC   CMPKCAM,QCAMX                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOXSPDIR+B#CAMREC'                      
         JNE   GETCAMER                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOXSPFIL+B#CAMREC'                     
         JNE   GETCAMER                                                         
         MVC   LP_ADATA,ACAMREC                                                 
         J     EXITY                                                            
GETCAMER MVC   LP_ERROR,=AL2(67)                                                
         J     QERROR                                                           
                                                                                
ARYCAM   LKOUT A,(*,CMPRECD),ROWNAME=CMPRECD,NROWS=1,ROWWIDTH=2000              
                                                                                
CamCo    LKOUT P,CMPKCAM,CAMCOMP                                                
CamNo    LKOUT C,35,(D,B#WORKD,FULL),LBIN                                       
Array    LKOUT C,1,(A,ARYCNM)                                                   
Array    LKOUT C,2,(A,ARYCDS)                                                   
Array    LKOUT C,3,(A,ARYCID2)                                                  
Array    LKOUT C,10,(A,ARYCCM)                                                  
Array    LKOUT C,11,(A,ARYCAC)                                                  
Array    LKOUT C,21,(A,ARYSEC)                                                  
Array    LKOUT C,22,(A,ARYCDM)                                                  
Array    LKOUT C,23,(A,ARYCFL)                                                  
Array    LKOUT C,33,(A,ARYCSE)                                                  
Array    LKOUT C,34,(A,ARYCSL)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYCNM   LKOUT A,(D,B#CLTREC,CLTHDR),ROWNAME=CLTHDR,ROWWIDTH=2000,     +        
               NROWS=1                                                          
                                                                                
CltNm    LKOUT C,1,CNAME,CHAR                                                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYCDS   LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPDSELD,CMPDSELQ),ROWWIDTH=(V,CMPDSLEN)                  
                                                                                
CamDs    LKOUT C,2,CMPDSDES,CHAR,LEN=V,ND=Y                                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYCID2  LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPIDELD,CMPIDELQ),ROWWIDTH=(V,CMPIDLEN)                  
                                                                                
CamSd    LKOUT C,3,CMPIDSDT,BDAT                                                
CamEd    LKOUT C,4,CMPIDEDT,BDAT                                                
CamSt    LKOUT C,5,CMPIDSTA,HEXD,ND=Y                                           
Pr1Cd    LKOUT C,6,CMPIDPR1,CHAR,ND=Y                                           
PRout    LKOUT P,CMPIDPR1,GETPRD                                                
Pr1Nm    LKOUT C,7,(D,B#WORKD,WORK),CHAR,LEN=L'PNAME,ND=Y                       
Pr2Cd    LKOUT C,8,CMPIDPR2,CHAR,ND=Y                                           
PRout    LKOUT P,CMPIDPR2,GETPRD                                                
Pr1Nm    LKOUT C,9,(D,B#WORKD,WORK),CHAR,LEN=L'PNAME,ND=Y                       
BType    LKOUT C,14,CMPIDTYP,CHAR,ND=Y                                          
StDOW    LKOUT C,15,CMPIDDAY,CHAR,ND=Y                                          
DptMn    LKOUT C,16,CMPIDDPT,CHAR,ND=Y                                          
SBook    LKOUT C,17,CMPIDSBK,BMON,ND=Y                                          
ConvPro  LKOUT C,19,CMPIDCDP,LBIN,ND=Y,FILTROUT=TST01LEN                        
SpclPro  LKOUT C,20,CMPIDSDP,LBIN,ND=Y,FILTROUT=TST01LEN                        
                                                                                
         LKOUT E                                                                
                                                                                
TST01LEN L     R1,LP_AINP          Test if latest order revision                
         CLI   CMPIDLEN-CMPIDEL(R1),CMPIDL2Q                                    
         JNL   EXITY                                                            
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Read product record                                                 *         
***********************************************************************         
                                                                                
GETPRD   L     R1,LP_AINP          Read product & extract name                  
         XC    WORK(L'PNAME),WORK                                               
         CLC   0(L'CMPIDPR1,R1),SPACES                                          
         JNH   EXITY                                                            
         MVC   SVIOVALS,IOVALS                                                  
         USING PKEY,IOKEY                                                       
         XC    PKEY,PKEY                                                        
         MVI   PKEYTYPE,PKEYTYPQ                                                
         MVC   PKEYAM,QMEDX                                                     
         MVC   PKEYCLT,QCLTX                                                    
         MVC   PKEYPRD,0(R1)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOSPTDIR+B#PRDREC'                      
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOSPTFIL+B#PRDREC'                     
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,APRDREC                                                       
         MVC   WORK(L'PNAME),PNAME-PRDHDR(R1)                                   
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
                                                                                
ARYCCM   LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPCMELD,CMPCMELQ),ROWWIDTH=(V,CMPCMLEN)                  
                                                                                
Comnt    LKOUT C,10,CMPCMCOM,CHAR,LEN=V                                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYCAC   LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CDSACELD,CDSACELQ),ROWWIDTH=(V,CDSACLEN)                  
                                                                                
PRout    LKOUT P,CDSACELD,SETRDT                                                
ActDt    LKOUT C,11,(D,B#WORKD,WORK),BDAT                                       
ActTm    LKOUT C,12,(D,B#WORKD,WORK+L'CDSACADT),HEXD,LEN=L'CDSACCTM             
                                                                                
         LKOUT E                                                                
                                                                                
ARYSEC   LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPSLELD,CMPSLELQ),ROWWIDTH=(V,CMPSLLEN)                  
                                                                                
Array    LKOUT C,21,(A,ARYSEC2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYSEC2  LKOUT A,(*,CMPSLSLN),ROWNAME=CMPSLELD,ROWWIDTH=L'CMPSLSLN,    +        
               NROWS=*                                                          
                                                                                
SptLn    LKOUT C,21,CMPSLSLN,LBIN                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYCDM   LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPDMELD,CMPDMELQ),ROWWIDTH=(V,CMPDMLEN)                  
                                                                                
Array    LKOUT C,22,(A,ARYCDM2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCDM2  LKOUT A,(*,CMPDMDEM),ROWNAME=CMPDMELD,ROWWIDTH=L'CMPDMDEM,    +        
               NROWS=*                                                          
                                                                                
DemCd    LKOUT C,22,CMPDMDEM,(U,#EDTDCD,$EDTDCD)                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYCFL   LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPFLELD,CMPFLELQ),ROWWIDTH=(V,CMPFLLEN)                  
                                                                                
Array    LKOUT C,23,(A,ARYCFL2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCFL2  LKOUT A,(*,CMPFLLST),ROWNAME=CMPFLELD,ROWWIDTH=CMPFLLSQ,      +        
               NROWS=*                                                          
                                                                                
StrDt    LKOUT C,23,CMPFLSDT,BDAT                                               
EndDt    LKOUT C,24,CMPFLEDT,BDAT                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYCSE   LKOUT A,(R,NXTCSE),ROWNAME=STAESTD,MULTIROW=Y                          
                                                                                
PrdCd    LKOUT C,1,STAEPRD,(U,#EDTPRD,$EDTPRD),ND=Y                             
Array    LKOUT C,2,(A,ARYSES)                                                   
Array    LKOUT C,4,(A,ARYWKE)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read station/estimate records                                       *         
***********************************************************************         
                                                                                
T        USING TSARD,STAESTB                                                    
NXTCSE   L     R4,ASTAEST                                                       
         USING STAESTD,R4                                                       
         ST    R4,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTCSE16                                                         
         XC    T.TSARD(TSPNEWL),T.TSARD                                         
         MVI   T.TSACTN,TSAINI                                                  
         ST    R4,T.TSAREC                                                      
         MVC   T.TSACOM,ACOMFACS                                                
         MVI   T.TSKEYL,STAEKEYL                                                
         LHI   R0,STAESTL                                                       
         STCM  R0,3,T.TSRECL                                                    
         MVI   T.TSRECI,TSRXTN+TSRMINB1                                         
         LHI   R0,4096                                                          
         STCM  R0,3,T.TSBUFFL                                                   
         GOTOR VTSAR,T.TSARD                                                    
         JE    NXTCSE02                                                         
         DC    H'0'                                                             
                                                                                
NXTCSE02 GOTOR (#NXTREC,ANXTREC),DMCB,CSEKEYT,('B#CSEREC',SVCAMKEY),   +        
               ('$NXTRXSP',SAVED),0,0                                           
         JNE   NXTCSE14                                                         
         LA    R0,STAESTD                                                       
         LHI   R1,STAESTL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R2,ACSEREC                                                       
         USING CSERECD,R2          R2=A(station/estimate record)                
         LA    R3,CSEFRST                                                       
         USING CSEWKELD,R3                                                      
NXTCSE04 CLI   CSEWKEL,0           Locate week/estimate element                 
         JE    NXTCSE02                                                         
         CLI   CSEWKEL,CSEWKELQ                                                 
         JE    *+16                                                             
         LLC   R0,CSEWKLEN                                                      
         AR    R3,R0                                                            
         J     NXTCSE04                                                         
         LLC   R1,CSEWKLEN         Copy into output record                      
         LA    R0,STAEWKS                                                       
         LA    RE,CSEWKELD                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    RE,CSEWKELD         Compute checksum for element                 
         LLC   RF,CSEWKLEN                                                      
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,STAECKSM      Set checksum as key of TSAR record           
         MVC   STAEPRD,CSEKPRD     Set product                                  
         CLI   STAEPRD,0                                                        
         JNE   *+8                                                              
         MVI   STAEPRD,POLPRDQ     Set product to pol if zero                   
         DROP  R3                                                               
                                                                                
         L     R3,AIO3                                                          
         ST    R3,T.TSAREC                                                      
F        USING STAESTD,R3                                                       
NXTCSE06 MVC   F.STAEKEY(STAEKEYL),STAEKEY                                      
         MVI   T.TSACTN,TSARDH                                                  
         GOTOR VTSAR,T.TSARD                                                    
         TM    T.TSERRS,TSERNF     Not found - add a new record                 
         JNZ   NXTCSE12                                                         
         CLC   F.STAEWKS,STAEWKS   Test same weeks/estimates                    
         JNE   NXTCSE08            No - get next                                
         LHI   R1,STAESTAM                                                      
         CLM   R1,3,F.STAESTAN     Test station list is full                    
         JNE   NXTCSE10                                                         
                                                                                
NXTCSE08 SR    R0,R0               Read for next record                         
         ICM   R0,3,STAESEQ#                                                    
         AHI   R0,1                                                             
         STCM  R0,3,STAESEQ#                                                    
         J     NXTCSE06                                                         
                                                                                
NXTCSE10 ICM   R1,3,F.STAESTAN     Bump and set number of entries               
         AHI   R1,1                                                             
         STCM  R1,3,F.STAESTAN                                                  
         MHI   R1,STAESTAL                                                      
         LA    R1,F.STAESTA-L'STAESTA(R1)                                       
         MVC   0(STAESTAL,R1),CSEKSTTY                                          
         MVI   T.TSACTN,TSAPUT                                                  
         GOTOR VTSAR,T.TSARD       Put back the updated record                  
         JE    NXTCSE02                                                         
         DC    H'0'                                                             
                                                                                
NXTCSE12 MVC   STAESTA(STAESTAL),CSEKSTTY                                       
         LHI   R0,1                                                             
         STCM  R0,3,STAESTAN                                                    
         MVC   T.TSAREC,ASTAEST                                                 
         MVI   T.TSACTN,TSAADD                                                  
         GOTOR VTSAR,T.TSARD                                                    
         JE    NXTCSE02                                                         
         DC    H'0'                                                             
                                                                                
NXTCSE14 XC    STAEKEY(STAEKEYL),STAEKEY                                        
         MVC   T.TSAREC,ASTAEST                                                 
         MVI   T.TSACTN,TSARDH                                                  
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,STAESTD                                                       
         ST    R0,LP_ADATA         (was set by NXTREC call above)               
         XC    ELEM,ELEM                                                        
                                                                                
NXTCSE16 GOTOR VTSAR,T.TSARD       Get first/next TSAR record                   
         MVI   T.TSACTN,TSANXT                                                  
         TM    T.TSERRS,TSEEOF                                                  
         JNZ   NOMORE                                                           
         CLC   ELEM,STAEWKS        Clear weeks if same as previous              
         MVC   ELEM,STAEWKS                                                     
         JNE   *+10                                                             
         XC    STAEWKS,STAEWKS                                                  
         J     EXITY                                                            
                                                                                
STAESTD  DSECT ,                                                                
                                                                                
STAEKEY  DS    0X                                                               
STAECKSM DS    XL4                 Checksum                                     
STAEPRD  DS    X                   Product code                                 
STAESEQ# DS    XL2                 Sequence number                              
STAEKEYL EQU   *-STAESTD                                                        
                                                                                
STAEWKS  DS    XL256               CSEWKELD                                     
STAEWKES EQU   STAEWKS+(CSESTLST-CSEWKELD)                                      
                                                                                
STAESTAN DS    AL2                 Number of entries in STAESTA                 
STAESTAL EQU   L'CSEKSTTY+L'CSEKSTA                                             
STAESTAM EQU   256                                                              
STAESTA  DS    (STAESTAM+1)XL(STAESTAL)                                         
                                                                                
STAESTL  EQU   *-STAESTD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
                                                                                
ARYSES   LKOUT A,(*,STAESTA),ROWNAME=STAESTD,EOT=EOR,                  +        
               ROWWIDTH=STAESTAL                                                
                                                                                
Stype    LKOUT C,2,STAESTA,UBIN,LEN=L'CSEKSTTY                                  
StaCd    LKOUT C,3,STAESTA+L'CSEKSTTY,CHAR,LEN=L'CSEKSTA                        
                                                                                
         LKOUT E                                                                
                                                                                
ARYWKE   LKOUT A,(*,STAEWKES),ROWNAME=STAESTD,EOT=EOR,                 +        
               ROWWIDTH=CSESTLNQ                                                
                                                                                
WkStd    LKOUT C,4,STAEWKES,CDAT,LEN=L'CSESTWK                                  
EstNo    LKOUT C,5,STAEWKES+L'CSESTWK,LBIN,LEN=L'CSESTEST                       
                                                                                
         LKOUT E                                                                
                                                                                
ARYCSL   LKOUT A,(R,NXTCSL),ROWNAME=CSLRECD,MULTIROW=Y                          
                                                                                
PRout    LKOUT P,CSLKLIST,TESTLIST                                              
ListNum  LKOUT C,1,CSLKLIST,LBIN,ND=Y                                           
PrdLst   LKOUT C,3,(A,ARYCSLP)                                                  
WkEstLst LKOUT C,4,(A,ARYCSLW)                                                  
StaLst   LKOUT C,2,(A,ARYCSLS)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
TESTLIST L     R1,LP_AINP          Point to list number                         
         CLC   HALF1,0(R1)         Test sent this already                       
         MVC   HALF1,0(R1)                                                      
         BNER  RE                                                               
         XC    0(L'CSLKLIST,R1),0(R1)                                           
         BR    RE                                                               
                                                                                
***********************************************************************         
* Read station list records                                           *         
***********************************************************************         
                                                                                
NXTCSL   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTCSL02                                                         
         XC    HALF1,HALF1         Cleat value for TESTLIST                     
                                                                                
NXTCSL02 GOTOR (#NXTREC,ANXTREC),DMCB,CSLKEYT,('B#CSLREC',SVCAMKEY),   +        
               ('$NXTRXSP',SAVED),0,0                                           
         J     EXITY                                                            
                                                                                
ARYCSLS  LKOUT A,(D,B#CSLREC,CSLFRST),EOT=EOR,                         +        
               ROWID=(CSLSTELD,CSLSTELQ),ROWWIDTH=(V,CSLSTLEN)                  
                                                                                
Array    LKOUT C,2,(A,ARYCSLS2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCSLS2 LKOUT A,(*,CSLSTSTA),ROWNAME=CSLSTELD,ROWWIDTH=L'CSLSTSTA,    +        
               NROWS=*                                                          
                                                                                
Station  LKOUT C,2,CSLSTSTA,CHAR                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYCSLP  LKOUT A,(D,B#CSLREC,CSLFRST),EOT=EOR,                         +        
               ROWID=(CSLPRELD,CSLPRELQ),ROWWIDTH=(V,CSLPRLEN)                  
                                                                                
Array    LKOUT C,3,(A,ARYCSLP2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCSLP2 LKOUT A,(*,CSLPRPRD),ROWNAME=CSLPRELD,ROWWIDTH=L'CSLPRPRD,    +        
               NROWS=*                                                          
                                                                                
Product  LKOUT C,3,CSLPRPRD,(U,#EDTPRD,$EDTPRD)                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCSLW  LKOUT A,(D,B#CSLREC,CSLFRST),EOT=EOR,                         +        
               ROWID=(CSLWKELD,CSLWKELQ),ROWWIDTH=(V,CSLWKLEN)                  
                                                                                
Array    LKOUT C,4,(A,ARYCSLW2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCSLW2 LKOUT A,(*,CSLWELST),ROWNAME=CSLWKELD,ROWWIDTH=CSLWELNQ,      +        
               NROWS=*                                                          
                                                                                
WeekStDt LKOUT C,4,CSLWEWSD,CDAT                                                
EstNum   LKOUT C,5,CSLWEEST,UBIN                                                
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Order Browse X'0282'                                                *         
***********************************************************************         
                                                                                
REQORB   LKREQ H,I#COORDB,OUTORB,NEXTREQ=REQORD                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),DEFAULT=Y,  +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),DEFAULT=Y,  +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*,LIST=F           
CamNo    LKREQ F,3,(I,B#SAVED,QCAMIND),(R,VALCAM),OLEN=L'CORKCAM,      +        
               MAXLEN=L'EZEROS,LIST=F,DEFAULT=Y,TEXT=(*,CAMNLIT),COL=*          
OAStr    LKREQ F,4,(D,B#SAVED,QACTSDAT),BDAT,TEXT=SP#STDT,COL=*                 
OAEnd    LKREQ F,5,(D,B#SAVED,QACTEDAT),BDAT,TEXT=SP#ENDT,COL=*                 
Stats    LKREQ F,6,(I,B#SAVED,QSTSIND),LBIN,TEXT=(*,STATLIT),LIST=F,   +        
               OLEN=L'CORSTST,COL=*                                             
StNet    LKREQ F,7,(I,B#SAVED,QSTAIND),(U,#VALSTA,$VALSTA),DEFAULT=Y,  +        
               OLEN=L'CORIDSTA,TEXT=SP#STA,LIST=F,COL=*                         
Buyer    LKREQ F,8,(I,B#SAVED,QBYRIND),CHAR,TEXT=(*,BUYRLIT),LIST=F,   +        
               OLEN=L'CORIDBYR,COL=*                                            
Sellr    LKREQ F,9,(I,B#SAVED,QSLRIND),(R,VALSTR),TEXT=(*,SELRLIT),    +        
               LIST=F,OLEN=L'CORRLWHO+1,COL=*                                   
                                                                                
         LKREQ E                                                                
                                                                                
OUTORB   LKOUT H                                                                
                                                                                
ORDORB   LKOUT R,1                 Orders and revision records                  
Array    LKOUT C,1,(A,ARYOBR)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYOBR   LKOUT A,(R,NXTORD),ROWNAME=CORRECD,MULTIROW=Y                          
                                                                                
Array    LKOUT C,1,(A,ARYORD2),FILTROUT=TSTLREV                                 
Array    LKOUT C,2,(A,ARYORV)                                                   
Array    LKOUT C,34,(A,ARYOST)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read orders and revisions                                           *         
***********************************************************************         
                                                                                
NXTORD   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTORD10                                                         
         XC    LORD,LORD           YES - INITIALIZE LAST ORDER NUMBER           
         XC    SVCORKEY,SVCORKEY                                                
         ICM   RE,7,QAMED          AND MEDIA                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
*                                                                               
NXTORD10 LARL  R0,FLTORD           R0=A(RECORD FILTER ROUTINE)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,CORKEYT,('B#CORREC',0),          +        
               ('$NXTRXSP',SAVED),0,(R0)                                        
         JNE   EXITN                                                            
*                                                                               
*  LOOK AT THE CLIENT AND SEE IF IT PASSES OUR LIMIT ACCESS TESTS LIKE          
*    IT SHOULD IN CLIENT BROWSE                                                 
*                                                                               
         CLC   SVCORKEY(CORKCAM-CORKEY),IOKEY  SAME MEDIA/CLIENT?               
         JNE   NXTORD20                        NO                               
         OC    SVCORKEY+CORKCAM-CORKEY(4),SVCORKEY+CORKCAM-CORKEY               
         JZ    NXTORD10            ZEROS IF CLT DIDN'T PASS OFFICER             
         J     NXTORDXY            WE KNOW CLT PASSES LIMIT ACCESS              
*                                                                               
NXTORD20 MVC   SVCORKEY,IOKEY                                                   
         MVC   NXOIOSAV,IOVALS     SAVE CURRENT I/O VALUES                      
         MVC   QCLTX,IOKEY+CORKCLT-CORKEY                                       
*                                                                               
         GOTOR (#GETCLT,AGETCLT)   GETCLT CALLS LIMCLT                          
         MVC   IOVALS(IOVALL),NXOIOSAV                                          
         JE    NXTORDXY                                                         
         XC    SVCORKEY+CORKCAM-CORKEY(4),SVCORKEY+CORKCAM-CORKEY               
         J     NXTORD10            CLEAR CAMP IF CLT HAS NO ACCESS              
*                                                                               
NXTORDXY J     EXITY                                                            
*                                                                               
ARYORV   LKOUT A,(*,CORRECD),ROWNAME=CORRECD,NROWS=1,ROWWIDTH=2000,    +        
               NEWEL=B                                                          
                                                                                
RevRn    LKOUT P,(B#CORREC,CORKREV),REVCOMP                                     
RevNo    LKOUT C,1,(D,B#WORKD,HALF),LBIN                                        
Array    LKOUT C,2,(A,ARYORV2)                                                  
Array    LKOUT C,3,(A,ARYORV3)                                                  
Array    LKOUT C,4,(A,ARYORV4)                                                  
Array    LKOUT C,6,(A,ARYORV5)                                                  
Array    LKOUT C,10,(A,ARYECN)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYORV2  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORCMELD,CORCMELQ),ROWWIDTH=(V,CORCMLEN)                  
                                                                                
Comnt    LKOUT C,2,CORCMCMT,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYORV3  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORRLELD,CORRLELQ),ROWWIDTH=(V,CORRLLEN)                  
                                                                                
RList    LKOUT C,3,CORRLWHO,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYORV4  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CDSACELD,CDSACELQ),ROWWIDTH=(V,CDSACLEN)                  
                                                                                
PRout    LKOUT P,CDSACELD,SETADT                                                
ActDt    LKOUT C,4,(D,B#WORKD,WORK),BDAT                                        
ActTm    LKOUT C,5,(D,B#WORKD,WORK+L'CDSACADT),(R,EDTHMS)                       
                                                                                
         LKOUT E                                                                
                                                                                
ARYORV5  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORIDELD,CORIDELQ),ROWWIDTH=(V,CORIDLEN)                  
                                                                                
ByrCd    LKOUT C,6,CORIDBYR,CHAR                                                
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Order Detail X'0283'                                                *         
***********************************************************************         
                                                                                
REQORD   LKREQ H,I#COORDD,OUTORD,NEXTREQ=REQOWS                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
CamNo    LKREQ F,3,(I,B#SAVED,QCAMIND),(R,VALCAM),OLEN=L'CORKCAM,      +        
               MAXLEN=L'EZEROS,TEXT=(*,CAMNLIT),COL=*                           
OrdNo    LKREQ F,4,(D,B#SAVED,QORDX),(R,VALORD),TEXT=(*,ORDNLIT),      +        
               COL=*                                                            
                                                                                
         LKREQ E                                                                
                                                                                
OUTORD   LKOUT H                                                                
                                                                                
ORDORD   LKOUT R,1                 Ordres and revision records                  
Array    LKOUT C,1,(A,ARYORD)                                                   
         LKOUT E                                                                
                                                                                
ORDPRG   LKOUT R,3                 Program/demo records                         
Array    LKOUT C,3,(A,ARYPRG)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYORD   LKOUT A,(R,GETORD),ROWNAME=CORRECD                                     
                                                                                
Array    LKOUT C,1,(A,ARYORD2),FILTROUT=TSTLREV                                 
Array    LKOUT C,2,(A,ARYORV)                                                   
Array    LKOUT C,34,(A,ARYOST)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
TSTLREV  L     R1,LP_AINP          Test if latest order revision                
         CLC   ORDREV,CORKREV-CORRECD(R1)                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Read order and its revisions                                        *         
***********************************************************************         
                                                                                
GETORD   LA    R2,IOKEY                                                         
         USING CORRECD,R2                                                       
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   GETORD02                                                         
                                                                                
         ICM   RE,7,QAMED          Set media                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
         JE    *+6                                                              
         DC    H'0'                                                             
         ICM   RE,7,QACLT          Set client                                   
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   QCLTX,LW_DATA1-LW_D(RE)                                          
         ICM   RE,7,QACAM          Set campaign                                 
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   QCAMX,LW_DATA1-LW_D(RE)                                          
                                                                                
         XC    CORKEY,CORKEY       Read for latest order revision               
         MVI   CORKTYPE,CORKTYPQ                                                
         MVI   CORKSTYP,CORKSTYQ                                                
         MVC   CORKAGMD,QMEDX                                                   
         MVC   CORKCLT,QCLTX                                                    
         MVC   CORKCAM,QCAMX                                                    
         MVC   CORKORD,QORDX                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOXSPDIR+B#CORREC'                      
         CLC   CORKEY(CORKREV-CORKEY),IOKEYSAV                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   ORDREV,CORKREV      Save latest order revision number            
         J     GETORD04                                                         
                                                                                
GETORD02 GOTOR (#IOEXEC,AIOEXEC),'IOBSQ+IOXSPDIR+B#CORREC'                      
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   CORKEY(CORKREV-CORKEY),IOKEYSAV                                  
         JNE   NOMORE                                                           
                                                                                
GETORD04 GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOXSPFIL+B#CORREC'                     
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,ACORREC                                                       
         ST    R2,LP_ADATA                                                      
         J     MORE                                                             
         DROP  R2                                                               
                                                                                
ARYORD2  LKOUT A,(*,CORRECD),ROWNAME=CORRECD,NROWS=1,ROWWIDTH=2000              
                                                                                
MedCd    LKOUT C,1,CORKAGMD,(U,#EDTMED,$EDTMED)                                 
CltCd    LKOUT C,2,CORKCLT,(U,#EDTCLT,$EDTCLT)                                  
CamCo    LKOUT P,CORKCAM,CAMCOMP                                                
CamNo    LKOUT C,3,(D,B#WORKD,FULL),LBIN                                        
CamCo    LKOUT P,CORKORD,ORDCOMP                                                
CamNo    LKOUT C,4,(D,B#WORKD,FULL),LBIN                                        
Array    LKOUT C,5,(A,ARYORD3)                                                  
Array    LKOUT C,9,(A,ARYORD4)                                                  
Array    LKOUT C,10,(A,ARYORD6)                                                 
Array    LKOUT C,11,(A,ARYORD7)                                                 
PRout    LKOUT P,CORKEY,GETCAMO    Get campaign record                          
Array    LKOUT C,12,(A,ARYDSC2)    Send campaign name                           
                                                                                
         LKOUT E                                                                
                                                                                
GETCAMO  L     R1,LP_AINP          Read campaign record for an order            
R        USING CORRECD,R1                                                       
         MVC   SVIOVALS,IOVALS                                                  
K        USING CMPKEY,IOKEY                                                     
         XC    K.CMPKEY,K.CMPKEY                                                
         MVI   K.CMPKTYPE,CMPKTYPQ                                              
         MVI   K.CMPKSTYP,CMPKSTYQ                                              
         MVC   K.CMPKCLT,R.CORKCLT                                              
         MVC   K.CMPKCAM,R.CORKCAM                                              
         DROP  R1                                                               
*                                                                               
         MVC   K.CMPKAGMD,R.CORKAGMD  GET A/M FROM ORDER RECORD                 
         MVC   WORK(1),R.CORKAGMD     CHANGE TO COMBINED?                       
         NI    WORK,X'0F'             ISOLATE THE BINARY MEDIA                  
         CLI   WORK,2                 RADIO?                                    
         JE    GTCAMO10                                                         
         CLI   WORK,4                 NTWK RADIO?                               
         JE    GTCAMO10                                                         
         MVI   WORK,COMMEDQ           NEITHER, CAMP UNDER COMBINED              
         GOTOR (#VALMED,AVALMED),DMCB,WORK,,K.CMPKAGMD                          
*                                                                               
GTCAMO10 GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOXSPDIR+B#CAMREC'                      
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOXSPFIL+B#CAMREC'                     
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
ARYDSC2  LKOUT A,(D,B#CAMREC,CMPFRST),EOT=EOR,                         +        
               ROWID=(CMPDSELD,CMPDSELQ),ROWWIDTH=(V,CMPDSLEN)                  
                                                                                
CamDs    LKOUT C,12,CMPDSDES,CHAR,LEN=V,ND=Y                                    
                                                                                
         LKOUT E                                                                
                                                                                
REVCOMP  L     R1,LP_AINP          Complement revision number                   
         MVC   HALF,0(R1)                                                       
         XC    HALF,EFFS                                                        
         BR    RE                                                               
                                                                                
ARYORD3  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORIDELD,CORIDELQ),ROWWIDTH=(V,CORIDLEN)                  
                                                                                
PRout    LKOUT P,CORIDMKT,BLDMKT                                                
MktNo    LKOUT C,5,CORIDMKT,LBIN                                                
PRout    LKOUT P,CORIDSTA,BLDSTA                                                
StaCd    LKOUT C,6,CORIDSTA,(R,EDTSTA)                                          
OffCd    LKOUT C,7,CORIDOFC,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
BLDMKT   TM    MAP#ORDD,MAPSORDD   Test order detail download                   
         BZR   RE                                                               
         L     R1,LP_AINP          Build market key driver entry                
         L     RF,LP_AWMP                                                       
         STCM  RF,7,QAMKT                                                       
         MVI   LW_TYPE-LW_D(RF),LW_TLSTQ                                        
         LHI   R0,1                                                             
         STCM  R0,3,LW_NUMN-LW_D(RF)                                            
         MVC   LW_DATA2-LW_D(L'CORIDMKT,RF),0(R1)                               
         AHI   RF,LW_LN2Q+L'CORIDMKT                                            
         ST    RF,LP_AWMP                                                       
         BR    RE                                                               
                                                                                
BLDSTA   TM    MAP#ORDD,MAPSORDD   Test order detail download                   
         BZR   RE                                                               
         L     R1,LP_AINP          Build station key driver entry               
         OC    0(L'CORIDSTA,R1),0(R1)                                           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,LP_AWMP                                                       
         STCM  RF,7,QASTA                                                       
         MVI   LW_TYPE-LW_D(RF),LW_TLSTQ                                        
         LHI   R0,1                                                             
         STCM  R0,3,LW_NUMN-LW_D(RF)                                            
         MVC   LW_DATA2-LW_D(L'CORIDSTA,RF),0(R1)                               
         AHI   RF,LW_LN2Q+L'CORIDSTA                                            
         ST    RF,LP_AWMP                                                       
         BR    RE                                                               
                                                                                
ARYORD4  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORESELD,CORESELQ),ROWWIDTH=(V,CORESLEN)                  
                                                                                
Array    LKOUT C,9,(A,ARYORD5)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYORD5  LKOUT A,(*,CORESLST),ROWNAME=CORESELD,ROWWIDTH=L'CORESLST,    +        
               NROWS=*                                                          
                                                                                
EstNo    LKOUT C,9,CORESLST,LBIN                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYORD6  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORNMELD,CORNMELQ),ROWWIDTH=(V,CORNMLEN)                  
                                                                                
OName    LKOUT C,10,CORNMNAM,CHAR,LEN=V                                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYORD7  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORURELD,CORURELQ),ROWWIDTH=(V,CORURLEN)                  
                                                                                
ORURL    LKOUT C,11,CORURL,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYOST   LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(CORSTELD,CORSTELQ),ROWWIDTH=(V,CORSTLEN),        +        
               SETFILT=(CORSTST,L'CORSTST)                                      
                                                                                
Stat.    LKOUT C,1,CORSTST,LBIN,ND=Y                                            
Actn.    LKOUT C,2,CORSTACT,CHAR,ND=Y                                           
Spots    LKOUT C,3,CORSTSPT,LBIN,ND=Y                                           
Cost.    LKOUT C,4,CORSTCOS,LBIN,ND=Y                                           
ExpDt    LKOUT C,5,CORSTEXD,BDAT,ND=Y                                           
ExpTm    LKOUT C,6,CORSTEXT,LBIN,ND=Y                                           
TLine    LKOUT C,7,CORSTLNS,LBIN,ND=Y                                           
Array    LKOUT C,8,(A,ARYOST2)                                                  
ARRAY    LKOUT C,9,(A,ARYACTD)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYOST2  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORSCELD,CORSCELQ),ROWWIDTH=(V,CORSCLEN),        +        
               TESTFILT=(CORSCST,L'CORSCST)                                     
                                                                                
Commt    LKOUT C,8,CORSCCMT,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYACTD  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORACELD,CORACELQ),ROWWIDTH=(V,CORACLEN)                  
                                                                                
ActedOn  LKOUT C,9,CORACTXT,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYECN   LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORECELD,CORECELQ),ROWWIDTH=(V,CORECLEN)                  
                                                                                
EContr   LKOUT C,10,CORECTXT,CHAR,LEN=V                                         
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Open Worksheet X'0284'                                              *         
***********************************************************************         
                                                                                
REQOWS   LKREQ H,I#COOWRK,OUTOWS,NEXTREQ=REQCOB                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),DEFAULT=Y,  +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
CamNo    LKREQ F,3,(I,B#SAVED,QCAMIND),(R,VALCAM),OLEN=L'CORKCAM,      +        
               MAXLEN=L'EZEROS,LIST=F,DEFAULT=Y,TEXT=(*,CAMNLIT),COL=*          
StaCd    LKREQ F,4,(I,B#SAVED,QSTAIND),(U,#VALSTA,$VALSTA),            +        
               LIST=F,DEFAULT=Y,OLEN=L'CORIDSTA,TEXT=SP#STA,COL=*               
MktNo    LKREQ F,5,(I,B#SAVED,QMKTIND),LBIN,LIST=F,DEFAULT=Y,          +        
               OLEN=L'CORIDMKT,TEXT=SP#MKT,COL=*                                
StrDt    LKREQ F,6,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,7,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
PrdCd    LKREQ F,8,(I,B#SAVED,QPRDIND),CHAR,LIST=F,DEFAULT=Y,          +        
               OLEN=L'EKEYPRD,TEXT=SP#PRO,COL=*                                 
SlnFt    LKREQ F,9,(I,B#SAVED,QSLNIND),LBIN,LIST=F,DEFAULT=Y,          +        
               OLEN=L'GKEYSLN,TEXT=(*,SLNFLIT),COL=*                            
DemCd    LKREQ F,10,(I,B#SAVED,QDEMIND),(U,#VALDCD,$VALDCD),LIST=F,    +        
               TEXT=(*,DEMOLIT),SORT=N,OLEN=L'NDEMNO,COL=*                      
SendCli  LKREQ F,11,(D,B#SAVED,QSENDCLI),CHAR,TEXT=(*,SCLILIT),COL=*            
SendCam  LKREQ F,12,(D,B#SAVED,QSENDCAM),CHAR,TEXT=(*,SCAMLIT),COL=*            
StateLst LKREQ F,13,(I,B#SAVED,QLSTIND),HEXD,TEXT=(*,LSTILIT),LIST=F,  +        
               OLEN=L'DPRPXST,COL=*                                             
                                                                                
         LKREQ E                                                                
                                                                                
OUTOWS   LKOUT H                                                                
                                                                                
OWSCLT   LKOUT R,1                 Client values                                
Array    LKOUT C,1,(A,ARYCLV),FILTROUT=TSTCLID                                  
         LKOUT E                                                                
                                                                                
OWSCAM   LKOUT R,2                 Campaign/program/demo records                
Array    LKOUT C,2,(A,ARYOWC)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
TSTCLID  CLI   QSENDCLI,C'N'       Test suppress client details                 
         J     SETCCC                                                           
                                                                                
ARYOWC   LKOUT A,(R,NXTCAM),MULTIROW=Y,ROWNAME=CMPRECD                          
                                                                                
Array    LKOUT C,2,(A,ARYCAM),FILTROUT=TSTCAMD                                  
Array    LKOUT C,3,(A,ARYPRG)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
TSTCAMD  CLI   QSENDCAM,C'N'       Test suppress campaign details               
         J     SETCCC                                                           
                                                                                
ARYPRG   LKOUT A,(R,NXTPRG),MULTIROW=Y,ROWNAME=DPRRECD                          
                                                                                
MedCd    LKOUT C,1,DPRKAGMD,(U,#EDTMED,$EDTMED)                                 
CltCd    LKOUT C,2,DPRKCLT,(U,#EDTCLT,$EDTCLT)                                  
CamCo    LKOUT P,DPRKCAM,CAMCOMP                                                
CamNo    LKOUT C,3,(D,B#WORKD,FULL),LBIN                                        
MktNo    LKOUT C,4,DPRKMKT,LBIN                                                 
StaCd    LKOUT C,5,DPRKSTA,(R,EDTSTA)                                           
Line#    LKOUT C,6,DPRKLIN,LBIN                                                 
Array    LKOUT C,7,(A,ARYDPR)                                                   
                                                                                
Array    LKOUT C,4,(A,ARYDDM)      Demo records                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYDDM   LKOUT A,(R,NXTDDM),MULTIROW=Y,ROWNAME=DDMRECD                          
                                                                                
StaCd    LKOUT C,1,DDMKLCL,(R,EDTSTA),ND=Y                                      
Array    LKOUT C,2,(A,ARYDDM2)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read demo records for a program/station                             *         
***********************************************************************         
                                                                                
NXTDDM   GOTOR (#NXTREC,ANXTREC),DMCB,DDMKEYT,('B#DDMREC',SVPRGKEY),   +        
               ('$NXTRXSP',SAVED),0,0                                           
         J     EXITY                                                            
                                                                                
ARYDDM2  LKOUT A,(D,B#DDMREC,DDMFRST),EOT=EOR,                         +        
               ROWID=(DDMDDELD,DDMDDELQ),ROWWIDTH=(V,DDMDDLEN)                  
                                                                                
RBook    LKOUT C,2,DDMDDBOK,BMON,ND=Y                                           
BType    LKOUT C,3,DDMDDBTY,CHAR,ND=Y                                           
LProg    LKOUT C,4,DDMDDPGM,CHAR,ND=Y                                           
LMrkt    LKOUT C,5,DDMDDMKT,CHAR                                                
LSttn    LKOUT C,6,DDMDDSTA,CHAR,ND=Y                                           
RtgSv    LKOUT C,7,DDMDDRSV,CHAR                                                
Array    LKOUT C,8,(A,ARYDDM3)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYDDM3  LKOUT A,(D,B#DDMREC,DDMFRST),EOT=EOR,                         +        
               ROWID=(DDMDVELD,DDMDVELQ),ROWWIDTH=(V,DDMDVLEN)                  
                                                                                
HMrkt    LKOUT C,8,DDMDVMKT,LBIN                                                
Array    LKOUT C,9,(A,ARYDDM4)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYDDM4  LKOUT A,(*,DDMDVDEM),ROWNAME=DDMDVELD,NROWS=*,                +        
               ROWWIDTH=DDMDVDLQ                                                
                                                                                
PROUT    LKOUT P,DDMDVDEM,SETDEMF                                               
DemCd    LKOUT C,9,DDMDVDEM,(U,#EDTDCD,$EDTDCD),LEN=L'EDEMLIST,        +        
               FILTROUT=TSTDEMF,SKIPCOLS=3                                      
DemLV    LKOUT C,10,DDMDVEST,(R,EDTDEM),ND=Y                                    
DemEV    LKOUT C,11,DDMDVLKU,(R,EDTDEM),ND=Y                                    
DemRV    LKOUT C,12,DDMDVREP,(R,EDTDEM),ND=Y                                    
                                                                                
         LKOUT E                                                                
                                                                                
SETDEMF  MVI   BYTE1,0             Set to send this demo                        
         OC    QADEM,QADEM         Test any demo filters                        
         JZ    EXIT                                                             
         SR    RE,RE                                                            
         ICM   RE,7,QADEM          Point to demo filter list                    
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q                                                       
         L     R1,LP_AINP          Point to input demo code                     
SETDEMF2 CLC   0(3,RE),0(R1)       Match to demo filter entry                   
         JE    EXIT                                                             
         AHI   RE,3                Bump to next                                 
         JCT   R0,SETDEMF2         Do for entire list                           
         MVI   BYTE1,1             Set not to send this demo                    
         J     EXIT                                                             
                                                                                
TSTDEMF  CLI   BYTE1,0             Test sending this demo                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Read program records (for a campaign or order)                      *         
***********************************************************************         
                                                                                
NXTPRG   GOTOR (#NXTREC,ANXTREC),DMCB,DPRKEYT,('B#DPRREC',SVCAMKEY),   +        
               ('$NXTRXSP',SAVED),0,0                                           
         JNE   EXITY                                                            
         OC    QALST,QALST         Test state list passed                       
         JZ    EXITY                                                            
         L     R2,ADPRREC                                                       
         USING DPRRECD,R2          Point to program record                      
         GOTOR GETEL,DMCB,('DPRPXELQ',DPRFRST)                                  
         JNE   NXTPRG                                                           
         LR    R2,R1                                                            
         USING DPRPXELD,R2                                                      
         GOTOR LP_ASETK,DMCB,(1,LSTFLT),DPRPXST,SAVED,('FF',LP_D)               
         JNE   NXTPRG                                                           
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
ARYDPR   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRPDELD,DPRPDELQ),ROWWIDTH=(V,DPRPDLEN)                  
                                                                                
EstOv    LKOUT C,7,DPRESTOV,LBIN,ND=Y                                           
StrDt    LKOUT C,8,DPRPDSDT,BDAT                                                
EndDt    LKOUT C,9,DPRPDEDT,BDAT                                                
DayNo    LKOUT C,10,DPRPDDAY,HEXD                                               
StrTm    LKOUT C,11,DPRPDSTM,LBIN                                               
EndTm    LKOUT C,12,DPRPDETM,LBIN                                               
DayPt    LKOUT C,13,DPRPDDPT,CHAR                                               
PrdCd    LKOUT C,14,DPRPDPRD,CHAR,ND=Y                                          
PigCd    LKOUT C,15,DPRPDPR2,CHAR,ND=Y                                          
PrdLn    LKOUT C,16,DPRPDSLN,LBIN                                               
PigLn    LKOUT C,17,DPRPDSL2,LBIN,ND=Y                                          
ShwCd    LKOUT C,18,DPRPDSHO,CHAR,ND=Y                                          
PDesc    LKOUT C,19,DPRPDPGM,CHAR                                               
SCost    LKOUT C,20,DPRPDCOS,LBIN                                               
AdjCd    LKOUT C,21,DPRPDADJ,(R,EDTADJ),ND=Y                                    
RCode    LKOUT C,22,DPRPDREP,CHAR,ND=Y                                          
BuyID    LKOUT C,23,DPRPDID,CHAR,ND=Y                                           
OrdNo    LKOUT C,24,DPRPDORD,LBIN,ND=Y                                          
Array    LKOUT C,31,(A,ARYPDX)                                                  
Array    LKOUT C,25,(A,ARYPCM)                                                  
Array    LKOUT C,26,(A,ARYPAC)                                                  
Array    LKOUT C,28,(A,ARYPUW)                                                  
Array    LKOUT C,29,(A,ARYPSW)                                                  
Array    LKOUT C,29,(A,ARYPSW3)                                                 
Array    LKOUT C,32,(A,ARYPNP)                                                  
Array    LKOUT C,35,(A,ARYPOR),FILTROUT=TSTPORD                                 
RepCs    LKOUT C,37,DPRPDRCS,LBIN,ND=Y                                          
Array    LKOUT C,38,(A,ARYRCM)                                                  
Array    LKOUT C,39,(A,ARYAID)                                                  
Array    LKOUT C,40,(A,ARYOTY)                                                  
                                                                                
Array    LKOUT C,101,(A,ARYPACT),PCVERSION=1.6.0.17                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYPDX   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRPXELD,DPRPXELQ),ROWWIDTH=(V,DPRPXLEN)                  
                                                                                
State    LKOUT C,31,DPRPXST,HEXD                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYPCM   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRCMELD,DPRCMELQ),ROWWIDTH=(V,DPRCMLEN)                  
                                                                                
Comnt    LKOUT C,25,DPRCMCOM,CHAR,LEN=V                                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYPAC   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(CDSACELD,CDSACELQ),ROWWIDTH=(V,CDSACLEN)                  
                                                                                
PRout    LKOUT P,CDSACELD,SETRDT                                                
ActDt    LKOUT C,26,(D,B#WORKD,WORK),BDAT                                       
ActTm    LKOUT C,27,(D,B#WORKD,WORK+L'CDSACADT),HEXD,LEN=L'CDSACCTM             
                                                                                
         LKOUT E                                                                
                                                                                
ARYPUW   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRUNELD,DPRUNELQ),ROWWIDTH=(V,DPRUNLEN)                  
                                                                                
Array    LKOUT C,28,(A,ARYPUW2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYPUW2  LKOUT A,(*,DPRUNWK),ROWNAME=DPRUNELD,ROWWIDTH=L'DPRUNWK,      +        
               NROWS=*                                                          
                                                                                
UWeek    LKOUT C,28,DPRUNWK,CDAT                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYPSW   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRSWELD,DPRSWELQ),ROWWIDTH=(V,DPRSWLEN)                  
                                                                                
Array    LKOUT C,29,(A,ARYPSW2)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYPSW2  LKOUT A,(*,DPRSWWK),ROWNAME=DPRSWELD,ROWWIDTH=DPRSWWKL,       +        
               NROWS=*                                                          
                                                                                
WDate    LKOUT C,29,DPRSWWK,CDAT                                                
Spots    LKOUT C,30,DPRSWNUM,LBIN                                               
                                                                                
         LKOUT E                                                                
***********************************************************************         
* For the new spots per week elem                                               
***********************************************************************         
ARYPSW3  LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRSW2ED,DPRSW2EQ),ROWWIDTH=(V,DPRSW2LN)                  
                                                                                
ARRAY    LKOUT C,29,(A,ARYPSW4)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYPSW4  LKOUT A,(*,DPRSW2WK),ROWNAME=DPRSW2ED,ROWWIDTH=DPRSW2WL,      +        
               NROWS=*                                                          
                                                                                
WDate    LKOUT C,29,DPRSW2WK,CDAT                                               
Spots    LKOUT C,30,DPRSW2NM,LBIN                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYPNP   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRNPELD,DPRNPELQ),ROWWIDTH=(V,DPRNPLEN)                  
                                                                                
MktNo    LKOUT C,32,DPRNPMKT,LBIN                                               
StaCd    LKOUT C,33,DPRNPSTA,(R,EDTSTA)                                         
Cost%    LKOUT C,34,DPRNPPCT,LBIN                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYPOR   LKOUT A,(R,GETPOR),ROWNAME=CORSTELD                                    
                                                                                
OrdSt    LKOUT C,35,CORSTST,LBIN,ND=Y                                           
OrdAc    LKOUT C,36,CORSTACT,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYRCM   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRRCOMD,DPRRCMLQ),ROWWIDTH=(V,DPRRCMLN)                  
                                                                                
RepCm    LKOUT C,38,DPRRCMCM,CHAR,LEN=V                                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYAID   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRAVELD,DPRAVELQ),ROWWIDTH=(V,DPRAVLEN)                  
                                                                                
AvlID    LKOUT C,39,DPRAVID,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYOTY   LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPROFELD,DPROFELQ),ROWWIDTH=(V,DPROFLEN)                  
                                                                                
OfrTy    LKOUT C,40,DPROFTYP,CHAR,LEN=V                                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYPACT  LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRACTD,DPRACELQ),ROWWIDTH=(V,DPRACLEN)                   
                                                                                
PID      LKOUT C,100,DPRACPID,CHAR                                              
DateTime LKOUT C,101,DPRACDTM,CHAR                                              
                                                                                
         LKOUT E                                                                
                                                                                
TSTPORD  L     R1,LP_AINP          Test order number present                    
         MVC   WORK(L'DPRPDORD),DPRPDORD-DPRPDELD(R1)                           
         OC    WORK(L'DPRPDORD),WORK                                            
         J     SETCCC                                                           
                                                                                
***********************************************************************         
* Read order record and point to order status element                 *         
***********************************************************************         
                                                                                
GETPOR   MVC   SVIOVALS,IOVALS                                                  
P        USING IOVALS,SVIOVALS                                                  
         LA    R2,IOKEY                                                         
         USING CORRECD,R2                                                       
         XC    CORKEY,CORKEY                                                    
         MVI   CORKTYPE,CORKTYPQ                                                
         MVI   CORKSTYP,CORKSTYQ                                                
         MVC   CORKAGMD,P.IOKEY+(DPRKAGMD-DPRRECD)                              
         MVC   CORKCLT,P.IOKEY+(DPRKCLT-DPRRECD)                                
         MVC   CORKCAM,P.IOKEY+(DPRKCAM-DPRRECD)                                
         MVC   CORKORD,WORK                                                     
         XC    CORKORD,EFFS                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOXSPDIR+B#CORREC'                      
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   CORKEY(CORKORD-CORKEY),IOKEYSAV                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOXSPFIL+B#CORREC'                     
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ACORREC                                                       
         LA    R2,CORFRST                                                       
         USING CORSTELD,R2                                                      
GETPOR02 CLI   CORSTEL,0           Test end of record                           
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CORSTEL,CORSTELQ    Test order status element                    
         JE    *+16                                                             
         LLC   R0,CORSTLEN                                                      
         AR    R2,R0                                                            
         J     GETPOR02                                                         
         ST    R2,LP_ADATA         Point to status element                      
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Contact browse X'0285'                                              *         
***********************************************************************         
                                                                                
REQCOB   LKREQ H,I#COCONB,OUTCOB,NEXTREQ=REQOSU                                 
                                                                                
StNet    LKREQ F,1,(I,B#SAVED,QSTAIND),CHAR,OLEN=L'DCTDSSTA,LIST=F,    +        
               TEXT=SP#STA,COL=*                                                
                                                                                
         LKREQ E                                                                
                                                                                
OUTCOB   LKOUT H                                                                
                                                                                
COBCOB   LKOUT R,1                 Contact records                              
Array    LKOUT C,1,(A,ARYCOB)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCOB   LKOUT A,(R,NXTCOB),MULTIROW=Y,ROWNAME=DCTRECD                          
                                                                                
PRout    LKOUT P,DCTKCONT,CONCOMP                                               
ConNo    LKOUT C,1,(D,B#WORKD,FULL),LBIN                                        
Array    LKOUT C,2,(A,ARYCOB2)                                                  
Array    LKOUT C,8,(A,ARYCOB3)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYCOB2  LKOUT A,(D,B#DCTREC,DCTFRST),EOT=EOR,                         +        
               ROWID=(DCTDSELD,DCTDSELQ),ROWWIDTH=(V,DCTDSLEN)                  
                                                                                
StaCd    LKOUT C,2,DCTDSSTA,CHAR                                                
EffDt    LKOUT C,3,DCTDSEFF,BDAT                                                
OffCd    LKOUT C,4,DCTDSOFF,CHAR,ND=Y                                           
CliCd    LKOUT C,5,DCTDSCLT,CHAR,ND=Y                                           
FstNm    LKOUT C,6,DCTDSFNM,CHAR,ND=Y                                           
LstNm    LKOUT C,7,DCTDSLNM,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYCOB3  LKOUT A,(D,B#DCTREC,DCTFRST),EOT=EOR,                         +        
               ROWID=(DCTEMELD,DCTEMELQ),ROWWIDTH=(V,DCTEMLEN)                  
                                                                                
EMail    LKOUT C,8,DCTEMEML,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read contact records                                                *         
***********************************************************************         
                                                                                
NXTCOB   GOTOR (#NXTREC,ANXTREC),DMCB,DCTKEYT,('B#DCTREC',0),          +        
               ('$NXTRXSP',SAVED),0,0                                           
         JNE   EXITY                                                            
         OC    QASTA,QASTA         Test any station filters                     
         JZ    EXITY                                                            
         L     R2,ADCTREC                                                       
         USING DCTRECD,R2          R2=A(contact record)                         
         GOTOR GETEL,DMCB,('DCTDSELQ',DCTFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING DCTDSELD,R1                                                      
         ICM   RE,7,QASTA          Apply station filters                        
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q                                                       
NXTCOB02 CLC   DCTDSSTA,0(RE)      Match station to filter                      
         JE    EXITY                                                            
         AHI   RE,L'DCTDSSTA       Bump to next filter value                    
         JCT   R0,NXTCOB02                                                      
         J     NXTCOB                                                           
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Desktop Order Summary X'0286'                                       *         
***********************************************************************         
                                                                                
REQOSU   LKREQ H,I#COOSUM,OUTOSU,NEXTREQ=REQMMP                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
CamNo    LKREQ F,3,(I,B#SAVED,QCAMIND),(R,VALCAM),OLEN=L'CORKCAM,      +        
               MAXLEN=L'EZEROS,LIST=F,DEFAULT=Y,TEXT=(*,CAMNLIT),COL=*          
StrDt    LKREQ F,4,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,5,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
MktNo    LKREQ F,6,(I,B#SAVED,QMKTIND),LBIN,LIST=F,DEFAULT=Y,          +        
               OLEN=L'DDMKMKT,TEXT=SP#MKT,COL=*                                 
Stats    LKREQ F,7,(I,B#SAVED,QSTSIND),LBIN,TEXT=(*,STATLIT),LIST=F,   +        
               OLEN=L'CORSTST,COL=*                                             
SlnFt    LKREQ F,8,(I,B#SAVED,QSLNIND),LBIN,LIST=F,DEFAULT=Y,          +        
               OLEN=L'GKEYSLN,TEXT=(*,SLNFLIT),COL=*                            
DPTFt    LKREQ F,9,(I,B#SAVED,QDPTIND),CHAR,LIST=F,DEFAULT=Y,          +        
               OLEN=L'GKEYDPT,TEXT=(*,DPTFLIT),COL=*                            
DemCd    LKREQ F,10,(I,B#SAVED,QDEMIND),(U,#VALDCD,$VALDCD),LIST=F,    +        
               TEXT=(*,DEMOLIT),SORT=N,OLEN=L'NDEMNO,COL=*                      
UnOrdPgm LKREQ F,11,(D,B#SAVED,QIUNO),CHAR,TEXT=(*,IUNOLIT),COL=*               
TotOnly  LKREQ F,12,(D,B#SAVED,QTOTONLY),CHAR,TEXT=(*,TOTOLIT),COL=*            
                                                                                
         LKREQ E                                                                
                                                                                
OUTOSU   LKOUT H                                                                
                                                                                
OSUOPD   LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYOPD),FILTROUT=TSTUORDN                                 
         LKOUT E                                                                
                                                                                
OSUUNO   LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYUOP),FILTROUT=TSTUORDY                                 
         LKOUT E                                                                
                                                                                
OSUOSS   LKOUT R,1                 Spill demos (programs and orders)            
Array    LKOUT C,1,(A,ARYOSS)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
TSTUORDN CLI   QIUNO,C'Y'          Test not unordered programs only             
         J     SETCCC                                                           
                                                                                
TSTUORDY CLI   QIUNO,C'Y'          Test unordered programs only                 
         BR    RE                                                               
                                                                                
ARYOPD   LKOUT A,(R,NXTOSO),MULTIROW=Y,ROWNAME=CORRECD                          
                                                                                
CamCo    LKOUT P,CORKCAM,CAMCOMP                                                
CamNo    LKOUT C,1,(D,B#WORKD,FULL),LBIN                                        
OrdCo    LKOUT P,CORKORD,ORDCOMP                                                
OrdNo    LKOUT C,2,(D,B#WORKD,FULL),LBIN                                        
Array    LKOUT C,3,(A,ARYOPD2)                                                  
Array    LKOUT C,5,(A,ARYOPD3)                                                  
Array    LKOUT C,2,(A,ARYOSP)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read Order records for Order Summary                                *         
***********************************************************************         
                                                                                
NXTOSO   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTOSO02                                                         
         ICM   RE,7,QAMED          Set media                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
                                                                                
K        USING CORRECD,IOKEY                                                    
NXTOSO02 XC    K.CORKREV,K.CORKREV                                              
         LARL  R0,FLTORD           R0=A(record filter routine)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,OSOKEYT,('B#CORREC',0),          +        
               ('$NXTRXSP',SAVED),0,(R0)                                        
         J     EXITY                                                            
                                                                                
ARYOPD2  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORIDELD,CORIDELQ),ROWWIDTH=(V,CORIDLEN)                  
                                                                                
PRout    LKOUT P,CORIDMKT,SETMKST                                               
MktNo    LKOUT C,3,CORIDMKT,LBIN                                                
StaCd    LKOUT C,4,CORIDSTA,(R,EDTSTA)                                          
                                                                                
         LKOUT E                                                                
                                                                                
SETMKST  L     R1,LP_AINP          Save market/station for nested reads         
         MVC   DMKTSTA,0(R1)                                                    
         BR    RE                                                               
                                                                                
ARYOPD3  LKOUT A,(D,B#CORREC,CORFRST),EOT=EOR,                         +        
               ROWID=(CORSTELD,CORSTELQ),ROWWIDTH=(V,CORSTLEN)                  
                                                                                
PROUT    LKOUT P,CORSTEL,NXTOST                                                 
Stat.    LKOUT C,5,CORSTST,LBIN,ND=Y                                            
Actn.    LKOUT C,6,CORSTACT,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Check for an older revision of the order for status                 *         
***********************************************************************         
                                                                                
NXTOST   L     R3,LP_AINP          Save status and action flag                  
         USING CORSTELD,R3                                                      
         MVC   OSUSTST(L'CORSTST+L'CORSTACT),CORSTST                            
                                                                                
         L     R2,ACORREC                                                       
         USING CORRECD,R2                                                       
         MVC   LORD,CORKORD                                                     
                                                                                
         CLI   OSUSTACT,C'1'       Seller action?                               
         JNE   NXTOST10            No, buyer action                             
*                                  Yes, save off date/time of status            
         MVC   OSUDATE(L'CORSTEXD+L'CORSTEXT),CORSTEXD                          
         J     NXTOST20                                                         
         DROP  R3                                                               
                                                                                
NXTOST10 GOTOR GETEL,DMCB,('CDSACELQ',CORFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CDSACELD,R1                                                      
         MVC   WORK(L'CDSACCDT+L'CDSACCDT),CDSACCDT                             
         OC    CDSACCDT,CDSACCDT   Use change date/time if present              
         JNZ   NXTOST20            otherwise add date/time                      
         MVC   WORK(L'CDSACADT+L'CDSACATM),CDSACADT                             
                                                                                
NXTOST20 CLC   CORKREV,=X'FFFE'    1st instance of order?                       
         JE    NXTOSTX             Yes - what we have is good                   
                                                                                
         MVC   SVIOVALS,IOVALS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOXSPDIR+IO7'                            
         CLC   IOKEY(CORKORD-CORKEY),IOKEYSAV                                   
         JNE   NXTOST90                                                         
         CLC   LORD,IOKEY+CORKORD-CORKEY                                        
         JNE   NXTOST90                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOXSPFIL+IO7'                           
         L     R2,AIO7                                                          
                                                                                
         GOTOR GETEL,DMCB,('CORSTELQ',CORFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CORSTELD,R1                                                      
         CLI   CORSTACT,C'1'          Prior revision is seller action?          
         JNE   NXTOST90               No, was buyer action, we're done          
         CLI   OSUSTACT,C'2'          Recent revision has buyer act?            
         JE    NXTOST50               Yes, buyer loses to seller                
                                                                                
         CLC   OSUDATE,CORSTEXD       My saved date > prior revision?           
         JH    NXTOST90               Yes, we're good                           
         JL    NXTOST50                                                         
         CLC   OSUTIME,CORSTEXT       Same date, save time > prior?             
         JH    NXTOST90               Yes, we're good                           
                                                                                
NXTOST50 MVC   OSUSTST,CORSTST                                                  
         MVC   OSUSTACT,CORSTACT                                                
                                                                                
NXTOST90 MVC   IOVALS(IOVALL),SVIOVALS                                          
                                                                                
NXTOSTX  MVC   CORSTST-CORSTELD(L'CORSTST+L'CORSTACT,R3),OSUSTST                
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
ARYOSP   LKOUT A,(R,NXTOSP),MULTIROW=Y,ROWNAME=DPRRECD                          
                                                                                
Line#    LKOUT C,1,DPRKLIN,LBIN                                                 
Array    LKOUT C,2,(A,ARYOSP2)                                                  
Array    LKOUT C,11,(A,ARYOSP4)                                                 
Array    LKOUT C,3,(A,ARYOSD)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get program records for an Order                                    *         
***********************************************************************         
                                                                                
NXTOSP   LARL  R0,FLTPRG           R0=A(RECORD FILTER ROUTINE)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,OSPKEYT,('B#DPRREC',SVORDKEY),   +        
               ('$NXTRXSP',SAVED),0,(R0)                                        
         J     EXITY                                                            
                                                                                
ARYUOP   LKOUT A,(R,NXTUOP),MULTIROW=Y,ROWNAME=DPRRECD                          
                                                                                
CamCo    LKOUT P,DPRKCAM,CAMCOMP   Emulated order fields                        
CamNo    LKOUT C,1,(D,B#WORKD,FULL),LBIN,FILTROUT=TSTCAM                        
PRout    LKOUT P,,SETMED                                                        
MktNo    LKOUT C,3,DPRKMKT,LBIN,FILTROUT=TSTMKS,SKIPCOLS=1                      
StaCd    LKOUT C,4,DPRKSTA,(R,EDTSTA)                                           
Array    LKOUT C,2,(A,ARYUOP2)     Program record details                       
                                                                                
         LKOUT E                                                                
                                                                                
SETMED   ICM   RF,7,QAMED          Set media                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
         J     EXITY                                                            
                                                                                
ARYUOP2  LKOUT A,(*,DPRRECD),ROWNAME=DPRRECD,NEWEL=B                            
                                                                                
Line#    LKOUT C,1,DPRKLIN,LBIN                                                 
Array    LKOUT C,2,(A,ARYOSP2)                                                  
Array    LKOUT C,11,(A,ARYOSP4)                                                 
Array    LKOUT C,3,(A,ARYOSD)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
TSTCAM   CLC   LCAM,FULL           Test change of campaign                      
         MVC   LCAM,FULL                                                        
         J     SETCCC              Send campaign if different                   
                                                                                
TSTMKS   L     R1,LP_AINP          Point to program record                      
         CLC   LMKTSTA,DPRKMKT-DPRRECD(R1)                                      
         MVC   LMKTSTA,DPRKMKT-DPRRECD(R1)                                      
         J     SETCCC              Send market/station if different             
                                                                                
***********************************************************************         
* Get unordered program lines                                         *         
***********************************************************************         
                                                                                
NXTUOP   LARL  R0,FLTUOP           R0=A(record filter routine)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,UOPKEYT,('B#DPRREC',0),          +        
               ('$NXTRXSP',SAVED),0,(R0)                                        
         J     EXITY                                                            
                                                                                
ARYOSP2  LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRPDELD,DPRPDELQ),ROWWIDTH=(V,DPRPDLEN)                  
                                                                                
DayPt    LKOUT C,2,DPRPDDPT,CHAR                                                
PrdLn    LKOUT C,3,DPRPDSLN,LBIN                                                
PigLn    LKOUT C,4,DPRPDSL2,LBIN,ND=Y                                           
PrdCd    LKOUT C,5,DPRPDPRD,CHAR                                                
PigCd    LKOUT C,6,DPRPDPR2,CHAR,ND=Y                                           
SCost    LKOUT C,7,DPRPDCOS,LBIN                                                
Array    LKOUT C,31,(A,ARYPDX)                                                  
Array    LKOUT C,8,(A,ARYOSP3)         We should only have original             
Array    LKOUT C,8,(A,ARYOSP3A)          or the new, NOT both                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSP3  LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRSWELD,DPRSWELQ),ROWWIDTH=(V,DPRSWLEN)                  
                                                                                
PRout    LKOUT P,DPRSWELD,BLDOSW                                                
Array    LKOUT C,8,(A,ARYOSP5)                                                  
                                                                                
         LKOUT E                                                                
ARYOSP3A LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRSW2ED,DPRSW2EQ),ROWWIDTH=(V,DPRSW2LN)                  
                                                                                
PRout    LKOUT P,DPRSW2ED,BLDOSW                                                
Array    LKOUT C,8,(A,ARYOSP5)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSP4  LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRNPELD,DPRNPELQ),ROWWIDTH=(V,DPRNPLEN)                  
                                                                                
MktNo    LKOUT C,11,DPRNPMKT,LBIN                                               
StaCd    LKOUT C,12,DPRNPSTA,(R,EDTSTA)                                         
Cost%    LKOUT C,13,DPRNPPCT,LBIN                                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Build optimized list of weeks and spots for a program                         
*                                                                               
* Processes the original (x'0B') spots per week elem and                        
*               the new  (x'10') spots per week elem                            
***********************************************************************         
                                                                                
BLDOSW   L     R2,LP_AINP                                                       
         CLI   0(R2),DPRSW2EQ      x'10' - new spots/week elem?                 
         JE    BLDOSW50                                                         
*                                                                               
         USING DPRSWELD,R2                                                      
         LLC   R1,DPRSWLEN                                                      
         SHI   R1,DPRSWLNQ                                                      
         SR    R0,R0                                                            
         LHI   RE,DPRSWWKL                                                      
         DR    R0,RE                                                            
         LTR   R4,R1               R4=number of weeks in element                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LTR   R0,R0               Must be no remainder                         
         JZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,DPRSWWK                                                       
         USING DPRSWWK,R2          R2=A(week/spot input array)                  
         LA    R3,OSPWKS                                                        
BLDOSWD  USING OSPWKS,R3           R3=A(OUTPUT ARRAY)                           
         CLI   QTOTONLY,C'Y'       Test want totals only                        
         JE    BLDOSW10            Yes - just add up the spots                  
         J     BLDOSW06                                                         
                                                                                
BLDOSW02 LLC   RF,DPRSWNUM                                                      
         CLM   RF,3,BLDOSWD.OSPWKSPT       TEST SAME SPOTS IN WEEK              
         JNE   BLDOSW04                                                         
         GOTOR VDATCON,DMCB,(2,OSPWEEK),(0,WORK)                                
         GOTOR VADDAY,DMCB,WORK,WORK+6,7                                        
         GOTOR VDATCON,DMCB,(0,WORK+6),(2,OSPWEEK)                              
         CLC   DPRSWWK,OSPWEEK     Test week in sequence                        
         JNE   BLDOSW04                                                         
         LLC   RF,BLDOSWD.OSPWKREP         BUMP REPLICATION FACTOR              
         AHI   RF,1                                                             
         STC   RF,BLDOSWD.OSPWKREP                                              
         J     BLDOSW08                                                         
                                                                                
BLDOSW04 AHI   R3,OSPWKLEN         Bump pointer and add entry                   
                                                                                
BLDOSW06 MVC   OSPWEEK,DPRSWWK     Save start week of sequence                  
         MVC   BLDOSWD.OSPWKSDT,DPRSWWK ADD NEW ENTRY TO OUTPUT ARRAY           
         LLC   RF,DPRSWNUM                                                      
         STCM  RF,3,BLDOSWD.OSPWKSPT                                            
         MVI   BLDOSWD.OSPWKREP,0                                               
         AHI   R0,1                Bump number of list entries                  
                                                                                
BLDOSW08 AHI   R2,DPRSWWKL         Bump to next week/spots entry                
         JCT   R4,BLDOSW02         Do for number of weeks                       
         J     BLDOSWX                                                          
                                                                                
BLDOSW10 MVC   BLDOSWD.OSPWKSDT,DPRSWWK  SAVE START WEEK OF SEQUENCE            
         MVI   BLDOSWD.OSPWKREP,0                                               
BLDOSW12 LLC   RE,DPRSWNUM         Add spots into total                         
         AR    R0,RE                                                            
         AHI   R2,DPRSWWKL         Bump to next week/spots entry                
         JCT   R4,BLDOSW12         Do for number of weeks                       
         STCM  R0,3,BLDOSWD.OSPWKSPT   SET TOTAL NUMBER OF SPOTS                
         LHI   R0,1                Set one row in output array                  
         J     BLDOSWX                                                          
         DROP  BLDOSWD                                                          
*                                                                               
         USING DPRSW2ED,R2                                                      
BLDOSW50 LLC   R1,DPRSW2LN                                                      
         SHI   R1,DPRSW2LQ                                                      
         SR    R0,R0                                                            
         LHI   RE,DPRSW2WL                                                      
         DR    R0,RE                                                            
         LTR   R4,R1               R4=number of weeks in element                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LTR   R0,R0               Must be no remainder                         
         JZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,DPRSW2WK                                                      
         USING DPRSW2WK,R2         R2=A(week/spot input array)                  
         LA    R3,OSPWKS                                                        
BLDOSWD2 USING OSPWKS,R3           R3=A(OUTPUT ARRAY)                           
         CLI   QTOTONLY,C'Y'       Test want totals only                        
         JE    BLDOSW60            Yes - just add up the spots                  
         J     BLDOSW56                                                         
                                                                                
BLDOSW52 XR    RF,RF                                                            
         ICM   RF,3,DPRSW2NM                                                    
         CLM   RF,3,BLDOSWD2.OSPWKSPT       TEST SAME SPOTS IN WEEK             
         JNE   BLDOSW54                                                         
         GOTOR VDATCON,DMCB,(2,OSPWEEK),(0,WORK)                                
         GOTOR VADDAY,DMCB,WORK,WORK+6,7                                        
         GOTOR VDATCON,DMCB,(0,WORK+6),(2,OSPWEEK)                              
         CLC   DPRSW2WK,OSPWEEK    Test week in sequence                        
         JNE   BLDOSW54                                                         
         LLC   RF,BLDOSWD2.OSPWKREP         BUMP REPLICATION FACTOR             
         AHI   RF,1                                                             
         STC   RF,BLDOSWD2.OSPWKREP                                             
         J     BLDOSW58                                                         
                                                                                
BLDOSW54 AHI   R3,OSPWKLEN         Bump pointer and add entry                   
                                                                                
BLDOSW56 MVC   OSPWEEK,DPRSW2WK    Save start week of sequence                  
         MVC   BLDOSWD2.OSPWKSDT,DPRSW2WK ADD NEW ENTRY TO OUTPUT ARRAY         
         XR    RF,RF                                                            
         ICM   RF,3,DPRSW2NM                                                    
         STCM  RF,3,BLDOSWD2.OSPWKSPT                                           
         MVI   BLDOSWD2.OSPWKREP,0                                              
         AHI   R0,1                Bump number of list entries                  
                                                                                
BLDOSW58 AHI   R2,DPRSW2WL         Bump to next week/spots entry                
         JCT   R4,BLDOSW52         Do for number of weeks                       
         J     BLDOSWX                                                          
                                                                                
BLDOSW60 MVC   BLDOSWD2.OSPWKSDT,DPRSW2WK   SAVE START WEEK OF SEQUENCE         
         MVI   BLDOSWD2.OSPWKREP,0                                              
BLDOSW62 XR    RE,RE               Add spots into total                         
         ICM   RE,3,DPRSW2NM                                                    
         AR    R0,RE                                                            
         AHI   R2,DPRSW2WL         Bump to next week/spots entry                
         JCT   R4,BLDOSW62         Do for number of weeks                       
         STCM  R0,3,BLDOSWD2.OSPWKSPT       SET TOTAL NUMBER OF SPOTS           
         LHI   R0,1                Set one row in output array                  
                                                                                
BLDOSWX  STH   R0,DNROWS           Set number of rows                           
         J     EXITY                                                            
         DROP  R2,BLDOSWD2                                                      
                                                                                
ARYOSP5  LKOUT A,(D,B#SAVED,OSPWKS),NROWS=(B#SAVED,DNROWS),            +        
               ROWNAME=OSPWKS,ROWWIDTH=OSPWKLEN                                 
                                                                                
WDate    LKOUT C,8,OSPWKSDT,CDAT                                                
Spots    LKOUT C,9,OSPWKSPT,LBIN                                                
RreFc    LKOUT C,10,OSPWKREP,LBIN,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSD   LKOUT A,(R,NXTOSD),MULTIROW=Y,ROWNAME=DDMRECD                          
                                                                                
StaCd    LKOUT C,1,DDMKLCL,(R,EDTSTA),ND=Y                                      
PRout    LKOUT P,,BLDDINI                                                       
Array    LKOUT C,2,(A,ARYOSD2)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
BLDDINI  MVI   BYTE3,0                                                          
         BR    RE                                                               
                                                                                
***********************************************************************         
* Read original demo records for a program                            *         
***********************************************************************         
                                                                                
NXTOSD   GOTOR (#NXTREC,ANXTREC),DMCB,OSDKEYT,('B#DDMREC',SVPRGKEY),   +        
               ('$NXTRXSP',SAVED),0,0                                           
         J     EXITY                                                            
                                                                                
ARYOSD2  LKOUT A,(D,B#DDMREC,DDMFRST),EOT=EOR,                         +        
               ROWID=(DDMDVELD,DDMDVELQ),ROWWIDTH=(V,DDMDVLEN)                  
                                                                                
Array    LKOUT P,DDMDVELD,BLDDEM                                                
MktNo    LKOUT C,2,DDMDVMKT,LBIN,FILTROUT=TSTDEM,SKIPCOLS=1                     
Array    LKOUT C,3,(A,ARYOSD3)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSS   LKOUT A,(R,NXTOSS),MULTIROW=Y,ROWNAME=DDMRECD                          
                                                                                
Array    LKOUT C,1,(A,ARYOSSO),FILTROUT=TSTUORDN                                
Array    LKOUT C,1,(A,ARYOSSU),FILTROUT=TSTUORDY                                
Array    LKOUT C,2,(A,ARYOSS3)                                                  
Array    LKOUT C,4,(A,ARYOSS7)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSSO  LKOUT A,(I,B#LP_D,ACORREC),ROWNAME=CORRECD,                   +        
               ROWWIDTH=4000,NROWS=1,NEWEL=Y                                    
                                                                                
CamCo    LKOUT P,CORKCAM,CAMCOMP                                                
CamNo    LKOUT C,1,(D,B#WORKD,FULL),LBIN                                        
OrdCo    LKOUT P,CORKORD,ORDCOMP                                                
OrdNo    LKOUT C,2,(D,B#WORKD,FULL),LBIN                                        
Array    LKOUT C,3,(A,ARYOPD2)                                                  
Array    LKOUT C,5,(A,ARYOPD3)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSSU  LKOUT A,(I,B#LP_D,ADPRREC),ROWNAME=DPRRECD,                   +        
               ROWWIDTH=4000,NROWS=1,NEWEL=Y                                    
                                                                                
CamCo    LKOUT P,DPRKCAM,CAMCOMP   Emulated order fields                        
CamNo    LKOUT C,1,(D,B#WORKD,FULL),LBIN,FILTROUT=TSTCAM                        
PRout    LKOUT P,,SETMED                                                        
MktNo    LKOUT C,3,DPRKMKT,LBIN,FILTROUT=TSTMKS,SKIPCOLS=1                      
StaCd    LKOUT C,4,DPRKSTA,(R,EDTSTA)                                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSS3  LKOUT A,(I,B#LP_D,ADPRREC),ROWNAME=DPRRECD,NEWEL=Y,           +        
               ROWWIDTH=4000,NROWS=1                                            
                                                                                
Line#    LKOUT C,1,DPRKLIN,LBIN                                                 
Array    LKOUT C,2,(A,ARYOSS4)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSS4  LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRPDELD,DPRPDELQ),ROWWIDTH=(V,DPRPDLEN)                  
                                                                                
DayPt    LKOUT C,2,DPRPDDPT,CHAR                                                
PrdLn    LKOUT C,3,DPRPDSLN,LBIN                                                
PigLn    LKOUT C,4,DPRPDSL2,LBIN,ND=Y                                           
PrdCd    LKOUT C,5,DPRPDPRD,CHAR                                                
PigCd    LKOUT C,6,DPRPDPR2,CHAR,ND=Y                                           
Array    LKOUT C,31,(A,ARYPDX)                                                  
Array    LKOUT C,8,(A,ARYOSS5)         We should only have original             
Array    LKOUT C,8,(A,ARYOSS5A)          or the new, NOT both                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSS5  LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRSWELD,DPRSWELQ),ROWWIDTH=(V,DPRSWLEN)                  
                                                                                
PRout    LKOUT P,DPRSWELD,BLDOSW                                                
Array    LKOUT C,8,(A,ARYOSS6)                                                  
                                                                                
         LKOUT E                                                                
ARYOSS5A LKOUT A,(D,B#DPRREC,DPRFRST),EOT=EOR,                         +        
               ROWID=(DPRSW2ED,DPRSW2EQ),ROWWIDTH=(V,DPRSW2LN)                  
                                                                                
PRout    LKOUT P,DPRSW2ED,BLDOSW                                                
Array    LKOUT C,8,(A,ARYOSS6)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSS6  LKOUT A,(D,B#SAVED,OSPWKS),NROWS=(B#SAVED,DNROWS),            +        
               ROWNAME=OSPWKS,ROWWIDTH=OSPWKLEN                                 
                                                                                
WDate    LKOUT C,8,OSPWKSDT,CDAT                                                
Spots    LKOUT C,9,OSPWKSPT,LBIN                                                
RreFc    LKOUT C,10,OSPWKREP,LBIN,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSS7  LKOUT A,(I,B#LP_D,ADDMREC),ROWNAME=DDMRECD,NEWEL=Y,           +        
               ROWWIDTH=4000,NROWS=1                                            
                                                                                
StaCd    LKOUT C,1,DDMKLCL,(R,EDTSTA),ND=Y                                      
Array    LKOUT C,2,(A,ARYOSS8)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYOSS8  LKOUT A,(D,B#DDMREC,DDMFRST),EOT=EOR,                         +        
               ROWID=(DDMDVELD,DDMDVELQ),ROWWIDTH=(V,DDMDVLEN)                  
                                                                                
Array    LKOUT P,DDMDVELD,BLDDEM                                                
MktNo    LKOUT C,2,DDMDVMKT,LBIN,FILTROUT=TSTDEM,SKIPCOLS=1                     
Array    LKOUT C,3,(A,ARYOSD3)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read spill demo records                                             *         
***********************************************************************         
                                                                                
NXTOSS   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTOSS02                                                         
         XC    LCAM,LCAM                                                        
         XC    LMKTSTA,LMKTSTA                                                  
                                                                                
NXTOSS02 LARL  R0,FLTOSR           R0=A(record filter routine)                  
         GOTOR (#NXTREC,ANXTREC),DMCB,OSSKEYT,('B#DDMREC',SVPRGKEY),   +        
               ('$NXTRXSP',SAVED),0,(R0)                                        
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Build demos for a demo record                                       *         
***********************************************************************         
                                                                                
BLDDEM   L     R2,LP_AINP                                                       
         USING DDMDVELD,R2                                                      
         XC    DNROWS,DNROWS                                                    
K        USING DDMKEY,IOKEY                                                     
         CLI   K.DDMKSPL,0         Test spill                                   
         JE    BLDDEM02                                                         
         CLC   DDMDVMKT,K.DDMKMKT  Yes - test correct market                    
         JE    BLDDEM12            Yes - keep this one                          
         J     EXITY                                                            
                                                                                
BLDDEM02 OC    K.DDMKMKT,K.DDMKMKT Test network (market 0)                      
         JNZ   BLDDEM04                                                         
         OC    DDMDVMKT,DDMDVMKT   Test network demos                           
         JZ    BLDDEM12                                                         
         TM    BYTE3,1             Only send first market                       
         JNZ   EXITY                                                            
         OI    BYTE3,1                                                          
         GOTOR FLTMKT,DDMDVMKT     Apply market filter (if any)                 
         JNE   EXITY                                                            
         J     BLDDEM12                                                         
                                                                                
BLDDEM04 OC    DDMDVMKT,DDMDVMKT   Test home market (non-network)               
         JNZ   EXITY                                                            
                                                                                
BLDDEM12 LLC   R1,DDMDVLEN                                                      
         SHI   R1,DDMDVLNQ                                                      
         SR    R0,R0                                                            
         LHI   RE,DDMDVDLQ                                                      
         DR    R0,RE                                                            
         LTR   R4,R1               R4=number of demos in element                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LTR   R0,R0               Must be no remainder                         
         JZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,DDMDVDEM                                                      
         USING DDMDVDEM,R2         R2=A(demo code/value array)                  
         LA    R3,OSDDEM                                                        
         USING OSDDEM,R3           R3=A(output array)                           
                                                                                
BLDDEM14 CLI   DDMDVDEM+1,C'R'     Test rating                                  
         JE    *+12                                                             
         CLI   DDMDVDEM+1,C'E'     Test extended demo                           
         JNE   BLDDEM20                                                         
                                                                                
         CLC   DDMDVEST,EFFS       Test 'null' demo                             
         JE    BLDDEM20                                                         
         OC    DDMDVEST,DDMDVEST   Test any estimated demo value                
         JZ    BLDDEM20                                                         
                                                                                
         ICM   R1,7,QADEM          Test any demo filters given                  
         JZ    BLDDEM18                                                         
         SR    RE,RE                                                            
         ICM   RE,3,LW_NUMN-LW_D(R1)                                            
         AHI   R1,LW_LN2Q          R1=A(demo filters)                           
BLDDEM16 CLC   DDMDVDEM,0(R1)      Match demo to filter value                   
         JE    BLDDEM18                                                         
         AHI   R1,L'DDMDVDEM       Bump to next demo filter                     
         JCT   RE,BLDDEM16         Do for number of filters                     
         J     BLDDEM20                                                         
                                                                                
BLDDEM18 MVC   OSDCODE,DDMDVDEM    Build demo entry to send                     
         MVC   OSDVALUE,DDMDVEST                                                
         AHI   R3,OSDDEMLN                                                      
         AHI   R0,1                Bump number of entries                       
                                                                                
BLDDEM20 AHI   R2,DDMDVDLQ         Bump to next demo in element                 
         JCT   R4,BLDDEM14         Do for number of demos                       
         STH   R0,DNROWS           Set number of rows                           
         J     EXITY                                                            
         DROP  R2,R3,K                                                          
                                                                                
ARYOSD3  LKOUT A,(D,B#SAVED,OSDDEM),NROWS=(B#SAVED,DNROWS),            +        
               ROWNAME=OSDDEM,ROWWIDTH=OSDDEMLN                                 
                                                                                
DemCd    LKOUT C,3,OSDCODE,(U,#EDTDCD,$EDTDCD)                                  
DemVl    LKOUT C,4,OSDVALUE,LBIN                                                
                                                                                
         LKOUT E                                                                
                                                                                
TSTDEM   OC    DNROWS,DNROWS       Test any demos to send                       
         J     SETCCC                                                           
         EJECT                                                                  
***********************************************************************         
* Desktop Market Mapping X'0213'                                                
***********************************************************************         
                                                                                
REQMMP   LKREQ H,I#CDMKMP,OUTMMP,NEXTREQ=REQSMP                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),DEFAULT=Y,  +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
MktLst   LKREQ F,2,(I,B#SAVED,QMMPIND),CHAR,LIST=F,                    +        
               OLEN=L'DMPDTEXT,TEXT=SP#MKT,COL=*                                
StrDt    LKREQ F,3,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,4,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
                                                                                
         LKREQ E                                                                
                                                                                
OUTMMP   LKOUT H                                                                
                                                                                
MMPMKT   LKOUT R,1                 Get Market Mapping Records                   
Array    LKOUT C,1,(A,ARYMMP)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
*                                                                               
ARYMMP   LKOUT A,(R,NXTCMP),MULTIROW=Y,ROWNAME=DMPRECD                          
                                                                                
Array    LKOUT C,255,(A,ARYACTV)                                                
Array    LKOUT C,255,(A,ARYMDDS)                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET Market Mappings Records                                                   
***********************************************************************         
NXTCMP   L     R1,AIO8             USING IO8 IN IOEXEC IS NOT EASY              
         ST    R1,IOADDR                                                        
         ST    R1,LP_ADATA                                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTCMPSQ                                                         
*                                                                               
         XC    CRDTRSTR,CRDTRSTR   SETUP CREATION DATE RANGE                    
         MVC   CRDTREND,EFFS                                                    
*                                                                               
         OC    QESTSDAT,QESTSDAT   ANY START DATE ENTERED                       
         JZ    *+10                                                             
         MVC   CRDTRSTR,QESTSDAT                                                
*                                                                               
         OC    QESTEDAT,QESTEDAT   ANY END DATE ENTERED                         
         JZ    *+10                                                             
         MVC   CRDTREND,QESTEDAT                                                
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING DMPKEY,R4                                                        
         MVI   DMPKTYPE,DMPKTYPQ   X'0D'                                        
         MVI   DMPKSTYP,DMPKSTMQ   X'0D' - MARKET NAME MAPPING                  
         CLC   LP_QMAPN,M#CDSTMP    TEST STATION MAPPING                        
         JNE   *+8                                                              
         MVI   DMPKSTYP,DMPKSTSQ   X'0E' - STATION NAME MAPPING                 
         CLC   LP_QMAPN,M#CDDMMP    TEST DEMO MAPPING                           
         JNE   NXTCMP03                                                         
         MVI   DMPKSTYP,DMPKSTDQ   X'0F' - DEMOCAT NAME MAPPING                 
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         MVC   QAGYHX,AGYMEDBT     SAVE OFF THE AGENCY HEX VALUE                
         NI    QAGYHX,X'F0'        NO MEDIA                                     
         MVC   DMPKAGMD,QAGYHX                                                  
         J     NXTCMP06                                                         
         DROP  R3                                                               
*                                                                               
NXTCMP03 ICM   RF,7,QAMED          Get media                                    
         JZ    NOMORE                                                           
         MVC   DMPKAGMD,LW_DATA1-LW_D(RF)                                       
*                                                                               
NXTCMP06 OC    QESTSDAT(L'QESTSDAT*2),QESTSDAT   ANY DATES ENTERED?             
         JNZ   NXTCMP10                          YES, USE PASSIVE KEYS          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,QACMP          ANY TEXT FILTER?                             
         JZ    NXTCMP20            NONE, ALL TEXT                               
         MVC   DMPKTEXT,LW_DATA2-LW_D(RF)                                       
         J     NXTCMP20                                                         
*                                                                               
NXTCMP10 OI    DMPPKSTY,X'80'      SET FOR THE PASSIVE SUBTYPE                  
         MVC   DMPPKADT,CRDTRSTR                                                
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,QACMP          ANY TEXT FILTER?                             
         JZ    NXTCMP20            NONE, ALL TEXT                               
         MVC   DMPPKTXT,LW_DATA2-LW_D(RF)                                       
*                                                                               
NXTCMP20 MVC   SVDMPKEY,IOKEY                                                   
*                                                                               
NXTCMP25 XC    IOKEY,IOKEY                                                      
         MVC   IOKEY,SVDMPKEY                                                   
*                                                                               
NXTCMPHI GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR'                                
         JNE   NXTCMPH0                                                         
*                                                                               
NXTCMP30 CLC   IOKEY(DMPKTEXT-DMPKEY),IOKEYSAV    TYPE, SUBTYPE, & A/M          
         JNE   EXITN               NO, THEN WE'RE DONE                          
*                                                                               
         OC    QESTSDAT(L'QESTSDAT*2),QESTSDAT   ANY DATES ENTERED?             
         JNZ   NXTCMP40                          YES, USE PASSIVE KEYS          
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,7,QACMP          ANY TEXT FILTER?                             
         JZ    NXTCMPGT            NONE, GET THE RECORD                         
         USING LW_D,R2                                                          
         XR    RE,RE                                                            
         ICM   RE,3,LW_NUMN        RE = Number of entries requested             
         LA    RF,LW_DATA2                                                      
         DROP  R2                                                               
NXTCMP33 CLC   DMPKTEXT,0(RF)                                                   
         JE    NXTCMPGT                                                         
         JL    NXTCMP36            THIS SHOULD NEVER HAPPEN                     
         LA    RF,L'DMPDTEXT(RF)                                                
         JCT   RE,NXTCMP33                                                      
         J     EXITN                                                            
*                                                                               
NXTCMP36 MVC   DMPKTEXT,0(RF)      SET TEXT IN KEY TO NEXT IN LIST              
         XC    DMPKSEQ,DMPKSEQ      AND CLEAR THE REST                          
         J     NXTCMPHI                                                         
*                                                                               
NXTCMP40 CLC   DMPPKADT,CRDTRSTR                                                
         JL    EXITN                                                            
         CLC   DMPPKADT,CRDTREND                                                
         JH    EXITN                                                            
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,7,QACMP          ANY TEXT FILTER?                             
         JZ    NXTCMPGT            NONE, GET THE RECORD                         
         USING LW_D,R2                                                          
         XR    RE,RE                                                            
         ICM   RE,3,LW_NUMN        RE = Number of entries requested             
         LA    RF,LW_DATA2                                                      
         DROP  R2                                                               
NXTCMP43 CLC   DMPPKTXT,0(RF)                                                   
         JE    NXTCMPGT                                                         
         JL    NXTCMP46                                                         
         LA    RF,L'DMPDTEXT(RF)                                                
         JCT   RE,NXTCMP43                                                      
         MVC   DMPPKTXT,=24X'FF'   FORCE THE NEXT DATE                          
         J     NXTCMPHI                                                         
*                                                                               
NXTCMP46 MVC   DMPPKTXT,0(RF)                                                   
         XC    DMPPKSEQ,DMPPKSEQ                                                
         J     NXTCMPHI                                                         
*                                                                               
NXTCMPGT GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOXSPFIL'                               
         JNE   NXTCMPH0                                                         
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,7,QACMP          ANY TEXT FILTER?                             
         JZ    NXTCMPX             NONE, GET THE RECORD                         
         USING LW_D,R2                                                          
         XR    RE,RE                                                            
         ICM   RE,3,LW_NUMN        RE = Number of entries requested             
         LA    RF,LW_DATA2                                                      
         DROP  R2                                                               
*                                                                               
         L     R1,ADCMREC                                                       
         LA    R6,DMPFRST-DMPRECD(R1)                                           
NXTCMP50 CLI   0(R6),0                                                          
         JE    NXTCMPSQ                                                         
         CLI   0(R6),DMPDSELQ      X'01' - DESCRIPTION ELEMENT?                 
         JE    NXTCMP55                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     NXTCMP50                                                         
*                                                                               
         USING DMPDSELD,R6                                                      
NXTCMP55 CLC   DMPDTEXT,0(RF)                                                   
         JE    NXTCMPX                                                          
         LA    RF,L'DMPDTEXT(RF)                                                
         JCT   RE,NXTCMP55                                                      
         DROP  R6                                                               
*                                                                               
NXTCMPSQ LA    R4,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOXSPDIR'                                
         JE    NXTCMP30                                                         
NXTCMPH0 DC    H'0'                                                             
*                                                                               
NXTCMPX  CLC   LP_QMAPN,M#CDDMMP    TEST DEMO MAPPING                           
         JE    EXITY                NO MEDIA                                    
         L     RF,ADCMREC                                                       
         MVC   QMEDX,DMPKAGMD-DMPKEY(RF)                                        
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
         J     EXITY                                                            
***********************************************************************         
* Array Definitiion for Mapping activity                                        
***********************************************************************         
ARYACTV  LKOUT A,(D,B#DCMREC,DMPFRST),EOT=EOR,                         +        
               ROWID=(DMPDSELD,DMPDSELQ),ROWWIDTH=(V,DMPDSLEN)                  
MktTxt   LKOUT C,1,DMPDTEXT,CHAR,ND=Y                                           
         LKOUT E                                                                
                                                                                
TSTCHNGD L     R1,LP_AINP                                                       
         USING DMPDSELD,R1                                                      
         OC    DMPDSCDT,DMPDSCDT         Any change date?                       
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Array Definition for DDS Market                                               
***********************************************************************         
ARYMDDS  LKOUT A,(D,B#DCMREC,DMPFRST),EOT=EOR,                         +        
               ROWID=(DMPMKELD,DMPMKELQ),ROWWIDTH=(V,DMPMKLEN)                  
DDSMkt   LKOUT C,2,DMPMKMKT,UBIN,ND=Y                                           
DtChngd  LKOUT C,3,DMPMKADT,CDAT,ND=Y                                           
TmChngd  LKOUT C,4,DMPMKATM,HEXD,ND=Y                                           
         LKOUT E                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop Station Mapping X'0214'                                               
***********************************************************************         
                                                                                
REQSMP   LKREQ H,I#CDSTMP,OUTSMP,NEXTREQ=REQDMP                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
StaLst   LKREQ F,2,(I,B#SAVED,QSMPIND),CHAR,LIST=F,                    +        
               OLEN=L'DMPDTEXT,TEXT=SP#STA,COL=*                                
StrDt    LKREQ F,3,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,4,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
                                                                                
         LKREQ E                                                                
                                                                                
OUTSMP   LKOUT H                                                                
                                                                                
SMPSTA   LKOUT R,1                 Get Station Mapping Records                  
Array    LKOUT C,1,(A,ARYSMP)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
*                                                                               
ARYSMP   LKOUT A,(R,NXTCMP),MULTIROW=Y,ROWNAME=DMPRECD                          
                                                                                
Array    LKOUT C,255,(A,ARYACTV)                                                
Array    LKOUT C,255,(A,ARYSDDS)                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array Definition for DDS Station                                              
***********************************************************************         
ARYSDDS  LKOUT A,(D,B#DCMREC,DMPFRST),EOT=EOR,                         +        
               ROWID=(DMPSTELD,DMPSTELQ),ROWWIDTH=(V,DMPSTLEN)                  
DDSSta   LKOUT C,2,DMPSTSTA,(R,EDTSTA),ND=Y                                     
DtChngd  LKOUT C,3,DMPSTADT,CDAT,ND=Y                                           
TmChngd  LKOUT C,4,DMPSTATM,HEXD,ND=Y                                           
         LKOUT E                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop Demo Category Mapping X'0215'                                         
***********************************************************************         
                                                                                
REQDMP   LKREQ H,I#CDDMMP,OUTDMP,NEXTREQ=REQDPB                                 
                                                                                
DmoLst   LKREQ F,1,(I,B#SAVED,QDMPIND),CHAR,LIST=F,                    +        
               OLEN=L'DMPDTEXT,TEXT=SP#DEMO,COL=*                               
StrDt    LKREQ F,2,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,3,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
         LKREQ E                                                                
                                                                                
OUTDMP   LKOUT H                                                                
                                                                                
DMPDMO   LKOUT R,1                 Get Demo Category Mapping Records            
Array    LKOUT C,1,(A,ARYDMP)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
ARYDMP   LKOUT A,(R,NXTCMP),MULTIROW=Y,ROWNAME=DMPRECD                          
                                                                                
Array    LKOUT C,255,(A,ARYACTV)                                                
Array    LKOUT C,255,(A,ARYDDDS)                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array Definition for DDS Demo                                                 
***********************************************************************         
ARYDDDS  LKOUT A,(D,B#DCMREC,DMPFRST),EOT=EOR,                         +        
               ROWID=(DMPDMELD,DMPDMELQ),ROWWIDTH=(V,DMPDMLEN)                  
DDSDmo   LKOUT C,2,DMPDMDMO,UBIN,ND=Y                                           
DtChngd  LKOUT C,3,DMPDMADT,CDAT,ND=Y                                           
TmChngd  LKOUT C,4,DMPDMATM,HEXD,ND=Y                                           
         LKOUT E                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* Desktop Daypart Browse X'0210'                                      *         
***********************************************************************         
                                                                                
REQDPB   LKREQ H,I#CDDPTB,OUTDPB,NEXTREQ=REQDTS                                 
                                                                                
MedCd    LKREQ F,1,(D,B#SAVED,QMEDIA),CHAR,TEXT=SP#MED,COL=*                    
                                                                                
         LKREQ E                                                                
                                                                                
OUTDPB   LKOUT H                                                                
                                                                                
DPBDPT   LKOUT R,1                 Get daypart records                          
Array    LKOUT C,1,(A,ARYDPT)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* DATE/TIME/SECOND DOWNLOAD X'0211'                                   *         
***********************************************************************         
                                                                                
REQDTS   LKREQ *,I#CDACTV,OUTDTS,NEXTREQ=REQPGG                                 
                                                                                
OUTDTS   LKOUT H                                                                
                                                                                
DTSDTS   LKOUT R,1                 GET DATE/TIME/SEC                            
PRout    LKOUT P,,GETDTS                                                        
DtTmSc   LKOUT C,1,(D,B#WORKD,WORK),CHAR,ND=Y                                   
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
GETDTS   NTR1  LABEL=NO                                                         
         GOTOR VDATCON,DMCB,(5,0),(23,WORK)                                     
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,FULL                                                       
         ST    R1,FULL2                                                         
         AP    FULL,FULL2                                                       
*                                                                               
         ICM   R1,15,FULL                                                       
         SRL   R1,4                GET RID OF SIGN                              
         XC    WORK2,WORK2                                                      
         STCM  R1,7,WORK2                                                       
         GOTOR VHEXOUT,DMCB,WORK2,WORK+11,1                                     
         MVI   WORK+13,C':'                                                     
         GOTOR VHEXOUT,DMCB,WORK2+1,WORK+14,1                                   
         MVI   WORK+16,C':'                                                     
         GOTOR VHEXOUT,DMCB,WORK2+2,WORK+17,1                                   
         J     EXITY                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Desktop P&G Goal download X'0216'                                   *         
***********************************************************************         
                                                                                
REQPGG   LKREQ H,I#CDPGGO,OUTPGG,NEXTREQ=REQUESTX                               
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,TEXT=SP#MED,COL=*                                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,TEXT=SP#CLI,COL=*                                   
StrDt    LKREQ F,3,(D,B#SAVED,QESTSDAT),CDAT,TEXT=SP#STDT,COL=*                 
EndDt    LKREQ F,4,(D,B#SAVED,QESTEDAT),CDAT,TEXT=SP#ENDT,COL=*                 
                                                                                
         LKREQ E                                                                
                                                                                
OUTPGG   LKOUT H                                                                
                                                                                
PGGOAL   LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYPPG)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYPPG   LKOUT A,(R,NXTPPG),MULTIROW=Y,ROWNAME=GOALRECD                         
                                                                                
PrdCd    LKOUT C,1,GKEYPRD,(U,#EDTPRD,$EDTPRD),ND=Y                             
EstNo    LKOUT C,2,GKEYEST,LBIN,ND=Y                                            
MktNo    LKOUT C,3,GKEYMKT,LBIN,ND=Y                                            
DayPt    LKOUT C,4,GKEYDPT,CHAR,ND=Y                                            
SptLn    LKOUT C,5,GKEYSLN,LBIN,ND=Y                                            
TotLn    LKOUT C,6,GKEYSEC,LBIN,ND=Y                                            
PigCd    LKOUT C,7,GKEYPRD2,(U,#EDTPRD,$EDTPRD),ND=Y,FILTROUT=TSTGPPP           
Array    LKOUT C,10,(A,ARYGWK)     Goal Weekly Values                           
                                                                                
         LKOUT E                                                                
                                                                                
ARYGWK   LKOUT A,(D,B#GOLREC,GDELEM),EOT=EOR,                          +        
               ROWID=(GLCODE,GLCODEQ),ROWWIDTH=(V,GLEN)                         
                                                                                
WSDat    LKOUT C,10,GLWEEK,CDAT,ND=Y                                            
GGRPs    LKOUT C,11,GLGRP,LBIN,ND=Y                                             
GDlrs    LKOUT C,12,GLBUDGET,LBIN,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
TSTGPPP  L     R1,AGOLREC                                                       
         USING GOALRECD,R1                                                      
         TM    GKEYAGY,GKEYPPPQ    Passive piggyback pointer?                   
         J     SETCCC                                                           
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Interpret P# profile from Start and End dates                       *         
* Then use the P# profile values to get corresponding Goal records    *         
*                                                                     *         
* Do not clobber following working storage:                           *         
* FULL2 - Pointer for list of Profiles to be processed                *         
* TEMP2 - List of Profiles (4 bytes each) X'FF' = end of list         *         
* WORK2 - Profile value returned by GETPRF                            *         
* HALF2 - Converted binary market number                              *         
*                                                                     *         
***********************************************************************         
                                                                                
NXTPPG   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTPPG40                                                         
                                                                                
         ICM   RF,7,QAMED          Initialize media                             
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         MVC   QMEDIA,QMEDX                                                     
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
                                                                                
         ICM   RF,7,QACLT          Initialize client                            
         MVC   QCLTX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#GETCLT,AGETCLT)   Read client record                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    QESTSDAT,QESTSDAT   Test start date given                        
         JZ    NOMORE                                                           
         GOTOR VDATCON,DMCB,(2,QESTSDAT),(0,DLODATEE)                           
                                                                                
         OC    QESTEDAT,QESTEDAT   Test end date given                          
         JZ    NOMORE                                                           
         GOTOR VDATCON,DMCB,(2,QESTEDAT),(0,DHIDATEE)                           
                                                                                
         XC    FULL2,FULL2         Init loop counter                            
         XC    TEMP2,TEMP2         Init Profiles to look up                     
                                                                                
         LA    RE,TEMP2                                                         
         MVC   0(4,RE),=C'S0P#'    Init P# Profile                              
         MVC   3(1,RE),DLODATEE+1  Get year digit                               
         LA    RE,4(RE)            Point to next entry                          
         CLC   DLODATEE+1(1),DHIDATEE+1                                         
         JE    *+20                                                             
         MVC   0(4,RE),=C'S0P#'    Init P# Profile                              
         MVC   3(1,RE),DHIDATEE+1  Get year digit                               
         LA    RE,4(RE)            Point to next entry                          
         MVI   0(RE),X'FF'         mark end of Profiles to look up              
         LA    RE,TEMP2                                                         
         ST    RE,FULL2            Save pointer for looping                     
                                                                                
NXTPPG20 L     R1,FULL2            Point to Profile to be processed             
         CLI   0(R1),X'FF'         No more Profiles to process?                 
         JE    NOMORE                                                           
                                                                                
         GOTOR GETPRF,(R1)         Look-up profiles                             
         LA    R1,4(R1)            Point to next Profile                        
         ST    R1,FULL2            For next round                               
                                                                                
         OC    WORK2+2(4),=C'0000' IN CASE PROFILE REC DOES NOT EXIST           
                                                                                
         PACK  DUB,WORK2+2(4)      GET MARKET NUMBER INTO BINARY                
         CVB   RF,DUB                                                           
         STCM  RF,3,HALF2                                                       
                                                                                
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING GKEY,RE                                                          
         MVI   GKEYTYPE,GKEYTYPQ   Record type - Goal record                    
         MVC   GKEYAM,QMEDIA                                                    
         MVC   GKEYCLT,QCLTX                                                    
         DROP  RE                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     NXTPPG60                                                         
                                                                                
NXTPPG40 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
                                                                                
NXTPPG60 CLC   IOKEY(GKEYPRD-GKEY),IOKEYSAV                                     
         JNE   NXTPPG20                                                         
                                                                                
         LA    RE,IOKEY                                                         
         USING GKEY,RE                                                          
         CLC   GKEYMKT,HALF2       Market match that of profile?                
         JNE   NXTPPG40                                                         
         CLC   GKEYEST,WORK2+0     Estimate match that of profile?              
         JNE   NXTPPG40                                                         
         CLC   GKEYDPT,WORK2+1     Daypart match that of profile?               
         JNE   NXTPPG40                                                         
         DROP  RE                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   LP_ADATA,AGOLREC    Point to client record                       
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* ORDER ACTIVITY DOWNLOAD                                             *         
***********************************************************************         
*                                                                               
REQORA   LKREQ H,I#CDORAC,OUTORA,NEXTREQ=REQUESTX                               
DTime    LKREQ F,1,(D,B#SAVED,QTIME),HEXD,TEXT=SP#TIME,COL=*                    
Media    LKREQ F,2,(I,B#SAVED,QORAIND),(U,#VALMED,$VALMED),            +        
               OLEN=L'ORAQMED,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*,ARRAY=S          
CltCd    LKREQ F,3,,(U,#VALCLT,$VALCLT),OLEN=L'ORAQCLT,                +        
               TEXT=SP#CLI,COL=*                                                
Networks LKREQ F,4,,(R,VALONC),VPARM=ORAQSTAQ,OLEN=ORAQSTAL,           +        
               TEXT=SP#NTWRK,COL=*                                              
Campaign LKREQ F,5,,(R,VALONC),VPARM=ORAQCAMQ,OLEN=ORAQCAML,           +        
               TEXT=(*,CAMNLIT),COL=*                                           
Token    LKREQ F,6,,CHAR,OLEN=L'ORAQTOKN,TEXT=SP#KEY,COL=*,ARRAY=E              
         LKREQ E                                                                
                                                                                
OUTORA   LKOUT H                   ** Order Activity Download **                
                                                                                
ORATIM   LKOUT R,X'00FF'           CURRENT TIME                                 
OTIME    LKOUT C,1,(D,B#WORKD,FULL),HEXD,FILTROUT=GETTIME                       
         LKOUT E                                                                
                                                                                
ORAORD   LKOUT R,X'0001'                                                        
Array    LKOUT C,1,(A,ARYORA)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
*                                                                               
ARYORA   LKOUT A,(R,NXTORA),ROWNAME=CORRECD,MULTIROW=Y                          
MedCd    LKOUT C,1,CORKAGMD,(U,#EDTMED,$EDTMED)                                 
CltCd    LKOUT C,2,CORKCLT,(U,#EDTCLT,$EDTCLT)                                  
CamCo    LKOUT P,CORKCAM,CAMCOMP                                                
CamNo    LKOUT C,3,(D,B#WORKD,FULL),LBIN                                        
OrdCo    LKOUT P,CORKORD,ORDCOMP                                                
OrdNo    LKOUT C,4,(D,B#WORKD,FULL),LBIN                                        
Time     LKOUT C,5,(D,B#WORKD,FULL2),HEXD                                       
*rray    LKOUT C,1,(A,ARYORD2),FILTROUT=TSTLREV                                 
*rray    LKOUT C,2,(A,ARYORV)                                                   
*rray    LKOUT C,34,(A,ARYOST)                                                  
         LKOUT E                                                                
                                                                                
REQUESTX LKREQ X                                                                
*                                                                               
***********************************************************************         
* READ ORDER ACTIVITY POINTERS                                                  
***********************************************************************         
                                                                                
NXTORA   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTORA50                                                         
                                                                                
         ICM   R2,7,QAORA                                                       
         JZ    NOMORE                                                           
         GOTOR BUFFER,DMCB,('ORABUFQ',TSAINI),('ORARKEYL',ORARRECL)             
         GOTOR VDATCON,DMCB,(5,0),(2,TODAYCMP)                                  
         SR    R3,R3                                                            
         ICM   R3,3,LW_NUMN-LW_D(R2)                                            
         AHI   R2,LW_LN2Q                                                       
         USING ORAQD,R2            R2=A(REQUEST ARRAY)                          
                                                                                
         L     RE,LP_AWMP                                                       
         MVI   LW_TYPE-LW_D(RE),LW_TRNGQ                                        
         XC    LW_DATA1-LW_D(L'QTIME,RE),LW_DATA1-LW_D(RE)                      
         XC    QTIME,EFFS                                                       
         MVC   LW_DATA1+L'QTIME-LW_D(L'QTIME,RE),QTIME                          
         AHI   RE,LW_LN1Q+(L'QTIME*2)                                           
         STCM  RE,7,QATIM          BUILD TIME RANGE ENTRY                       
                                                                                
         STCM  RE,7,QAMED          BUILD VIRGIN MEDIA ENTRY                     
         MVI   LW_TYPE-LW_D(RE),LW_TLSTQ                                        
         XC    LW_NUMN-LW_D(,RE),LW_NUMN-LW_D(RE)                               
                                                                                
NXTORA10 ICM   RE,7,QAMED          UPDATE MEDIA LIST IF NECESSARY               
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN-LW_D(RE)                                            
         JZ    NXTORA20                                                         
         LA    R1,LW_DATA2-LW_D(RE)                                             
         CLC   ORAQMED,0(R1)                                                    
         JE    NXTORA30                                                         
         AHI   R1,L'ORAQMED                                                     
         JCT   RF,*-14                                                          
                                                                                
                                                                                
NXTORA20 ICM   RF,3,LW_NUMN-LW_D(RE)                                            
         LR    R1,RF                                                            
         MHI   R1,L'ORAQMED                                                     
         LA    R1,LW_LN2Q(R1,RE)                                                
         MVC   0(L'ORAQMED,R1),ORAQMED                                          
         AHI   R1,L'ORAQMED                                                     
         ST    R1,LP_AWMP                                                       
         AHI   RF,1                                                             
         STCM  RF,3,LW_NUMN-LW_D(RE)                                            
                                                                                
NXTORA30 AHI   R2,ORAQL            BUMP INPUT ARRAY POINTER                     
         JCT   R3,NXTORA10         DO FOR NUMBER OF ENTRIES                     
         DROP  R2                                                               
                                                                                
NXTORA50 LARL  R0,FLTORA           R0=A(KEY FILTER ROUTINE)                     
         GOTOR (#NXTREC,ANXTREC),DMCB,OSOKEYT,('B#CORREC',0),          +        
               ('$NXTRXSP',SAVED),(R0),0                                        
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
         LKARY T                                                                
         EJECT                                                                  
***********************************************************************         
* Validate network client                                             *         
***********************************************************************         
                                                                                
VALNCL   MVI   QMEDA,NETMEDQ                                                    
         GOTOR (#VALMED,AVALMED),DMCB,QMEDA,,QMEDX                              
         JNE   EXIT                                                             
         GOTOR (#VALCLT,AVALCLT),LP_AINP                                        
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate search string                                              *         
***********************************************************************         
                                                                                
VALSTR   LM    R2,R4,LP_AINP                                                    
         CHI   R3,L'NPGMPGM                                                     
         JH    EXITN                                                            
         BCTR  R3,0                                                             
         STC   R3,0(R4)            Returns L'string-1,string                    
         MVC   1(L'NPGMPGM,R4),0(R2)                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate demo value (DOVDEMV)                                       *         
***********************************************************************         
                                                                                
VALDEM   LM    R2,R4,LP_AINP                                                    
         XC    0(2,R4),0(R4)                                                    
         CHI   R3,1                Test one byte of input                       
         JNE   *+12                                                             
         CLI   0(R2),C'*'          Test look-up demo value                      
         JE    EXITY                                                            
                                                                                
         MVC   DUB,EZEROS          Validate demo value                          
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         MVZ   DUB(0),0(R2)                                                     
         EX    R3,0(RE)                                                         
         CLC   DUB,EZEROS                                                       
         JNE   EXITN                                                            
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,0(R4)                                                       
         LTR   R0,R0                                                            
         JNZ   EXITY                                                            
         OI    0(R4),X'80'         Set input value is zero                      
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate station/market                                             *         
***********************************************************************         
                                                                                
VALSTM   LM    R2,R4,LP_AINP                                                    
         XC    0(L'DSDSTMK+L'DSDNETW,R4),0(R4)                                  
         CHI   R3,4                Greater than 4 means cable                   
         JH    VALSTM02                                                         
         MVC   0(L'DSDSTMK,R4),SPACES                                           
         SHI   R3,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    R3,0(RE)                                                         
         J     EXITY                                                            
                                                                                
VALSTM02 LR    R1,R2               Split into station/network                   
         LHI   R0,L'DSDSTMK+1                                                   
         SR    RF,RF                                                            
                                                                                
VALSTM04 CLI   0(R1),C'/'          Test for delimiter                           
         JE    VALSTM06                                                         
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         JCT   R0,VALSTM04                                                      
         DC    H'0'                                                             
                                                                                
VALSTM06 AHI   RF,2                                                             
         SR    R3,RF                                                            
         BASR  RE,0                                                             
         MVC   L'DSDSTMK(0,R4),1(R1)                                            
         EX    R3,0(RE)                                                         
         OC    L'DSDSTMK(L'DSDNETW,R4),SPACES                                   
                                                                                
         SR    R1,R2                                                            
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    R1,0(RE)                                                         
         OC    0(L'DSDSTMK,R4),SPACES                                           
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate campaign/order number and output complemented key value    *         
***********************************************************************         
                                                                                
VALORD   DS    0H                                                               
VALCAM   LM    R2,R4,LP_AINP                                                    
         BCTR  R3,0                                                             
         MVC   WORK(L'EZEROS),EZEROS                                            
         BASR  RE,0                                                             
         MVZ   WORK(0),0(R2)                                                    
         EX    R3,0(RE)                                                         
         CLC   WORK(L'EZEROS),EZEROS                                            
         JNE   EXITN                                                            
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,0(R4)                                                      
         XC    0(L'QCAMX,R4),EFFS                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT/MARKET FOR SUBSCRIPTION ORDER DOWNLOAD              *         
***********************************************************************         
                                                                                
VALONC   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),C'*'          TEST ALL NETWORK/CAMPAIGNS                   
         JE    VALONCY                                                          
         CLC   =C'ALL',0(R2)       TEST ALL NETWORK/CAMPAIGNS                   
         JE    VALONCY                                                          
                                                                                
         MVC   TEMP,SPACES                                                      
         SHI   R3,1                                                             
         BASR  RE,0                                                             
         MVC   TEMP(0),0(R2)                                                    
         EX    R3,0(RE)                                                         
         ICM   R0,15,SCANCHAR                                                   
         GOTOR VSCANNER,DMCB,(C'C',TEMP),ELEM,(R0)                              
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         JZ    VALONCN                                                          
         STC   R0,0(R4)                                                         
         AHI   R4,1                                                             
         LA    R2,ELEM                                                          
                                                                                
         CLI   LP_VPARM,ORAQSTAQ   TEST STATION VALIDATION                      
         JNE   VALONC20                                                         
         CHI   R0,ORAQSTA$         TEST TOO MANY STATIONS                       
         JH    VALONCN                                                          
                                                                                
VALONC02 LLC   RF,0(R2)                                                         
         GOTOR (#VALSTA,AVALSTA),DMCB,12(R2),(RF),(R4)                          
         JNE   VALONCN                                                          
         J     VALONCY                                                          
                                                                                
VALONC20 CLI   LP_VPARM,ORAQCAMQ   TEST CAMPAIGN VALIDATION                     
         JNE   VALONCN                                                          
         CHI   R0,ORAQCAM$         TEST TOO MANY CAMPAIGNS                      
         JH    VALONCN                                                          
                                                                                
VALSCM06 LLC   RF,0(R2)                                                         
         BCTR  RF,0                                                             
         MVC   WORK(L'EZEROS),EZEROS                                            
         BASR  RE,0                                                             
         MVZ   WORK(0),12(R2)                                                   
         EX    RF,0(RE)                                                         
         CLC   WORK(L'EZEROS),EZEROS                                            
         JNE   EXITN                                                            
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,0(R4)                                                      
         XC    0(L'ORAQCAM,R4),EFFS                                             
VALONCY  J     EXITY                                                            
VALONCN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Apply filters to a goal record                                      *         
***********************************************************************         
FLTGOK   TM    IOKEY+GKEYAGY-GKEY,GKEYPPPQ                                      
         JZ    EXITY                                                            
         J     EXITN                                                            
***********************************************************************         
* Apply filters to a station record                                   *         
***********************************************************************         
                                                                                
FLTSTA   L     R2,ASTAREC                                                       
         USING STARECD,R2                                                       
                                                                                
         CLI   STAKCALL+4,C'D'     Temp code for Canada                         
         JE    EXITN                                                            
         CLI   STAKCALL+4,C'S'                                                  
         JE    EXITN                                                            
                                                                                
         OC    MARKETA,MARKETA     Filter market number if required             
         JZ    *+14                                                             
         CLC   SMKT,MARKETA                                                     
         JNE   EXIT                                                             
                                                                                
         OC    QCLTA,QCLTA         Test client filter given                     
         JNZ   *+14                                                             
         CLC   STAKCLT,EZEROS      No - only return defaults                    
         J     EXIT                                                             
                                                                                
         CLC   STAKCLT,EZEROS      Is this a default record?                    
         JE    *+14                                                             
         CLC   STAKCLT,QCLTA       No - match to filter client                  
         J     EXIT                                                             
                                                                                
         MVC   SVIOVALS,IOVALS     Read for client override                     
         MVC   IOKEY(L'STAKEY),STAKEY                                           
         MVC   IOKEY+(STAKCLT-STAKEY)(L'STAKCLT),QCLTA                          
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOSTAFIL+IOB9'                          
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         CLC   STAKCLT,QCLTA       Test if client record exists                 
         JE    EXITN                                                            
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Apply filters to a buy directory key                                *         
***********************************************************************         
                                                                                
FLTBYK   TM    IOKEY+(BUYKBUY-BUYKEY),X'80'                                     
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Apply filters to a buy record                                       *         
***********************************************************************         
                                                                                
         USING BUYREC,R1                                                        
FLTBYR   LR    R2,R1               SAVE ADDRESS OF BUY IN R2                    
         TM    MAP#MGAN,MAPSMGAN   Test doing makegood analysis                 
         JZ    FLTBYR20                                                         
         OC    BDMGDATE,BDMGDATE   Test makegood code on buy                    
         JNZ   FLTBYRY                                                          
                                                                                
***NOP***SHOULDN'T BE NEEDED**                                                  
         XC    FULL,FULL           EXTRACT COST FOR FLTMGD ROUTINE              
         MVC   FULL+(L'FULL-L'BDCOST),BDCOST                                    
***NOP***SHOULDN'T BE NEEDED**                                                  
                                                                                
         J     FLTBYR40            (no period checks for makegood)              
                                                                                
FLTBYR20 CLC   BDSTART,BUYEDAT     Test buy period overlaps request             
         JH    EXIT                                                             
         CLC   BDEND,BUYSDAT                                                    
         JL    EXIT                                                             
         J     FLTBYRY             Keep buy if dates overlap request            
                                                                                
FLTBYR40 LA    R1,BDELEM                                                        
         USING REGELEM,R1                                                       
FLTBYR45 LLC   R0,RLEN             Bump to next element                         
         AR    R1,R0                                                            
         CLI   RCODE,EOR           Test end of record                           
         JE    EXITN                                                            
         CLI   RCODE,RCORGQ        Test for spot elements                       
         JL    FLTBYR45                                                         
         CLI   RCODE,X'0D'                                                      
         JH    FLTBYR45                                                         
         GOTOR FLTSPT              Apply spot filters                           
         JNE   FLTBYR45                                                         
         DROP  R1                                                               
*                                                                               
         USING BUYREC,R2                                                        
***FLTBYRY  BRAS  RE,GETCS2           SET BUY COST2                             
*                                                                               
FLTBYRY  TM    MAP#PIND,MAPSPIND   PINERGY BUY DOWNLOAD?                        
         JZ    EXITY                                                            
         OC    BDTIMEND,BDTIMEND   Yes, any end time set?                       
         JNZ   *+10                     Yes, no problem                         
         MVC   BDTIMEND,BDTIMST         No, make it same as start time          
*                                                                               
         OC    BUYKMKTN,BUYKMKTN   Do we have a market number?                  
         JZ    FLTBYRY5            No                                           
         MVC   FULL(1),BUYKAM      Yes, check the media                         
         NI    FULL,X'0F'                                                       
         CLI   FULL,X'01'               only want TV if market in key           
         JNE   EXITN                                                            
FLTBYRY5 GOTOR LP_AAWMP,DMCB,(L'BUYKSTA,BUYKSTA),DSTAIND,DSTAMAXQ,LP_D          
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Apply filters to a spot element                                     *         
***********************************************************************         
                                                                                
         USING REGELEM,R1                                                       
FLTSPT   CLC   RDATE,QESTSDAT      Test spot within request period              
         BLR   RE                                                               
         CLC   RDATE,QESTEDAT                                                   
         BHR   RE                                                               
                                                                                
FLTMGD   TM    MAP#MGAN,MAPSMGAN   Test doing makegood analysis                 
         JNZ   *+8                 Yes                                          
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
         L     RF,ABUYREC          Test magegood buy                            
         CLC   BDMGDATE-BUYREC(,RF),SPACES                                      
         JNH   *+8                 No                                           
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
         TM    RSTATUS,RSMINSDQ    Test missed spot                             
         JZ    *+8                                                              
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
         TM    RSTATUS,RSRATOVQ    Test rate override                           
         JNZ   FLTMGD02                                                         
         OC    FULL,FULL           No - test spot cost                          
         BZR   RE                  Include if zero spot cost                    
         J     FLTMGD04                                                         
                                                                                
FLTMGD02 OC    RPCOST,RPCOST       Include if cost override is zero             
         BZR   RE                                                               
                                                                                
FLTMGD04 CLI   RCODE,RCPOTOQ       Test OTO                                     
         JNE   FLTMGD06                                                         
         TM    RSTATUS,RSMINUSQ    Test pre-empt (-OTO)                         
         JZ    FLTMGD06                                                         
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
FLTMGD06 CLI   RLEN,RLPOL1LQ                                                    
         BLR   RE                                                               
         TM    RPSTAT2,FF-X'80'    Test makegood code present                   
         JZ    *+8                                                              
         CR    RE,RE               Include if there is                          
         BR    RE                                                               
         LTR   RE,RE               Exclude if there isn't                       
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Apply filters to an order record (no filtering of order revisions)  *         
***********************************************************************         
                                                                                
FLTORD   L     R2,ACORREC                                                       
         USING CORRECD,R2          R2=A(order record)                           
         TM    MAP#ORDS,MAPSORDS   Test order summary                           
         JNZ   FLTORD02                                                         
                                                                                
         OC    LORD,LORD           Test first time                              
         JZ    *+14                                                             
         CLC   CORKORD,LORD        or change of order number                    
         JE    EXITY               No - must be a revision so keep it           
                                                                                
         MVC   LORD,CORKORD        Set current order number                     
         MVC   ORDREV,CORKREV      And its latest revision number               
                                                                                
FLTORD02 OC    QASTS,QASTS         Test any status filters                      
         JZ    FLTORD06                                                         
         GOTOR GETEL,DMCB,('CORSTELQ',CORFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CORSTELD,R1         R1=A(order status element)                   
         ICM   RE,7,QASTS                                                       
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q          Re=A(status filter list)                     
FLTORD04 CLC   CORSTST,0(RE)       Match current status to list                 
         JE    FLTORD06                                                         
         AHI   RE,L'CORSTST        Bump to next filter status                   
         JCT   R0,FLTORD04                                                      
         J     FLTORDN                                                          
         DROP  R1                                                               
                                                                                
FLTORD06 OC    QASTA,QASTA         Test any station/network filters             
         JZ    FLTORD10                                                         
         GOTOR GETEL,DMCB,('CORIDELQ',CORFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CORIDELD,R1         R1=A(order id element)                       
         ICM   RE,7,QASTA                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   QSTAIND,LW_TALLQ                                                 
         JE    FLTORD10                                                         
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q          RE=A(station/network filter list)            
FLTORD08 CLC   CORIDSTA,0(RE)      Match current station to list                
         JE    FLTORD10                                                         
         AHI   RE,L'CORIDSTA       Bump to next station/network                 
         JCT   R0,FLTORD08                                                      
         J     FLTORDN                                                          
         DROP  R1                                                               
                                                                                
FLTORD10 OC    QABYR,QABYR         Test any buyer filters                       
         JZ    FLTORD14                                                         
         GOTOR GETEL,DMCB,('CORIDELQ',CORFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CORIDELD,R1         R1=A(order id element)                       
         ICM   RE,7,QABYR                                                       
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q          Re=A(buyer filter list)                      
FLTORD12 CLC   CORIDBYR,0(RE)      Match current buyer to list                  
         JE    FLTORD14                                                         
         AHI   RE,L'CORIDBYR       Bump to next buyer                           
         JCT   R0,FLTORD12                                                      
         J     FLTORDN                                                          
         DROP  R1                                                               
                                                                                
FLTORD14 OC    QASLR,QASLR         Test any recipient filters                   
         JZ    FLTORD18                                                         
         GOTOR GETEL,DMCB,('CORRLELQ',CORFRST)                                  
         JNE   FLTORDN             No recipients - reject order                 
         USING CORRLELD,R1         R1=a(recipient element)                      
FLTORD16 GOTOR FLTREC              Apply filter to current element              
         JE    FLTORD18                                                         
         LLC   R0,CORRLLEN         Bump to next element on record               
         AR    R1,R0                                                            
         CLI   CORRLEL,CORRLELQ    Test if a recipient element                  
         JE    FLTORD16                                                         
         J     FLTORDN                                                          
         DROP  R1                                                               
                                                                                
FLTORD18 OC    QACTSDAT,QACTSDAT   Apply activity date filters                  
         JNZ   *+14                                                             
         OC    QACTEDAT,QACTEDAT                                                
         JZ    FLTORD20                                                         
         GOTOR GETEL,DMCB,('CDSACELQ',CORFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CDSACELD,R1         R1=A(activity element)                       
         MVC   WORK(L'CDSACADT),CDSACADT                                        
         OC    CDSACCDT,CDSACCDT                                                
         JZ    *+10                                                             
         MVC   WORK(L'CDSACCDT),CDSACCDT                                        
         OC    QACTSDAT,QACTSDAT   Apply activity start date filter             
         JZ    *+14                                                             
         CLC   WORK(L'CDSACCDT),QACTSDAT                                        
         JL    FLTORDN                                                          
         OC    QACTEDAT,QACTEDAT   Apply activity end date filter               
         JZ    FLTORD20                                                         
         CLC   WORK(L'CDSACCDT),QACTEDAT                                        
         JH    FLTORDN                                                          
         DROP  R1                                                               
                                                                                
FLTORD20 OC    QAMKT,QAMKT         Test any market filters                      
         JZ    FLTORD22                                                         
         GOTOR GETEL,DMCB,('CORIDELQ',CORFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CORIDELD,R1         R1=A(order id element)                       
         OC    CORIDMKT,CORIDMKT   Test network                                 
         JZ    FLTORD22                                                         
         GOTOR FLTMKT,CORIDMKT     Apply market filter (if any)                 
         JNE   EXITN                                                            
         DROP  R1                                                               
                                                                                
FLTORD22 DS    0H                  Next filter code goes here                   
                                                                                
FLTORDY  J     EXITY                                                            
                                                                                
FLTORDN  LA    R2,IOKEY                                                         
         TM    MAP#ORDS,MAPSORDS   Test order summary                           
         JNZ   FLTORDN1                                                         
         ICM   R0,15,CORKORD       Rejected order - bump to next                
         AHI   R0,1                                                             
         STCM  R0,15,CORKORD                                                    
FLTORDN1 XC    CORKREV,CORKREV                                                  
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Apply filters to a program record                                   *         
***********************************************************************         
                                                                                
FLTPRG   L     R2,ADPRREC                                                       
         USING DPRRECD,R2          R2=A(program record)                         
                                                                                
         GOTOR GETEL,DMCB,('DPRPDELQ',DPRFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R1                                                            
         USING DPRPDELD,R3         R3=A(program description element)            
                                                                                
         TM    MAP#ORDS,MAPSORDS   Test order summary                           
         JZ    FLTPRG02                                                         
         MVC   WORK(L'CORKORD),SVORDKEY+(CORKORD-CORKEY)                        
         XC    WORK(L'CORKORD),EFFS                                             
         CLC   DPRPDORD,WORK       Yes - match order number                     
         JNE   EXITN                                                            
                                                                                
FLTPRG02 GOTOR LP_ASETK,DMCB,(1,DPTFLT),DPRPDDPT,SAVED,('FF',LP_D)              
         JNE   EXITN                                                            
                                                                                
         GOTOR LP_ASETK,DMCB,(1,SECFLT),DPRPDSLN,SAVED,('FF',LP_D)              
         JNE   EXITN                                                            
                                                                                
         OC    QESTSDAT,QESTSDAT   Apply start date filter                      
         JZ    *+14                                                             
         CLC   DPRPDEDT,QESTSDAT                                                
         JL    EXITN                                                            
         OC    QESTEDAT,QESTEDAT   Apply end date filter                        
         JZ    *+14                                                             
         CLC   DPRPDSDT,QESTEDAT                                                
         JH    EXITN                                                            
                                                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Test for unordeed program line                                      *         
***********************************************************************         
                                                                                
FLTUOP   L     R2,ADPRREC                                                       
         USING DPRRECD,R2          R2=A(program record)                         
                                                                                
         GOTOR GETEL,DMCB,('DPRPDELQ',DPRFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R1                                                            
         USING DPRPDELD,R3         R3=A(program description element)            
         OC    DPRPDORD,DPRPDORD   Test this line ordered                       
         JNZ   EXITN               Yes - discard                                
         J     FLTPRG02            Apply other filters                          
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Read program and order record for a found spill demo record         *         
***********************************************************************         
                                                                                
FLTOSR   MVC   SVIOVALS,IOVALS                                                  
                                                                                
         L     R3,IOADDR                                                        
         USING DDMRECD,R3                                                       
K        USING DDMKEY,IOKEY        Check key                                    
         CLC   K.DDMKEY(DDMKMKT-DDMKEY),DDMKEY                                  
         JNE   FLTOSRN                                                          
         CLC   K.DDMKSTA(DDMKSPL-DDMKSTA),DDMKSTA                               
         JNE   FLTOSRN                                                          
K        USING DPRKEY,IOKEY        Build key of program record                  
         XC    K.DPRKEY,K.DPRKEY                                                
         MVI   K.DPRKTYPE,DPRKTYPQ                                              
         MVI   K.DPRKSTYP,DPRKSTYQ                                              
         MVC   K.DPRKAGMD,DDMKAGMD                                              
         MVC   K.DPRKCLT,DDMKCLT                                                
         MVC   K.DPRKCAM,DDMKCAM                                                
         CLI   QMEDA,NETMEDQ                                                    
         JE    *+10                                                             
         MVC   K.DPRKMKT,DDMKMKT                                                
         MVC   K.DPRKSTA,DDMKSTA                                                
         MVC   K.DPRKLIN,DDMKLIN                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDD+IOXSPDIR+B#DPRREC'                     
         JE    FLTOSR02                                                         
         TM    IOERR,IOEDEL        Test program record is deleted               
         JNZ   FLTOSRN                                                          
         DC    H'0'                Program record doesn't exist                 
                                                                                
FLTOSR02 GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOXSPFIL+B#DPRREC'                     
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ADPRREC                                                       
         USING DPRRECD,R2          R2=A(program record)                         
         GOTOR GETEL,DMCB,('DPRPDELQ',DPRFRST)                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING DPRPDELD,R1         R1=A(program description element)            
         CLI   QIUNO,C'Y'          Test unordered program lines only            
         JNE   FLTOSR04                                                         
         OC    DPRPDORD,DPRPDORD   Test order number present                    
         JZ    FLTOSRY             No - keep                                    
         J     FLTOSRN                                                          
                                                                                
FLTOSR04 OC    DPRPDORD,DPRPDORD   Test order number present                    
         JZ    FLTOSRN             No - discard                                 
                                                                                
K        USING CORKEY,IOKEY        Build key of order record                    
         XC    K.CORKEY,K.CORKEY                                                
         MVI   K.CORKTYPE,CORKTYPQ                                              
         MVI   K.CORKSTYP,CORKSTYQ                                              
         MVC   K.CORKAGMD,DDMKAGMD                                              
         MVC   K.CORKCLT,DDMKCLT                                                
         MVC   K.CORKCAM,DDMKCAM                                                
         MVC   K.CORKORD,DPRPDORD                                               
         XC    K.CORKORD,EFFS                                                   
         DROP  R1                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOXSPDIR+B#CORREC'                      
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   K.CORKEY(CORKREV-CORKEY),IOKEYSAV                                
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOXSPFIL+B#CORREC'                     
         JE    FLTOSRY                                                          
         DC    H'0'                                                             
                                                                                
FLTOSRY  MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
                                                                                
FLTOSRN  MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITN                                                            
         DROP  R2,R3,K                                                          
         EJECT                                                                  
***********************************************************************         
* Apply recipient (seller) filters to recipient list element          *         
*                                                                     *         
* Ntry:- R1=A(Recipient list element)                                 *         
***********************************************************************         
                                                                                
         USING CORRLELD,R1                                                      
FLTREC   NTR1  LABEL=NO                                                         
         ICM   R2,7,QASLR                                                       
         SR    R3,R3               Get number of filters into R3                
         ICM   R3,3,LW_NUMN-LW_D(R2)                                            
         AHI   R2,LW_LN2Q          R2=A(recipient filters)                      
                                                                                
FLTREC02 SR    R4,R4                                                            
         ICM   R4,1,0(R2)          R4=length of current filter value-1          
         SR    R0,R0                                                            
         ICM   R0,1,CORRLLEN                                                    
         SHI   R0,CORRLLNQ         R0=L'recipient value                         
         LA    RF,CORRLWHO         Rf=A(recipient value)                        
                                                                                
FLTREC04 CR    R4,R0               Test filter longer than recipient            
         JNL   FLTREC06                                                         
         BASR  RE,0                                                             
         CLC   0(0,RF),1(R2)                                                    
         EX    R4,0(RE)            Test filter string in recipient              
         JE    EXITY               Yes - include on first match                 
         AHI   RF,1                Bump to next character in recipient          
         JCT   R0,FLTREC04         Do until looked at entire string             
                                                                                
FLTREC06 AHI   R2,L'CORRLWHO+1     Bump to next filter value                    
         JCT   R3,FLTREC02         Do for number of filters                     
         J     EXITN               No filters match - reject this one           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Apply market filter (R1=A(Market number to test))                   *         
***********************************************************************         
                                                                                
FLTMKT   CLI   QMKTIND,LW_TALLQ                                                 
         BER   RE                                                               
         ICM   RF,7,QAMKT                                                       
         BZR   RE                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF)                                            
         AHI   RF,LW_LN2Q          RF=A(market filter list)                     
FLTMKT02 CLC   0(L'CORIDMKT,R1),0(RF)                                           
         BER   RE                                                               
         AHI   RF,L'CORIDMKT       Bump to next market                          
         JCT   R0,FLTMKT02                                                      
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* APPLY FILTER FOR ORDER ACTIVITY                                               
***********************************************************************         
         USING CORKEY,IOKEY                                                     
FLTORA   DS    0H                                                               
         ICM   R1,7,QAORA                                                       
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R1)                                            
         AHI   R1,LW_LN2Q                                                       
         USING ORAQD,R1                                                         
*                                                                               
FLTORA10 CLC   CODKAGMD(L'ORAQMED),ORAQMED                                      
         JNE   FLTORA50                                                         
*                                                                               
         OC    ORAQCLT,ORAQCLT     APPLY CLIENT FILTER                          
         JZ    *+14                                                             
         CLC   ORAQCLT,CORKCLT                                                  
         JNE   FLTORA50                                                         
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,ORAQSTA#       TEST ALL STATION                             
         JZ    FLTORA20                                                         
         LA    RF,ORAQSTA          APPLY STATION FILTERS                        
         CLC   CODKSTA,0(RF)                                                    
         JE    FLTORA20                                                         
         AHI   RF,L'ORAQSTA                                                     
         JCT   R2,*-14                                                          
         J     FLTORA50                                                         
*                                                                               
FLTORA20 SR    R2,R2                                                            
         ICM   R2,1,ORAQCAM#       TEST ALL CAMPAIGNS                           
         JZ    FLTORA50                                                         
         LA    RF,ORAQCAM          APPLY CAMPAIGN FILTERS                       
         CLC   CODKCAM,0(RF)                                                    
         JE    FLTORA50                                                         
         AHI   RF,L'ORAQCAM                                                     
         JCT   R2,*-14                                                          
         J     FLTORA50                                                         
*                                                                               
FLTORA50 AHI   R1,ORAQL            BUMP TO NEXT FILTER ENTRY                    
         JCT   R0,FLTORA10         DO FOR NUMBER OF ENTRIES                     
         LTR   RE,RE               TEST MATCHES ANY REQUEST                     
         JZ    EXITN               NO - NOT APPLICABLE                          
*                                                                               
         USING ORARECD,ORAREC                                                   
FLTORA60 MVC   ORARAGM,CODKAGMD                                                 
         MVC   ORARCLT,CODKCLT                                                  
         MVC   ORARCAM,CODKCAM                                                  
         MVC   ORARORD,CODKORD                                                  
         MVC   ORAREC2,ORAREC                                                   
         GOTOR BUFFER,DMCB,('ORABUFQ',TSARDH)                                   
         JE    EXITN               PREVIOUSLY SENT                              
         MVC   ORAREC,ORAREC2                                                   
         GOTOR BUFFER,DMCB,('ORABUFQ',TSAADD)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   FULL2,CODKTIME                                                   
         XC    FULL2,EFFS                                                       
         J     EXITY                                                            
*                                                                               
                                                                                
ORAQD    DSECT                     ** ORDER ACTIVITY REQUEST **                 
ORAQMED  DS    XL(L'CODKAGMD)      AGENCY/MEDIA CODE                            
ORAQCLT  DS    XL(L'CODKCLT)       CLIENT CODE                                  
                                                                                
ORAQSTAQ EQU   C'S'                VPARM VALUE FOR STATION                      
ORAQSTA$ EQU   6                   MAXIMUM NUMBER OF STATION ENTRIES            
ORAQSTA# DS    X                   NUMBER OF STATION ENTRIES (0=ALL)            
ORAQSTA  DS    (ORAQSTA$)XL(L'CORIDSTA)                                         
ORAQSTAL EQU   *-ORAQSTA#          LENGTH OF MARKET FILTERS                     
                                                                                
ORAQCAMQ EQU   C'C'                VPARM VALUE FOR CAMPAIGN                     
ORAQCAM$ EQU   6                   MAXIMUM NUMBER OF CLIENT ENTRIES             
ORAQCAM# DS    X                   NUMBER OF CLIENT ENTRIES (0=ALL)             
ORAQCAM  DS    (ORAQCAM$)XL(L'CORKCAM)                                          
ORAQCAML EQU   *-ORAQCAM#          LENGTH OF CLIENT FILTERS                     
                                                                                
ORAQTOKN DS    CL4                 REQUEST TOKEN                                
                                                                                
ORAQL    EQU   *-ORAQD                                                          
                                                                                
ORARECD  DSECT                     ** CHECK FOR CHANGES BUFFER **               
ORARAGM  DS    XL(L'CODKAGMD)      AGENCY/MEDIA                                 
ORARCLT  DS    XL(L'CODKCLT)       CLIENT                                       
ORARCAM  DS    XL(L'CODKCAM)       CAMPAIGN                                     
ORARORD  DS    XL(L'CODKORD)       ORDER NUMBER                                 
ORARKEYL EQU   *-ORARECD                                                        
ORARRECL EQU   *-ORARECD                                                        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Locate first occurrence of an element on a record                   *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows:-                   *         
*                                                                     *         
*        P1/B0   - Element code                                       *         
*        P1/B1-3 - A(First element on record)                         *         
*                                                                     *         
* Exit:- CC=Equal if element found (R1 points to element), not equal  *         
*           if not found (R1 set to zero)                             *         
***********************************************************************         
                                                                                
GETEL    ICM   RF,7,1(R1)          RF=A(first element on record)                
GETEL02  CLI   0(RF),0             Test end of record                           
         JE    GETELN                                                           
         CLC   0(1,RF),0(R1)       Match element code to input                  
         JE    GETELY                                                           
         LLC   R0,1(RF)            No match - bump to next element              
         AR    RF,R0                                                            
         J     GETEL02                                                          
GETELY   LR    R1,RF               Point to located element                     
         J     GETELX                                                           
GETELN   SR    R1,R1               Clear address if not found                   
GETELX   CR    R1,RF               Set condition code for caller                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Read market record                                                  *         
*                                                                     *         
* Ntry:- R1=A(Binary market number)                                   *         
*                                                                     *         
* Exit:- WORK=Market name, AMKTREC points to a market record          *         
***********************************************************************         
                                                                                
GETMKT   NTR1  LABEL=NO                                                         
         MVC   SVIOVALS,IOVALS                                                  
         SR    R0,R0                                                            
         ICM   R0,3,0(R1)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK(L'MKTKMKT),DUB                                              
         LA    R1,IOKEY                                                         
         USING MKTREC,R1           Read market record                           
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMEDA                                                    
         MVC   MKTKMKT,WORK                                                     
         MVC   MKTKAGY,AGENCY                                                   
         MVC   MKTKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOSTAFIL+B#MKTREC'                      
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         L     R1,AMKTREC                                                       
         JE    *+14                                                             
         MVI   MKTNAME,C'?'                                                     
         MVC   MKTNAME+1(L'MKTNAME-1),MKTNAME                                   
         MVC   WORK(L'MKTNAME),MKTNAME                                          
         J     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Read address record for a station                                   *         
*                                                                     *         
* Ntry:- ASTAREC points to a station record (STARECD)                 *         
*                                                                     *         
* Exit:- AADDREC points to an address record (ADDRECD),  CC=equal if  *         
*        record was found, CC=not equal if record not found           *         
***********************************************************************         
                                                                                
GETADD   NTR1  LABEL=NO                                                         
         MVC   SVIOVALS,IOVALS                                                  
         L     R2,ASTAREC                                                       
         USING STARECD,R2          R2=A(station record)                         
         LA    R1,IOKEY                                                         
         USING ADDRECD,R1                                                       
         MVI   ADDKTYPE,ADDKTYPQ                                                
         MVC   ADDKMED,STAKMED                                                  
         MVC   ADDKCALL,STAKCALL                                                
         MVC   ADDKAGY,STAKAGY                                                  
         MVC   ADDKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOSTAFIL+B#ADDREC'                      
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         JE    EXIT                                                             
         L     R1,AADDREC          Dummy up an address record                   
         XC    ADDRECD(ADRREC2Q),ADDRECD                                        
         MVC   ADDRKEY,IOKEYSAV                                                 
         MVI   ANAME,C'?'                                                       
         MVC   ANAME+1(L'ANAME-1),ANAME                                         
         J     EXITN                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Calculate checksum for a record                                     *         
*                                                                     *         
* Ntry:- LP_AINP points to record address                             *         
*                                                                     *         
* Exit:- FULL=Calculated checksum                                     *         
***********************************************************************         
                                                                                
GETSUM   L     RE,LP_AINP          RE=A(A(record))                              
         L     RE,0(RE)                                                         
         SR    RF,RF                                                            
         ICM   RF,3,BUYRLEN-BUYREC(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,FULL                                                       
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Calculate Pinergy Checksum for a Buyline                                      
*                                                                               
* Ntry:- LP_AINP points to record address                                       
*                                                                               
* EXIT:- LINCKSUM=CALCULATED CHECKSUM                                           
***********************************************************************         
                                                                                
PGETSUM  L     RE,LP_AINP          RE= A( A(buyline) )                          
         LA    R3,LINCKSUM                                                      
         L     R2,0(RE)            R2 is now pointing to the buyline            
         USING BUYRECD,R2                                                       
*                                                                               
         LA    RE,BDPROGRM         PROGRAM NAME                                 
         LA    RF,L'BDPROGRM                                                    
         SR    R0,R0                                                            
         CKSM  R0,RE               CKSM GOOD FOR STORING 4 BYTES/MORE           
         JO    *-4                                                              
         STCM  R0,12,HALF          CHECKSM HAS FOR EXAMPLE 1234ABCD             
         XR    RE,RE               TOP HALF=1234   LOWER HALF=ABCD              
         ICM   RE,1,HALF           Add HOB of TOP to LOB of LOWER               
         ICM   RE,2,HALF+1         Add LOB of TOP to HOB of LOWER               
         AR    R0,RE               SO  3412 + ABCD = DFDF                       
         STCM  R0,3,0(R3)          This is our 2 byte checksum                  
         AHI   R3,2                                                             
*                                                                               
         MVC   0(L'BDPROGT,R3),BDPROGT   ADJ CODE                               
         AHI   R3,L'BDPROGT                                                     
*                                                                               
         MVC   0(L'BDDAY,R3),BDDAY       DAYS                                   
         AHI   R3,L'BDDAY                                                       
*                                                                               
         MVC   0(L'BDDAYPT,R3),BDDAYPT     DAYPART                              
         AHI   R3,L'BDDAYPT                                                     
*                                                                               
         XR    R0,R0                     4 bytes so cksm is not good            
         ICM   R0,3,BDTIMST                                                     
         XR    RE,RE                                                            
         ICM   RE,1,BDTIMEND             Add HOB of end to LOB of start         
         ICM   RE,2,BDTIMEND+1           Add LOB of end to HOB of start         
         AR    R0,RE                                                            
         STCM  R0,3,0(R3)                                                       
         AHI   R3,2                                                             
*                                                                               
         LA    RE,BDELEM                                                        
         SR    R0,R0                     IN CASE WE DON'T HAVE BUYID            
PCKSM010 CLI   0(RE),0                                                          
         JE    PCKSM030                                                         
         CLI   0(RE),IDELCODQ            X'70' - ID ELEMENT?                    
         JE    PCKSM020                                                         
         LLC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         J     PCKSM010                                                         
*                                                                               
PCKSM020 LR    R2,RE                                                            
         USING IDELEM,R2                                                        
         LA    RE,0(R2)                                                         
         LLC   RF,1(R2)                                                         
         CKSM  R0,RE               4 byte checksum to 2 byte checksum           
         JO    *-4                                                              
         STCM  R0,12,HALF          Checksum, has, for example 1234ABCD          
         XR    RE,RE               TOP HALF=1234   LOWER HALF=ABCD              
         ICM   RE,1,HALF           Add HOB of TOP to LOB of LOWER               
         ICM   RE,2,HALF+1         Add LOB of TOP to HOB of LOWER               
         AR    R0,RE               So  3412 + ABCD = DFDF                       
*                                                                               
PCKSM030 STCM  R0,3,0(R3)                                                       
*                                                                               
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to get station call letters                                 *         
*                                                                     *         
* Ntry:- R1=A(Compressed market/station)                              *         
***********************************************************************         
                                                                                
GETSTA   NTR1  LABEL=NO                                                         
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,QMEDA                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,0(R1)                                                   
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPMED,RADMEDQ     Test Radio                                   
         JE    GETSTA02                                                         
         CLI   STAPMED,NTRMEDQ     Test Network Radio                           
         JE    GETSTA02                                                         
         MVI   STAPQSTA+L'STAPQSTA-1,C' '                                       
         CLC   STAPQNET,SPACES     Test cable channel set                       
         JE    GETSTAX                                                          
         MVI   STAPQSTA+L'STAPQSTA-1,C'/'                                       
         CLI   STAPQSTA+3,C' '     Deal with 3 character station codes          
         JNE   GETSTAX                                                          
         MVC   STAPQSTA+3(L'STAPQSTA+L'STAPQNET-3),STAPQSTA+4                   
         MVI   STAPQSTA+L'STAPQSTA+L'STAPQNET-1,C' '                            
         J     GETSTAX                                                          
                                                                                
GETSTA02 LLC   R0,STAPQSTA+L'STAPQSTA-1                                         
         LA    RE,STAPQSTA+L'STAPQSTA-2                                         
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
         STC   R0,2(RE)                                                         
         MVI   3(RE),C' '                                                       
                                                                                
GETSTAX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Use CPP guide (if any) to calculate missing goal value              *         
*                                                                     *         
* Ntry:- R1=A(Goal week element)                                      *         
*        AGOLREC=A(Goal record)                                       *         
*                                                                     *         
* Exit:- Goal element updated if CCP guide used                       *         
***********************************************************************         
                                                                                
GETCPP   NTR1  LABEL=NO                                                         
         ST    R1,FULL1            Save A(goal element)                         
         L     R2,AGOLREC                                                       
         USING GOALREC,R2          R2=A(goal record)                            
         MVC   SVIOVALS,IOVALS     Save current i/o values                      
                                                                                
         L     RF,ACLTREC                                                       
         CLI   CPROF+8-CLTRECD(RF),C'0'                                         
         JNE   GETCPPX                                                          
                                                                                
         MVC   WORK(L'GDCPPES),GDCPPES                                          
         CLI   GDCPPES,0                                                        
         JNE   *+10                                                             
         MVC   WORK(L'GKEYEST),GKEYEST                                          
         CLC   GVPPEST,WORK        Test have established CPP guide              
         JE    GETCPP36                                                         
                                                                                
         MVC   GVPPEST,GKEYEST     No - build CPP guide                         
                                                                                
         LA    R3,IOKEY                                                         
         USING ESTHDR,R3           Read estimate to establish period            
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,GKEYAM                                                    
         MVC   EKEYCLT,GKEYCLT                                                  
         MVC   EKEYPRD,POLPRD                                                   
         MVC   EKEYEST,GKEYEST                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO7'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO7'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,IOADDR                                                        
         MVC   QSTRDATE(L'ESTART+L'EEND),ESTART                                 
         DROP  R3                                                               
                                                                                
         GOTOR GETPRF,PROFS00      Get SPOT/00 profile (into WORK2)             
         LLC   R0,WORK2+2          Get profile date control                     
         CHI   R0,6                Only allow 6 through 8                       
         JL    *+12                                                             
         CHI   R0,8                                                             
         JNH   *+6                                                              
         SR    R0,R0                                                            
         MVC   WORK+00(4),VGETBRD                                               
         MVC   WORK+04(4),VADDAY                                                
         MVC   WORK+08(4),VGETDAY                                               
         MVC   WORK+12(4),VDATCON                                               
         GOTOR VMOBILE,DMCB,(12,QSTRDATE),((R0),GVPEMOS),WORK,WORK2             
                                                                                
         L     R4,AIO5                                                          
         USING CPPTABD,R4          R4=A(CPP guide)                              
         MVI   CPPTABD,CPPTEOTQ    Set end of CPP guide array                   
                                                                                
         LA    R3,IOKEY                                                         
CPP      USING GOALREC,R3                                                       
         MVC   CPP.GKEY,GKEY                                                    
         MVI   CPP.GKEYPRD,POLPRDQ                                              
         MVI   GVPPEST2,0                                                       
         CLI   GDCPPES,0           Test CPP estimate override present           
         JE    GETCPP12                                                         
         MVC   CPP.GKEYCLT,GDCPPCL                                              
         MVC   CPP.GKEYEST,GDCPPES                                              
         MVC   GVPPEST2,GDCPPES2                                                
                                                                                
GETCPP12 XC    CPP.GKEYDPT(GKCNTRLS-GKEYDPT),CPP.GKEYDPT                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO6'                               
         JE    GETCPP16                                                         
         DC    H'0'                                                             
                                                                                
GETCPP14 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO6'                               
         JE    GETCPP16                                                         
         DC    H'0'                                                             
                                                                                
GETCPP16 CLC   CPP.GKEY(GKEYDPT-GKEY),IOKEYSAV                                  
         JNE   GETCPP32                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO6'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,IOADDR           R3=A(goal record)                            
                                                                                
         L     R4,AIO5             R4=A(CPP guide array)                        
         LHI   R0,CPPTMAXD                                                      
GETCPP18 CLI   CPPTABD,CPPTEOTQ    Test end of array                            
         JE    GETCPP20                                                         
         CLC   CPPTDPT,CPP.GKEYDPT Match array key                              
         JE    GETCPP22                                                         
         AHI   R4,CPPTABL                                                       
         JCT   R0,GETCPP18                                                      
         DC    H'0'                Too many dayparts                            
                                                                                
GETCPP20 XC    CPPTABD(CPPTABL),CPPTABD                                         
         MVI   CPPTABD+CPPTABL,CPPTEOTQ                                         
         MVC   CPPTDPT,CPP.GKEYDPT                                              
         J     GETCPP22                                                         
                                                                                
GETCPP22 LA    R3,CPP.GDELEM                                                    
CPPEL    USING GLEMENT,R3                                                       
         LA    RE,GVPEMOS                                                       
         LA    RF,CPPTVALS                                                      
         SR    R0,R0                                                            
                                                                                
GETCPP24 CLI   CPPEL.GLCODE,EOR    Test end of record                           
         JE    GETCPP14                                                         
         CLI   CPPEL.GLCODE,GLCODEQ                                             
         JNE   GETCPP30                                                         
         CLC   CPPEL.GLWEEK,0(RE)  Test before request start date               
         JL    GETCPP30                                                         
                                                                                
GETCPP26 CLC   CPPEL.GLWEEK,2(RE)  Test in this month                           
         JNH   GETCPP28                                                         
         AHI   RE,L'GVPEMOS        No - bump to next month                      
         AHI   RF,L'CPPTVALS                                                    
         CLI   0(RE),FF            Test end of request period                   
         JNE   GETCPP26                                                         
         J     GETCPP14            Yes - get next record                        
                                                                                
GETCPP28 MVC   0(L'GLBUDGET,RF),CPPEL.GLBUDGET                                  
                                                                                
GETCPP30 LLC   R0,CPPEL.GLEN       Bump to next element on record               
         AR    R3,R0                                                            
         J     GETCPP24                                                         
                                                                                
GETCPP32 CLI   GVPPEST2,0          Test second estimate present                 
         JE    GETCPP34                                                         
         LA    R3,IOKEY                                                         
         MVC   CPP.GKEYEST,GVPPEST2                                             
         MVI   GVPPEST2,0                                                       
         J     GETCPP12                                                         
                                                                                
GETCPP34 GOTOR GETSLN              Get A(spot length array)                     
                                                                                
GETCPP36 MVC   HALF,CLTEQU30       Default to 30 second factor                  
         LLC   R1,GKEYSEC                                                       
         MHI   R1,2                                                             
         A     R1,ASLNTAB                                                       
         CLI   1(R1),0             Test spot length is equivalenced             
         JE    GETCPP38                                                         
         LLC   RE,0(R1)            Yes - use equivalent spot length             
         SRL   RE,2                                                             
         MHI   RE,DEQTABW                                                       
         LA    RE,DEQTAB(RE)                                                    
         MVC   HALF,DEQTFACT-DEQTAB(RE)                                         
                                                                                
GETCPP38 OC    HALF,HALF           Test no equivalency factor                   
         JZ    GETCPPX                                                          
                                                                                
         L     R4,AIO5             R4=A(CPP guide)                              
GETCPP40 CLI   CPPTABD,CPPTEOTQ    Test end of array                            
         JE    GETCPPX                                                          
         CLC   CPPTDPT,GKEYDPT     Match goal daypart to array                  
         JE    *+12                                                             
         AHI   R4,CPPTABL                                                       
         J     GETCPP40                                                         
                                                                                
         L     R2,FULL1            R2=A(goal week element)                      
         USING GLEMENT,R2                                                       
         LA    RE,GVPEMOS          Locate correct month for goal week           
         LA    RF,CPPTVALS                                                      
GETCPP42 CLC   GLWEEK,0(RE)                                                     
         JL    *+14                                                             
         CLC   GLWEEK,2(RE)                                                     
         JNH   GETCPP44                                                         
         AHI   RE,L'GVPEMOS                                                     
         AHI   RF,L'CPPTVALS                                                    
         CLI   0(RE),FF                                                         
         JNE   GETCPP42                                                         
         J     GETCPPX                                                          
                                                                                
GETCPP44 OC    GLBUDGET,GLBUDGET   Test zero dollars                            
         JNZ   GETCPP46                                                         
                                                                                
         ICM   R0,15,GLGRP         Calculate dollars from grp                   
         MH    R0,HALF                                                          
         ICM   R1,15,0(RF)                                                      
         MR    R0,R0                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CLTEQU30                                                    
         MHI   RF,1000                                                          
         DR    R0,RF                                                            
         SRA   R1,1                                                             
         CR    R0,RF                                                            
         JL    *+8                                                              
         AHI   R1,1                                                             
         STCM  R1,15,GLBUDGET                                                   
         J     GETCPPX                                                          
                                                                                
GETCPP46 ICM   R1,15,GLBUDGET      Calculate grp from dollars                   
         MHI   R1,2000                                                          
         SR    R0,R0                                                            
         ICM   RF,15,0(RF)                                                      
         DR    R0,RF                                                            
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         SLL   R1,1                                                             
         SR    RF,RF                                                            
         ICM   RF,3,CLTEQU30                                                    
         MR    R0,RF                                                            
         LH    RF,HALF                                                          
         DR    R0,RF                                                            
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STCM  R1,15,GLGRP                                                      
                                                                                
GETCPPX  MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXIT                                                             
         DROP  CPP,CPPEL,R2,R4                                                  
                                                                                
CPPTABD  DSECT ,                   ** Dsect for goal cpp guide **               
CPPTEOTQ EQU   FF                  End of array indicator                       
CPPTDPT  DS    CL(L'GKEYDPT)       Daypart code                                 
CPPTMAXM EQU   12                  Maximum number of months                     
CPPTVALS DS    (CPPTMAXM)XL(L'GLBUDGET)                                         
CPPTABL  EQU   *-CPPTDPT           Width of array entry                         
CPPTMAXD EQU   15                  Maximum number of dayparts                   
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Look up program profiles                                            *         
*                                                                     *         
* Ntry:- R1=A(4 byte profile key)                                     *         
*                                                                     *         
* Exit:- WORK2 contains profile values                                *         
***********************************************************************         
                                                                                
GETPRF   NTR1  LABEL=NO                                                         
         XC    WORK(12),WORK       Build profile key & get profiles             
         MVC   WORK+00(4),0(R1)                                                 
         MVC   WORK+04(L'AGENCY),AGENCY                                         
         MVC   WORK+06(L'QMEDA),QMEDA                                           
         MVC   WORK+07(L'QCLTA),QCLTA                                           
         MVI   WORK+10,C'*'                                                     
         L     RF,ACLTREC                                                       
         SR    R0,R0                                                            
         CLC   PROFS00A,WORK                                                    
         JNE   *+8                                                              
         LHI   R0,X'90'            Read agency level profile only               
         MVC   WORK+11(L'COFFICE),COFFICE-CLTHDR(RF)                            
         GOTOR VGETPROF,DMCB,((R0),WORK),WORK2,VDATAMGR                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Add an entry to estimate/product/demo array                         *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows:-                   *         
*                                                                     *         
*        P1      - A(Estimate number)                                 *         
*        P2      - A(Product number)                                  *         
*        P3      - A(Demo code)                                       *         
***********************************************************************         
                                                                                
ADDEPD   NTR1  LABEL=NO                                                         
         LM    R1,R3,0(R1)                                                      
         LA    R4,ELEM                                                          
         USING EPDTABD,R4          R4=record build area                         
         MVC   EPDTEST,0(R1)                                                    
         MVC   EPDTPRD,0(R2)                                                    
         MVC   EPDTDEM,0(R3)                                                    
         LH    R0,DEPDTAB#         Get number of array entries                  
*                                                                               
         LHI   RF,EPDMAXN1                                                      
         TM    LP_FLAG,LP_FOFFL    TEST OFF-LINE                                
         JZ    *+8                                                              
         LHI   RF,EPDMAXN2                                                      
*                                                                               
         GOTOR VBINSRCH,DMCB,(1,EPDTABD),AEPDTAB,(R0),EPDTABL,         +        
               EPDKEYL,(RF)                                                     
         MVC   DEPDTAB#,10(R1)     Save number of array entries                 
         OC    1(3,R1),1(R1)       Test array is full                           
         JNZ   EXITY                                                            
         DC    H'0'                                                             
         DROP  R4                                                               
                                                                                
EPDTABD  DSECT ,                   ** Estimate/Product/Demo array **            
EPDTEST  DS    XL(L'EKEYEST)       Estimate number                              
EPDTPRD  DS    XL(L'GKEYPRD)       Product number                               
EPDKEYL  EQU   *-EPDTABD                                                        
EPDTDEM  DS    XL(L'EDEMLIST)      Demo code                                    
EPDTABL  EQU   *-EPDTABD                                                        
EPDMAXN1 EQU   NMSTMAX1*NMSTABL/EPDTABL  ONLINE MAX # OF ARRAY ENTRIES          
EPDMAXN2 EQU   NMSTMAX2*NMSTABL/EPDTABL  OFFLINE MAX # OF ARRAY ENTRIES         
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Locate spot length array to use for current agency/media            *         
*                                                                     *         
* Ntry:- LP_AGY is alpha agency code                                  *         
*        QMEDIA is media code                                         *         
*                                                                     *         
* Exit:- ASLNTAB points to spot length array                          *         
***********************************************************************         
                                                                                
GETSLN   STM   RE,R1,12(RD)                                                     
         L     R1,VSLNTAB                                                       
         LH    RE,0(R1)            RE=L'each agy/media entry                    
         L     RF,2(R1)                                                         
         AR    RF,R1               RF=A(end of array)                           
         AHI   R1,6                R1=A(first array entry)                      
                                                                                
         LHI   R0,TELMEDQ          Set TV media                                 
         CLI   QMEDA,TELMEDQ       Use for television                           
         JE    GETSLN02                                                         
         CLI   QMEDA,NETMEDQ       and Network television                       
         JE    GETSLN02                                                         
         CLI   QMEDA,COMMEDQ       and Combined                                 
         JE    GETSLN02                                                         
*                                                                               
         LHI   R0,RADMEDQ          Set radio media                              
         CLI   QMEDA,RADMEDQ       Use for radio                                
         JE    GETSLN02                                                         
         CLI   QMEDA,NTRMEDQ       Use for Network radio                        
         JNE   *+2                                                              
                                                                                
GETSLN02 CLC   EZEROS(L'LP_AGY),0(R1)                                           
         JE    *+14                                                             
         CLC   LP_AGY,0(R1)        No, match on agency                          
         JNE   *+12                                                             
         CLM   R0,1,2(R1)          Match on media                               
         JE    GETSLN04                                                         
         JXLE  R1,RE,GETSLN02      Invalid agency/media                         
         DC    H'0'                                                             
                                                                                
GETSLN04 AHI   R1,4                                                             
         ST    R1,ASLNTAB          Save A(start of spot length array)           
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Locate network pointer element on buy record                        *         
*                                                                     *         
* Ntry:- R1=A(buy record)                                             *         
*                                                                     *         
* Exit:- WORK contains network station value or binary zeroes         *         
***********************************************************************         
                                                                                
GETNET   XC    WORK(L'GPWRNET),WORK                                             
         AHI   R1,BDELEMX-BUYREC   Point to first subsidiary element            
         USING NTWKELEM,R1                                                      
GETNET02 CLI   NTWKCODE,EOR        Test end of record                           
         BER   RE                                                               
         CLI   NTWKCODE,NTWKCODQ   Test network pointer element                 
         JE    *+16                                                             
         LLC   R0,NTWKLEN                                                       
         AR    R1,R0                                                            
         J     GETNET02                                                         
         CLI   NTWKLEN,L'NTWKCODE+L'NTWKLEN+L'GPWRNET                           
         BNER  RE                                                               
         MVC   WORK(L'GPWRNET),NTWKMKST                                         
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GET CURRENT TIME IN TU'S IN FULL                                              
***********************************************************************         
GETTIME  LR    R2,RE               GET CURRENT TIME IN TU'S IN FULL             
         TIME  TU                                                               
         ST    R0,FULL                                                          
         LR    RE,R2                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Edit network station code                                           *         
***********************************************************************         
                                                                                
EDTNET   LM    R2,R4,LP_AINP                                                    
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,QMEDA                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPSTA,0(R2)                                                    
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   QMEDA,RADMEDQ       Test Radio                                   
         JE    EDTNET02                                                         
         CLI   QMEDA,NTRMEDQ       Test Network Radio                           
         JE    EDTNET02                                                         
         MVC   0(L'STAPQSTA-1,R4),STAPQSTA                                      
         LHI   R3,L'STAPQSTA-1                                                  
         STCM  R3,15,LP_OLEN                                                    
         CLI   STAPQSTA+L'STAPQSTA-1,C'/'                                       
         JNE   EXITY                                                            
         MVC   0(L'STAPQSTA+L'STAPQNET,R4),STAPQSTA                             
         LHI   R3,L'STAPQSTA+L'STAPQNET                                         
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTNET02 MVC   0(L'STAPQSTA,R4),STAPQSTA                                        
         LHI   R3,L'STAPQSTA                                                    
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit network program show code                                      *         
***********************************************************************         
                                                                                
EDTSHW   CLI   QMEDA,NETMEDQ       Test network television                      
         JNE   XCOLEN                                                           
         LM    R2,R4,LP_AINP                                                    
         LHI   R0,3                                                             
         LHI   R1,3-1                                                           
         LA    RF,3(R2)                                                         
         BASR  RE,0                                                             
         CLI   0(RF),C'-'                                                       
         JE    EDTSHW02                                                         
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCTR  R0,RE                                                            
         J     XCOLEN                                                           
                                                                                
EDTSHW02 BASR  RE,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         STCM  R1,15,LP_OLEN                                                    
                                                                                
         CLI   L'BDPROGRM-1(R2),0     (-S?) estimated=actual?                   
         JNE   EXITY                                                            
         CLI   0(R2),C'='             Worried about short like =ROS-S           
         JNE   EXITY                                                            
         LA    R6,L'BDPROGRM-2(R2)                                              
         CLI   0(R6),C' '             Find first nonspace from end              
         JH    *+8                                                              
         JCT   R6,*-8                                                           
         CLI   0(R6),C'S'             Program name ends in an 'S'?              
         JNE   EXITY                                                            
         SR    R6,RF                  Rest of program is "-S"?                  
         CHI   R6,1                   Should be a difference of 1               
         JH    EXITY                                                            
         J     XCOLEN                 Not a show code if  =XXX-S                
                                                                                
***********************************************************************         
* Edit start/end time                                                           
***********************************************************************         
                                                                                
EDTTIM   LM    R2,R4,LP_AINP                                                    
         OC    0(2,R2),0(R2)                                                    
         JZ    XCOLEN                                                           
         XR    R3,R3                                                            
         ICM   R3,3,0(R2)                                                       
*                                                                               
         TM    MAP#PIND,MAPSPIND   Pinergy buy download?                        
         JNZ   EDTTIM80            Yes, some transformation needed              
EDTTIM10 CVD   R3,DUB              Most common to NOT be Pinergy                
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R4),DUB         Always 4 digits                              
         LHI   R3,4                                                             
*                                                                               
EDTTIM50 STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
EDTTIM80 CHI   R3,600              Are we less than 0600h?                      
         JNL   EDTTIM10                                                         
         AHI   R3,2400             Yes, adjut with 2400h                        
         J     EDTTIM10                                                         
                                                                                
***********************************************************************         
* Edit programming                                                    *         
***********************************************************************         
                                                                                
EDTPRG   LM    R2,R4,LP_AINP                                                    
         CLI   QMEDA,NETMEDQ       Test network television                      
         JNE   EDTPRG10                                                         
         LHI   R0,3                                                             
         LHI   R1,3-1                                                           
         LA    RF,3(R2)                                                         
         BASR  RE,0                                                             
         CLI   0(RF),C'-'                                                       
         JE    EDTPRG20                                                         
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCTR  R0,RE                                                            
                                                                                
EDTPRG10 MVC   0(L'BDPROGRM,R4),0(R2)                                           
         LHI   R1,L'BDPROGRM       Send full program name                       
         STCM  R1,15,LP_OLEN                                                    
*                                                                               
         TM    MAP#PIND,MAPSPIND   Pinergy buy download?                        
         JZ    EXITY               No, don't need to Translate                  
*                                                                               
****     GOTO1 VGETFACT,DMCB,0     if on-line we can call GETFACT               
****     L     RE,0(R1)            to get this address in FAXLATES              
****     USING FACTSD,RE           but we're offline so ++INCLUDE               
         LA    RE,CTRYXLAT         A(Translation table matrix)                  
         L     RE,12(RE)           A(Lower case input table UK/US)              
*                                                                               
         TR    0(L'BDPROGRM,R4),0(RE)                                           
         J     EXITY                                                            
                                                                                
EDTPRG20 LR    R0,RF               Save off where hyphen is                     
         LHI   RF,L'BDPROGRM-3                                                  
         SR    RF,R1                                                            
         LA    R1,2(R2,R1)                                                      
         BASR  RE,0                                                             
         MVC   0(0,R4),0(R1)       Set programming                              
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         STCM  RF,15,LP_OLEN                                                    
                                                                                
         CLI   L'BDPROGRM-1(R2),0  (-S?) estimated=actual?                      
         JNE   EXITY                                                            
         CLI   0(R2),C'='          Worried about short like =XXX-S              
         JNE   EXITY                                                            
         LA    R6,L'BDPROGRM-2(R2)                                              
         CLI   0(R6),C' '          Find first non-space from end                
         JH    *+8                                                              
         JCT   R6,*-8                                                           
         CLI   0(R6),C'S'          Program name ends in an 'S'?                 
         JNE   EXITY                                                            
         SR    R6,R0               Rest of program is "-S"?                     
         CHI   R6,1                Should be a difference of 1                  
         JH    EXITY                                                            
         J     EDTPRG10            If =XXX-S, then just a program               
                                                                                
***********************************************************************         
* Edit program adjacency code                                         *         
***********************************************************************         
                                                                                
EDTADJ   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0                                                          
         JE    XCOLEN                                                           
         MVC   0(L'BDPROGT,R4),0(R2)                                            
         LHI   R3,1                                                             
         L     RF,ACLTREC                                                       
         CLI   CPROF+9-CLTRECD(RF),C'1'                                         
         JE    EDTADJ02                                                         
         ICM   R1,8,0(R2)                                                       
         SLDL  R0,4                                                             
         SLL   R0,4                                                             
         SLDL  R0,4                                                             
         STCM  R0,3,0(R4)                                                       
         OC    0(2,R4),EZEROS                                                   
         LHI   R3,2                                                             
                                                                                
EDTADJ02 STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* HEXOUT the Line checksum, but want leading zeros.  DDLINK actually            
*    strips the leading zeros                                                   
***********************************************************************         
                                                                                
HXOLCS   LM    R2,R4,LP_AINP                                                    
         GOTOR VHEXOUT,DMCB,0(R2),0(R4),L'LINCKSUM                              
         LA    R1,L'LINCKSUM*2                                                  
         STCM  R1,15,LP_OLEN        Force output length of 4 characters         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* HEXOUT the Spot element checksum, but want leading zeros.  DDLINK             
*    actually strips the leading zeros                                          
***********************************************************************         
                                                                                
HXOSCS   LM    R2,R4,LP_AINP                                                    
         GOTOR VHEXOUT,DMCB,0(R2),0(R4),L'$SCKSUM                               
         LA    R1,L'$SCKSUM*2                                                   
         STCM  R1,15,LP_OLEN        Force output length of 4 characters         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit out the spot cost override?                                              
***********************************************************************         
                                                                                
EDTSCST  LM    R2,R4,LP_AINP                                                    
         OC    0(L'RPCOST,R2),0(R2)   Nonzero cost override?                    
         JZ    ESCST50                                                          
         XR    R6,R6                USING A REGISTER AS EDIT CONSIDERS          
         ICM   R6,7,0(R2)              FFFFF0 AS -16 AND OUTPUT THAT            
*        EDIT  (B3,0(R2)),(8,0(R4)),ALIGN=LEFT                                  
         EDIT  (R6),(8,0(R4)),ALIGN=LEFT                                        
         LA    R1,8                 FFFFFF=16777215                             
ESCST20  STCM  R1,15,LP_OLEN        Set output length                           
         J     EXITY                                                            
*                                                                               
ESCST50  DS    0H                                                               
         TM    MAP#PIND,MAPSPIND    Pinergy buy download?                       
         JZ    EXITY                                                            
********                                                                        
* SOME TRICKY CODE                                                              
********                                                                        
         LA    R1,0(R2)                                                         
         SHI   R1,$SDCOST-$SDSTAT4  See if cost override (X'40')                
         TM    0(R1),$SDSCOVR       $SDSTAT4 has diff bits than RSTATUS         
         JZ    EXITY                                                            
         MVI   0(R4),C'0'           Output a zero                               
         LA    R1,1                                                             
         J     ESCST20                                                          
                                                                                
***********************************************************************         
* Edit film code if film code not already sent                        *         
***********************************************************************         
                                                                                
EDTFLM   L     R1,LP_AINP                                                       
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)          RE=film number                               
         JZ    XCOLEN                                                           
         TM    LP_FLAG,LP_FRUNX    Test running other process on exit           
         JNZ   EDTFLM02                                                         
         BCTR  RE,0                                                             
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         A     RE,AFLMMSK                                                       
         LLC   RF,BITLIST(RF)                                                   
         BASR  R2,0                                                             
         TM    0(RE),0                                                          
         EX    RF,0(R2)            Test film sent mask bit on                   
         JNZ   XCOLEN                                                           
         BASR  R2,0                                                             
         OI    0(RE),0                                                          
         EX    RF,0(R2)            Set film sent mask bit on                    
         J     EDTFLM02                                                         
                                                                                
EDTFLM02 MVC   SVIOVALS,IOVALS                                                  
         ST    RC,AFLMREC                                                       
         LA    R2,IOKEY                                                         
         USING CMLRECD,R2          Read commercial passive                      
         XC    CMLKEY,CMLKEY                                                    
         MVI   CMLPID+0,CMLPTYPQ                                                
         MVI   CMLPID+1,CMLPSUBQ                                                
         MVC   CMLPAM,QMEDX                                                     
         MVC   CMLPCLT,QCLTX                                                    
         MVC   CMLPSEQ+1(L'CMLPSEQ-1),0(R1)                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOTRFDIR+B#FLMREC'                      
         JNE   EDTFLM06                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOTRFFIL+B#FLMREC'                     
         JNE   EDTFLM06                                                         
                                                                                
         L     R2,AFLMREC          R2=A(film record)                            
         MVC   WORK(L'CMLADID),SPACES                                           
         MVC   WORK(L'CMLKCML),CMLKCML                                          
                                                                                
         LA    R2,CMLDTAEL                                                      
         USING CMLADIEL,R2         Look for ad-id element                       
EDTFLM04 LLC   R0,CMLADILN                                                      
         AR    R2,R0                                                            
         CLI   CMLADIEL,EOR        Test end of record                           
         JE    EDTFLM08                                                         
         CLI   CMLADIEL,CMLADIEQ   Test ad-id element                           
         JNE   EDTFLM04                                                         
         MVC   WORK(L'CMLADID),CMLADID                                          
         J     EDTFLM08                                                         
         DROP  R2                                                               
                                                                                
EDTFLM06 MVI   WORK,C'?'           Send question marks if not found             
         MVC   WORK+1(L'CMLADID-1),WORK                                         
                                                                                
EDTFLM08 L     R1,LP_AOUT                                                       
         MVC   0(L'CMLADID,R1),WORK                                             
         LHI   R0,L'CMLADID                                                     
         STCM  R0,15,LP_OLEN                                                    
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
                                                                                
**********************************************************************          
* Edit person ID and extract person name from PID record             *          
**********************************************************************          
                                                                                
         USING EPWORKD,RC                                                       
         USING SA0REC,EPIO                                                      
EDTPID   MVC   EPIOSAVE,IOVALS     Save current i/o values                      
         XC    PERNAME,PERNAME     Clear person name                            
         LHI   R0,L'SAPALPID                                                    
         ST    R0,LP_OLEN                                                       
         L     R1,LP_AINP          R1=A(password number)                        
         OC    0(L'SA0KNUM,R1),0(R1)                                            
         JZ    EDTPIDN                                                          
                                                                                
         XC    SA0KEY,SA0KEY       Read person password record                  
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KNUM,0(R1)                                                    
         L     R1,LP_ASECD                                                      
         MVC   SA0KAGY,SECAGY-SECD(R1)                                          
         OC    SECOAGPE-SECD(,R1),SECOAGPE-SECD(R1)                             
         JZ    *+10                                                             
         MVC   SA0KAGY,SECOAGPE-SECD(R1)                                        
         MVC   IOKEY,SA0KEY                                                     
         LA    R1,EPIO                                                          
         ST    R1,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE'                                
         JNE   EDTPIDN                                                          
                                                                                
         LA    R1,SA0DATA                                                       
         USING SAPALD,R1                                                        
EDTPID02 CLI   SAPALEL,EOR         Test end of record                           
         JE    EDTPIDN                                                          
         CLI   SAPALEL,SAPALELQ    Test new security person element             
         JE    *+16                                                             
         LLC   R0,SAPALLN          Bump to next element                         
         AR    R1,R0                                                            
         J     EDTPID02                                                         
         L     RF,LP_AOUT                                                       
         MVC   0(L'SAPALPID,RF),SAPALPID                                        
                                                                                
         USING SAPEREC,EPIO                                                     
         XC    SAPEKEY,SAPEKEY     Read new security person record              
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         L     R1,LP_ASECD                                                      
         MVC   SAPEAGY,SECAGY-SECD(R1)                                          
         OC    SECOAGPE-SECD(,R1),SECOAGPE-SECD(R1)                             
         JZ    *+10                                                             
         MVC   SAPEAGY,SECOAGPE-SECD(R1)                                        
         MVC   SAPEPID,0(RF)                                                    
         MVC   IOKEY,SA0KEY                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE'                                
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   SAPEKEY(SAPEAGY-SAPEKEY),IOKEYSAV                                
         JNE   EDTPIDY                                                          
                                                                                
         LA    R1,SAPEDATA         Locate personnel details element             
         USING SANAMD,R1                                                        
EDTPID04 CLI   SANAMEL,EOR                                                      
         JE    EDTPIDY                                                          
         CLI   SANAMEL,SANAMELQ                                                 
         JE    *+16                                                             
         LLC   R0,SANAMLN                                                       
         AR    R1,R0                                                            
         J     EDTPID04                                                         
                                                                                
         MVC   EPNAMIND,SANAMIND                                                
         LA    R1,SANAMES                                                       
         LA    R2,PERNAME                                                       
         SR    RF,RF                                                            
         USING SANAMES,R1                                                       
                                                                                
         TM    EPNAMIND,SANAMIFN   Test first name present                      
         JZ    EDTPID06                                                         
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),SANAME                                                   
         EX    RF,0(RE)                                                         
         LA    R2,1(R2,RF)                                                      
         MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
         LA    R1,2(RF,R1)                                                      
                                                                                
EDTPID06 TM    EPNAMIND,SANAMIMN   Test middle name present                     
         JZ    EDTPID08                                                         
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),SANAME                                                   
         EX    RF,0(RE)                                                         
         LA    R2,1(R2,RF)                                                      
         MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
         LA    R1,2(RF,R1)                                                      
                                                                                
EDTPID08 TM    EPNAMIND,SANAMILN   Test last name present                       
         JZ    EDTPIDY                                                          
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),SANAME                                                   
         EX    RF,0(RE)                                                         
         DROP  R1                                                               
                                                                                
EDTPIDY  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     EXITY                                                            
                                                                                
EDTPIDN  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     XCOLEN                                                           
         DROP  RC                                                               
                                                                                
EPWORKD  DSECT ,                   ** EDTPID local working storage **           
EPIOSAVE DS    XL(IOVALL)          Saved i/o values                             
EPNAMIND DS    XL(L'SANAMIND)      Name indicators                              
EPIO     DS    XL1000              I/O area                                     
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Edit station code                                                   *         
***********************************************************************         
                                                                                
EDTSTA   L     R1,LP_AINP                                                       
         OC    0(L'STAPSTA,R1),0(R1)                                            
         JZ    XCOLEN                                                           
         XC    WORK2(L'STAPMKT),WORK2                                           
         MVC   WORK2+L'STAPMKT(L'STAPSTA),0(R1)                                 
         GOTOR GETSTA,WORK2                                                     
         L     R1,LP_AOUT                                                       
         MVC   0(L'STAPQSTA+L'STAPQNET,R1),STAPQSTA                             
         LHI   R0,L'STAPQSTA+L'STAPQNET                                         
         STCM  R0,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit binary seconds into ebcdic HHMMSS                              *         
***********************************************************************         
                                                                                
EDTHMS   L     R1,LP_AINP                                                       
         L     R2,LP_AOUT                                                       
         LHI   R0,6                                                             
         STCM  R0,15,LP_OLEN                                                    
         ICM   RE,15,0(R1)                                                      
         SRDL  RE,32               RE/RF=binary 1/100 second                    
         LHI   R0,100                                                           
         DR    RE,R0               RF=binary seconds                            
         SR    RE,RE                                                            
         LHI   R0,60                                                            
         DR    RE,R0                                                            
         CVD   RE,DUB              RE=seconds,RF=minutes                        
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  4(2,R2),DUB         Edit seconds                                 
         SR    RE,RE                                                            
         DR    RE,R0               RE=minutes,RF=hours                          
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  2(2,R2),DUB         Edit minutes                                 
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R2),DUB         Edit hours                                   
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit demo value - return value of '*' for null demo value           *         
***********************************************************************         
                                                                                
EDTDEM   L     R2,LP_AINP                                                       
         L     R3,LP_AOUT                                                       
         XC    LP_OLEN,LP_OLEN                                                  
         CLC   0(3,R2),EFFS        Test special null value                      
         JE    EXITY                                                            
         SR    R0,R0                                                            
         ICM   R0,7,0(R2)                                                       
         CURED (R0),(8,(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                          
         ST    R0,LP_OLEN                                                       
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Extract latest activity date/time from an activity element then     *         
* adjust date/time to real time                                       *         
***********************************************************************         
                                                                                
SETADT   NTR1  LABEL=NO                                                         
         L     R1,LP_AINP                                                       
         USING CDSACELD,R1                                                      
         MVC   WORK(L'CDSACCDT+L'CDSACCTM),CDSACCDT                             
         OC    CDSACCDT,CDSACCDT   Use change date/time if present              
         JNZ   *+10                                                             
         MVC   WORK(L'CDSACADT+L'CDSACATM),CDSACADT                             
         ICM   R0,15,WORK+L'CDSACCDT                                            
         A     R0,HSECADJS         Adjust seconds to DDS time                   
         STCM  R0,15,WORK+L'CDSACCDT                                            
         C     R0,HSECADAY         Test past midnight                           
         JNH   EXITY                                                            
         S     R0,HSECADAY         Yes - adjust time and date                   
         STCM  R0,15,WORK+L'CDSACCDT                                            
         GOTOR VDATCON,DMCB,(3,WORK),(0,DUB)                                    
         GOTOR VADDAY,DMCB,DUB,WORK2,1                                          
         GOTOR VDATCON,DMCB,(0,WORK2),(3,WORK)                                  
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Extract latest activity date/time from an activity element          *         
***********************************************************************         
                                                                                
SETRDT   L     R1,LP_AINP                                                       
         USING CDSACELD,R1                                                      
         MVC   WORK(L'CDSACCDT+L'CDSACCTM),CDSACCDT                             
         OC    CDSACCDT,CDSACCDT   Use change date/time if present              
         JNZ   *+10                                                             
         MVC   WORK(L'CDSACADT+L'CDSACATM),CDSACADT                             
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR (on-line) or BUFFERIN (off-line)                  *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows:-                   *         
*                                                                     *         
*        P1/0   - Buffer number                                       *         
*          /1-3 - TSAR action code                                    *         
*        P2/0   - Key length (initialization call)                    *         
*          /2-3 - Record length (initialization call)                 *         
***********************************************************************         
                                                                                
XPLBUFQ  EQU   1                                                                
ALLBUFQ  EQU   2                                                                
GPWBUFQ  EQU   3                                                                
ORABUFQ  EQU   4                                                                
                                                                                
BUFFER   NTR1  LABEL=NO                                                         
         LR    R2,R1               R2=A(parameter list)                         
         LA    RE,BUFBLK1                                                       
         LA    R1,ALLREC                                                        
         CLI   0(R2),ALLBUFQ                                                    
         JE    BUFFER02                                                         
         LA    RE,BUFBLK2                                                       
         LA    R1,XPLREC                                                        
         CLI   0(R2),XPLBUFQ                                                    
         JE    BUFFER02                                                         
         LA    RE,BUFBLK1                                                       
         LA    R1,GPWREC                                                        
         CLI   0(R2),GPWBUFQ                                                    
         JE    BUFFER02                                                         
         LA    RE,BUFBLK1                                                       
         LA    R1,ORAREC                                                        
         CLI   0(R2),ORABUFQ                                                    
         JE    BUFFER02                                                         
         DC    H'0'                                                             
                                                                                
BUFFER02 LTR   R3,RE                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING TSARD,R3            R3=A(TSAR control block)                     
                                                                                
         MVC   TSACTN,3(R2)        Set action code                              
         ST    R1,TSAREC           Set A(record)                                
                                                                                
         CLI   TSACTN,TSAINI       Test initialization call                     
         JNE   BUFFER04                                                         
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSKEYL,4(R2)                                                     
         MVC   TSRECL,6(R2)                                                     
         LHI   R0,4*ONEK           Get 4MB off-line                             
         STCM  R0,3,TSBUFFL                                                     
         MVI   TSRECI,TSRMINB1+TSRXTN                                           
         CLI   0(R2),XPLBUFQ       Test buffer 1                                
         JE    BUFFER04                                                         
         MVI   TSRECI,TSRMINB2+TSRXTN  GETS STORAGE FROM MINIO                  
*                                                                               
         CLI   0(R2),GPWBUFQ       NEED MORE THAN MINIO CAN PROVIDE?            
         JNE   BUFFER04            No                                           
         OI    TSIND2,TSI2MANY     GOING TO GET MORE THAN 64K RECORDS           
*                                                                               
         L     RE,LP_ATWA                                                       
         MVC   TSBUFFA,SVTSARB-TWAD(RE)                                         
         OC    TSBUFFA,TSBUFFA     IF WE ALREADY REQUESTED AN AREA              
         JNZ   BUFFER04            THEN WE'LL STILL USE IT                      
*                                                                               
         LHI   R0,40                IN OFFLINE TSAR                             
         STCM  R0,3,TSBUFFL         40MB                                        
         OI    TSRECI,TSRMGB                                                    
*                                                                               
BUFFER04 GOTOR VTSAR,TSARD                                                      
         MVC   BUFFRET,TSERRS                                                   
         TM    BUFFRET,TSEINIF     Test initialization failure                  
         JNZ   BFFRDCH0                                                         
         CLI   0(R2),GPWBUFQ                                                    
         JNE   BUFFER10                                                         
         L     RE,LP_ATWA                                                       
         MVC   SVTSARB-TWAD(L'SVTSARB,RE),TSBUFFA DON'T KEEP GETTING            
BUFFER10 CLI   BUFFRET,0           SET CONDITION CODE FOR CALLER                
         J     EXIT                                                             
BFFRDCH0 DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO CANADIAN MAPPING RECORD                           
*                                                                               
* ON ENTRY:    (R1)                A(RECORD RETREIVED)                          
***********************************************************************         
FLTCMP   OC    QACMP,QACMP         ANY MAPPING TEXT?                            
         JZ    EXITY               NONE, ALL SHALL PASS                         
*                                                                               
         USING LW_D,R2                                                          
         XR    R2,R2                                                            
         ICM   R2,7,QACMP                                                       
         XR    RE,RE                                                            
         ICM   RE,3,LW_NUMN        RE = Number of entries requested             
         LA    RF,LW_DATA2                                                      
         DROP  R2                                                               
*                                                                               
         LA    R6,DMPFRST-DMPRECD(R1)                                           
FCMP010  CLI   0(R6),0                                                          
         JE    FCMPXN                                                           
         CLI   0(R6),DMPDSELQ      X'01' - DESCRIPTION ELEMENT?                 
         JE    FCMP020                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     FCMP010                                                          
*                                                                               
         USING DMPDSELD,R6                                                      
FCMP020  CLC   DMPDTEXT,0(RF)                                                   
         JE    FCMPXY                                                           
         LA    RF,L'DMPDTEXT(RF)                                                
         JCT   RE,FCMP020                                                       
         DROP  R6                                                               
*                                                                               
FCMPXN   J     EXITN                                                            
FCMPXY   J     EXITY                                                            
         EJECT                                                                  
SETCCC   JE    *+8                 Set converse condition code                  
SETEQ    CR    RE,RE               Not equal to equal/not zero to zero          
         BR    RE                                                               
SETNEQ   LTR   RE,RE               Equal to not equal/zero to not zero          
         BR    RE                                                               
                                                                                
MORE     MVI   LP_RMODE,LP_RMORE   Set more to come                             
         J     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   Set no more records & exit                   
         J     EXITY                                                            
                                                                                
QERROR   MVI   LP_RMODE,LP_RERRR   Set request error & exit                     
         J     EXITY                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     Clear output field length & exit             
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,0                Set condition code to not equal              
         J     EXITCC                                                           
EXITY    LHI   RE,1                Set condition code to equal                  
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   General exit point                           
         EJECT                                                                  
GLOBALS  DS    0D                  ** Global literals **                        
                                                                                
LVALUES  DS    0F                  ** Literals moved to SAVED **                
         DC    A(FLTCMP)                                                        
         DC    AL1(YESQ)                                                        
         DC    C'POL'                                                           
         DC    AL1(1,3,0,020)      ASSOCIATED WITH LABEL V130020                
         DC    AL1(1,4,0,0)        ASSOCIATED WITH LABEL V140000                
                                                                                
DEMOMOD  DC    C'E',C'R',C'I',AL1(0)                                            
                                                                                
DEMOFILE DC    C'TP '              Demo file name                               
*                                                                               
M#CDMKMP DC    AL2(I#CDMKMP)       MARKET MAPPING                               
M#CDSTMP DC    AL2(I#CDSTMP)       STATION MAPPING                              
M#CDDMMP DC    AL2(I#CDDMMP)       DEMO CAT MAPPING                             
*                                                                               
                                                                                
MAPTAB   DS    0XL4                ** Map/Indicators array **                   
                                                                                
CDBUYD#  DC    AL2(I#CDBUYD)       Desktop buy download                         
         DC    AL1(MAPSPRDS+MAPSDMNU+MAPSDEST+MAPSMBDL)                         
         DC    AL1(0)                                                           
                                                                                
CDPIND#  DC    AL2(I#CDPIND)       Pinergy buy download                         
         DC    AL1(MAPSDEST)                                                    
         DC    AL1(MAPSPIND)                                                    
                                                                                
CDSBDL#  DC    AL2(I#CDSBDL)       Desktop single buy download                  
         DC    AL1(MAPSSBDL)                                                    
         DC    AL1(0)                                                           
                                                                                
CDSNDL#  DC    AL2(I#CDSNDL)       Desktop single network buy download          
         DC    AL1(MAPSSNDL)                                                    
         DC    AL1(0)                                                           
                                                                                
CDMGAN#  DC    AL2(I#CDMGAN)       Desktop makegood analysis                    
         DC    AL1(MAPSMGAN)                                                    
         DC    AL1(0)                                                           
                                                                                
CDSDDM#  DC    AL2(I#CDSDDM)       ShowDef/DemoDef/DemOver maintenance          
         DC    AL1(MAPSSDDM)                                                    
         DC    AL1(0)                                                           
                                                                                
COORDD#  DC    AL2(I#COORDD)       Order details                                
         DC    AL1(0)                                                           
         DC    AL1(MAPSORDD)                                                    
                                                                                
COOFFB#  DC    AL2(I#COOSUM)       Order summary                                
         DC    AL1(0)                                                           
         DC    AL1(MAPSORDS)                                                    
                                                                                
CDMKMP#  DC    AL2(I#CDMKMP)       Market Mapping                               
         DC    AL1(0)                                                           
         DC    AL1(MAPSORDS)                                                    
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
BITLIST  DC    X'8040201008040201'                                              
EZEROS   DC    C'0000000000'                                                    
PZERO    DC    P'0'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
SPTDIR   DC    C'SPTDIR '                                                       
SPTFIL   DC    C'SPTFILE'                                                       
STAFIL   DC    C'STATION'                                                       
XSPDIR   DC    C'XSPDIR '                                                       
XSPFIL   DC    C'XSPFILE'                                                       
TRFDIR   DC    C'TRFDIR '                                                       
         EJECT                                                                  
SCANCHAR DC    C',=/',AL1(FF)      DELIMITERS FOR ORDER ACTIVITY FILTRS         
CLTKEYT  LKKEY H,CKEY,SAVED        ** Client driver **                          
         LKKEY LIT,CKEYTYPE,CKEYTYPQ                                            
         LKKEY WMP,CKEYAM,QAMED                                                 
         LKKEY NZR,CKEYCLT                                                      
         LKKEY LIT,CKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
ESTKEYT  LKKEY H,EPKEY             ** Estimate passive driver **                
         LKKEY LIT,EPKEYTYP,EPKEYTYQ                                            
         LKKEY LIT,EPKEYSUB,EPKEYSBQ                                            
         LKKEY WMP,EPKEYAM,QAMED                                                
         LKKEY WMP,EPKEYCLT,QACLT                                               
         LKKEY RNG,EPKEYSDT,QESTSTDR                                            
         LKKEY RNG,EPKEYEDT,QESTENDR                                            
         LKKEY WMP,EPKEYEST,QAEST                                               
         LKKEY SIN,EPKEYPRD,POLPRD                                              
         LKKEY E                                                                
                                                                                
PRDKEYT  LKKEY H,PKEY              ** Product driver **                         
         LKKEY LIT,PKEYTYPE,PKEYTYPQ                                            
         LKKEY WMP,PKEYAM,QAMED                                                 
         LKKEY WMP,PKEYCLT,QACLT                                                
         LKKEY WMP,PKEYPRD,DAPRC                                                
         LKKEY LIT,PKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
DPTKEYT  LKKEY H,DPTKEY            ** Daypart menu driver **                    
         LKKEY LIT,DPTKTYPE,DPTKTYPQ                                            
         LKKEY SIN,DPTKAGY,AGENCY                                               
         LKKEY SIN,DPTKMED,QMEDIA                                               
         LKKEY WMP,DPTKMENU,DADPT                                               
         LKKEY LIT,DPTKREST,0                                                   
         LKKEY E                                                                
                                                                                
MKTKEYT  LKKEY H,MKTKEY            ** Market driver **                          
         LKKEY LIT,MKTKTYPE,MKTKTYPQ                                            
         LKKEY SIN,MKTKMED,QMEDIA                                               
         LKKEY NZR,MKTKMKT                                                      
         LKKEY SIN,MKTKAGY,AGENCY                                               
         LKKEY LIT,MKTKFILL,C'0'                                                
         LKKEY E                                                                
                                                                                
REPKEYT  LKKEY H,REPKEY            ** Rep driver **                             
         LKKEY LIT,REPKTYPE,C'R'                                                
         LKKEY SIN,REPKMED,QMEDIA                                               
         LKKEY ALL,REPKREP                                                      
         LKKEY SIN,REPKAGY,AGENCY                                               
         LKKEY LIT,REPKFILL,C'0'                                                
         LKKEY E                                                                
                                                                                
STAKEYT  LKKEY H,STAKEY            ** Station driver **                         
         LKKEY LIT,STAKTYPE,STAKTYPQ                                            
         LKKEY SIN,STAKMED,QMEDIA                                               
         LKKEY ALL,STAKCALL                                                     
         LKKEY SIN,STAKAGY,AGENCY                                               
         LKKEY ALL,STAKCLT                                                      
         LKKEY LIT,STAKFILL,C'0'                                                
         LKKEY E                                                                
                                                                                
NBYKEYT  LKKEY H,BUYKEY            ** Network buy driver **                     
         LKKEY WMP,BUYKAM,QAMED                                                 
         LKKEY WMP,BUYKCLT,QACLT                                                
         LKKEY LIT,BUYKPRD,FF                                                   
         LKKEY LIT,BUYKMKTN,0                                                   
         LKKEY WMP,BUYKSTAC,QASTA                                               
         LKKEY WMP,BUYKEST,DAEST                                                
         LKKEY ALL,BUYKBUY                                                      
*        LKKEY ALL,BUYKBUY,1                                                    
*        LKKEY ALL,BUYKLIN                                                      
         LKKEY E                                                                
                                                                                
SBYKEYT  LKKEY H,BUYKEY            ** Selective buy driver **                   
         LKKEY WMP,BUYKAM,QAMED                                                 
         LKKEY WMP,BUYKCLT,QACLT                                                
         LKKEY LIT,BUYKPRD,FF                                                   
         LKKEY WMP,BUYKMKTN,QAMKT                                               
         LKKEY WMP,BUYKSTAC,QASTA                                               
         LKKEY WMP,BUYKEST,DAEST                                                
         LKKEY ALL,BUYKBUY                                                      
*        LKKEY ALL,BUYKBUY,1                                                    
*        LKKEY ALL,BUYKLIN                                                      
         LKKEY E                                                                
                                                                                
SHWKEYT  LKKEY H,NPGMKEY           ** ShowCode driver **                        
         LKKEY LIT,NPGMKTYP+0,NPGMTYPQ,1                                        
         LKKEY LIT,NPGMKTYP+1,NPGMSUBQ,1                                        
         LKKEY SIN,NPGMKAGY,AGENCY                                              
         LKKEY WMP,NPGMKNET,QASTA                                               
         LKKEY WMP,NPGMKID,QASHW                                                
         LKKEY E                                                                
                                                                                
GOLKEYT  LKKEY H,GKEY              ** Goal driver **                            
         LKKEY LIT,GKEYTYPE,GKEYTYPQ                                            
         LKKEY SIN,GKEYAM,QMEDIA                                                
         LKKEY WMP,GKEYCLT,QACLT                                                
         LKKEY WMP,GKEYPRD,DAPR#                                                
         LKKEY WMP,GKEYMKT,QAMKT                                                
         LKKEY WMP,GKEYEST,DAEST                                                
         LKKEY WMP,GKEYDPT,QADPT                                                
         LKKEY WMP,GKEYSLN,QASLN                                                
         LKKEY ALL,GKEYSEC                                                      
         LKKEY ALL,GKEYAGY                                                      
         LKKEY ALL,GKEYPRD2                                                     
         LKKEY ALL,GKCNTRLS                                                     
         LKKEY E                                                                
                                                                                
CAMKEYT  LKKEY H,CMPKEY            ** Campaign driver **                        
         LKKEY LIT,CMPKTYPE,CMPKTYPQ                                            
         LKKEY LIT,CMPKSTYP,CMPKSTYQ                                            
         LKKEY LIT,CMPKSPR1,0                                                   
         LKKEY SIN,CMPKAGMD,QMEDIA                                              
         LKKEY WMP,CMPKCLT,QACLT                                                
         LKKEY WMP,CMPKCAM,QACAM                                                
         LKKEY LIT,CMPKSPR2,0                                                   
         LKKEY E                                                                
                                                                                
CSEKEYT  LKKEY H,CSEKEY            ** Sta-est driver **                         
         LKKEY LIT,CSEKTYPE,CSEKTYPQ                                            
         LKKEY LIT,CSEKSTYP,CSEKSTYQ                                            
         LKKEY LIT,CSEKSPR1,0                                                   
         LKKEY SIN,CSEKAGMD,QMEDIA                                              
         LKKEY WMP,CSEKCLT,QACLT                                                
         LKKEY SIN,CSEKCAM,QCAMX                                                
         LKKEY ALL,CSEKSTTY                                                     
         LKKEY ALL,CSEKSTA                                                      
         LKKEY E                                                                
                                                                                
CSLKEYT  LKKEY H,CSLKEY            ** Station list driver **                    
         LKKEY LIT,CSLKTYPE,CSLKTYPQ                                            
         LKKEY LIT,CSLKSTYP,CSLKSTYQ                                            
         LKKEY LIT,CSLKSPR1,0                                                   
         LKKEY SIN,CSLKAGMD,QMEDIA                                              
         LKKEY WMP,CSLKCLT,QACLT                                                
         LKKEY SIN,CSLKCAM,QCAMX                                                
         LKKEY ALL,CSLKLIST,,L'CSLKLIST+L'CSLKSEQN                              
         LKKEY E                                                                
                                                                                
CORKEYT  LKKEY H,CORKEY            ** Order driver **                           
         LKKEY LIT,CORKTYPE,CORKTYPQ                                            
         LKKEY LIT,CORKSTYP,CORKSTYQ                                            
         LKKEY LIT,CORKSPR1,0                                                   
         LKKEY WMP,CORKAGMD,QAMED                                               
         LKKEY WMP,CORKCLT,QACLT                                                
         LKKEY WMP,CORKCAM,QACAM                                                
         LKKEY ALL,CORKORD                                                      
         LKKEY ALL,CORKREV                                                      
         LKKEY E                                                                
                                                                                
*                                  ** ORDER (SUMMARY) DRIVER **                 
OSOKEYT  LKKEY H,CORKEY,,CORKREV-CORKEY                                         
         LKKEY LIT,CORKTYPE,CORKTYPQ                                            
         LKKEY LIT,CORKSTYP,CORKSTYQ                                            
         LKKEY LIT,CORKSPR1,0                                                   
         LKKEY WMP,CORKAGMD,QAMED                                               
         LKKEY WMP,CORKCLT,QACLT                                                
         LKKEY WMP,CORKCAM,QACAM                                                
         LKKEY ALL,CORKORD                                                      
         LKKEY E                                                                
                                                                                
*                                  ** ORDER ACTIVITY DRIVER **                  
OAOKEYT  LKKEY H,CORKEY,,CODKCAM-CORKEY                                         
         LKKEY LIT,CODKTYPE,CODKTYPQ                                            
         LKKEY LIT,CODKSTYP,CODKSTYQ                                            
         LKKEY LIT,CODKSPR1,0                                                   
         LKKEY WMP,CODKAGMD,QAMED                                               
         LKKEY SIN,CODKDATE,TODAYCMP                                            
         LKKEY WMP,CODKTIME,QATIM                                               
         LKKEY ALL,CODKCLT                                                      
         LKKEY ALL,CODKSTA                                                      
         LKKEY ALL,CODKCAM                                                      
         LKKEY ALL,CODKORD                                                      
         LKKEY E                                                                
                                                                                
DPRKEYT  LKKEY H,DPRKEY            ** Program driver **                         
         LKKEY LIT,DPRKTYPE,DPRKTYPQ                                            
         LKKEY LIT,DPRKSTYP,DPRKSTYQ                                            
         LKKEY LIT,DPRKSPR1,0                                                   
         LKKEY WMP,DPRKAGMD,QAMED                                               
         LKKEY WMP,DPRKCLT,QACLT                                                
         LKKEY SIN,DPRKCAM,QCAMX                                                
         LKKEY WMP,DPRKMKT,QAMKT                                                
         LKKEY WMP,DPRKSTA,QASTA                                                
         LKKEY ALL,DPRKLIN                                                      
         LKKEY E                                                                
                                                                                
OSPKEYT  LKKEY H,DPRKEY            ** Program (summary) driver **               
         LKKEY LIT,DPRKTYPE,DPRKTYPQ                                            
         LKKEY LIT,DPRKSTYP,DPRKSTYQ                                            
         LKKEY LIT,DPRKSPR1,0                                                   
         LKKEY WMP,DPRKAGMD,QAMED                                               
         LKKEY WMP,DPRKCLT,QACLT                                                
         LKKEY SIN,DPRKCAM,SVORDKEY+(CORKCAM-CORRECD)                           
         LKKEY SIN,DPRKMKT,DMKTSTA,L'DMKTSTA                                    
         LKKEY ALL,DPRKLIN                                                      
         LKKEY E                                                                
                                                                                
UOPKEYT  LKKEY H,DPRKEY            ** Unordered prgrams driver **               
         LKKEY LIT,DPRKTYPE,DPRKTYPQ                                            
         LKKEY LIT,DPRKSTYP,DPRKSTYQ                                            
         LKKEY LIT,DPRKSPR1,0                                                   
         LKKEY WMP,DPRKAGMD,QAMED                                               
         LKKEY WMP,DPRKCLT,QACLT                                                
         LKKEY WMP,DPRKCAM,QACAM                                                
         LKKEY WMP,DPRKMKT,QAMKT                                                
         LKKEY ALL,DPRKSTA                                                      
         LKKEY ALL,DPRKLIN                                                      
         LKKEY E                                                                
                                                                                
DDMKEYT  LKKEY H,DDMKEY            ** Demo driver **                            
         LKKEY LIT,DDMKTYPE,DDMKTYPQ                                            
         LKKEY LIT,DDMKSTYP,DDMKSTYQ                                            
         LKKEY LIT,DDMKSPR1,0                                                   
         LKKEY SIN,DDMKAGMD,SVPRGKEY+(DPRKAGMD-DPRKEY)                          
         LKKEY SIN,DDMKCLT,SVPRGKEY+(DPRKCLT-DPRKEY)                            
         LKKEY SIN,DDMKCAM,SVPRGKEY+(DPRKCAM-DPRKEY)                            
         LKKEY SIN,DDMKMKT,SVPRGKEY+(DPRKMKT-DPRKEY)                            
         LKKEY SIN,DDMKSTA,SVPRGKEY+(DPRKSTA-DPRKEY)                            
         LKKEY SIN,DDMKLIN,SVPRGKEY+(DPRKLIN-DPRKEY)                            
         LKKEY ALL,DDMKLCL                                                      
         LKKEY LIT,DDMKSPL,0                                                    
         LKKEY E                                                                
                                                                                
OSDKEYT  LKKEY H,DDMKEY            ** Demo (summary) driver **                  
         LKKEY LIT,DDMKTYPE,DDMKTYPQ                                            
         LKKEY LIT,DDMKSTYP,DDMKSTYQ                                            
         LKKEY LIT,DDMKSPR1,0                                                   
         LKKEY SIN,DDMKAGMD,SVPRGKEY+(DPRKAGMD-DPRKEY)                          
         LKKEY SIN,DDMKCLT,SVPRGKEY+(DPRKCLT-DPRKEY)                            
         LKKEY SIN,DDMKCAM,SVPRGKEY+(DPRKCAM-DPRKEY)                            
         LKKEY SIN,DDMKMKT,SVPRGKEY+(DPRKMKT-DPRKEY)                            
         LKKEY SIN,DDMKSTA,SVPRGKEY+(DPRKSTA-DPRKEY)                            
         LKKEY SIN,DDMKLIN,SVPRGKEY+(DPRKLIN-DPRKEY)                            
         LKKEY ALL,DDMKLCL                                                      
         LKKEY LIT,DDMKSPL,0                                                    
         LKKEY E                                                                
                                                                                
OSSKEYT  LKKEY H,DDMKEY            ** Demo (summary spill) driver **            
         LKKEY LIT,DDMKTYPE,DDMKTYPQ                                            
         LKKEY LIT,DDMKSTYP,DDMKSTYQ                                            
         LKKEY LIT,DDMKSPR1,0                                                   
         LKKEY WMP,DDMKAGMD,QAMED                                               
         LKKEY WMP,DDMKCLT,QACLT                                                
         LKKEY WMP,DDMKCAM,QACAM                                                
         LKKEY WMP,DDMKMKT,QAMKT                                                
         LKKEY ALL,DDMKSTA                                                      
         LKKEY ALL,DDMKLIN                                                      
         LKKEY ALL,DDMKLCL                                                      
         LKKEY LIT,DDMKSPL,DDMKSPLQ                                             
         LKKEY E                                                                
                                                                                
DCTKEYT  LKKEY H,DCTKEY            ** Contact driver **                         
         LKKEY LIT,DCTKTYPE,DCTKTYPQ                                            
         LKKEY LIT,DCTKSTYP,DCTKSTYQ                                            
         LKKEY LIT,DCTKSPR1,0                                                   
         LKKEY SIN,DCTKAGMD,AGENCYB                                             
         LKKEY ALL,DCTKCONT                                                     
         LKKEY E                                                                
                                                                                
MMPKEYT  LKKEY H,DMPKEY            ** Market Mapping driver **                  
         LKKEY LIT,DMPKTYPE,DMPKTYPQ   PASSIVE                                  
         LKKEY LIT,DMPPKSTY,DMPPKSMQ                                            
         LKKEY WMP,DMPPKAM,QAMED                                                
         LKKEY RNG,DMPPKADT,CRDTRNGE                                            
         LKKEY WMP,DMPPKTXT,QACMP                                               
         LKKEY ALL,DMPPKSEQ                                                     
         LKKEY LIT,DMPPKSPR,0                                                   
         LKKEY E                                                                
                                                                                
MMPKY2T  LKKEY H,DMPKEY            ** MARKET MAPPING DRIVER **                  
         LKKEY LIT,DMPKTYPE,DMPKTYPQ   ACTIVE                                   
         LKKEY LIT,DMPKSTYP,DMPKSTMQ                                            
         LKKEY WMP,DMPKAGMD,QAMED                                               
         LKKEY WMP,DMPKTEXT,QACMP                                               
         LKKEY ALL,DMPKSEQ                                                      
         LKKEY LIT,DMPKSPAR,0                                                   
         LKKEY E                                                                
                                                                                
SMPKEYT  LKKEY H,DMPKEY            ** Station Mapping driver **                 
         LKKEY LIT,DMPKTYPE,DMPKTYPQ   PASSIVE                                  
         LKKEY LIT,DMPPKSTY,DMPPKSSQ                                            
         LKKEY WMP,DMPPKAM,QAMED                                                
         LKKEY RNG,DMPPKADT,CRDTRNGE                                            
         LKKEY WMP,DMPPKTXT,QACMP                                               
         LKKEY ALL,DMPPKSEQ                                                     
         LKKEY LIT,DMPPKSPR,0                                                   
         LKKEY E                                                                
                                                                                
SMPKY2T  LKKEY H,DMPKEY            ** STATION MAPPING DRIVER **                 
         LKKEY LIT,DMPKTYPE,DMPKTYPQ   ACTIVE                                   
         LKKEY LIT,DMPKSTYP,DMPKSTSQ                                            
         LKKEY WMP,DMPKAGMD,QAMED                                               
         LKKEY WMP,DMPKTEXT,QACMP                                               
         LKKEY ALL,DMPPKSEQ                                                     
         LKKEY LIT,DMPKSPAR,0                                                   
         LKKEY E                                                                
                                                                                
DMPKEYT  LKKEY H,DMPKEY            ** DemoCat Mapping driver **                 
         LKKEY LIT,DMPKTYPE,DMPKTYPQ   PASSIVE                                  
         LKKEY LIT,DMPPKSTY,DMPPKSDQ                                            
         LKKEY SIN,DMPPKAM,QAGYHX                                               
         LKKEY RNG,DMPPKADT,CRDTRNGE                                            
         LKKEY WMP,DMPPKTXT,QACMP                                               
         LKKEY ALL,DMPPKSEQ                                                     
         LKKEY LIT,DMPPKSPR,0                                                   
         LKKEY E                                                                
                                                                                
DMPKY2T  LKKEY H,DMPKEY            ** DEMOCAT MAPPING DRIVER **                 
         LKKEY LIT,DMPKTYPE,DMPKTYPQ   ACTIVE                                   
         LKKEY LIT,DMPKSTYP,DMPKSTDQ                                            
         LKKEY SIN,DMPKAGMD,QAGYHX                                              
         LKKEY WMP,DMPKTEXT,QACMP                                               
         LKKEY ALL,DMPPKSEQ                                                     
         LKKEY LIT,DMPKSPAR,0                                                   
         LKKEY E                                                                
                                                                                
DPTFLT   LKKEY H,BDDAYPT           ** Daypart filter **                         
         LKKEY WMP,BDDAYPT,QADPT                                                
         LKKEY E                                                                
                                                                                
SECFLT   LKKEY H,BDSEC             ** Seconds filter **                         
         LKKEY WMP,BDSEC,QASLN                                                  
         LKKEY E                                                                
                                                                                
LSTFLT   LKKEY H,DPRPXST           ** State filter **                           
         LKKEY WMP,DPRPXST,QALST                                                
         LKKEY E                                                                
         EJECT                                                                  
         LTORG ,                                                                
                                                                                
HSECADJS DC    A(100*60*60*6)      DDS time adjustment (1/100 second)           
HSECADAY DC    A(100*60*60*24)     Number of 1/100 seconds in a day             
                                                                                
PROFS00  DC    C'S000',AL1(23)     SPOT/00 profile key/map number               
PROFS1W  DC    C'S01W',AL1(24)     SPOT/1W profile key/map number               
PROFSB0  DC    C'S0B0',AL1(25)     SPOT/B0 profile key/map number               
PROFSST  DC    C'S0ST',AL1(26)     SPOT/ST profile key/map number               
PROFSAJ  DC    C'S0AJ',AL1(27)     SPOT/AJ profile key/map number               
PROFS00A DC    C's00A',AL1(28)     SPOT/00A profile key/map number              
PROFSD0  DC    C'S0D0',AL1(29)     SPOT/D0 profile key/map number               
                                                                                
MONSUN   DC    X'17'               DAYVAL Monday-Sunday                         
                                                                                
ALLOWCHR DC    256XL1'00'          Allowable program characters                 
         ORG   ALLOWCHR+C'a'                                                    
         DC    C'abcdefghi'                                                     
         ORG   ALLOWCHR+C'j'                                                    
         DC    C'jklmnopqr'                                                     
         ORG   ALLOWCHR+C's'                                                    
         DC    C'stuvwxyz'                                                      
         ORG   ALLOWCHR+C'A'                                                    
         DC    C'ABCDEFGHI'                                                     
         ORG   ALLOWCHR+C'J'                                                    
         DC    C'JKLMNOPQR'                                                     
         ORG   ALLOWCHR+C'S'                                                    
         DC    C'STUVWXYZ'                                                      
         ORG   ALLOWCHR+C'0'                                                    
         DC    C'0123456789'                                                    
         ORG                                                                    
                                                                                
OPTOLIT  DC    C'Turn optimizer off?'                                           
ROTDLIT  DC    C'Rotation day(s)'                                               
STRTLIT  DC    C'Start time'                                                    
ENDTLIT  DC    C'End time'                                                      
KILLLIT  DC    C'Kill date'                                                     
BBOKLIT  DC    C'Base book'                                                     
UBOKLIT  DC    C'Use till book'                                                 
STRILIT  DC    C'Search string(s)'                                              
TOTOLIT  DC    C'Totals only'                                                   
TYPFLIT  DC    C'Record type filter'                                            
SLNFLIT  DC    C'Seconds length filter(s)'                                      
DPTFLIT  DC    C'Daypart filter(s)'                                             
DEMOLIT  DC    C'Demo code(s)'                                                  
SCLILIT  DC    C'Send client data'                                              
SCAMLIT  DC    C'Send campaign data'                                            
LSTILIT  DC    C'State(s)'                                                      
IUNOLIT  DC    C'Unordered programs'                                            
RTGSLIT  DC    C'Rating service (0, 1)'                                         
DOVDLIT  DC    C'D/A of DemoDef record'                                         
LIVELIT  DC    C'Live show?'                                                    
PGMDLIT  DC    C'Program name/description'                                      
OOWDLIT  DC    C'Out-of-week rotation days'                                     
DEMFLIT  DC    C'Demo value(s) input flag'                                      
NADDLIT  DC    C'NAD code'                                                      
NADVLIT  DC    C'NAD value'                                                     
STMKLIT  DC    C'Station/Market'                                                
DEMVLIT  DC    C'Demo value'                                                    
CAMNLIT  DC    C'Campaign number'                                               
ORDNLIT  DC    C'Order number'                                                  
DETLLIT  DC    C'Detail?'                                                       
STATLIT  DC    C'Status filter(s)'                                              
BUYRLIT  DC    C'Buyer(s)'                                                      
SELRLIT  DC    C'Seller(s)'                                                     
                                                                                
FILES    DS    0C                  ** System/File list **                       
                                                                                
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
                                                                                
FACS     DS    0XL(RFACTABL)       ** System facilties **                       
         DC    AL1(QDEMTABS),AL2(CDEMTABS-COMFACSD,0)                           
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,0)                             
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,0)                           
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,0)                             
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD,0)                               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,0)                           
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD,0)                           
         DC    AL1(QDDISP),AL2(CT00AD0-COMFACSD,0)                              
         DC    AL1(RFACEOTQ)                                                    
         EJECT                                                                  
       ++INCLUDE FACTRYXLAT                                                     
         EJECT                                                                  
B#CLTREC EQU   3                   IO2 - Client record                          
B#PRDREC EQU   4                   IO3 - Product RECORD                         
APRDREC  EQU   LP_BLKS+((B#PRDREC-1)*L'LP_BLKS),,C'A'                           
B#SHWREC EQU   4                       - ShowCode record                        
ASHWREC  EQU   LP_BLKS+((B#PRDREC-1)*L'LP_BLKS),,C'A'                           
B#EQUREC EQU   4                       - Equivalence header                     
AEQUREC  EQU   LP_BLKS+((B#EQUREC-1)*L'LP_BLKS),,C'A'                           
B#STAREC EQU   4                       - Station record                         
ASTAREC  EQU   LP_BLKS+((B#STAREC-1)*L'LP_BLKS),,C'A'                           
B#REPREC EQU   4                       - Rep record                             
AREPREC  EQU   LP_BLKS+((B#REPREC-1)*L'LP_BLKS),,C'A'                           
B#SDRREC EQU   4                       - SDR record                             
ASDRREC  EQU   LP_BLKS+((B#SDRREC-1)*L'LP_BLKS),,C'A'                           
B#CTIREC EQU   4                       - ID Record                              
ACTIREC  EQU   LP_BLKS+((B#CTIREC-1)*L'LP_BLKS),,C'A'                           
*                                                                               
B#DPTREC EQU   5                   IO4 - Daypart menu record                    
ADPTREC  EQU   LP_BLKS+((B#DPTREC-1)*L'LP_BLKS),,C'A'                           
B#SPDREC EQU   5                       - Spill definition                       
B#NDFREC EQU   5                       - Network/cable definition               
ANDFREC  EQU   LP_BLKS+((B#NDFREC-1)*L'LP_BLKS),,C'A'                           
B#ESTREC EQU   5                       - Estimate record                        
AESTREC  EQU   LP_BLKS+((B#ESTREC-1)*L'LP_BLKS),,C'A'                           
B#GOLREC EQU   5                       - Goal record                            
AGOLREC  EQU   LP_BLKS+((B#GOLREC-1)*L'LP_BLKS),,C'A'                           
B#CAMREC EQU   5                       - Campaign record                        
ACAMREC  EQU   LP_BLKS+((B#CAMREC-1)*L'LP_BLKS),,C'A'                           
B#DCTREC EQU   5                       - Contact record                         
ADCTREC  EQU   LP_BLKS+((B#DCTREC-1)*L'LP_BLKS),,C'A'                           
B#BUYREC EQU   6                   IO5 - Buy record                             
ABUYREC  EQU   LP_BLKS+((B#BUYREC-1)*L'LP_BLKS),,C'A'                           
B#CORREC EQU   6                       - Order record                           
ACORREC  EQU   LP_BLKS+((B#CORREC-1)*L'LP_BLKS),,C'A'                           
B#MKTREC EQU   7                   IO6 - Market record                          
AMKTREC  EQU   LP_BLKS+((B#MKTREC-1)*L'LP_BLKS),,C'A'                           
B#DDMREC EQU   7                       - Demo record                            
ADDMREC  EQU   LP_BLKS+((B#DDMREC-1)*L'LP_BLKS),,C'A'                           
B#STAEST EQU   7                       - Station/estimate                       
ASTAEST  EQU   LP_BLKS+((B#STAEST-1)*L'LP_BLKS),,C'A'                           
B#ADDREC EQU   8                   IO7 - Address record                         
AADDREC  EQU   LP_BLKS+((B#ADDREC-1)*L'LP_BLKS),,C'A'                           
B#DOVREC EQU   8                       - DemoDef/DemOver record                 
ADOVREC  EQU   LP_BLKS+((B#DOVREC-1)*L'LP_BLKS),,C'A'                           
B#DPRREC EQU   8                       - Program record                         
ADPRREC  EQU   LP_BLKS+((B#DPRREC-1)*L'LP_BLKS),,C'A'                           
B#CSEREC EQU   8                       - Station/estimate record                
ACSEREC  EQU   LP_BLKS+((B#CSEREC-1)*L'LP_BLKS),,C'A'                           
B#CSLREC EQU   8                       - Station list record                    
ACSLREC  EQU   LP_BLKS+((B#CSEREC-1)*L'LP_BLKS),,C'A'                           
*                                                                               
B#DCMREC EQU   9                   IO8 - Canadian Mapping Records               
ADCMREC  EQU   LP_BLKS+((B#DCMREC-1)*L'LP_BLKS),,C'A'                           
*                                                                               
B#FLMREC EQU   10                  Film record (uses passed storage)            
AFLMREC  EQU   LP_BLKS+((B#FLMREC-1)*L'LP_BLKS),,C'A'                           
B#AGYREC EQU   11                  IO1 - Agency record                          
B#SVRDEF EQU   12                  SPLNK22                                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS),,C'A'                           
B#LP_D   EQU   14                  LP_D                                         
                                                                                
CANAGYQ  EQU   C'C'                Canadian agency identifier                   
NETMEDQ  EQU   C'N'                Canadian network media                       
COMMEDQ  EQU   C'C'                Combined media                               
RADMEDQ  EQU   C'R'                Radio media                                  
TELMEDQ  EQU   C'T'                Television media                             
NTRMEDQ  EQU   C'X'                Network radio media                          
NETNUMQ  EQU   3                   Network media number                         
                                                                                
SEC30Q   EQU   30                  30 seconds                                   
EOR      EQU   0                   End of record element                        
DOVTYPQ  EQU   X'0D'               DemOver record type                          
DOVSUBQ  EQU   X'17'               DemOver record sub-type                      
DOVEL01Q EQU   X'01'               DemOver header element                       
DOVEL02Q EQU   X'02'               National audience demo element               
DOVEL05Q EQU   X'05'               Station demo element                         
NPGMTYPQ EQU   X'0D'               Network program record type                  
NPGMSUBQ EQU   X'12'               Network program record sub-type              
CMLPTYPQ EQU   X'0A'               Commercial record passive type               
CMLPSUBQ EQU   X'A1'               Commercial record passive sub-type           
CMLADIEQ EQU   X'A0'               Ad-id element                                
ORBELEMQ EQU   X'67'               Orbit element                                
ACTVEL2Q EQU   X'F1'               Record activity element                      
DLTGELQ  EQU   X'18'               Dealer cml/tag assign element                
PDELEMQ  EQU   X'22'               Post buy demo original element               
SDELEMQ  EQU   X'23'               Post buy demo spill element                  
DLOVELQ  EQU   X'24'               Demo look-up override element                
UPELEMQ  EQU   X'62'               Upgrade element                              
IDELEMQ  EQU   X'70'               Id element                                   
CMELEMQ  EQU   X'66'               Comment element                              
MGRCELQ  EQU   X'65'               Makegood reason code element                 
COS2ELQ  EQU   X'71'               Cost2 element                                
C2FCELQ  EQU   X'73'               Cost2 factor element                         
ACTVELQ  EQU   X'99'               Buy activity element                         
DPTKTYPQ EQU   X'08'               Daypart record code                          
EQUKTYPQ EQU   X'09'               Equivalency header record type               
NTWKCTXQ EQU   X'69'               Network tax element                          
VATELQ   EQU   X'6A'               VAT (GST) element                            
PSTELQ   EQU   X'6B'               PST element                                  
POLPRDQ  EQU   X'FF'               Pool product number                          
*                                                                               
         EJECT                                                                  
SAVED    DSECT ,                   ** Dsect to cover saved storage **           
                                                                                
WVALUES  DS    0X                  ** Literal values **                         
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AFLTCMP  DS    A                   CANADIAN MAPPING FILTER                      
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
*                                                                               
YES      DS    C                   C'Y'                                         
POLPRD   DS    CL3                 C'POL'                                       
V130020  DS    XL4                 PC VERSION 1.3.0.020                         
V140000  DS    XL4                 PC VERSION 1.4.0.0                           
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
AEPDTAB  DS    0A                  A(estimate/product/demo array)               
ANMSTAB  DS    A                   A(network market/station array)              
ASDDTAB  DS    A                   A(spot array)                                
AFLMMSK  DS    A                   A(films sent mask)                           
ASDDTBL  DS    A                   A(LAST ENTRY)                                
                                                                                
NMSTMAX  DS    H                   Maximum number of NMSTAB entries             
NMSTMAX1 EQU   1024                Maximum entries on-line                      
NMSTMAX2 EQU   4096                Maximum entries off-line                     
                                                                                
STAESTB  DS    XL(TSPXTNL)         Station/estimate buffer                      
                                                                                
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
                                                                                
MAPI1    DS    X                   ** Map indicator byte1 1 **                  
MAP#PRDS EQU   MAPI1                                                            
MAPSPRDS EQU   X'80'               Download products                            
MAP#DMNU EQU   MAPI1                                                            
MAPSDMNU EQU   X'40'               Download daypart menus                       
MAP#DEST EQU   MAPI1                                                            
MAPSDEST EQU   X'20'               Build dynamic estimate list                  
MAP#SBDL EQU   MAPI1                                                            
MAPSSBDL EQU   X'10'               Single buy download                          
MAP#SNDL EQU   MAPI1                                                            
MAPSSNDL EQU   X'08'               Single network buy download                  
MAP#MGAN EQU   MAPI1                                                            
MAPSMGAN EQU   X'04'               Makegood analysis                            
MAP#SDDM EQU   MAPI1                                                            
MAPSSDDM EQU   X'02'               ShowDef/DemoDef/DemOver maintenance          
MAP#MBDL EQU   MAPI1                                                            
MAPSMBDL EQU   X'01'               Multiple buy download                        
                                                                                
MAPI2    DS    X                   ** Map indicator byte1 2 **                  
MAP#ORDD EQU   MAPI2                                                            
MAPSORDD EQU   X'80'               Order detail (program/demo lookup)           
MAP#ORDS EQU   MAPI2                                                            
MAPSORDS EQU   X'40'               Order summary                                
MAP#PIND EQU   MAPI2                                                            
MAPSPIND EQU   X'20'               Pinergy buy download                         
                                                                                
RUNI     DS    0XL(L'RUNI1)                                                     
                                                                                
RUNI1    DS    X                   ** Run indicator byte1 1 **                  
RUN#CUTI EQU   RUNI1                                                            
RUNSCUTI EQU   X'80'               Build cutin records                          
RUN#GOAL EQU   RUNI1                                                            
RUNSGOAL EQU   X'40'               Goal records found                           
RUN#SWAP EQU   RUNI1                                                            
RUNSSWAP EQU   X'20'               Have swapped media for goal reads            
RUN#XPLA EQU   RUNI1                                                            
RUNSXPLA EQU   X'10'               Exploded network buy record added            
RUN#NSTA EQU   RUNI1                                                            
RUNSNSTA EQU   X'08'               Doing network station lookup                 
                                                                                
BUFFRET  DS    X                   Buffer s/r return values                     
                                                                                
         DS    0F                                                               
QVALUES  DS    0XL256              ** Request values **                         
                                                                                
QORAIND  DS    0X                                                               
QMEDIND  DS    X                   Media                                        
QAORA    DS    0AL3                                                             
QAMED    DS    AL3                                                              
                                                                                
QCLTIND  DS    X                   Client                                       
QACLT    DS    AL3                                                              
                                                                                
QTIME    DS    0XL4                TIME (ORDER ACTIVITY DOWNLOAD)               
QPRDIND  DS    X                   Product                                      
QATIM    DS    0AL3                TIME                                         
QAPRD    DS    AL3                                                              
                                                                                
QMKTIND  DS    X                   Market                                       
QAMKT    DS    AL3                                                              
                                                                                
QSTAIND  DS    X                   Station                                      
QASTA    DS    AL3                                                              
                                                                                
QESTIND  DS    X                   Estimate                                     
QAEST    DS    AL3                                                              
                                                                                
QRDAIND  DS    X                   Record disk address                          
QARDA    DS    AL3                                                              
                                                                                
QSHWIND  DS    X                   Show code                                    
QASHW    DS    AL3                                                              
                                                                                
QDPTIND  DS    X                   Daypart                                      
QADPT    DS    AL3                                                              
                                                                                
QSLNIND  DS    X                   Seconds length                               
QASLN    DS    AL3                                                              
                                                                                
QSTRIND  DS    X                   Search string                                
QASTR    DS    AL3                                                              
                                                                                
QPNDIND  DS    X                   Program name/description                     
QAPND    DS    AL3                                                              
                                                                                
QDEMIND  DS    X                   Demo code                                    
QADEM    DS    AL3                                                              
                                                                                
QNADIND  DS    X                   National audience demos                      
QANAD    DS    AL3                                                              
                                                                                
QSMDIND  DS    X                   Station/market demos                         
QASMD    DS    AL3                                                              
                                                                                
QCAMIND  DS    X                   Campaign                                     
QACAM    DS    AL3                                                              
                                                                                
QSTSIND  DS    X                   Status                                       
QASTS    DS    AL3                                                              
                                                                                
QBYRIND  DS    X                   Buyer                                        
QABYR    DS    AL3                                                              
                                                                                
QSLRIND  DS    X                   Seller                                       
QASLR    DS    AL3                                                              
                                                                                
QLSTIND  DS    X                   List of states                               
QALST    DS    AL3                                                              
                                                                                
QMMPIND  DS    0X                  List of Mapping text                         
QSMPIND  DS    0X                                                               
QDMPIND  DS    X                                                                
QACMP    DS    AL3                                                              
                                                                                
QDA      DS    XL(L'IODA)          Disk address                                 
QAGYHX   DS    X                   Agency Hex                                   
QMEDIA   DS    X                   Media letter/media code                      
QMARKET# DS    XL2                 Market number                                
QSTACALL DS    CL(L'SDEFKSTA)      Station call letters                         
QESTNO   DS    X                   Estimate number                              
QLIVE    DS    C                   Live show                                    
QOPTOOPT DS    C                   Optimizer option                             
QEXCLOPT DS    C                   Optimizer option                             
QRTGSVC  DS    C                   Rating service                               
QROTDAY  DS    X                   Rotation day(s)                              
QOOWDAY  DS    X                   Out-of-week rotator day(s)                   
QDAYPART DS    C                   Daypart code                                 
QSTRTIME DS    AL2                 Military start time                          
QSTRTIMX DS    AL2                 Start time end                               
QENDTIMS DS    AL2                 End time start                               
QENDTIME DS    AL2                 Military end time                            
QKILLDAT DS    XL(L'NPGMKDAT)      Kill date                                    
QBBOOK   DS    XL(L'DOVBBK)        Base book                                    
QUTBOOK  DS    XL(L'DOVUTBK)       Use till book                                
QTYPFLT  DS    X                   Record type filter (GvP report)              
QIUNO    DS    C                   Include unordered                            
QSENDCLI DS    C                   N=suppress client details                    
QSENDCAM DS    C                   N=suppress campaign details                  
QTOTONLY DS    C                   Totals only                                  
OOWRFLAG DS    C                   OOWR Flag                                    
                                                                                
TODAYCMP DS    0XL2                TODAY'S DATE (COMPRESSED)                    
QACTSDAT DS    0XL(L'CDSACADT)     Activity start date                          
QESTSTDR DS    0XL(L'EPKEYSDT*2)   ** Start date driver range **                
QESTSSTR DS    XL(L'EPKEYSDT)      X'0000'                                      
QESTEDAT DS    XL(L'EPKEYSDT)      Request estimate end date                    
                                                                                
QACTEDAT DS    0XL(L'CDSACADT)     Activity end date                            
QESTENDR DS    0XL(L'EPKEYEDT*2)   ** End date driver range **                  
QESTSDAT DS    XL(L'EPKEYEDT)      Request estimate start date                  
QESTEEND DS    XL(L'EPKEYEDT)      X'FFFF'                                      
                                                                                
QCAMX    DS    XL(L'CMPKCAM)       Campaign number (complemented)               
QORDX    DS    XL(L'CORKORD)       Order number (complemented)                  
                                                                                
LINCKSUM DS    XL9                 We have 39 characters in DDSPtr              
                                                                                
CRDTRNGE DS    0XL(L'QESTSDAT+L'QESTEDAT)                                       
CRDTRSTR DS    XL(L'QESTSDAT)                                                   
CRDTREND DS    XL(L'QESTEDAT)                                                   
         ORG   QVALUES+L'QVALUES                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
         DS    0F                                                               
DVALUES  DS    0X                  ** Derived values **                         
                                                                                
DPRCIND  DS    X                   Built product code in WMP                    
DAPRC    DS    AL3                                                              
DPRDMAXQ EQU   255                 Maximum N'products supported                 
                                                                                
DPR#IND  DS    X                   Built product number in WMP                  
DAPR#    DS    AL3                                                              
                                                                                
DDPTIND  DS    X                   Built daypart menu in WMP                    
DADPT    DS    AL3                                                              
DDPTMAXQ EQU   64                  Maximum n'daypart menus supported            
                                                                                
DESTIND  DS    X                   Built estimate in WMP                        
DAEST    DS    AL3                                                              
DESTMAXQ EQU   255                 Maximum n'estimates supported                
                                                                                
DSTAIND  DS    X                   Built station in WMP                         
DASTA    DS    AL3                                                              
DSTAMAXQ EQU   256                 Maximum n'stations supported                 
                                                                                
DKEYIND  DS    X                   Built KEYS in WMP                            
DAKEY    DS    AL3                                                              
DKEYMAXQ EQU   500                 MAXIMUM N'STATIONS SUPPORTED                 
                                                                                
DNROWS   DS    H                   N'rows in output array                       
                                                                                
DLODATEE DS    CL6                 Low date (ebcdic)                            
DHIDATEE DS    CL6                 High date (ebcdic)                           
                                                                                
DLODATEC DS    XL2                 Low date (compressed)                        
DHIDATEC DS    XL2                 High date (compressed)                       
                                                                                
DMKTSTAL EQU   L'DPRKMKT+L'DPRKSTA                                              
DMKTSTA  DS    XL(DMKTSTAL)        Market/station                               
                                                                                
DEQTAB   DS    0X                  ** Spot length equivalency array **          
DEQTSLEN DS    AL1                 Prime spot length                            
DEQTFACT DS    AL2                 Equivalency factor                           
DEQTABW  EQU   *-DEQTAB            Width of array entry                         
DEQTABN  EQU   30                  Maximum number of entries in DEQTAB          
         DS    (DEQTABN-1)XL(DEQTABW)                                           
DEQTABL  EQU   *-DEQTAB            Array entry length                           
                                                                                
DVALUEL  EQU   *-DVALUES                                                        
                                                                                
LASTS    DS    0X                  ** Last time values **                       
LCAM     DS    XL(L'DDMKCAM)       Last campaign sent                           
LMKTSTA  DS    XL(L'DDMKSTA+L'DDMKLIN)                                          
LASTSL   EQU   *-LASTS                                                          
                                                                                
ASLNTAB  DS    A                   A(spot length array entry)                   
ANEXTEL  DS    A                   A(next element to process)                   
BUYSDAT  DS    XL(L'BDSTART)       Buy start date                               
BUYEDAT  DS    XL(L'BDEND)         Buy end date                                 
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
AGENCY   DS    CL(L'LP_AGY)        Agency alpha id                              
AGENCYB  DS    CL(L'LP_AGYB)       Agency binary value                          
MARKETA  DS    CL(L'SMKT)          Market number in ebcdic                      
CLTEQU30 DS    AL2                 30 second equivalence factor                 
PERNAME  DS    CL62                Person name                                  
                                                                                
AALL     DS    AL3                 All value wmp entry                          
ANZR     DS    AL3                 Non-zero value WMP entry                     
AZER     DS    AL3                 Zero value WMP entry                         
                                                                                
SVORDKEY DS    0XL(L'IOKEY)        Saved order key                              
SVDMPKEY DS    0XL(L'IOKEY)        Saved mapping key                            
SVCAMKEY DS    XL(L'IOKEY)         Saved campaign key                           
SVPRGKEY DS    XL(L'IOKEY)         Saved program key                            
SVCORKEY DS    XL(L'IOKEY)         Saved current Order key                      
SAVE1OR2 DS    X                                                                
                                                                                
OVALUES  DS    0D                  ** Output values **                          
                                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
                                                                                
         ORG   OVALUES                                                          
SLNSLEN  DS    AL1                 Spot length                                  
SLNEQIV  DS    AL1                 Spot length equivalent                       
                                                                                
         ORG   OVALUES                                                          
MARKETN  DS    CL(L'MKTNAME)       Market name                                  
STAFLAG1 DS    CL(L'SFLAG1)        (SELECTIVE) STATION FLAG 1                   
                                                                                
         ORG   OVALUES                                                          
NETTYPE  DS    CL(L'SNETTYPE)      Network type                                 
NETFLAG1 DS    CL(L'SFLAG1)        (NETWORK) STATION FLAG 1                     
                                                                                
         ORG   OVALUES                                                          
ESTV     DS    0X                  ** Estimate values **                        
                                                                                
ESTFRZMO DS    XL(L'ELOCKYM)       Frozen month                                 
                                                                                
ESTINDS  DS    0XL4                ** Estimate indicators **                    
ESTINDS1 DS    X                   ** Estimate indicators byte1 1 **            
ESTINDS2 DS    X                   ** Estimate indicators byte1 2 **            
ESTINDS3 DS    X                   ** Estimate indicators byte1 3 **            
ESTINDS4 DS    X                   ** Estimate indicators byte1 4 **            
EST#LOCK EQU   ESTINDS4                                                         
ESTILOCK EQU   X'04'               Estimate locked for all months               
EST#FSUB EQU   ESTINDS4                                                         
ESTIFSUB EQU   X'02'               Frozen month subsequent                      
EST#FPRI EQU   ESTINDS4                                                         
ESTIFPRI EQU   X'01'               Frozen month prior                           
                                                                                
ESTC2FAC DS    PL4                 Est Cost2 factor                             
                                                                                
ESTVL    EQU   *-ESTV                                                           
                                                                                
         ORG   OVALUES                                                          
CLTV     DS    0X                  ** Client values **                          
                                                                                
CLTFRZMO DS    XL(L'CLOCKYM)       Frozen year/month                            
                                                                                
CLTINDS  DS    0XL4                ** Client indicators **                      
CLTINDS1 DS    X                   ** Client indicators byte1 1 **              
CLTRUNI1 DS    X                   ** Client indicators byte1 2 **              
CLTINDS3 DS    X                   ** Client indicators byte1 3 **              
CLTINDS4 DS    X                   ** Client indicators byte1 4 **              
                                                                                
CLT#PRGA EQU   CLTINDS3                                                         
CLTIPRGA EQU   X'02'               PROCTER & GAMBLE (P&G)                       
CLT#C2TR EQU   CLTINDS3                                                         
CLTIC2TR EQU   X'01'               Frozen month subsequent                      
CLT#FSUB EQU   CLTINDS4                                                         
CLTIFSUB EQU   X'80'               Frozen month subsequent                      
CLT#FPRI EQU   CLTINDS4                                                         
CLTIFPRI EQU   X'40'               Frozen month prior                           
CLT#FRZN EQU   CLTINDS4                                                         
CLTIFRZN EQU   X'20'               Frozen client                                
CLT#C2OP EQU   CLTINDS4                                                         
CLTIC2OP EQU   X'10'               Cost2 optional                               
CLT#OOWC EQU   CLTINDS4                                                         
CLTIOOWC EQU   X'08'               Out of week client                           
CLT#GLRQ EQU   CLTINDS4                                                         
CLTIGLRQ EQU   X'04'               Goal required for buying                     
CLT#USSP EQU   CLTINDS4                                                         
CLTIUSSP EQU   X'02'               US spill                                     
CLT#C2RQ EQU   CLTINDS4                                                         
CLTIC2RQ EQU   X'01'               Cost2 required                               
                                                                                
CLTC2FAC DS    PL4                 Clt Cost2 factor                             
                                                                                
CLTVL    EQU   *-CLTV                                                           
                                                                                
         ORG   OVALUES                                                          
PROFVALS DS    0XL16               ** Profile values **                         
PROFV01  DS    X                                                                
PROFV02  DS    X                                                                
PROFV03  DS    X                                                                
PROFV04  DS    X                                                                
PROFV05  DS    X                                                                
PROFV06  DS    X                                                                
PROFV07  DS    X                                                                
PROFV08  DS    X                                                                
PROFV09  DS    X                                                                
PROFV10  DS    X                                                                
PROFV11  DS    X                                                                
PROFV12  DS    X                                                                
PROFV13  DS    X                                                                
PROFV14  DS    X                                                                
PROFV15  DS    X                                                                
PROFV16  DS    X                                                                
                                                                                
PROFVM#  DS    AL1                 Map number                                   
                                                                                
         ORG   OVALUES                                                          
DPTMENU  DS    CL(L'DPTKMENU)      Daypart menu code                            
DPTTAB   DS    (DPTTMAXN)CL(DPTTABL),X                                          
                                                                                
         ORG   OVALUES                                                          
SVLFLAG  DS    X                   ** Station validation flags **               
SVLFSPL  EQU   X'80'               Spill definition record found                
SVLFCBL  EQU   X'40'               Cable definition record found                
SVLFNET  EQU   X'20'               Network definition record found              
                                                                                
         ORG   OVALUES                                                          
BUYV     DS    0X                  ** Buy record values **                      
                                                                                
BUYFLAG  DS    X                   ** Buy flags **                              
BUYFNTWF EQU   X'80'               First network element                        
BUYFNTWK EQU   X'40'               Network same as previous                     
                                                                                
BUYMKTL  DS    XL(L'BUYKMKTN)      Last market                                  
BUYSTAL  DS    XL(L'BUYKSTAC)      Last station/network                         
BUYESTL  DS    XL(L'BUYKEST)       Last estimate                                
                                                                                
DEPDTAB# DS    0H                  N'entries in EPDTAB                          
DNMSTAB# DS    H                   N'entries in NMSTAB                          
                                                                                
NMSVALS  DS    0X                  ** Network market/station values **          
NMSSEQ   DS    AL(L'NMSTSEQ)       Market/station sequence number               
NMSMKT   DS    XL(L'BUYKMKTN)      Market number                                
NMSSTA   DS    CL(L'STAPQSTA+L'STAPQNET)                                        
NMSVALL  EQU   *-NMSVALS                                                        
                                                                                
SVNTWKLN DS    H                   Length of saved network data                 
SVNTWKEL DS    XL(ONEK)            Network data                                 
*                                                                               
BUYC2FAC DS    XL4                 COST 2 FACTOR                                
                                                                                
BUYVL    EQU   *-BUYV                                                           
                                                                                
ANWKELEM DS    AL4                 A(current network element)                   
                                                                                
                                                                                
XPLREC   DS    0X                  ** Cutin buffer record **                    
XPLMKST  DS    0XL(L'NTWKMKST)     ** Market/station **                         
XPLMKT   DS    XL(L'BUYKMKTN)      Market                                       
XPLSTA   DS    XL(L'BUYKSTAC)      Station                                      
XPLEST   DS    XL(L'BUYKEST)       Estimate number                              
XPLLIN   DS    XL2                 Buy Line number                              
XPLKEYL  EQU   *-XPLREC            Key length                                   
                                                                                
XPLNBYDA DS    XL(L'IODA)          Disk address of network buy record           
XPLMSSEQ DS    XL(L'NMSTSEQ)       Market/station sequence number               
XPLRECL  EQU   *-XPLREC            Record length                                
                                                                                
ALLREC   DS    0X                  ** allocation record **                      
ALLNBYDA DS    XL(L'IODA)          Disk address of network buy record           
ALLKEYL  EQU   *-ALLREC            Key length                                   
                                                                                
MAXALLOC EQU   (20*1024)/$SDDDL                                                 
ALLALLOC DS    (MAXALLOC)XL($ALPRDL) Buy Spot Allocations                       
ALLOCL   EQU   *-ALLALLOC                                                       
ALLRECL  EQU   *-ALLREC            Record length                                
                                                                                
         ORG   OVALUES                                                          
SHOWVALS DS    0X                                                               
SHOWCODE DS    CL(L'NPGMKID)       ShowCode (upload)                            
SHOWDA   DS    XL(L'IODA)          Disk address of ShowDef record               
DOVDATE  DS    XL(L'DOVCDAT)       Creation date                                
DOVRDA   DS    XL(L'IODA)          Disk address of DemOver record               
SHOWVALL EQU   *-SHOWVALS                                                       
                                                                                
DOVDEMOS DS    AL2                 Number of demos in DemOver record            
                                                                                
DOVV     DS    0X                  ** DemOver record values **                  
DOVMKT#  DS    CL4                 Market number                                
DOVSTAC  DS    CL(L'STAPQSTA+L'STAPQNET)                                        
DOVL     EQU   *-DOVV                                                           
                                                                                
         ORG   OVALUES                                                          
GVPV     DS    0A                  ** GvP report values **                      
GVPADEM  DS    A                   A(demo element on buy record)                
GVPAROW  DS    A                   A(current row)                               
GVPWEEK  DS    XL(L'GPWRWKSD)      Current week start date                      
GVPDOLS  DS    PL(L'GPWRDOLS)      Previous dollars                             
GVPGRPS  DS    PL(L'GPWRGRPS)      Previous grps                                
GVPDEM#  DS    AL(L'GPWRDEM#)      Previous number of demos                     
GVPDEMO  DS    XL(GPWRDEML)        Previous demo codes/values                   
GVPEMOS  DS    (CPPTMAXM)XL4,X     Estimate months                              
GVPPEST  DS    XL(L'GDCPPES)       CPP guide estimate 2                         
GVPPEST2 DS    XL(L'GDCPPES2)      CPP guide estimate override                  
GVPLKEY  DS    XL(GVPRKEYL)        Last GvP key sent                            
                                                                                
GVPWEEK# DS    H                   Actual N'entries in GVPWEEKS                 
GVPWEEKS DS    (GVPWEEKN)XL(L'RDATE)                                            
GVPWEEKN EQU   160                 Enough for 3 and a bit years                 
                                                                                
GPWREC   DS    XL(GPWRRECL)        GvP weekly record                            
                                                                                
GPWFLAG  DS    X                   ** Flag byte **                              
GPWFANY  EQU   X'80'               At least one record read                     
GPWFBUF  EQU   X'40'               Record buffered                              
GPWFEOF  EQU   X'20'               End of file last time                        
GVPL     EQU   *-GVPV                                                           
                                                                                
         ORG   OVALUES                                                          
LORD     DS    XL(L'CORKORD)       Last order number                            
ORDREV   DS    XL(L'CORKREV)       Latest order revision number                 
OSUSTST  DS    XL(L'CORSTST)       Order summary status                         
OSUSTACT DS    XL(L'CORSTACT)      Order summary action flag                    
OSUDATE  DS    XL(L'CORSTEXD)      Date                                         
OSUTIME  DS    XL(L'CORSTEXT)      Time                                         
                                                                                
         ORG   OVALUES                                                          
OSPWEEK  DS    XL(L'DPRSWWK)       Latest week                                  
                                                                                
OSPWKS   DS    0X                  ** Order summary spots per week **           
OSPWKSDT DS    XL(L'DPRSWWK)       Week start date                              
OSPWKSPT DS    XL2                 Number of spots                              
OSPWKREP DS    X                   Replication factor                           
OSPWKLEN EQU   *-OSPWKS                                                         
                                                                                
         ORG   OVALUES                                                          
OSDDEM   DS    0X                  ** Demo codes and values **                  
OSDCODE  DS    XL(L'DDMDVDEM)      Demo code                                    
OSDVALUE DS    XL(L'DDMDVEST)      Demo value                                   
OSDDEMLN EQU   *-OSDDEM                                                         
                                                                                
         ORG   OVALUES                                                          
ORAREC   DS    XL(ORARRECL)                                                     
ORAREC2  DS    XL(ORARRECL)                                                     
                                                                                
         ORG   SAVED+L'SVSERVER                                                 
FLMMSK   DS    XL(8*ONEK)          Films sent mask                              
         EJECT                                                                  
       ++INCLUDE SPLNKWRK                                                       
                                                                                
CLTRECD  DSECT ,                                                                
         ORG   CKEYCLT+L'CKEYCLT                                                
CKEYREST DS    XL(L'CKEY-(*-CKEY))                                              
                                                                                
PRDRECD  DSECT ,                                                                
         ORG   PKEYPRD+L'PKEYPRD                                                
PKEYREST DS    XL(L'PKEY-(*-PKEY))                                              
                                                                                
ESTRECD  DSECT ,                                                                
         ORG   EKEYEST+L'EKEYEST                                                
EKEYREST DS    XL(L'EKEY-(*-EKEY))                                              
                                                                                
WORKD    DSECT ,                   ** Redefine overwork **                      
         ORG   OVERWORK                                                         
                                                                                
BUFBLK1  DS    XL(TSPXTNL)         TSAR buffer 1                                
BUFBLK2  DS    XL(TSPXTNL)         TSAR buffer 2                                
                                                                                
SAVERE   DS    A                   Save RE                                      
SAVERF   DS    A                   Save RF                                      
SAVER0   DS    A                   Save R0                                      
SAVER1   DS    A                   Save R1                                      
SAVER2   DS    A                   Save R2                                      
SAVER3   DS    A                   Save R3                                      
SAVER4   DS    A                   Save R4                                      
*                                                                               
VRECUP   DS    V                   RECUP from COMFACS                           
*                                                                               
SVIOVALS DS    XL(IOVALL)          Saved i/o values                             
NXOIOSAV DS    XL(IOVALL)          SAVED I/O VALUES FOR NXTORD                  
         ORG                                                                    
                                                                                
* Other included books                                                          
         PRINT OFF                                                              
       ++INCLUDE GEGENSDR                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE SPEDUPD                                                        
       ++INCLUDE SPADBUYER                                                      
       ++INCLUDE SPMGADN                                                        
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE SPGENDAYPT                                                     
         ORG   DPTKMENU+L'DPTKMENU                                              
DPTKREST DS    XL(L'DPTKEY-(*-DPTKEY))                                          
       ++INCLUDE SPGENESTD                                                      
       ++INCLUDE SPGENEQU                                                       
       ++INCLUDE SPGENXLK                                                       
       ++INCLUDE SPGENCDORD                                                     
       ++INCLUDE SPGETBUBLD                                                     
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPTRCMML                                                       
STARECD  DSECT ,                                                                
       ++INCLUDE SPGENSTA                                                       
REPRECD  DSECT ,                                                                
       ++INCLUDE SPGENREP                                                       
ADDRECD  DSECT ,                                                                
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENDOV                                                       
       ++INCLUDE SPGENSDEF                                                      
       ++INCLUDE SPGENNPGM                                                      
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE SPLNKRECS                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067SPLNK22   11/06/18'                                      
         END                                                                    
