*          DATA SET NENAV10    AT LEVEL 006 AS OF 12/04/12                      
*PHASE T31810A                                                                  
NENAV14  TITLE '- Film Usage Analysis for Net MM'                               
         PRINT NOGEN                                                            
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,SERVERTYPE=TSTSTEW,WORKERKEY=NFUA,SEGMENT=Y,     +        
               APPEND=Y,REQUEST=*,CODE=CODE,SYSPHASE=SYSPHASE,IDF=Y,   +        
               SYSTEM=NETSYSQ,BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),    +        
               FILES=FILES,BLOCKS2=(B#PTNREC,NPTRECD)                           
                                                                                
B#PTNREC EQU   B#IOA3              I/O 3 used for product reading               
APATREC  EQU   LP_BLKS+((B#PTNREC-1)*L'LP_BLKS),,C'A'                           
         EJECT                                                                  
CODE     NMOD1 0,**NN10**                                                       
                                                                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK parameter block)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   INIT02                                                           
         L     R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         L     R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         J     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(Global w/s)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R8,LINKW            Point to end of work area                    
         USING SAVED,R8                                                         
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         LA    R0,LP_D                                                          
         ST    R0,ALP              Set A(LP_D)                                  
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    Extract DDLINK/RUNNER calling mode           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
                                                                                
         L     RF,ACOMFACS         Load support routines                        
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,0,X'D9000AFE'                                          
         MVC   VTRPACK,0(R1)       A(TRPACK)                                    
         DROP  R6,R7,RB                                                         
                                                                                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
                                                                                
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    Test first for run                           
         JNE   PRCWRK                                                           
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY                                                            
                                                                                
         L     RF,ACOMFACS         Load support routines                        
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   LP_AUIR1,0(R1)      Set A(support overlay 1)                     
         MVC   AROUTS1,0(R1)                                                    
                                                                                
         GOTOR (RF),(R1),('P#ROUTS2',0),0,0                                     
         MVC   LP_AUIR2,0(R1)      Set A(support overlay 2)                     
         MVC   AROUTS2,0(R1)                                                    
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize global working storage            
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    Test 'process work' mode                     
         JNE   RUNREQ                                                           
         XC    QVALUES(QVALUEL),QVALUES                                         
         XC    DVALUES(DVALUEL),DVALUES                                         
         LA    R0,QVALS                                                         
         LHI   R1,L'QVALS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
                                                                                
         L     RE,LP_AWMP          Build default range elements in WMP          
         USING LW_D,RE                                                          
         MVI   LW_TYPE,LW_TALLQ    All values                                   
         STCM  RE,7,AALL                                                        
         AHI   RE,LW_LN1Q                                                       
         MVI   LW_TYPE,LW_TNZRQ    Non-zero values                              
         STCM  RE,7,ANZR                                                        
         AHI   RE,LW_LN1Q                                                       
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
         MVC   AGENCY,LP_AGY       Set agency alpha id                          
         MVC   AGENCYB,LP_AGYB     Set agency binary value                      
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNREQ02                                                         
                                                                                
         L     RF,AMASTC           Set trace option                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,XSPDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPFIL,(4,0),0                               
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
RUNREQ02 GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY               Exit back to DDLINK                          
         EJECT                                                                  
***********************************************************************         
* Request for FUA for net MM                                          *         
***********************************************************************         
                                                                                
FUADLDN  LKREQ H,X'0139',OUTFUAN                                                
                                                                                
Media    LKREQ F,01,(I,B#SAVED,QMEDNDX),(U,#VALMED,$VALMED),           +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=(*,MEDCDLIT),COL=*             
Client   LKREQ F,02,(I,B#SAVED,QCLTNDX),(U,#VALCLT,$VALCLT),           +        
               OLEN=L'QCLTX,TEXT=(*,CLTCDLIT),COL=*                             
Preflst  LKREQ F,03,(I,B#SAVED,QPRFLSTX),LBIN,LIST=Y,SORT,             +        
               OLEN=3,TEXT=(*,PREFLIT),COL=*                                    
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
         EJECT                                                                  
                                                                                
MEDCDLIT DC    C'Media code'                                                    
CLTCDLIT DC    C'Client code'                                                   
PREFLIT  DC    C'Pattern Reference'                                             
                                                                                
***********************************************************************         
* Film usage analysis download for net                                *         
***********************************************************************         
                                                                                
OUTFUAN  LKOUT H                                                                
                                                                                
RDPATTN  LKOUT R,X'0140'           FILM USAGE ANALYSIS DOWNLOAD                 
Array    LKOUT C,1,(A,ARYPATN)     PATTERN RECORDS                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYPATN  LKOUT A,(R,NXTPAT),MULTIROW=Y                                          
         LKOUT C,X'0140',(A,ARYPATNT)                                           
         LKOUT E                                                                
                                                                                
ARYPATNT LKOUT A,(D,B#PTNREC,NPTRECD),NEWEL=Y,NROWS=1,ROWWIDTH=1000             
PatPrdn  LKOUT C,1,NPTXPRD,CHAR,ND=Y                                            
PatSln   LKOUT C,2,NPTXSLN,LBIN,ND=Y                                            
PatPrd2  LKOUT C,3,NPTXPRD2,CHAR,ND=Y                                           
PatSln2  LKOUT C,4,NPTXSLN2,LBIN,ND=Y                                           
PatRef   LKOUT C,5,NPTXR3F,LBIN,ND=Y                                            
PatNet   LKOUT C,6,NPTXNET,CHAR,ND=Y                                            
PatProg  LKOUT C,7,NPTXPROG,CHAR,ND=Y                                           
Array    LKOUT C,X'0141',(A,ARYPDATN)  X'10' ELEM DATA                          
Array    LKOUT C,X'0142',(A,ARYPCMLN)  COMMERCIAL LIST                          
Array    LKOUT C,X'0143',(A,ARYPROTN)  ROTATION LIST                            
         LKOUT E                                                                
********                                                                        
******** Pattern x'10' Element Data                                             
********                                                                        
ARYPDATN LKOUT A,(D,B#PTNREC,NPTXKEY+42),EOT=0,NEWEL=Y,                *        
               ROWID=(NPTDATA,X'10'),ROWWIDTH=(V,NPTDTALN)                      
PatDscrp LKOUT C,1,NPTDESC,CHAR,ND=Y                                            
PatStDt  LKOUT C,2,NPTSTART,BDAT,ND=Y                                           
PatEndDt LKOUT C,3,NPTEND,BDAT,ND=Y                                             
PatPseq  LKOUT C,4,NPTS3QNO,LBIN,BDAT,ND=Y                                      
PatStim  LKOUT C,5,NPTSTIM,LBIN,BDAT,ND=Y,FILTROUT=FILTTIM,SKIPCOLS=1           
PatEtim  LKOUT C,6,NPTETIM,LBIN,BDAT,ND=Y                                       
         LKOUT E                                                                
********                                                                        
******** Pattern x'30' Element Data (Commercial List)                           
********                                                                        
ARYPCMLN LKOUT A,(D,B#PTNREC,NPTXKEY+42),EOT=0,NEWEL=Y,                *        
               ROWID=(NPTCMLEL,X'30'),ROWWIDTH=(V,NPTCMLLN)                     
*                                                                               
ARRAY    LKOUT C,1,(A,ARYCMLNT)                                                 
         LKOUT E                                                                
                                                                                
ARYCMLNT LKOUT A,(*,NPTCML),ROWNAME=NPTCMLEL,NROWS=*,ROWWIDTH=16                
                                                                                
PRout    LKOUT P,NPTCML,EDTCML                                                  
CMLLST1  LKOUT C,1,(D,B#WORKD,COMML),CHAR,LEN=16                                
CMLLST2  LKOUT C,2,(D,B#WORKD,COMML2),CHAR,LEN=12,FILTROUT=FILTCML              
         LKOUT E                                                                
********                                                                        
******** Pattern x'32' Element Data (Rotation List)                             
********                                                                        
ARYPROTN LKOUT A,(D,B#PTNREC,NPTXKEY+42),EOT=0,NEWEL=Y,                *        
               ROWID=(NPTPTNEL,X'32'),ROWWIDTH=(V,NPTPTNLN)                     
*                                                                               
ARRAY    LKOUT C,1,(A,ARYROTN)                                                  
         LKOUT E                                                                
                                                                                
ARYROTN  LKOUT A,(*,NPTPTN),ROWNAME=NPTPTNEL,NROWS=*,ROWWIDTH=1                 
                                                                                
CMLROTN  LKOUT C,1,NPTPTN,CHAR,ND=Y                                             
         LKOUT E                                                                
                                                                                
FILTCML  NTR1                                                                   
         CLC   LP_VRSN1,=X'02000000'                                            
         JH    EXITY                                                            
         J     EXITN                                                            
                                                                                
FILTTIM  NTR1                                                                   
         L     R2,LP_AINP          A(PATTERN DATA ELEMENT)                      
         USING NPTDATA,R2          PATTERN DATA ELEMENT DSECT                   
         TM    NPTSTAT,NPTS_TIME   ELEM HAS START/END TIME?                     
         JZ    EXITN               NO - CC NEQ                                  
         CLC   NPTSTIM,=H'2400'    START TIME = 2400?                           
         JNE   EXITY               NO                                           
         XC    NPTSTIM,NPTSTIM     YES - 12M = 0000                             
         J     EXITY               YES - CC EQU                                 
         DROP  R2                  DROP PATTERN DATA ELEM USING                 
                                                                                
EDTCML   NTR1                                                                   
         L     R2,LP_AINP          A(CMML/AD-ID ENTRY)                          
         XC    COMML,COMML                                                      
         XC    COMML2,COMML2                                                    
         MVC   COMML(8),0(R2)      MOVE IN CMML                                 
         CLC   LP_VRSN1,=X'02000000'                                            
         JH    ECML10                                                           
         MVC   COMML(16),0(R2)                                                  
         J     EXITY                                                            
                                                                                
ECML10   MVC   COMML2(8),8(R2)     MOVE IN CMML2                                
         CLI   PACKED,C'Y'         ARE CMML PACKED?                             
         JNE   EXITY               NO - EXIT                                    
                                                                                
         GOTOR VTRPACK,DMCB,(C'U',0(R2)),COMML                                  
         OC    8(8,R2),8(R2)       HAVE PIGGY CMML?                             
         JZ    EXITY               NO - DONE                                    
         GOTOR VTRPACK,DMCB,(C'U',8(R2)),COMML2                                 
         J     EXITY                                                            
                                                                                
***  Next Pattern Record for Net                                                
NXTPAT   GOTOR (#NXTREC,ANXTREC),DMCB,PATKEYT,('B#PTNREC',0),          *        
               ('$NXTRXSP',SAVED),0,0                                           
         JNE   NOMORE                                                           
                                                                                
         L     R6,APATREC          A(PATTERN RECORD)                            
         USING NPTXKEY,R6                                                       
         OC    NPTXOR3G,NPTXOR3G   INACTIVE PATTERN?                            
         JNZ   NXTPAT              YES - IGNORE THIS RECORD!                    
         XC    NPTXR3F,=3X'FF'     UN-COMPLEMENT                                
         CLI   NPTXNET+2,X'FF'     MEDIA SPECIFIC?                              
         JNE   *+16                NO                                           
         MVC   NPTXNET+2(1),NPTXNET+1                                           
         MVC   NPTXNET(2),=C'M='                                                
         CLI   NPTXPROG,X'FF'      IS THIS A FEED OR DAYPART?                   
         JNE   *+10                NO                                           
         XC    NPTXPROG,NPTXPROG   YES - NO PROGRAM                             
         DROP  R6                                                               
                                                                                
         MVI   PACKED,C'N'         DEFAULT IS CMML ARE NOT PACKED               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JNE   NPATX                                                            
         USING NPTDATA,R6                                                       
         TM    NPTSTAT,NPTS_ADID   ALL CMML PACKED?                             
         JZ    *+8                 NO                                           
         MVI   PACKED,C'Y'         YES                                          
                                                                                
NPATX    J     EXITY                                                            
         DROP  R6                                                               
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
MORE     MVI   LP_RMODE,LP_RMORE   Set more to come                             
         J     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   Set no more records & exit                   
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,0                Set condition code to not equal              
         J     EXITCC                                                           
EXITY    LHI   RE,1                Set condition code to equal                  
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   General exit point                           
         EJECT                                                                  
*                                                                               
         GETEL R6,42,ELCODE                                                     
*                                                                               
GLOBALS  DS    0D                  ** Global literals **                        
                                                                                
         LTORG ,                                                                
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
STAFIL   DC    C'STATION'                                                       
                                                                                
FILES    DS    0X                  ** File info **                              
         DC    C'SPOT   '          System name for open                         
                                                                                
         DC    C'N'                                                             
SPTDIR   DC    C'SPTDIR '                                                       
         DC    C'N'                                                             
SPTFIL   DC    C'SPTFILE'                                                       
         DC    C'N'                                                             
XSPDIR   DC    C'XSPDIR '                                                       
         DC    C'N'                                                             
XSPFIL   DC    C'XSPFIL '                                                       
         DC    C'N'                                                             
UNTDIR   DC    C'UNTDIR '                                                       
         DC    C'N'                                                             
UNTFIL   DC    C'UNTFIL '                                                       
         DC    C'N'                                                             
         DC    C'STAFILE'                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
                                                                                
BZEROES  DC    XL4'00'                                                          
                                                                                
LVALUES  DS    0X                  ** Values moves to WVALUES **                
                                                                                
PATKEYT  LKKEY H,NPTXKEY,SAVED     ** PATTERN KEY DRIVER **                     
         LKKEY LIT,NPTPXID,X'0A',1                                              
         LKKEY LIT,NPTPXID+1,X'E1',1                                            
         LKKEY WMP,NPTPXAM,QAMED                                                
         LKKEY WMP,NPTPXCLT,QACLT                                               
         LKKEY WMP,NPTPXS3Q,QPRFLST                                             
         LKKEY LIT,NPTPREST,0                                                   
         LKKEY E                                                                
                                                                                
         LTORG ,                                                                
         EJECT                                                                  
SAVED    DSECT ,                   ** Saved storage **                          
                                                                                
WVALUES  DS    0X                  ** Values set from LVALUES **                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
VTRPACK  DS    A                   A(TRPACK)                                    
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
AGENCY   DS    CL(L'LP_AGY)        Agency alpha id                              
AGENCYB  DS    CL(L'LP_AGYB)       Agency binary value                          
                                                                                
AALL     DS    AL3                 All value WMP entry                          
ANZR     DS    AL3                 Non-zero value WMP entry                     
                                                                                
QVALUES  DS    0F                  ** Request values **                         
                                                                                
QMEDNDX  DS    0XL4                Media index                                  
QMEDIND  DS    X                                                                
QAMED    DS    AL3                                                              
                                                                                
QCLTNDX  DS    0XL4                Client index                                 
QCLTIND  DS    X                                                                
QACLT    DS    AL3                                                              
                                                                                
QPRFLSTX DS    0XL4                Pattern index                                
QPRFIND  DS    X                                                                
QPRFLST  DS    AL3                                                              
                                                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
DVALUES  DS    0F                  ** Derived/built values **                   
                                                                                
DMKTNUM  DS    AL2                 N'entries in MKTLST                          
DMKTLST  DS    12XL(L'GKEYMKT)     Market list                                  
                                                                                
DVALUEL  EQU   *-DVALUES                                                        
                                                                                
OVALUES  DS    0D                  ** Output values **                          
         EJECT                                                                  
*NENAVWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE NENAVWORKD                                                     
       ++INCLUDE SPTRNPAT                                                       
NPTRECD  DSECT                                                                  
         ORG   NPTPXS3Q+L'NPTPXS3Q                                              
NPTPREST DS    XL25                                                             
         PRINT ON                                                               
                                                                                
WORKD    DSECT ,                   ** Redefine OVERWORK **                      
         ORG   OVERWORK                                                         
COMML    DS    CL16                COMMERCIAL/AD-ID                             
COMML2   DS    CL12                PIGGY COMMERCIAL/AD-ID                       
PACKED   DS    CL1                 PACKED FLAG                                  
ELCODE   DS    XL1                 ELEMENT CODE FOR GETEL CALLS                 
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NENAV10   12/04/12'                                      
         END                                                                    
