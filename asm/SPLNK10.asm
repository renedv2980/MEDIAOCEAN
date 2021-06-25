*          DATA SET SPLNK10    AT LEVEL 040 AS OF 04/17/18                      
*PHASE T21E10C                                                                  
SPLNK10  TITLE '- MFM/MediaVantage Downloads'                                   
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,WORKERKEY=SPMV,        +        
               SYSPHASE=SYSPHASE,SYSTEM=SPTSYSQ,APPEND=Y,SEGMENT=Y,    +        
               SERVERTYPE=TSTSPOT,AUTOCLEAR=Y,                         +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#TWAD,TWAD,        +        
               B#LP_D,LP_D,B#AGYREC,AGYHDR,B#CLTREC,CLTRECD,           +        
               B#OFC,MOFRECD,B#SDRREC,SDRRECD,B#SECD,SECD,             +        
               B#RLP,GRPKEYD,B#MKGREC,MKGRECD,B#CLGREC,SP$GRPRECD,     +        
               B#TCMREC,CMLRECD,B#PRDREC,PRDRECD,B#TPHREC,PRHRECD,     +        
               B#TRCREC,SHPRECD,B#TCLREC,CLSRECD,B#CTXREC,CMTRECD,     +        
               B#PATREC,PATRECD,B#CNTREC,CNTRECD,B#FLTREC,FLTRECD,     +        
               B#MLSREC,MKLRECD)                                                
                                                                                
B#AGYREC EQU   3                   IO1 - agency record                          
B#OFC    EQU   3                   IO1 - office record                          
AOFCREC  EQU   AIO1                                                             
ARCPREC  EQU   AIO3                                                             
                                                                                
B#RLP    EQU   4                   IO2 - RLP group record                       
B#STAREC EQU   4                   IO2 - station record                         
B#CLTREC EQU   4                         client record                          
ARLPREC  EQU   AIO2                                                             
                                                                                
B#PRDREC EQU   5                   IO3 - product record                         
B#SDRREC EQU   5                         Self-Defining Record                   
B#MKGREC EQU   5                         Market group record                    
B#CLGREC EQU   5                         Client group record                    
B#TCMREC EQU   5                         Traffic commercial record              
B#TPHREC EQU   5                         Traffic prod house record              
B#TRCREC EQU   5                         Traffic ship recap record              
B#TCLREC EQU   5                         Traffic COMCLASS record                
B#CTXREC EQU   5                         Traffic COMTEXTS record                
B#PATREC EQU   5                         Traffic pattern record                 
B#CNTREC EQU   5                         Traffic contact record                 
B#FLTREC EQU   5                         Traffic flight record                  
B#MLSREC EQU   5                         Traffic market list record             
AMKGREC  EQU   AIO3                                                             
ACLGREC  EQU   AIO3                                                             
ATCMREC  EQU   AIO3                                                             
ATCLSREC EQU   AIO3                                                             
ACTXTREC EQU   AIO3                                                             
ACNTTREC EQU   AIO3                                                             
AFLGHREC EQU   AIO3                                                             
AMLSTREC EQU   AIO3                                                             
                                                                                
B#SECD   EQU   13                  SECD                                         
B#LP_D   EQU   14                  LP_D                                         
                                                                                
CODE     NMOD1 0,**SL10**,RR=RE                                                 
         DROP  RB                                                               
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
         L     R8,LP_ABLK2                                                      
         J     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(global w/s)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R8,WORKD+(((WORKL+2048+7)/8)*8)                                  
         USING SAVED,R8            R8=A(save w/s)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   GETUID,RGETUID      Set A(GETUID)                                
         MVC   RUNMODE,RUNPMODE    Extract DDLINK/RUNNER calling mode           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ABUFFRIN,RBUFFRIN                                                
         DROP  R6,R7                                                            
                                                                                
         ST    R5,ALP              Save A(LP_D)                                 
         ST    RE,SRVRRELO         Save program relocation factor               
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
                                                                                
         USING STABLKD,WORK                                                     
         USING STAPACKD,WORK                                                    
         USING CFMIOD,CFMIOC                                                    
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    Test first for run                           
         JNE   PRCWRK                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNSTR02                                                         
                                                                                
         L     RF,ACOMFACS         Load facilities overlays                     
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    Set A(index routines 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    Set A(index routines 2)                      
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#AGYREC-1)*L'LP_BLKS)(AIOLAST-AIO1),AIO1              
******   MVC   LP_BLKS+((B#CLTREC-1)*L'LP_BLKS)(AIOLAST-AIO2),AIO2              
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#LP_D-1)*L'LP_BLKS)(L'LP_BLKS),ALP                    
         MVC   LP_BLKS+((B#SECD-1)*L'LP_BLKS),LP_ASECD                          
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    Test 'process work' mode                     
         JNE   RUNREQ                                                           
                                                                                
*NOP     XC    REQVALS(REQVALL),REQVALS                                         
         LA    R0,REQVALS          CLEAR  storage                               
         LHI   R1,REQVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    PRVVALS(PRVVALL),PRVVALS                                         
         XC    SVRFPID,SVRFPID                                                  
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
                                                                                
         MVI   SYSLET,SYSSPTQ      Set spot system letter for CFM               
         XC    PRDRSTR,PRDRSTR     Product start range                          
         MVC   PRDREND,=X'FFFFFF'  Product end range                            
         MVI   QANYREQ,0           Init any request y/n switch                  
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run download request                                                *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
                                                                                
         MVC   AGENCY,LP_AGY       Set agency alpha id                          
         ICM   RF,15,AMASTC        Set trace option if off-line                 
         JZ    *+10                                                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNREQ02                                                         
         CLC   LP_QMAPN,=AL2(I#CFMCDL)                                          
         JE    RUNREQ02                                                         
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                               
         GOTOR (RF),(R1),DMKEY,SPTFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,GENDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,STAFIL,(4,0),0                                   
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
RUNREQ02 GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* MFM media download request                                          *         
***********************************************************************         
                                                                                
REQMED   LKREQ *,I#MFMMED,OUTMED,NEXTREQ=REQCLT                                 
                                                                                
OUTMED   LKOUT H                   ** MFM media download **                     
         LKOUT R,1                                                              
LimAc    LKOUT C,1,(D,B#LP_D,LP_ACCS),CHAR,ND=Y                                 
Array    LKOUT C,2,(A,ARYMED)                                                   
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* Array definition for MFM media download                             *         
***********************************************************************         
                                                                                
ARYMED   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,                           +        
               ROWID=(AGYMEDEL,AGYMEDEQ),ROWWIDTH=(V,AGYMEDLN)                  
                                                                                
MedCd    LKOUT C,2,AGYMEDCD,CHAR,FILTROUT=MEDFILT,SKIPCOLS=1                    
MedNm    LKOUT C,3,AGYMEDEX,CHAR                                                
                                                                                
         LKOUT E                                                                
                                                                                
MEDFILT  L     R1,AAGYREC                                                       
         CLI   AGYPCNDA-AGYHDR(R1),CANAGYQ                                      
         JNE   EXITY                                                            
         L     R1,LP_AINP                                                       
         CLI   AGYMEDCD-AGYMEDEL(R1),C'C'                                       
         JNE   EXITY               Drop Canadian combined media                 
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* MFM client download request                                         *         
***********************************************************************         
                                                                                
REQCLT   LKREQ *,I#MFMCLT,OUTCLT,NEXTREQ=REQPRD                                 
                                                                                
OUTCLT   LKOUT H                   ** MFM client download **                    
         LKOUT R,1                                                              
PRout    LKOUT P,,CLTINI           Build media look-up range                    
Array    LKOUT C,1,(A,ARYCLT)                                                   
         LKOUT X                                                                
                                                                                
CLTINI   L     R1,AAGYREC          Build media look-up range                    
         AHI   R1,AGYEL-AGYRECD                                                 
         USING AGYMEDEL,R1                                                      
CLTINI02 LLC   R0,AGYMEDLN                                                      
         AR    R1,R0                                                            
         CLI   AGYMEDEL,EOR                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   AGYMEDEL,AGYMEDEQ                                                
         JNE   CLTINI02                                                         
                                                                                
         L     RE,LP_AWMP                                                       
         USING LW_D,RE                                                          
         STCM  RE,7,AMED                                                        
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVC   LW_DATA1(L'AGYMEDBT),AGYMEDBT                                    
         NI    LW_DATA1,X'F0'                                                   
         MVC   LW_DATA1+L'AGYMEDBT(L'AGYMEDBT),AGYMEDBT                         
         NI    LW_DATA1+L'AGYMEDBT,X'F0'                                        
         OI    LW_DATA1+L'AGYMEDBT,X'07'  Media 1-7 for Canada                  
         L     R1,AAGYREC                                                       
         CLI   AGYPCNDA-AGYHDR(R1),CANAGYQ                                      
         JE    *+8                                                              
         OI    LW_DATA1+L'AGYMEDBT,X'0F'  Media 1-15 for USA                    
         AHI   RE,LQ_LN1Q+(L'AGYMEDBT*2)                                        
         ST    RE,LP_AWMP                                                       
         J     EXITY                                                            
         DROP  R1,RE                                                            
                                                                                
***********************************************************************         
* Array definition for MFM client download                            *         
***********************************************************************         
                                                                                
ARYCLT   LKOUT A,(R,NXTCLT),MULTIROW=Y,ROWNAME=CLTRECD                          
                                                                                
Media    LKOUT C,1,(D,B#SAVED,MEDCOD),CHAR,ND=Y                                 
CltCd    LKOUT C,2,(D,B#WORKD,QCLTA),CHAR                                       
CltNm    LKOUT C,3,(D,,CNAME),CHAR                                              
CltHx    LKOUT C,4,(D,,CKEYCLT),HEXD                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get client records for MFM client download                          *         
***********************************************************************         
                                                                                
NXTCLT   GOTOR (#NXTREC,ANXTREC),DMCB,CLTKEYT,('B#CLTREC',0),SAVED,0,  +        
               ('#LIMCLT',ALIMCLT)                                              
         JNE   EXITY                                                            
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         OC    CPLDATA(CPLDATAL),CPLDATA                                        
         JZ    NXTCLT                                                           
         MVC   SVIOVALS,IOVALS                                                  
         USING EKEY,IOKEY          Test any POL estimates exist                 
         XC    EKEY,EKEY                                                        
         MVC   EKEY,CKEY                                                        
         MVC   EKEYPRD,=C'POL'                                                  
         MVI   EKEYEST,1                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR'                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   EKEY(EKEYEST-EKEY),IOKEYSAV                                      
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         JNE   NXTCLT                                                           
         GOTOR (#EDTMED,AEDTMED),DMCB,CKEYAM,,MEDCOD                            
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   PMEDCOD,MEDCOD                                                   
         MVC   PMEDCOD,MEDCOD                                                   
         JNE   *+8                                                              
         MVI   MEDCOD,0            Only send media code once                    
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* MFM product download request                                        *         
***********************************************************************         
                                                                                
REQPRD   LKREQ H,I#MFMPRD,OUTPRD,NEXTREQ=REQVEN                                 
                                                                                
SysCd    LKREQ F,4,(I,B#SAVED,SMCIND),CHAR,OLEN=L'SMCTSYS,             +        
               TEXT=SP#SCODE,COL=*,ARRAY=S                                      
MedCd    LKREQ F,5,,CHAR,OLEN=L'SMCTMED,TEXT=SP#MED,COL=*                       
CltCd    LKREQ F,6,,CHAR,OLEN=L'SMCTCLT,TEXT=SP#CLI,COL=*,ARRAY=E               
                                                                                
         LKREQ E                                                                
                                                                                
SMCTABD  DSECT ,                   ** System/media/client table **              
SMCTSYS  DS    C                   System code                                  
SMCTMED  DS    C                   Media code                                   
SMCTCLT  DS    CL3                 Client code                                  
SMCTABL  EQU   *-SMCTABD                                                        
SVRDEF   CSECT ,                                                                
                                                                                
OUTPRD   LKOUT H                   ** MFM product download **                   
         LKOUT R,1                                                              
PRout    LKOUT P,,PRDINI           Build look-up list                           
Array    LKOUT C,1,(A,ARYPRD)                                                   
         LKOUT X                                                                
                                                                                
PRDINI   SR    R2,R2               Initialize for product download              
         ICM   R2,7,ASMC                                                        
         JZ    EXITN                                                            
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R2)                                            
         LA    R2,LW_DATA2-LW_D(R2)                                             
         USING SMCTABD,R2          R2=A(system/media/client list)               
         L     R7,LP_AWMP                                                       
         USING LW_D,R7                                                          
         LA    R3,LW_DATA2                                                      
         USING AMCTABD,R3          R3=A(agency/media/client list)               
         LA    R4,CLTTAB           R4=A(client conversion table)                
         SR    R5,R5               R5=N'entries in output table                 
                                                                                
PRDINI02 CLI   SMCTSYS,SPTLETQ     Test entry for Spot                          
         JNE   PRDINI04                                                         
         GOTOR (#VALMED,AVALMED),DMCB,SMCTMED,0,AMCTAGM                         
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR VCLPACK,DMCB,SMCTCLT,AMCTCLT                                     
         MVC   0(L'AMCTCLT,R4),AMCTCLT                                          
         MVC   L'AMCTCLT(L'SMCTCLT,R4),SMCTCLT                                  
         AHI   R4,L'CLTTAB         Bump client table pointer                    
         XC    0(L'CLTTAB,R4),0(R4)                                             
         AHI   R5,1                Bump N'output entries                        
         AHI   R3,AMCTABL          Bump output table pointer                    
                                                                                
PRDINI04 AHI   R2,SMCTABL          Bump input table pointer                     
         JCT   R0,PRDINI02         Do for number of entries                     
         LTR   R5,R5               Test any output entries created              
         JZ    PRDINI06                                                         
         GOTOR VXSORT,DMCB,LW_DATA2,(R5),AMCTABL,AMCTABL,0                      
                                                                                
PRDINI06 STCM  R7,7,AAMC           Set A(agency/media/client table)             
         STCM  R5,3,LW_NUMN                                                     
         MVI   LW_TYPE,LW_TLSTQ                                                 
         SR    R3,R7                                                            
         STCM  R3,3,LW_LN                                                       
         AR    R3,R7                                                            
         L     R5,ALP                                                           
         STCM  R3,15,LP_AWMP                                                    
         J     EXITY                                                            
         DROP  R2,R3,R7                                                         
                                                                                
AMCTABD  DSECT ,                   ** Agency/media/client table **              
AMCTAGM  DS    XL(L'CKEYAM)        Agency/media                                 
AMCTCLT  DS    XL(L'CKEYCLT)       Client code                                  
AMCTABL  EQU   *-AMCTABD                                                        
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Array definition for MFM product download                           *         
***********************************************************************         
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=PRDHDR                           
                                                                                
Media    LKOUT C,1,(D,B#SAVED,MEDCOD),CHAR,ND=Y                                 
CltCd    LKOUT C,2,(D,B#WORKD,QCLTA),CHAR,ND=Y                                  
PrdCd    LKOUT C,3,(D,,PKEYPRD),CHAR                                            
PrdNm    LKOUT C,4,(D,,PNAME),CHAR                                              
PrdHx    LKOUT C,5,(D,,PCODE+1),HEXD,LEN=L'PCODE-1                              
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get product records for MFM product download                        *         
***********************************************************************         
                                                                                
NXTPRD   GOTOR (#NXTREC,ANXTREC),DMCB,PRDKEYT,('B#PRDREC',0),SAVED,0,0          
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING PRDRECD,R2                                                       
         GOTOR (#EDTMED,AEDTMED),DMCB,PKEYAM,,MEDCOD                            
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   PMEDCOD,MEDCOD                                                   
         MVC   PMEDCOD,MEDCOD                                                   
         JNE   *+8                                                              
         MVI   MEDCOD,0            Only send media once                         
                                                                                
         LA    R1,CLTTAB           Look up client code in table                 
NXTPRD02 CLC   PKEYCLT,0(R1)                                                    
         JE    NXTPRD04                                                         
         OC    0(L'PKEYCLT,R1),0(R1)                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AHI   R1,L'CLTTAB                                                      
         J     NXTPRD02                                                         
                                                                                
NXTPRD04 MVC   QCLTA,L'PKEYCLT(R1)                                              
         CLC   PCLTCOD,QCLTA                                                    
         MVC   PCLTCOD,QCLTA                                                    
         JNE   *+10                                                             
         XC    QCLTA,QCLTA         Only send client code once                   
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* MFM vendor download request                                         *         
***********************************************************************         
                                                                                
REQVEN   LKREQ H,I#MFMVEN,OUTVEN,NEXTREQ=REQMKT                                 
                                                                                
SysCd    LKREQ F,1,(I,B#SAVED,SMCIND),CHAR,OLEN=L'SMDTSYS,             +        
               TEXT=SP#SCODE,COL=*,ARRAY=S                                      
MedCd    LKREQ F,2,,CHAR,OLEN=L'SMDTMED,TEXT=SP#MED,COL=*,ARRAY=E               
                                                                                
         LKREQ E                                                                
                                                                                
SMDTABD  DSECT ,                   ** System/media table **                     
SMDTSYS  DS    C                   System code                                  
SMDTMED  DS    C                   Media code                                   
SMDTABL  EQU   *-SMDTABD                                                        
SVRDEF   CSECT ,                                                                
                                                                                
OUTVEN   LKOUT H                   ** MFM vendor download **                    
                                                                                
VENSTA   LKOUT R,1                 Station records                              
PRout    LKOUT P,,VENINI           Build media table                            
Array    LKOUT C,1,(A,ARYVEN)                                                   
         LKOUT E                                                                
                                                                                
VENMKT   LKOUT R,11                Market records                               
Array    LKOUT C,11,(A,ARYMKT)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
VENINI   SR    R2,R2               Vendor download initialization               
         ICM   R2,7,ASMC                                                        
         JZ    EXITN                                                            
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R2)                                            
         LA    R2,LW_DATA2-LW_D(R2)                                             
         USING SMDTABD,R2          R2=A(system/media list)                      
         LA    R4,MEDIA            R4=A(media table)                            
         SR    RE,RE                                                            
VENINI02 CLI   SMDTSYS,SPTLETQ     Test entry for Spot                          
         JNE   VENINI04                                                         
         MVC   0(L'SMDTMED,R4),SMDTMED                                          
         AHI   R4,L'SMDTMED                                                     
         AHI   RE,1                                                             
VENINI04 AHI   R2,SMDTABL                                                       
         JCT   R0,VENINI02                                                      
         STCM  RE,3,MEDIA#                                                      
         LTR   RE,RE               Test any media filters found                 
         JZ    EXITN                                                            
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* MFM market download request                                         *         
***********************************************************************         
                                                                                
REQMKT   LKREQ H,I#MFMMKT,OUTMKT,NEXTREQ=REQMST                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,SMCIND),CHAR,OLEN=1,TEXT=SP#MED,LIST=Y,  +        
               COL=*                                                            
                                                                                
         LKREQ E                                                                
                                                                                
OUTMKT   LKOUT H                   ** MFM vendor download **                    
                                                                                
MKTSTA   LKOUT R,1                 Station records                              
PRout    LKOUT P,,MKTINI           Set media table                              
Array    LKOUT C,1,(A,ARYVEN)                                                   
         LKOUT E                                                                
                                                                                
MKTMKT   LKOUT R,11                Market records                               
Array    LKOUT C,11,(A,ARYMKT)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
MKTINI   SR    R2,R2               Market download initialization               
         ICM   R2,7,ASMC                                                        
         JZ    EXITY                                                            
         USING LW_D,R2                                                          
         LHI   R1,1                                                             
         LA    RF,LW_DATA1                                                      
         CLI   LW_TYPE,LW_TSINQ                                                 
         JE    *+12                                                             
         ICM   R1,3,LW_NUMN                                                     
         LA    RF,LW_DATA2                                                      
         DROP  R2                                                               
         STCM  R1,3,MEDIA#                                                      
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   MEDIA(0),0(RF)                                                   
         EX    R1,0(RE)                                                         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Array definition for MFM vendor download                            *         
***********************************************************************         
                                                                                
ARYVEN   LKOUT A,(R,NXTVEN),MULTIROW=Y,ROWNAME=STAREC                           
                                                                                
Media    LKOUT C,1,(D,,STAKMED),CHAR                                            
Statn    LKOUT C,2,(D,,STAKCALL),CHAR                                           
CblNt    LKOUT C,3,(D,,SSYSNAME),CHAR,ND=Y                                      
MktNo    LKOUT C,4,(D,,SMKT),CHAR                                               
Array    LKOUT C,5,(A,ARYNET)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get vendor records for MFM vendor download                          *         
***********************************************************************         
                                                                                
NXTVEN   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTVEN02                                                         
         L     R0,AIO3             Clear market reference tables                
         L     R1,AIO8                                                          
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
NXTVEN02 GOTOR (#NXTREC,ANXTREC),DMCB,STAKEYT,('B#STAREC',0),          +        
               (X'80',SAVED),0,0                                                
         JNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         USING STAREC,R2           R2=A(station record)                         
                                                                                
         CLC   STAKCLT,EZEROS      If default record                            
         JNE   *+10                                                             
         XC    STAKCLT,STAKCLT     Clear the client                             
                                                                                
         LA    R1,MEDIA            Locate market reference table                
         SR    R0,R0                                                            
         ICM   R0,3,MEDIA#                                                      
         LA    RE,AIO3                                                          
NXTVEN04 CLC   STAKMED,0(R1)                                                    
         JE    NXTVEN06                                                         
         AHI   RE,L'AIO3                                                        
         AHI   R1,L'STAKMED                                                     
         JCT   R0,NXTVEN04                                                      
         DC    H'0'                                                             
                                                                                
NXTVEN06 L     R0,0(RE)            R0=A(market reference table)                 
         PACK  DUB,SMKT                                                         
         CVB   RE,DUB                                                           
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RF,BITLIST(RF)                                                   
         AR    RE,R0                                                            
         OC    0(1,RE),0(RF)       Turn on market reference bit                 
         CLC   LP_QMAPN,MFMMKD#    Test download markets only                   
         JE    NXTVEN02            Yes - get next station                       
                                                                                
         XC    NETLIST#,NETLIST#   Clear cable list count                       
         LA    R3,NETLIST          R3=A(cable network list)                     
         TM    STAKCALL,X'F0'      Test cable headend                           
         JO    *+14                                                             
         XC    SSYSNAME,SSYSNAME   No - clear name (just in case)               
         J     EXITY                                                            
                                                                                
         L     RF,AAGYREC                                                       
         CLI   AGYPCNDA-AGYHDR(RF),CANAGYQ                                      
         JE    EXITY                                                            
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMED,STAKMED                                                  
                                                                                
         SR    R4,R4               Process top 24 list                          
         ICM   R4,B'1110',SCBL24                                                
         JZ    NXTVEN12                                                         
         LHI   R0,1                                                             
NXTVEN08 TMH   R4,X'8000'                                                       
         JZ    NXTVEN10                                                         
         XC    STAPMKST,STAPMKST                                                
         STC   R0,STAPMKST+L'STAPMKST-1                                         
         MVI   STAPMKST+2,C'0'                                                  
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,3,NETLIST#                                                    
         AHI   RE,1                                                             
         STCM  RE,3,NETLIST#                                                    
         MVC   0(L'NETLIST,R3),STAPQNET                                         
         AHI   R3,L'NETLIST                                                     
NXTVEN10 AHI   R0,1                Bump top 24 number                           
         SLL   R4,1                Shift off current test bit                   
         LTR   R4,R4               Test all done                                
         JNZ   NXTVEN08                                                         
                                                                                
NXTVEN12 MVI   STAPACT,C'P'                                                     
         MVC   STAPQMKT,EZEROS                                                  
         MVC   STAPQSTA,STAKCALL                                                
         XC    STAPQNET,STAPQNET                                                
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK2(L'STAPMKST),STAPMKST                                       
         MVI   STAPACT,C'U'                                                     
                                                                                
         LA    R4,SCBLSEQ          Process other cable list                     
         LHI   R0,25                                                            
NXTVEN14 OC    0(2,R4),0(R4)       Test end of list                             
         JZ    EXITY                                                            
         MVC   STAPMKST,WORK2                                                   
         STC   R0,BYTE1                                                         
         OC    STAPMKST+L'STAPMKST-1(1),BYTE1                                   
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JNE   NXTVEN16                                                         
         ICM   RE,3,NETLIST#                                                    
         AHI   RE,1                                                             
         STCM  RE,3,NETLIST#                                                    
         MVC   0(L'NETLIST,R3),STAPQNET                                         
         AHI   R3,L'NETLIST                                                     
NXTVEN16 AHI   R4,2                                                             
         AHI   R0,1                                                             
         J     NXTVEN14                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Array definition for cable network list download                    *         
***********************************************************************         
                                                                                
ARYNET   LKOUT A,(D,B#SAVED,NETLIST),NROWS=(B#SAVED,NETLIST#),         +        
               ROWWIDTH=L'NETLIST                                               
                                                                                
CblCh    LKOUT C,5,(D,,NETLIST),CHAR                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for MFM market download                            *         
***********************************************************************         
                                                                                
ARYMKT   LKOUT A,(R,NXTMKT),MULTIROW=Y,ROWNAME=MKTREC                           
                                                                                
Media    LKOUT C,1,(D,,MKTKMED),CHAR                                            
MktNo    LKOUT C,2,(D,,MKTKMKT),CHAR                                            
MktNm    LKOUT C,3,(D,,MKTNAME),CHAR                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get market records for MFM vendor download                          *         
***********************************************************************         
                                                                                
NXTMKT   MVC   LP_ADATA,AIO2                                                    
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTMKT02                                                         
         XC    MARKET#,MARKET#                                                  
         SR    RE,RE                                                            
         MVI   MARKET$,1                                                        
         J     NXTMKT04                                                         
                                                                                
NXTMKT02 SR    RE,RE               Bump marker number                           
         ICM   RE,3,MARKET#                                                     
         AHI   RE,1                                                             
         STCM  RE,3,MARKET#                                                     
         CHI   RE,9999             Test all done for this media                 
         JH    NXTMKT06                                                         
                                                                                
NXTMKT04 ST    RE,DUB              Save marker number                           
         LLC   RF,MARKET$                                                       
         SLL   RF,2                                                             
         L     R0,AIO3-L'AIO3(RF)  R0=A(market referecne table)                 
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         IC    RF,BITLIST(RF)                                                   
         LR    R1,RE                                                            
         AR    R1,R0                                                            
         BASR  RE,0                                                             
         EX    RF,8(RE)            Test market reference bit on                 
         JZ    NXTMKT02                                                         
         TM    0(R1),0                                                          
         LLC   RE,MARKET$          Point to media code                          
         LA    RE,MEDIA-L'MKTKMED(RE)                                           
         L     R0,DUB              Restore saved marker number                  
                                                                                
         LA    R1,IOKEY                                                         
         USING MKTREC,R1           Read market record                           
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,0(RE)                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,LP_AGY                                                   
         MVC   MKTKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO2'                            
         JE    EXITY                                                            
         L     R1,AIO2                                                          
         MVC   MKTKEY,IOKEYSAV                                                  
         MVI   MKTNAME,C'?'                                                     
         MVC   MKTNAME+1(L'MKTNAME-1),MKTNAME                                   
         J     EXITY                                                            
                                                                                
NXTMKT06 LLC   RF,MARKET$                                                       
         AHI   RF,1                                                             
         STC   RF,MARKET$                                                       
         XC    MARKET#,MARKET#                                                  
         SR    RE,RE                                                            
         CLM   RF,3,MEDIA#                                                      
         JNH   NXTMKT04                                                         
         J     NOMORE                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* MFM M-Street download request                                       *         
***********************************************************************         
                                                                                
REQMST   LKREQ H,I#MFMVEN,OUTMST,NEXTREQ=REQVAN                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,MEDIND),CHAR,OLEN=1,TEXT=SP#MED,LIST=Y,  +        
               COL=*                                                            
                                                                                
         LKREQ E                                                                
                                                                                
OUTMST   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* MediaVantage download request                                       *         
***********************************************************************         
                                                                                
REQVAN   LKREQ H,I#MVANDL,OUTVAN,NEXTREQ=REQCFMI                                
                                                                                
MNode    LKREQ F,21,(I,B#SAVED,MEDIND),LBIN,OLEN=4,LIST=F,             +        
               TEXT=SP#MCONT,COL=*                                              
ANode    LKREQ F,22,(I,B#SAVED,ADVIND),LBIN,OLEN=4,LIST=F,             +        
               TEXT=SP#ACONT,COL=*                                              
SNode    LKREQ F,23,(I,B#SAVED,SUPNODE),LBIN,TEXT=SP#SCONT,COL=*,      +        
               LIST=F,OLEN=4                                                    
SDate    LKREQ F,24,(D,B#WORKD,QSTRDATE),EDAT,TEXT=SP#STDT,COL=*                
EDate    LKREQ F,25,(D,B#WORKD,QENDDATE),EDAT,TEXT=SP#ENDT,COL=*                
CalOp    LKREQ F,26,(D,B#SAVED,CALOPTN),CHAR,TEXT=SP#CALOP,COL=*                
Split    LKREQ F,27,(D,B#SAVED,SPLOPTN),CHAR,TEXT=SP#SPLOP,COL=*                
Sumry    LKREQ F,28,(D,B#SAVED,SUMOPTN),CHAR,TEXT=SP#SUMRZ,COL=*                
PrgNm    LKREQ F,29,(D,B#SAVED,PNTOPTN),CHAR,TEXT=SP#PRGNM,COL=*                
BrdNd    LKREQ F,30,(I,B#SAVED,CLTIND),LBIN,OLEN=4,ARRAY=S,            +        
               TEXT=SP#BCONT,COL=*                                              
CltCd    LKREQ F,31,,CHAR,OLEN=L'QCLTA,TEXT=SP#CLI,COL=*                        
BrdCd    LKREQ F,32,,CHAR,OLEN=L'CPLPMNEM,TEXT=SP#PRO,COL=*,ARRAY=E             
EstOp    LKREQ F,33,(D,B#SAVED,ESTOPTN),CHAR,TEXT=SP#ESTOP,COL=*                
EsnOp    LKREQ F,34,(D,B#SAVED,ESNOPTN),CHAR,TEXT=(*,ESNRLIT),COL=*             
                                                                                
         LKREQ E                                                                
                                                                                
ESNRLIT  DC    C'Estimate names required?'                                      
                                                                                
OUTVAN   LKOUT H                   ** MediaVantage download **                  
         LKOUT R,1                                                              
PRout    LKOUT P,,VANINI                                                        
Array    LKOUT C,1,(A,ARYMVAN)                                                  
         LKOUT X                                                                
                                                                                
VANINI   OC    ENDDATEB,ENDDATEB   Set end dates to high values                 
         JNZ   *+10                if not input                                 
         MVC   ENDDATEB,EFFS                                                    
         OC    ENDDATEC,ENDDATEC                                                
         JNZ   *+10                                                             
         MVC   ENDDATEC,EFFS                                                    
         OC    QENDDATE,QENDDATE                                                
         JNZ   *+10                                                             
         MVC   QENDDATE,EFFS                                                    
                                                                                
         OC    QSTRDATE,QSTRDATE   Set start dates                              
         JZ    VANINI02                                                         
         GOTOR VDATCON,DMCB,QSTRDATE,(3,STRDATEB)                               
         GOTOR (RF),(R1),QSTRDATE,(2,STRDATEC)                                  
         GOTOR VGETDAY,DMCB,QSTRDATE,WORK                                       
         MVC   STRDMONC,STRDATEC                                                
         CLI   0(R1),1             Test start date is a Monday                  
         JE    VANINI02                                                         
         SR    R0,R0               No - get date of previous Monday             
         ICM   R0,1,0(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   R0,1                                                             
         LNR   R0,R0                                                            
         GOTOR VADDAY,DMCB,QSTRDATE,WORK,(R0)                                   
         GOTOR VDATCON,DMCB,WORK,(2,STRDMONC)                                   
                                                                                
VANINI02 CLC   QENDDATE,EFFS       Set end dates                                
         JE    VANINI04                                                         
         GOTOR VDATCON,DMCB,QENDDATE,(3,ENDDATEB)                               
         GOTOR (RF),(R1),QENDDATE,(2,ENDDATEC)                                  
                                                                                
VANINI04 LA    R0,CFMIOD           Build CFMIO control block                    
         LHI   R1,CFMIOL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   CFMSYS,SPTLETQ                                                   
         MVC   CFMABFIN,ABUFFRIN                                                
         MVC   CFMATSAR,VTSAR                                                   
         MVC   CFMAGRAT,VGETRATE                                                
         MVC   CFMAGBRD,VGETBRD                                                 
         MVC   CFMACOM,ACOMFACS                                                 
         MVC   CFMAIO,AIO5                                                      
         MVC   CFMSUMOP,SUMOPTN                                                 
         MVC   CFMCALOP,CALOPTN                                                 
         MVC   CFMSPLOP,SPLOPTN                                                 
         MVC   CFMPNTOP,PNTOPTN                                                 
         MVC   CFMESTOP,ESTOPTN                                                 
         MVC   CFMEDTOP,ESNOPTN                                                 
         LARL  RE,CNVMED                                                        
         STCM  RE,15,CFMACNVM                                                   
         LARL  RE,VALCLT                                                        
         STCM  RE,15,CFMAVALC                                                   
         LARL  RE,VALVEN                                                        
         STCM  RE,15,CFMAVALV                                                   
         MVC   CFMALP,ALP                                                       
         MVC   CFMAMED,MEDIND                                                   
         MVC   CFMADVN,ADVIND                                                   
         MVC   CFMVENN,SUPNODE                                                  
         MVC   CFMSTDTC,STRDATEC                                                
         MVC   CFMENDTC,ENDDATEC                                                
         MVC   CFMSTDTB,STRDATEB                                                
         MVC   CFMENDTB,ENDDATEB                                                
         MVC   CFMACBA,CLTIND                                                   
         MVI   CFMACTN,CFMAINI                                                  
         GOTOR VCFMIO,CFMIOD       Call CFMIO to initialize                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Convert media letter to agency/media code for CFMIO                 *         
***********************************************************************         
                                                                                
CNVMED   LM    RF,R0,0(R1)         RF=A(input),R0=A(output)                     
         GOTOR (#VALMED,AVALMED),DMCB,(RF),0,(R0)                               
         J     EXITCC              Exit with CC intact                          
                                                                                
***********************************************************************         
* Validate client for MediaVantage download                           *         
***********************************************************************         
                                                                                
VALCLT   LR    R2,R1                                                            
         L     R1,0(R2)                                                         
         MVC   QMEDX,0(R1)                                                      
         MVC   QCLTX,1(R1)                                                      
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   EXITCC                                                           
         MVI   0(R2),0                                                          
         L     RF,ACLTREC                                                       
         CLI   CPROF-CLTRECD(RF),C'0'                                           
         JNE   *+8                                                              
         OI    0(R2),X'80'         Set true pool indicator                      
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate vendor for MediaVantage download                           *         
***********************************************************************         
                                                                                
VALVEN   LR    R2,R1               R2=A(parameter list)                         
         L     R3,0(R2)            R3=A(input string)                           
         XC    STABLKD(STBLNQ),STABLKD                                          
         MVC   WORK2(20),SPACES                                                 
         MVC   WORK2(L'STBSTA),L'STAPQMKT(R3)                                   
         CLI   L'STAPQMKT+L'STBSTA(R3),C' '                                     
         JNH   *+14                                                             
         MVI   WORK+L'STBSTA-1,C'/'                                             
         MVC   WORK+L'STBSTA(L'STBNET),L'STAPQMKT+L'STBSTA(R3)                  
         LA    R0,WORK2                                                         
         ST    R0,STBADDR                                                       
         OI    STBADDR,X'C0'                                                    
         MVC   STBMED,0(R2)                                                     
         L     RF,AAGYREC                                                       
         MVC   STBCTRY,AGYPCNDA-AGYHDR(RF)                                      
         MVC   STBACOM,ACOMFACS                                                 
         MVC   STBSTRAD,VCABLETB                                                
         GOTOR VSTAVAL,DMCB,STABLKD                                             
         CLI   STBERR,0            Test valid station                           
         JNE   EXITN                                                            
         MVC   WORK2(L'STBSTA+L'STBNET),STBSTA                                  
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,0(R2)                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,0(R3)      Set market number                            
         MVC   STAPQSTA(STASTAL),WORK2                                          
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JNE   EXITN                                                            
         L     R3,4(R2)            R3=A(output area)                            
         MVC   0(L'STAPMKT+L'STAPSTA,R3),STAPMKT                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Array definition for MediaVantage download                          *         
***********************************************************************         
                                                                                
ARYMVAN  LKOUT A,(R,NXTMVAN),MULTIROW=Y,ROWNAME=SPVALUES                        
                                                                                
Media    LKOUT C,1,(D,,SPMEDCDE),CHAR,ND=Y                                      
CliCode  LKOUT C,2,(D,,SPCLTCDE),CHAR,ND=Y                                      
CliNode  LKOUT C,3,(D,,SPCLTNDE),UBIN,ND=Y                                      
Mkt/Sta  LKOUT C,4,(D,,SPMKTSTA),(R,EDTSTM),                           +        
               LEN=L'STAPQMKT+L'STAPQSTA+L'STAPQNET,ND=Y                        
VendorNd LKOUT C,5,(D,,SPVENNDE),UBIN,ND=Y                                      
PrdCode  LKOUT C,6,(D,,SPPRDCDE),(U,#EDTPRD,$EDTPRD),ND=Y                       
PrdNode  LKOUT C,7,(D,,SPPRDNDE),UBIN,ND=Y                                      
Period   LKOUT C,8,(D,,SPDATPER),CDAT,ND=Y                                      
Rotation LKOUT C,9,(D,,SPWKYROT),UBIN,ND=Y                                      
SpotLen  LKOUT C,10,(D,,SPSPTLEN),UBIN,ND=Y                                     
ProgName LKOUT C,11,(D,,SPPRGNAM),CHAR,ND=Y                                     
ProgTime LKOUT C,12,(D,,SPPRGTIM),(R,EDTTIM),LEN=11,ND=Y                        
EstNum   LKOUT C,17,(D,,SPESTNUM),UBIN,ND=Y                                     
EstDesc  LKOUT C,18,(D,,SPEDESC),CHAR,ND=Y                                      
Spots    LKOUT C,13,(D,,SPSPTNUM),UBIN,ND=Y                                     
ZeroSpts LKOUT C,14,(D,,SPZSPOTS),UBIN,ND=Y                                     
NetValue LKOUT C,15,(D,,SPNET),SPAK                                             
GrsValue LKOUT C,16,(D,,SPGROSS),SPAK                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Edit market/station code for MediaVantage download                  *         
***********************************************************************         
                                                                                
EDTSTM   L     R1,LP_AINP                                                       
         OC    0(L'STAPMKST,R1),0(R1)                                           
         JZ    XCOLEN                                                           
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,BUYMED                                                   
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,0(R1)                                                   
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPMED,RADMEDQ     Test radio                                   
         JE    EDTSTM02                                                         
                                                                                
         CLC   STAPQNET,SPACES     Test cable channel set                       
         JE    *+8                                                              
         MVI   STAPQSTA+L'STAPQSTA-1,C'/'                                       
         J     EDTSTM04                                                         
                                                                                
EDTSTM02 LLC   R0,STAPQSTA+L'STAPQSTA-1                                         
         LA    RE,STAPQSTA+L'STAPQSTA-2                                         
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
         STC   R0,2(RE)                                                         
         MVI   3(RE),C' '                                                       
                                                                                
EDTSTM04 L     R1,LP_AOUT                                                       
         MVC   0(L'STAPQMKT+L'STAPQSTA+L'STAPQNET,R1),STAPQMKT                  
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit time value for MediaVantage download                           *         
***********************************************************************         
                                                                                
EDTTIM   L     RF,LP_AINP                                                       
         OC    0(4,RF),0(RF)       Test time present                            
         JZ    XCOLEN                                                           
         GOTOR VUNTIME,DMCB,(RF),LP_AOUT                                        
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Call CFMIO to get next MediaVantage download record                 *         
***********************************************************************         
                                                                                
NXTMVAN  CLI   LP_RMODE,LP_RFRST   Test first time call                         
         JNE   NXTMVAN2                                                         
         CLI   CFMERR,0            If there were initialization errors          
         JNE   NOMORE              then we have nothing to do                   
                                                                                
NXTMVAN2 LA    R0,SPVALUES                                                      
         ST    R0,LP_ADATA                                                      
         GOTOR VCFMIO,CFMIOD       Call CFMIO to build next record              
         JNE   EXITY                                                            
         CLI   SPMEDCDE,0          Test media code present                      
         JE    *+10                                                             
         MVC   BUYMED,SPMEDCDE     Yes - set for internal use                   
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CFM initial download (X'FFF1')                                      *         
***********************************************************************         
                                                                                
REQCFMI  LKREQ *,I#CFMIDL,OUTCFMI,NEXTREQ=REQCFMC                               
                                                                                
OUTCFMI  LKOUT H                                                                
CFMI     LKOUT R,X'0001'                                                        
LimAcs   LKOUT C,001,(D,B#LP_D,LP_ACCS),HEXD                                    
UserId   LKOUT C,002,(D,B#LP_D,LP_USRID),HEXD                                   
Psplnk12 LKOUT C,003,(D,B#SECD,SECOPASS),HEXD                                   
AccGrp   LKOUT C,004,(D,B#SECD,SECOSAGN),HEXD                                   
PerAgy   LKOUT C,005,(D,B#SECD,SECOAGPE),CHAR                                   
SecSys   LKOUT C,006,(D,B#SECD,SECOSYS),HEXD                                    
SecPrg   LKOUT C,007,(D,B#SECD,SECOPRG),HEXD                                    
SysLet   LKOUT C,008,(D,B#SAVED,SYSLET),CHAR                                    
         LKOUT E                                                                
                                                                                
AGYR     LKOUT R,X'0021'                                                        
Array    LKOUT C,10,(A,ARYAGY)                                                  
Array    LKOUT C,40,(A,ARYAMD)                                                  
Array    LKOUT C,60,(A,ARYAAG)                                                  
Array    LKOUT C,70,(A,ARYEXT)                                                  
         LKOUT E                                                                
                                                                                
OFC2     LKOUT R,X'0022'                                                        
Array    LKOUT C,X'0022',(A,ARY2OF)                                             
         LKOUT E                                                                
                                                                                
OFC1     LKOUT R,X'0022'                                                        
Array    LKOUT C,X'0022',(A,ARY1OF),FILTROUT=TST1OF                             
         LKOUT E                                                                
                                                                                
RLPG     LKOUT R,X'0024'                                                        
Array    LKOUT C,X'0024',(A,ARYRLP)                                             
         LKOUT E                                                                
                                                                                
SDREC    LKOUT R,X'0026'                                                        
PRout    LKOUT P,,GETSDR                                                        
Array    LKOUT C,X'0026',(A,ARYSDR),PCVERSION=1.0.0.5                           
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYAGY   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,ROWID=(AGYEL,AGYELQ),      +        
               ROWWIDTH=(V,AGYELEN)                                             
         LKOUT C,010,AGYPRATS,CHAR                                              
         LKOUT C,011,AGYPCLIR,CHAR                                              
         LKOUT C,012,AGYPBPCT,CHAR                                              
         LKOUT C,013,AGYPEDEM,CHAR                                              
         LKOUT C,014,AGYPBOTO,CHAR                                              
         LKOUT C,015,AGYPSAUT,CHAR                                              
         LKOUT C,016,AGYPCNDA,CHAR                                              
         LKOUT C,017,AGYPOPTS,CHAR                                              
         LKOUT C,018,AGYPBREQ,CHAR                                              
         LKOUT C,019,AGYPCBLM,CHAR                                              
         LKOUT C,020,AGYPBPLR,CHAR                                              
         LKOUT C,021,AGYPMGMM,CHAR                                              
         LKOUT C,022,AGYPOREQ,CHAR                                              
         LKOUT C,023,AGYPBYBR,CHAR                                              
         LKOUT C,024,AGYPSOCD,CHAR                                              
         LKOUT C,025,AGYPBLNG,CHAR                                              
         LKOUT C,026,AGYPALPH,CHAR                                              
         LKOUT C,027,AGYPAHEX,CHAR                                              
         LKOUT C,028,AGYOFC2,CHAR                                               
                                                                                
PRout    LKOUT P,AGYFLAG1,SETBITS                                               
         LKOUT C,030,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,031,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,032,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,033,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,034,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,035,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,036,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,037,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PRout    LKOUT P,AGYFLAG2,SETBITS                                               
         LKOUT C,050,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,051,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,052,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,053,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,054,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,055,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,056,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,057,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYAMD   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,ROWID=(AGYMEDEL,AGYMEDEQ), +        
               ROWWIDTH=(V,AGYMEDLN)                                            
MEDCD    LKOUT C,040,AGYMEDCD,CHAR,FILTROUT=TSTNWK,SKIPCOLS=2                   
MedNm    LKOUT C,041,AGYMEDEX,CHAR                                              
Vendr    LKOUT C,042,AGYVENEX,CHAR                                              
         LKOUT E                                                                
                                                                                
ARYAAG   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,ROWID=(AGYACCEL,AGYACCEQ), +        
               ROWWIDTH=(V,AGYACCLN)                                            
AAgyL    LKOUT C,060,AGYACCAG,CHAR,LEN=V                                        
         LKOUT E                                                                
                                                                                
ARYEXT   LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,ROWID=(AGYEXTEL,AGYEXTEQ), +        
               ROWWIDTH=(V,AGYEXTLN)                                            
RFPId    LKOUT C,070,AGYPRNID,HEXD,ND=Y,FILTROUT=TSTRFP                         
         LKOUT E                                                                
                                                                                
ARY2OF   LKOUT A,(R,NXT2OF),MULTIROW=Y,ROWNAME=MOFRECD                          
OFCAlph  LKOUT C,010,MOFFC2OF,(R,EDTOFC)                                        
Array    LKOUT C,011,(A,ARYOFNM)                                                
         LKOUT E                                                                
                                                                                
ARY1OF   LKOUT A,(R,NXT1OF),MULTIROW=Y,ROWNAME=MOFRECD                          
Array    LKOUT C,010,(D,B#WORKD,BYTE1),CHAR                                     
         LKOUT E                                                                
                                                                                
ARYOFNM  LKOUT A,(D,B#OFC,MOFFSYS+L'MOFFSYS),EOT=EOR,                  +        
               ROWID=(MONAMEL,MONAMELQ),ROWWIDTH=(V,MONAMLN)                    
OFCNmSh  LKOUT C,011,MONAMSH,CHAR                                               
OFCNMLg  LKOUT C,012,MONAMLO,CHAR                                               
         LKOUT E                                                                
                                                                                
EDTOFC   LM    R2,R4,LP_AINP                                                    
         MVC   0(L'MOFK2OF,R4),0(R2)                                            
         LHI   R0,2                                                             
         CLI   1(R2),C' '          1 character office code?                     
         JH    SETOLENX                                                         
         LA    R1,ONEOFCTB         Point to 1 char office code table            
EDTOFC4  CLI   0(R1),X'FF'         End of table?                                
         JE    SETOLENX                                                         
         CLC   0(1,R1),0(R2)       Match 2 chars office code from rec?          
         JNE   *+12                                                             
         MVI   0(R1),0             Blank it out                                 
         J     SETOLENX                                                         
         LA    R1,1(R1)            next entry in table                          
         J     EDTOFC4                                                          
                                                                                
TST1OF   CLI   QONEOFC,YESQ        Test ONE character office code               
         BR    RE                                                               
                                                                                
TSTRFP   L     RF,LP_AINP                                                       
         MVC   SVRFPID,AGYPRNID-AGYEXTEL(RF)  Copy the RFP UserID #             
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
TSTNWK   L     RF,LP_AINP                                                       
         CLI   AGYMEDCD-AGYMEDEL(RF),C'N'                                       
         JE    SETCCC                                                           
         CLI   AGYMEDCD-AGYMEDEL(RF),C'C'                                       
         J     SETCCC                                                           
                                                                                
***********************************************************************         
* Read office records                                                 *         
***********************************************************************         
                                                                                
NXT2OF   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXT2OF10                                                         
                                                                                
         MVI   QONEOFC,NOQ         Set ONE char office code to NO               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING CT5KEY,RE           Read access record                           
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO1                                                          
         AHI   RE,CT5DATA-CT5REC                                                
         USING CTAADD,RE           Locate Agency Access Details                 
         SR    R0,R0                                                            
NXT2OF02 CLI   CTAADEL,0           Test end of record                           
         JE    NXT2OF03                                                         
         CLI   CTAADEL,CTAADELQ    Agency Access Details element?               
         JE    *+14                                                             
         IC    R0,CTAADLEN         No - bump to next                            
         AR    RE,R0                                                            
         J     NXT2OF02                                                         
         TM    CTAADFLG,CTAAD2OF   Agy using 2 chars media offices?             
         JNZ   *+12                                                             
NXT2OF03 MVI   QONEOFC,YESQ        Set ONE char office code to YES              
         J     NOMORE                                                           
         DROP  RE                                                               
                                                                                
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         L     RE,AIO1                                                          
         ST    RE,AOFCREC                                                       
         LA    RE,IOKEY                                                         
         USING MOFKEY,RE                                                        
         MVI   MOFKTYP,MOFKTYPQ                                                 
         MVI   MOFKSUB,MOFKS2Q                                                  
         MVC   MOFKAGY,AGENCY                                                   
         DROP  RE                                                               
NXT2OF05 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGENDIR+IO1'                            
         JNE   NOMORE                                                           
NXT2OF07 LA    RE,IOKEY                                                         
         USING MOFKEY,RE                                                        
         CLI   MOFKTYP,MOFKTYPQ    Office limit access record?                  
         JNE   NOMORE                                                           
         CLC   MOFKAGY,AGENCY      Same request agency?                         
         JNE   NOMORE                                                           
         CLI   MOFKSUB,MOFKS2Q     Two bytes office code?                       
         JNE   *+12                                                             
         CLI   MOFKSYS,SPTSYSQ     Spot system?                                 
         JE    NXT2OF20                                                         
         DROP  RE                                                               
NXT2OF10 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOGENDIR+IO1'                            
         JE    NXT2OF07                                                         
         J     NOMORE                                                           
NXT2OF20 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGENFIL+IO1'                           
         JNE   NOMORE                                                           
         MVC   LP_ADATA,AOFCREC    Point to office record                       
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Process one character office code table                             *         
***********************************************************************         
                                                                                
NXT1OF   MVI   BYTE1,0                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXT1OF10                                                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(ONEOFCLN),ONEOFCTB                                          
NXT1OF10 LA    RE,ELEM                                                          
NXT1OF15 CLI   0(RE),X'FF'         End of table?                                
         JE    NOMORE                                                           
         CLI   0(RE),0             Blank entry?                                 
         JNE   *+12                                                             
         LA    RE,1(RE)                                                         
         J     NXT1OF15                                                         
         MVC   BYTE1,0(RE)                                                      
         MVI   0(RE),0             Clear it                                     
         MVC   LP_ADATA,AOFCREC    Point to office record                       
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SPOT RLP GROUPS                                                               
***********************************************************************         
ARYRLP   LKOUT A,(R,NXTRLP),MULTIROW=Y,ROWNAME=GRPKEYD                          
RLPGroup LKOUT C,010,GRPKGRP,CHAR,ND=Y                                          
Array    LKOUT C,011,(A,ARYDESC)                                                
         LKOUT E                                                                
                                                                                
ARYDESC  LKOUT A,(D,B#RLP,GRPFSTEL),EOT=EOR,ROWID=(GRPHCD,GRPHCDQ),    +        
               ROWWIDTH=(V,GRPHLN)                                              
RLPDesc  LKOUT C,011,GRPHDESC,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* READ RLP GROUPS                                                               
***********************************************************************         
                                                                                
NXTRLP   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTRLP50                                                         
                                                                                
         XC    PRLPGRP,PRLPGRP     Haven't had a previous RLP group yet         
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING GRPKEY,RE                                                        
         MVI   GRPKSYS,GRPKSYSQ    x'00'                                        
         MVI   GRPKSTYP,GRPKSTYQ   x'2F'                                        
         MVI   GRPKSYST,C'S'       Spot                                         
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKUSER,SVRFPID    UserID number                                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGENDIR+IO2'                            
         JNE   NOMORE                                                           
*                                                                               
NXTRLP10 LA    RE,IOKEY                                                         
         USING GRPKEY,RE                                                        
         CLC   IOKEY(GRPKGRP-GRPKEY),IOKEYSAV                                   
         JNE   NOMORE                                                           
         OC    PRLPGRP,PRLPGRP                                                  
         JZ    NXTRLP20                                                         
         CLC   PRLPGRP,GRPKGRP     Same as previous group? (MINIO)              
         JE    NXTRLP50            Yes, skip it as we want a new one            
*                                                                               
NXTRLP20 MVC   PRLPGRP,GRPKGRP     We have a previous RLP group now             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGENFIL+IO2'                           
         JNE   NOMORE                                                           
         MVC   LP_ADATA,ARLPREC    POINT TO RLP RECORD                          
         J     EXITY                                                            
*                                                                               
NXTRLP50 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOGENDIR+IO2'                            
         JE    NXTRLP10                                                         
         J     NOMORE                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CALL DDLINK TO GET SELF-DEFINING RECORD                             *         
*                                                                               
* NOTE:  AIO3 WILL BE CLOBBERED TO READ THE GENERIC SDR                         
*        AIO5 WILL BE CLOBBERED TO READ THE AGENCY SPECIFIC SDR                 
***********************************************************************         
*                                                                               
GETSDR   GOTOR LP_AGSDR,DMCB,LP_D,AIO3,0                                        
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
         L     RE,AIO3               NEED THIS CHECK AS GETSDR DOES A           
         L     RF,AIO5                 DEFAULT READ BY CLEARING SDRKFFL         
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDR10                                                          
*                                                                               
         BRAS  RE,SDROVRW                                                       
*****                                                                           
* ENVIROMENT SDR OVERRIDE                                                       
*****                                                                           
GTSDR10  L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)  CLEAR OUT KEY IN IOAREA                    
*                                                                               
         CLI   ENVIRO,0              DO WE HAVE AN ENVIROMENT OVERRIDE?         
         JE    GTSDRX                NO                                         
         XC    WORK,WORK             READ INTO AIO5                             
         MVC   WORK+2(L'ENVIRO),ENVIRO                                          
         GOTOR LP_AGSDR,DMCB,LP_D,AIO5,WORK                                     
*                                                                               
         L     RE,AIO3               NEED THIS CHECK AS GETSDR DOES A           
         L     RF,AIO5                 DEFAULT READ BY CLEARING SDRKFFL         
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDRX                                                           
*                                                                               
         BRAS  RE,SDROVRW                                                       
*                                                                               
GTSDRX   J     EXITCC                                                           
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
         L     R6,AIO3                                                          
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
SDROV030 GOTO1 VRECUP,DMCB,(X'FE',AIO3),0(R6),0(R6),=X'002A002007D0'            
*                                                                               
* ADD ELEM IN GENERIC                                                           
SDROV040 GOTO1 VRECUP,DMCB,(X'FE',AIO3),0(R2),0(R6),=X'002A002007D0'            
*                                                                               
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     SDROV010                                                         
*                                                                               
SDROVX   J     EXITCC                                                           
                                                                                
***********************************************************************         
* Send Self-Defining Elements                                         *         
***********************************************************************         
                                                                                
ARYSDR   LKOUT A,(D,B#SDRREC,SDRRFRST),EOT=EOR,ROWWIDTH=(V,SDELEN),    +        
               ROWID=(SDELD,0)                                                  
                                                                                
***********************************************************************         
* FIELDS SENT FROM SELF-DEFINING RECORD ARE AS FOLLOWS:-              *         
*                                                                     *         
*        10    Agency uses Midas Feature (cannot be Pinergy)          *         
*        11    Agency uses Pinergy Feature (cannot be Midas)          *         
*        20    Maximum number of product records allowed              *         
***********************************************************************         
                                                                                
Defvl    LKOUT C,255,SDELD,SDEL                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* CFM client download (X'FFF2')                                       *         
***********************************************************************         
                                                                                
REQCFMC  LKREQ H,I#CFMCDL,OUTCFMC,NEXTREQ=REQMKTG                               
                                                                                
***********************************************************************         
* Agency connection array                                             *         
***********************************************************************         
                                                                                
System   LKREQ F,01,(I,B#SAVED,QAGYIND),CHAR,TEXT=(*,SYS1LIT),         +        
               OLEN=L'AA_SYS,ARRAY=S                                            
Agency   LKREQ F,02,,CHAR,TEXT=(*,AGY1LIT),OLEN=L'AA_OAGY                       
LIMACCS  LKREQ F,03,,HEXD,TEXT=(*,LIMALIT),OLEN=L'AA_ACCS                       
UserId#  LKREQ F,04,,HEXD,TEXT=(*,USIDLIT),OLEN=L'AA_USID                       
PID#     LKREQ F,05,,HEXD,TEXT=(*,PIDNLIT),OLEN=L'AA_PASS                       
SAGRP    LKREQ F,06,,HEXD,TEXT=(*,SAGRLIT),OLEN=L'AA_SAGN                       
PSAGY    LKREQ F,07,,CHAR,TEXT=(*,PSAGLIT),OLEN=L'AA_AGPE                       
SSYSM    LKREQ F,08,,HEXD,TEXT=(*,SSYSLIT),OLEN=L'AA_OSYS                       
SPRGM    LKREQ F,09,,HEXD,TEXT=(*,SPRGLIT),OLEN=L'AA_OPRG,ARRAY=E               
                                                                                
AA_D     DSECT ,                   ** Agency connection array **                
AA_SYS   DS    C                   System letter                                
AA_OAGY  DS    CL(L'SECOAGY)       Agency alpha id                              
AA_ACCS  DS    CL(L'LP_ACCS)       Limit access value                           
AA_USID  DS    XL(L'LP_USRID)      User id number                               
AA_PASS  DS    XL(L'SECOPASS)      PID number                                   
AA_SAGN  DS    XL(L'SECOSAGN)      Security agency group                        
AA_AGPE  DS    CL(L'SECOAGPE)      Person security agency                       
AA_OSYS  DS    XL(L'SECOSYS)       Security system number                       
AA_OPRG  DS    XL(L'SECOPRG)       Security program number                      
AA_LNQ   EQU   *-AA_D              Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Client request array                                                *         
***********************************************************************         
                                                                                
System   LKREQ F,10,(I,B#SAVED,CLTIND),CHAR,TEXT=(*,SYS2LIT),          +        
               OLEN=L'CA_SYS,ARRAY=S                                            
Agency   LKREQ F,11,,CHAR,TEXT=(*,AGY2LIT),OLEN=L'CA_AGYA                       
Media    LKREQ F,12,,CHAR,TEXT=(*,MEDCLIT),OLEN=L'CA_MEDC                       
Client   LKREQ F,13,,CHAR,TEXT=(*,CLTCLIT),OLEN=L'CA_CLTC                       
Produc   LKREQ F,14,,CHAR,TEXT=(*,PRDCLIT),OLEN=L'CA_PRDC                       
Token    LKREQ F,30,,CHAR,TEXT=(*,TOKNLIT),OLEN=L'CA_TOKEN,ARRAY=E              
                                                                                
CA_D     DSECT ,                   ** Client request array **                   
CA_SYS   DS    C                   System letter                                
CA_AGYA  DS    CL2                 Agency alpha id                              
CA_MEDC  DS    C                   Media code                                   
CA_CLTC  DS    CL5                 Client code                                  
CA_PRDC  DS    CL5                 Product code                                 
CA_TOKEN DS    CL8                 PC token value                               
CA_LNQ   EQU   *-CA_D              Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Other options                                                       *         
***********************************************************************         
                                                                                
DLClts?  LKREQ F,40,(D,B#SAVED,QCLTOPT),CHAR,TEXT=(*,DCLTLIT)                   
DLPrds?  LKREQ F,41,(D,B#SAVED,QPRDOPT),CHAR,TEXT=(*,DPRDLIT)                   
Option03 LKREQ F,42,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option04 LKREQ F,43,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option05 LKREQ F,44,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option06 LKREQ F,45,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option07 LKREQ F,46,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option08 LKREQ F,47,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option09 LKREQ F,48,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option10 LKREQ F,49,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option11 LKREQ F,50,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option12 LKREQ F,51,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option13 LKREQ F,52,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option14 LKREQ F,53,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option15 LKREQ F,54,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
Option16 LKREQ F,55,(D,B#SAVED,QOPTION),CHAR,TEXT=(*,DOPTLIT)                   
         LKREQ E                                                                
                                                                                
SYS1LIT  DC    C'Agency system'                                                 
AGY1LIT  DC    C'       agency alpha id'                                        
LIMALIT  DC    C'       limit access'                                           
USIDLIT  DC    C'       user id#'                                               
PIDNLIT  DC    C'       PID#'                                                   
SAGRLIT  DC    C'       access group#'                                          
PSAGLIT  DC    C'       person agency'                                          
SSYSLIT  DC    C'       security system#'                                       
SPRGLIT  DC    C'       security program#'                                      
SYS2LIT  DC    C'Client system'                                                 
AGY2LIT  DC    C'       agency alpha id'                                        
MEDCLIT  DC    C'       media code'                                             
CLTCLIT  DC    C'       client code'                                            
PRDCLIT  DC    C'       product code'                                           
TOKNLIT  DC    C'       PC request token'                                       
DCLTLIT  DC    C'Download clients?'                                             
DPRDLIT  DC    C'Download products?'                                            
DOPTLIT  DC    C'Download Options'                                              
                                                                                
OUTCFMC  LKOUT H                                                                
CFMC     LKOUT R,X'0021'                                                        
Array    LKOUT C,X'0021',(A,ARYCLI)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCLI   LKOUT A,(R,NXTCLI),MULTIROW=Y,ROWNAME=CLTRECD                          
CMedCod  LKOUT C,001,(D,B#WORKD,QMEDA),CHAR,FILTROUT=TSTCLT,           +        
               SKIPCOLS=CLTSKIPN                                                
CLTSKIPS DS    0X                  Start of columns to skip                     
CCltCod  LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CCltNam  LKOUT C,003,CNAME,CHAR,ND=Y                                            
CCltOff  LKOUT C,004,COFFICE,(R,EDTACS),ND=Y                                    
                                                                                
CProf01  LKOUT C,020,CPROF+00,CHAR,LEN=1,ND=Y                                   
CProf02  LKOUT C,021,CPROF+01,CHAR,LEN=1,ND=Y                                   
CProf03  LKOUT C,022,CPROF+02,CHAR,LEN=1,ND=Y                                   
CProf04  LKOUT C,023,CPROF+03,CHAR,LEN=1,ND=Y                                   
CProf05  LKOUT C,024,CPROF+04,CHAR,LEN=1,ND=Y                                   
CProf06  LKOUT C,025,CPROF+05,CHAR,LEN=1,ND=Y                                   
CProf07  LKOUT C,026,CPROF+06,CHAR,LEN=1,ND=Y                                   
CProf08  LKOUT C,027,CPROF+07,CHAR,LEN=1,ND=Y                                   
CProf09  LKOUT C,028,CPROF+08,CHAR,LEN=1,ND=Y                                   
CProf10  LKOUT C,029,CPROF+09,CHAR,LEN=1,ND=Y                                   
CProf11  LKOUT C,030,CPROF+10,CHAR,LEN=1,ND=Y                                   
CProf12  LKOUT C,031,CPROF+11,CHAR,LEN=1,ND=Y                                   
CProf13  LKOUT C,032,CPROF+12,CHAR,LEN=1,ND=Y                                   
CProf14  LKOUT C,033,CPROF+13,CHAR,LEN=1,ND=Y                                   
CProf15  LKOUT C,034,CPROF+14,CHAR,LEN=1,ND=Y                                   
                                                                                
CIntCod  LKOUT C,040,CCLTIFC,CHAR,ND=Y                                          
CAccOFC  LKOUT C,041,CACCOFC,CHAR,ND=Y                                          
CGrpAs1  LKOUT C,042,CGRP1,CHAR,ND=Y                                            
CGrpAs2  LKOUT C,043,CGRP2,CHAR,ND=Y                                            
CGrpAs3  LKOUT C,044,CGRP3,CHAR,ND=Y                                            
CGrpAs4  LKOUT C,045,CGRP4,CHAR,ND=Y                                            
CGrpAs5  LKOUT C,046,CGRP5,CHAR,ND=Y                                            
CMstClt  LKOUT C,047,CMCLTCOD,(R,EDTCLT),ND=Y                                   
CMCSeq#  LKOUT C,048,CMCLTUNQ,UBIN,ND=Y                                         
CMCPrdC  LKOUT C,049,CMCLTPRD,(R,EDTPRD),ND=Y                                   
CAAgyOv  LKOUT C,050,CACCAGY,CHAR,ND=Y                                          
CPolBuy  LKOUT C,051,CPOLONLY,CHAR,ND=Y                                         
CIntrfc  LKOUT C,052,CCLTINTR,UBIN,ND=Y                                         
                                                                                
CXPrf01  LKOUT C,060,CEXTRA+00,CHAR,LEN=1,ND=Y                                  
CXPrf02  LKOUT C,061,CEXTRA+01,CHAR,LEN=1,ND=Y                                  
CXPrf03  LKOUT C,062,CEXTRA+02,CHAR,LEN=1,ND=Y                                  
CXPrf04  LKOUT C,063,CEXTRA+03,CHAR,LEN=1,ND=Y                                  
CXPrf05  LKOUT C,064,CEXTRA+04,CHAR,LEN=1,ND=Y                                  
CXPrf06  LKOUT C,065,CEXTRA+05,CHAR,LEN=1,ND=Y                                  
CXPrf07  LKOUT C,066,CEXTRA+06,CHAR,LEN=1,ND=Y                                  
CXPrf08  LKOUT C,067,CEXTRA+07,CHAR,LEN=1,ND=Y                                  
CXPrf09  LKOUT C,068,CEXTRA+08,CHAR,LEN=1,ND=Y                                  
CXPrf10  LKOUT C,069,CEXTRA+09,CHAR,LEN=1,ND=Y                                  
CXPrf11  LKOUT C,070,CEXTRA+10,CHAR,LEN=1,ND=Y                                  
CXPrf12  LKOUT C,071,CEXTRA+11,CHAR,LEN=1,ND=Y                                  
CXPrf13  LKOUT C,072,CEXTRA+12,CHAR,LEN=1,ND=Y                                  
CXPrf14  LKOUT C,073,CEXTRA+13,CHAR,LEN=1,ND=Y                                  
CXPrf15  LKOUT C,074,CEXTRA+14,CHAR,LEN=1,ND=Y                                  
                                                                                
CIDTitl  LKOUT C,080,CTITLE,CHAR,ND=Y                                           
                                                                                
CPUser1  LKOUT C,090,CPU1,CHAR,ND=Y                                             
CPUTyp1  LKOUT C,091,CPU1TYPE,CHAR,ND=Y                                         
CPULen1  LKOUT C,092,CPU1LEN,UBIN,ND=Y                                          
PRout    LKOUT P,CPU1FLG1,SETBITS                                               
         LKOUT C,093,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,094,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,095,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,096,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,097,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,098,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,099,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,100,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CPUser2  LKOUT C,110,CPU2,CHAR,ND=Y                                             
CPUTyp2  LKOUT C,111,CPU2TYPE,CHAR,ND=Y                                         
CPULen2  LKOUT C,112,CPU2LEN,UBIN,ND=Y                                          
PRout    LKOUT P,CPU2FLG1,SETBITS                                               
         LKOUT C,113,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,114,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,115,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,116,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,117,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,118,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,119,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,120,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CEUser1  LKOUT C,130,CEU1,CHAR,ND=Y                                             
CEUTyp1  LKOUT C,131,CEU1TYPE,CHAR,ND=Y                                         
CEULen1  LKOUT C,132,CEU1LEN,UBIN,ND=Y                                          
PRout    LKOUT P,CEU1FLG1,SETBITS                                               
         LKOUT C,133,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,134,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,135,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,136,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,137,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,138,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,139,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,140,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
PRout    LKOUT P,CEU1FLG2,SETBITS                                               
         LKOUT C,141,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,142,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,143,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,144,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,145,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,146,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,147,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,148,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CEUser2  LKOUT C,150,CEU2,CHAR,ND=Y                                             
CEUTyp2  LKOUT C,151,CEU2TYPE,CHAR,ND=Y                                         
CEULen2  LKOUT C,152,CEU2LEN,UBIN,ND=Y                                          
PRout    LKOUT P,CEU2FLG1,SETBITS                                               
         LKOUT C,153,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,154,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,155,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,156,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,157,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,158,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,159,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,160,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
PRout    LKOUT P,CEU2FLG2,SETBITS                                               
         LKOUT C,161,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,162,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,163,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,164,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,165,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,166,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,167,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,168,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CMedNmO  LKOUT C,170,CMEDNAME,CHAR,ND=Y                                         
CNetwID  LKOUT C,171,CNETID,CHAR,ND=Y                                           
                                                                                
PRout    LKOUT P,COPT1,SETBITS                                                  
         LKOUT C,180,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,181,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,182,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,183,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,184,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,185,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,186,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,187,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PRout    LKOUT P,COPT2,SETBITS                                                  
         LKOUT C,190,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,191,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,192,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,193,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,194,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,195,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,196,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,197,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PRout    LKOUT P,COPT3,SETBITS                                                  
         LKOUT C,200,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,201,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,202,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,203,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,204,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,205,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,206,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,207,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PRout    LKOUT P,COPT4,SETBITS                                                  
         LKOUT C,210,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,211,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,212,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y,               +        
               PCVERSION=1.0.0.5                                                
         LKOUT C,213,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
         LKOUT C,214,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
         LKOUT C,215,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
         LKOUT C,216,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
         LKOUT C,217,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CPSTCod  LKOUT C,220,CPST,CHAR,ND=Y                                             
CEstDai  LKOUT C,221,CDAILY,CHAR,ND=Y                                           
CPrfPct  LKOUT C,222,CPWPCT,(R,OUTPWP),ND=Y,LEN=9                               
CZenClt  LKOUT C,223,CZENCLT,CHAR,ND=Y                                          
CPrvClt  LKOUT C,224,CPREVCLT,(R,EDTCLT),ND=Y                                   
CNxtClt  LKOUT C,225,CNEXTCLT,(R,EDTCLT),ND=Y                                   
CEDIVer  LKOUT C,226,CEDIBVRS,CHAR                                              
CEDIAdr  LKOUT C,227,CEDIBADR,CHAR                                              
CTCltTy  LKOUT C,228,CCLTTYPE,CHAR                                              
CExtrOp  LKOUT C,229,CCPPRS,CHAR                                                
                                                                                
PRout    LKOUT P,CINDS1,SETBITS                                                 
         LKOUT C,230,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,231,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,232,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,233,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,234,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,235,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,236,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,237,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
CCos2Fa  LKOUT C,240,CCOST2,(R,OUTCS2),ND=Y,LEN=7                               
CLimAcc  LKOUT C,241,CACCESS,(R,EDTACS),ND=Y                                    
CFrzLcY  LKOUT C,242,CLOCKYR,UBIN                                               
                                                                                
PRout    LKOUT P,CLOCKMON,SETBITS                                               
         LKOUT C,250,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,251,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
                                                                                
CFrzLcM  LKOUT P,CLOCKMON,SETLMON                                               
         LKOUT C,252,(D,B#WORKD,WORK),UBIN,LEN=1,ND=Y                           
                                                                                
CRFPGID  LKOUT C,260,CRFPGRP,CHAR                                               
CC2CvDt  LKOUT C,261,CC2CONV,CDAT                                               
CTrfOfC  LKOUT C,262,CTRAFOFC,UBIN                                              
CSLLFor  LKOUT C,263,CLTSLLMT,UBIN                                              
CGrpAs6  LKOUT C,264,CGRP6,CHAR,ND=Y                                            
CGrpAs7  LKOUT C,265,CGRP7,CHAR,ND=Y                                            
CGrpAs8  LKOUT C,266,CGRP8,CHAR,ND=Y                                            
CGrpAs9  LKOUT C,267,CGRP9,CHAR,ND=Y                                            
CGrpAsA  LKOUT C,268,CGRP10,CHAR,ND=Y                                           
CCorpPr  LKOUT C,269,CPRPRD,CHAR,ND=Y                                           
                                                                                
CPSTCod  LKOUT C,270,CPST+00,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,271,CPST+01,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,272,CPST+02,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,273,CPST+03,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,274,CPST+04,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,275,CPST+05,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,276,CPST+06,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,277,CPST+07,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,278,CPST+08,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,279,CPST+09,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
                                                                                
CMPSCOD  LKOUT C,280,CMPST+00,(R,EDTMPS),ND=Y,PCVERSION=1.0.5.0                 
         LKOUT C,281,CMPST+01,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                 
                                                                                
PRout    LKOUT P,CKEYCLT,SETAAA                                                 
         LKOUT C,290,(D,B#WORKD,WORK+00),CHAR,LEN=30,ND=Y                       
         LKOUT C,291,(D,B#WORKD,WORK+30),CHAR,LEN=30,ND=Y                       
         LKOUT C,292,(D,B#WORKD,WORK2+00),CHAR,LEN=30,ND=Y                      
         LKOUT C,293,(D,B#WORKD,WORK2+30),CHAR,LEN=30,ND=Y                      
                                                                                
CReqTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
CLTSKIPN EQU   (*-CLTSKIPS)/LX_COLSL                                            
                                                                                
Array    LKOUT C,X'0029',(A,ARYPRF),FILTROUT=TSTSYS                             
                                                                                
Array    LKOUT C,X'0022',(A,ARYPRO),FILTROUT=TSTPRD                             
         LKOUT E                                                                
                                                                                
ARYPRF   LKOUT A,(R,NXTPRF),MULTIROW=Y,ROWNAME=DUMMY_D                          
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
TOKEN    LKOUT C,002,(D,B#SAVED,SVREQTOK),CHAR                                  
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1                             
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1                             
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1                             
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1                             
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1                             
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1                             
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1                             
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1                             
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1                             
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1                             
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1                             
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1                             
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1                             
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1                             
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1                             
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1                             
         LKOUT E                                                                
                                                                                
TSTCLT   CLI   DUMMYCLI,YESQ       Test dummy client                            
         JNE   *+8                                                              
         LTR   RE,RE               Yes - don't send these                       
         BR    RE                                                               
         CLI   QCLTOPT,YESQ        Test downloading client details              
         BR    RE                                                               
                                                                                
TSTPRD   CLI   QPRDOPT,YESQ        Test downloading product details             
         BR    RE                                                               
                                                                                
TSTSYS   CLI   QANYREQ,YESQ        Test any request for this system             
         BR    RE                                                               
                                                                                
OUTCS2   L     R2,LP_AINP                                                       
         OC    0(4,R2),0(R2)                                                    
         JNZ   OUTCS2A                                                          
         XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
*******                                                                         
* COST2 WITH A VALUE OF 0.000000 IS STORED AS X'80000000'                       
*******                                                                         
OUTCS2A  TM    0(R2),X'80'         HIGH ORDER BIT IS ON?                        
         JZ    *+10                                                             
         XC    0(4,R2),0(R2)       YES, CHANGE TO 0.0 SO IT CAN DISPLAY         
         L     R3,LP_AOUT                                                       
         EDIT  (B4,0(R2)),(7,0(R3)),FILL=0                                      
         J     EXITY               DISPLAY THIS FIELD                           
                                                                                
OUTPWP   L     R2,LP_AINP                                                       
         OC    0(L'CPWPCT,R2),0(R2)                                             
         JNZ   OUTPWPA                                                          
         XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
*******                                                                         
* PWPCT WITH A VALUE OF 0.00000000 IS STORED AS X'800000'                       
*******                                                                         
OUTPWPA  TM    0(R2),X'80'         HIGH ORDER BIT IS ON?                        
         JZ    *+10                                                             
         XC    0(L'CPWPCT,R2),0(R2)  CHANGE TO 0.0 SO IT CAN DISPLAY            
         L     R3,LP_AOUT                                                       
         XC    FULL,FULL                                                        
         MVC   FULL+1(L'CPWPCT),0(R2)                                           
         EDIT  (B4,FULL),(9,0(R3)),FILL=0                                       
         J     EXITY               DISPLAY THIS FIELD                           
                                                                                
SETBITS  L     R1,LP_AINP                                                       
         LLC   RF,0(R1)                                                         
         SLL   RF,32-8                                                          
         LA    RE,WORK                                                          
         LHI   R0,8                                                             
SETBITS2 MVI   0(RE),C'N'                                                       
         TMH   RF,X'8000'                                                       
         JZ    *+8                                                              
         MVI   0(RE),C'Y'                                                       
         AHI   RE,1                                                             
         SLL   RF,1                                                             
         JCT   R0,SETBITS2                                                      
         J     EXITY                                                            
                                                                                
SETLMON  L     R1,LP_AINP                                                       
         MVC   WORK(L'CLOCKMON),0(R1)                                           
         NI    WORK,X'FF'-(X'80'+X'40')                                         
         J     EXITY                                                            
                                                                                
         USING OFFICED,RC                                                       
EDTACS   LM    R2,R4,LP_AINP       EDIT OFFICE ACCESS CONTROL                   
         CLI   0(R2),0             NULL ON FIRST BYTE?                          
         JE    XCOLEN              THEN NO OFFICE LIST                          
         LA    R0,3                MAX OF 3                                     
*                                                                               
         XC    OFFICED(OFCLENQ),OFFICED                                         
         MVI   OFCSYS,SYSSPTQ      LOOK-UP 2 CHARACTER OFFICE                   
         MVC   OFCAGY,LP_AGY                                                    
*                                                                               
         CHI   R3,1                SEE IF INPUT IS LENGTH 1                     
         JNE   EDTACS10                                                         
         LA    R0,1                THEN WE THIS IS OUTPUT FOR COFFICE           
         J     EDTACS20                                                         
*                                                                               
EDTACS10 CLI   0(R2),0             NULL FOR THIS OFFICE?                        
         JE    EDTACSX             DONE WITH LIMIT ACCESS                       
         CHI   R0,3                                                             
         JE    EDTACS20                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
EDTACS20 MVC   OFCOFC,0(R2)                                                     
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(X'01',LP_ACOM)                     
         CLI   0(R1),0                                                          
         JNE   EDTACSX             DONE ON ERROR                                
         MVC   0(L'OFCOFC2,R4),OFCOFC2                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         JNH   *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R2,1(R2)            BUMP TO NEXT OFFICE IN LIST                  
         JCT   R0,EDTACS10                                                      
*                                                                               
EDTACSX  L     R0,LP_AOUT                                                       
         SR    R4,R0                                                            
         STCM  R4,15,LP_OLEN       SET OUTPUT LENGTH AND EXIT                   
         J     EXITY                                                            
         DROP  RC                                                               
                                                                                
EDTCLT   L     R1,LP_AINP          Edit a client code                           
         OC    0(L'CKEYCLT,R1),0(R1)                                            
         JZ    XCOLEN                                                           
         MVC   SVIOVALS,IOVALS                                                  
K        USING CKEY,IOKEY                                                       
         XC    K.CKEY,K.CKEY                                                    
         MVI   K.CKEYTYPE,CKEYTYPQ                                              
         MVC   K.CKEYAM,QMEDX                                                   
         L     R1,LP_AINP                                                       
         MVC   K.CKEYCLT,0(R1)                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   EDTCLTN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO4'                           
         JNE   EDTCLTN                                                          
         L     R2,AIO4                                                          
         USING CLTRECD,R2                                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,CKEYCLT),LP_AOUT                           
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
                                                                                
EDTCLTN  MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     XCOLEN                                                           
         DROP  R2                                                               
                                                                                
SETAAA   XC    WORK,WORK           Set prd AAA's name & address                 
         XC    WORK2,WORK2         Clear work areas                             
         L     R1,LP_AINP                                                       
         OC    0(L'CKEYCLT,R1),0(R1)                                            
         JZ    XCOLEN                                                           
         MVC   SVIOVALS,IOVALS                                                  
K        USING PKEY,IOKEY                                                       
         XC    K.PKEY,K.PKEY                                                    
         MVI   K.PKEYTYPE,PKEYTYPQ                                              
         MVC   K.PKEYAM,QMEDX                                                   
         MVC   K.PKEYCLT,0(R1)                                                  
         MVC   K.PKEYPRD,=C'AAA'                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   SETAAAN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO4'                           
         JNE   SETAAAN                                                          
         L     R2,AIO4                                                          
         USING PRDRECD,R2                                                       
         MVC   WORK(60),PADDR1     Bill name & address line 1                   
         MVC   WORK2(60),PADDR3    Bill address lines 2 & 3                     
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
                                                                                
SETAAAN  MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     XCOLEN                                                           
         DROP  R2                                                               
                                                                                
EDTPRD   L     R1,LP_AINP          Edit product code                            
         CLI   0(R1),0                                                          
         JE    XCOLEN                                                           
         L     R0,ACLTREC                                                       
         MVC   ACLTREC,AIO4                                                     
         GOTOR (#EDTPRD,AEDTPRD),LP_AINP                                        
         STCM  R0,15,ACLTREC                                                    
         J     EXITY                                                            
                                                                                
EDTMPS   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have Main PST code?                          
         JE    XCOLEN                                                           
         SR    R0,R0                                                            
         IC    R0,0(R2)            Translate position to province code          
         LAY   R1,PSTVTAB                                                       
EDTMPS3  CLI   0(R1),X'00'         End of table?                                
         JE    XCOLEN                                                           
         LA    R1,2(R1)            Point to next province in table              
         JCT   R0,EDTMPS3                                                       
         MVC   0(2,R4),0(R1)                                                    
         LHI   R0,2                                                             
         J     SETOLENX                                                         
                                                                                
PSTVTAB  DC    C'()'               Dummy entry for looping                      
         DC    C'BC'                                                            
         DC    C'AL'                                                            
         DC    C'SA'                                                            
         DC    C'MA'                                                            
         DC    C'ON'                                                            
         DC    C'PQ'                                                            
         DC    C'NB'                                                            
         DC    C'NS'                                                            
         DC    C'PE'                                                            
         DC    C'NF'                                                            
         DC    X'00'               End of table                                 
                                                                                
***********************************************************************         
* Process first/next client array entry                               *         
***********************************************************************         
                                                                                
NXTCLI   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTCLI02                                                         
         SR    RE,RE                                                            
         ICM   RE,7,ACLT                                                        
         JZ    NOMORE                                                           
         MVC   NUMCLT,LW_NUMN-LW_D(RE)                                          
         AHI   RE,LW_LN2Q                                                       
         ST    RE,ANXTCLT          Set A(next client request)                   
                                                                                
NXTCLI02 SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
                                                                                
         L     R2,ANXTCLT                                                       
         USING CA_D,R2             R2=A(client row)                             
         MVC   SVREQTOK,CA_TOKEN   Save row token                               
         XC    SVCLTOFF,SVCLTOFF   Clear save client office code                
         LA    R0,CA_D+CA_LNQ                                                   
         ST    R0,ANXTCLT                                                       
                                                                                
         CLC   CA_SYS,SYSLET       Test for my system                           
         JNE   NXTCLI02                                                         
         MVI   QANYREQ,YESQ        Any request y/n switch for this sys          
         CLC   PAGYCOD,CA_AGYA     Test change of agency                        
         JE    NXTCLI10                                                         
         MVC   PAGYCOD,CA_AGYA     Set current agency code                      
                                                                                
         SR    R3,R3                                                            
         ICM   R3,7,QAAGY                                                       
         JNZ   *+6                                                              
         DC    H'0'                Agency definitions not passed                
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R3)                                            
         AHI   R3,LW_LN2Q                                                       
         USING AA_D,R3             Look up connection table entry               
NXTCLI04 CLC   AA_SYS,CA_SYS       Match system                                 
         JNE   *+14                                                             
         CLC   AA_OAGY,CA_AGYA     and agency alpha id                          
         JE    NXTCLI06                                                         
         AHI   R3,AA_LNQ           Bump to next entry                           
         JCT   R0,NXTCLI04                                                      
         DC    H'0'                System/agency not provided                   
                                                                                
NXTCLI06 TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    NXTCLI08                                                         
         GOTOR GETUID,DMCB,AA_USID,AA_PASS                                      
         JNE   NXTCLI02                                                         
*****                                                                           
         MVC   LP_ACCS,AA_ACCS     USE WHAT WAS PASSED TO US                    
*****                                                                           
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                               
         GOTOR (RF),(R1),DMKEY,SPTFIL,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,GENDIR,(4,0),0                                   
         GOTOR (RF),(R1),DMKEY,STAFIL,(4,0),0                                   
                                                                                
NXTCLI08 MVI   DUMMYCLI,NOQ        Set not a dummy client request               
         GOTOR (#GETAGY,AGETAGY),CA_AGYA                                        
                                                                                
NXTCLI10 GOTOR (#VALMED,AVALMED),DMCB,CA_MEDC,0,QMEDX                           
         JNE   NXTCLI62                                                         
         GOTOR (#VALCLT,AVALCLT),DMCB,CA_CLTC,3,QCLTX                           
         JNE   NXTCLI62                                                         
         L     R4,ACLTREC                                                       
         USING CLTHDR,R4                                                        
         MVC   SVCLTKEY,0(R4)      SAVE KEY OF CLIENT RECORD                    
         MVC   SVCLTOFF,COFFICE                                                 
*****                                                                           
* REPLACE NULLS THAT ARE GIVING ORGANIZER PROBLEMS                              
*****                                                                           
         CLI   CPROF+1,0           NULL IN LOCK BOX NUMBER?                     
         JNE   *+8                                                              
         MVI   CPROF+1,C'0'                                                     
         CLI   CPROF+4,0           NULL IN BILL FORMULA CONTROL?                
         JNE   *+8                                                              
         MVI   CPROF+4,C'0'                                                     
         CLI   CPROF+5,0           NULL IN BILL EST CONTROL?                    
         JNE   *+8                                                              
         MVI   CPROF+5,C'0'                                                     
         CLI   CPROF+8,0           NULL IN GOALS CPP OVERRIDE?                  
         JNE   *+8                                                              
         MVI   CPROF+8,C'0'                                                     
*                                                                               
         CLI   CEXTRA+1,0          NULL IN CANADIAN NETWORK TAX?                
         JNE   *+8                                                              
         MVI   CEXTRA+1,C'0'                                                    
         CLI   CEXTRA+4,0          NULL IN CAMPAIGNS?                           
         JNE   *+8                                                              
         MVI   CEXTRA+4,C'0'                                                    
         CLI   CEXTRA+7,0          NULL IN MKGDS IN MISSED MONTH?               
         JNE   *+8                                                              
         MVI   CEXTRA+7,C'N'                                                    
         CLI   CCPPRS,0            NULL IN CPPRS?                               
         JNE   *+8                                                              
         MVI   CCPPRS,C'N'                                                      
*                                                                               
         CLI   CPOLONLY,0          NULL IN CPOLONLY?                            
         JNE   *+8                                                              
         MVI   CPOLONLY,C'N'       NULL AND C'N' ARE THE SAME                   
*                                                                               
         CLI   CDAILY,0            NULL IN DAILY ESTIMATE                       
         JNE   *+8                                                              
         MVI   CDAILY,C'N'                                                      
*****                                                                           
* REPLACE MISLEADING VALUES FOR ORGANIZER WHEN THINGS                           
*****                                                                           
         LA    R1,CPROF+6          FOR PRINT CLIENT AS AAN                      
         BRAS  RE,YES4YOR1         'Y' OR '1' IS 'Y', ALL ELSE IS 'N'           
         LA    R1,CPROF+11         FORCE EST SERIES REQ                         
         BRAS  RE,YES4YOR1         'Y' OR '1' IS 'Y', ALL ELSE IS 'N'           
         LA    R1,CPROF+12         PRD REQUEST FOR TPOL                         
         BRAS  RE,YES4YOR1         'Y' OR '1' IS 'Y', ALL ELSE IS 'N'           
*                                                                               
         LA    R1,CEXTRA+3         ESTIMATE FILTERS REQ                         
         BRAS  RE,YES4YOR1         'Y' OR '1' IS 'Y', ALL ELSE IS 'N'           
         LA    R1,CEXTRA+7         MKGDS IN MISSED MONTH                        
         BRAS  RE,YES4YOR1         'Y' OR '1' IS 'Y', ALL ELSE IS 'N'           
*                                                                               
         CLI   CEXTRA+8,C'P'                                                    
         JE    *+12                                                             
         LA    R1,CEXTRA+8         GOAL REQUIRED FOR BUY?                       
         BRAS  RE,YES4YOR1         'Y' OR '1' IS 'Y', ALL ELSE IS 'N'           
*                                                                               
         LA    R1,CEXTRA+10        OUT-OF-WEEK CLIENT                           
         BRAS  RE,YES4YOR1         'Y' OR '1' IS 'Y', ALL ELSE IS 'N'           
         LA    R1,CEXTRA+12        SPECIAL DEMO ADJUSTMENT                      
         BRAS  RE,YES4YOR1         'Y' OR '1' IS 'Y', ALL ELSE IS 'N'           
*****                                                                           
         DROP  R4                                                               
*                                                                               
         XC    PRDRSTR,PRDRSTR     Product start range                          
         MVC   PRDREND,=X'FFFFFF'  Product end range                            
         OC    CA_PRDC,CA_PRDC                                                  
         CLC   CA_PRDC,SPACES      Filtering on prduct codes?                   
         JNH   *+16                                                             
         MVC   PRDRSTR,CA_PRDC                                                  
         MVC   PRDREND,CA_PRDC                                                  
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
                                                                                
NXTCLI62 MVI   DUMMYCLI,YESQ       Set dummy client request                     
         MVC   LP_ADATA,AIO1                                                    
         J     EXITY                                                            
                                                                                
         DROP  R2,R3                                                            
***********************************                                             
* 0(R1) = 'Y' OR '1' SHOULD BE 'Y'                                              
*         ANYTHING ELSE SHOULD BE 'N'                                           
***********************************                                             
YES4YOR1 CLI   0(R1),C'Y'                                                       
         JE    YS4Y1X                                                           
         CLI   0(R1),C'1'                                                       
         JNE   *+12                                                             
         MVI   0(R1),C'Y'                                                       
         J     YS4Y1X                                                           
         MVI   0(R1),C'N'                                                       
YS4Y1X   BR    RE                                                               
                                                                                
ARYPRO   LKOUT A,(R,NXTPRO),MULTIROW=Y,ROWNAME=PRDRECD                          
PMedCod  LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
PCltCod  LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
PPrdCod  LKOUT C,003,(D,,PKEYPRD),CHAR                                          
PPrdAcc  LKOUT C,004,PACCT,(R,EDTPA#)                                           
PPrdNam  LKOUT C,005,PNAME,CHAR                                                 
PHexPrd  LKOUT C,006,PCODE,HEXD,ND=Y                                            
PBAdrL1  LKOUT C,007,PADDR1,CHAR,ND=Y                                           
PBAdrL2  LKOUT C,008,PADDR2,CHAR,ND=Y                                           
PBAdrL3  LKOUT C,009,PADDR3,CHAR,ND=Y                                           
PBAdrL4  LKOUT C,010,PADDR4,CHAR,ND=Y                                           
PDivCod  LKOUT C,011,PDIV,CHAR,ND=Y                                             
PYrMoSv  LKOUT C,012,PBILLDT,BMON,ND=Y                                          
PBilBas  LKOUT C,013,PBILLBAS,UBIN,FILTROUT=TSVS0810                            
PBilCom  LKOUT C,014,PBILLCOM,CBIN,ND=Y                                         
PAgyFee  LKOUT C,015,PAGYFEE,SPAK,ND=Y                                          
PProfil  LKOUT C,016,PPROF,CHAR,ND=Y                                            
PGrpAs1  LKOUT C,017,PGRP1,HEXD,ND=Y                                            
PGrpAs2  LKOUT C,018,PGRP2,HEXD,ND=Y                                            
PGrpAs3  LKOUT C,019,PGRP3,HEXD,ND=Y                                            
PPClass  LKOUT C,020,PCLASS,(R,EDTPCL),ND=Y                                     
PGrpAs4  LKOUT C,021,PGRP4,HEXD,ND=Y                                            
PGrpAs5  LKOUT C,022,PGRP5,HEXD,ND=Y                                            
PPrdLoc  LKOUT C,023,PLOCK,CHAR,ND=Y                                            
PPLAcDt  LKOUT C,024,PLKDAT,CDAT,ND=Y                                           
PGSTaxC  LKOUT C,025,PGSTCODE,CHAR,ND=Y                                         
PExtOpt  LKOUT C,026,PCPPRS,CHAR,ND=Y                                           
PUserF1  LKOUT C,027,PUSER1,CHAR,ND=Y                                           
PUserF2  LKOUT C,028,PUSER2,CHAR,ND=Y                                           
PPSTCod  LKOUT C,029,PPST,CHAR,ND=Y                                             
PTalAgy  LKOUT C,030,PTALAGY,CHAR,ND=Y                                          
PTalFac  LKOUT C,031,PTAL,UBIN,FILTROUT=TSGMICLT                                
PPrdRat  LKOUT C,032,PRATE,CHAR,ND=Y                                            
                                                                                
PRout    LKOUT P,POPT1,SETBITS                                                  
         LKOUT C,040,(D,B#WORKD,WORK+0),CHAR,LEN=1,ND=Y                         
         LKOUT C,041,(D,B#WORKD,WORK+1),CHAR,LEN=1,ND=Y                         
         LKOUT C,042,(D,B#WORKD,WORK+2),CHAR,LEN=1,ND=Y                         
         LKOUT C,043,(D,B#WORKD,WORK+3),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,044,(D,B#WORKD,WORK+4),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,045,(D,B#WORKD,WORK+5),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,046,(D,B#WORKD,WORK+6),CHAR,LEN=1,ND=Y                         
* * * *  LKOUT C,047,(D,B#WORKD,WORK+7),CHAR,LEN=1,ND=Y                         
                                                                                
PMedOff  LKOUT C,050,POFFICE,CHAR,ND=Y                                          
PTrfOff  LKOUT C,051,PTRAFOFC,CHAR,ND=Y                                         
PAccOff  LKOUT C,052,PACCOFC,CHAR,ND=Y                                          
PAccAgy  LKOUT C,053,PACCAGY,CHAR,ND=Y                                          
PGrpAsg  LKOUT C,054,PGRP6,HEXD,ND=Y                                            
                                                                                
PBillBas LKOUT C,080,PBILLBAS,(R,EDTBBS),ND=Y,PCVERSION=0.8.10.0                
PBBComm  LKOUT C,081,PBILLBAS,(R,EDTBBC),ND=Y,PCVERSION=0.8.10.0                
PCommBas LKOUT C,082,PBILLBAS,(R,EDTCBS),ND=Y,PCVERSION=0.8.10.0                
                                                                                
PPSTCod  LKOUT C,270,PPST+00,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,271,PPST+01,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,272,PPST+02,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,273,PPST+03,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,274,PPST+04,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,275,PPST+05,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,276,PPST+06,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,277,PPST+07,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,278,PPST+08,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
         LKOUT C,279,PPST+09,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                  
                                                                                
PMPSCod  LKOUT C,280,PMPST+00,(R,EDTMPS),ND=Y,PCVERSION=1.0.5.0                 
         LKOUT C,281,PMPST+01,CHAR,LEN=1,ND=Y,PCVERSION=1.0.5.0                 
                                                                                
PReqTok  LKOUT C,300,(D,B#SAVED,SVREQTOK),CHAR                                  
         LKOUT E                                                                
                                                                                
EDTPA#   LM    R2,R4,LP_AINP       Edit product account number                  
         CLI   0(R2),X'FF'                                                      
         JNE   *+18                                                             
         UNPK  0(5,R4),1(3,R2)                                                  
         LHI   R0,5                                                             
         J     SETOLENX                                                         
         MVC   0(L'PACCT,R4),0(R2)                                              
         LHI   R0,L'PACCT                                                       
         J     SETOLENX                                                         
                                                                                
EDTPCL   LM    R2,R4,LP_AINP       Edit product class                           
         CLI   0(R2),0                                                          
         JE    XCOLEN                                                           
         LHI   R0,1                                                             
         MVC   0(1,R4),0(R2)                                                    
         CLI   0(R2),X'99'                                                      
         JH    SETOLENX                                                         
         LHI   R0,2                                                             
         PACK  0(1,R4),0(1,R2)     There are 2 product classes                  
         NI    0(R4),X'0F'                                                      
         OI    0(R4),X'C0'                                                      
         MVC   1(1,R4),0(R2)                                                    
         NI    1(R4),X'0F'                                                      
         OI    1(R4),X'C0'                                                      
         J     SETOLENX                                                         
                                                                                
EDTBBS   LM    R2,R4,LP_AINP       Edit Bill Basis                              
         OC    SVPBLDAT,SVPBLDAT                                                
         JZ    XCOLEN                                                           
         LHI   R0,1                                                             
         MVI   0(R4),C'G'                                                       
         TM    SVPBLBAS,X'10'                                                   
         JZ    SETOLENX                                                         
         MVI   0(R4),C'N'                                                       
         J     SETOLENX                                                         
                                                                                
EDTBBC   LM    R2,R4,LP_AINP       Edit   Commission Only Billing               
         OC    SVPBLDAT,SVPBLDAT                                                
         JZ    XCOLEN                                                           
         LHI   R0,1                                                             
         MVI   0(R4),C'N'                                                       
         TM    SVPBLBAS,X'40'                                                   
         JZ    SETOLENX                                                         
         MVI   0(R4),C'Y'                                                       
         J     SETOLENX                                                         
                                                                                
EDTCBS   LM    R2,R4,LP_AINP       Edit Comm Basis                              
         OC    SVPBLDAT,SVPBLDAT                                                
         JZ    XCOLEN                                                           
         OC    SVPBLCOM,SVPBLCOM                                                
         JZ    XCOLEN                                                           
         LHI   R0,1                                                             
         MVI   0(R4),C'G'                                                       
         TM    SVPBLBAS,X'01'                                                   
         JZ    SETOLENX                                                         
         MVI   0(R4),C'N'                                                       
         J     SETOLENX                                                         
                                                                                
TSGMICLT L     RE,ACLTREC                                                       
         TM    COPT1-CLTHDR(RE),COP1GMI  THIS CLIENT IS A GMI CLIENT?           
         JNZ   EXITY                     YES, IT IS                             
         J     EXITN                     NOT THIS CLIENT                        
                                                                                
TSVS0810 CLC   LP_VRSN,VS081000    Test against version 0.8.10.x                
         JL    EXITY               Lower, we want the field                     
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Get profiles                                                        *         
***********************************************************************         
                                                                                
NXTPRF   XC    FULL,FULL           Init output profile name                     
         XC    WORK,WORK           Init output profile values                   
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRF06                                                         
         LA    RF,PROFTAB          Point to profile table                       
NXTPRF02 STCM  RF,15,PREVPROF                                                   
         J     NXTPRF12                                                         
                                                                                
NXTPRF06 ICM   RF,15,PREVPROF                                                   
         LA    RF,PROFTABL(RF)     Next entry in table                          
         J     NXTPRF02                                                         
                                                                                
NXTPRF12 ICM   RF,15,PREVPROF      Get previous profile entry                   
         CLC   0(2,RF),=X'0000'    End of profile table?                        
         JE    NOMORE                                                           
                                                                                
         MVC   FULL(4),0(RF)       Set output profile name                      
         XC    TEMP,TEMP                                                        
         MVC   TEMP+00(2),=C'S0'                                                
         MVC   TEMP+02(2),0(RF)                                                 
         CLI   2(RF),C' '          3 character profile?                         
         JNH   NXTPRF16                                                         
         MVI   TEMP+00,C's'        Yes                                          
         MVC   TEMP+01(3),0(RF)                                                 
*                                                                               
NXTPRF16 MVC   TEMP+04(L'AGENCY),AGENCY                                         
         MVC   TEMP+06(L'QMEDA),QMEDA                                           
         MVC   TEMP+07(L'QCLTA),QCLTA                                           
         MVI   TEMP+10,C'*'                                                     
         MVC   TEMP+11(L'SVCLTOFF),SVCLTOFF                                     
         TM    4(RF),X'01'         Agency level only?                           
         JZ    *+10                                                             
         MVC   TEMP+06(06),SPACES                                               
         GOTOR VGETPROF,DMCB,(FULL+3,TEMP),WORK,VDATAMGR                        
         MVC   LP_ADATA,AOFCREC                                                 
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Read product records for current client                             *         
***********************************************************************         
                                                                                
NXTPRO   XC    SVPBLDAT,SVPBLDAT                                                
         XC    SVPBLBAS,SVPBLBAS                                                
         XC    SVPBLCOM,SVPBLCOM                                                
*                                                                               
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',CFMPRKT),                +        
               ('B#PRDREC',0),SAVED,0,0                                         
         JNE   EXITY              Used to be  "J  EXITY"                        
*                                                                               
         L     R2,IOADDR                                                        
         MVC   SVPBLDAT,PBILLDT-PRDHDR(R2)                                      
         MVC   SVPBLBAS,PBILLBAS-PRDHDR(R2)                                     
         MVC   SVPBLCOM,PBILLCOM-PRDHDR(R2)                                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Market group download                                               *         
***********************************************************************         
                                                                                
REQMKTG  LKREQ H,I#MKTGDL,OUTMKG,NEXTREQ=REQCLTG                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
MkGCd    LKREQ F,002,(I,B#SAVED,QMKGIND),CHAR,TEXT=(*,MKGCLIT),        +        
               OLEN=L'MKGCHAR,LIST=F,SORT=N                                     
MkGOp    LKREQ F,003,(D,B#SAVED,MKGOPT1),CHAR,TEXT=(*,MKGOLIT)                  
         LKREQ E                                                                
                                                                                
OUTMKG   LKOUT H                                                                
         LKOUT R,1                                                              
PRout    LKOUT P,,MKGINI           Init market group array values               
Array    LKOUT C,1,(A,ARYMKG)                                                   
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
MKGCLIT  DC    C'Market Group Codes'                                            
MKGOLIT  DC    C'Market Group Download Option'                                  
                                                                                
ARYMKG   LKOUT A,(R,NXTMKG),MULTIROW=Y,ROWNAME=MKGRECD                          
Media    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,MKGKCLT,(R,EDTCL2)                                         
PRout    LKOUT P,,EDTPRG           Edit product group code                      
PGrCd    LKOUT C,003,(D,B#SAVED,PRGCHAR),CHAR                                   
MKtGp    LKOUT C,004,(D,B#SAVED,MKGCHAR),CHAR                                   
Array    LKOUT C,010,(A,ARYBKNM)                                                
*                                                                               
Array    LKOUT C,2,(A,ARYMKTN)     List of market# for group                    
*                                                                               
         LKOUT E                                                                
                                                                                
ARYBKNM  LKOUT A,(D,B#MKGREC,MKGEL),EOT=EOR,ROWID=(MKGEL10,X'10'),     +        
               ROWWIDTH=(V,MKGEL10+1)                                           
         LKOUT C,010,MKGNAM1,CHAR,ND=Y                                          
         LKOUT C,011,MKGNAM2,CHAR,ND=Y                                          
         LKOUT C,012,MKGNAM3,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
ARYMKTN  LKOUT A,(R,NXTMK#),MULTIROW=Y                                          
         LKOUT C,001,(D,B#SAVED,MKGMKT#),(R,EDTMK#)                             
         LKOUT E                                                                
                                                                                
MKGINI   GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
EDTPRG   MVC   PRGCHAR,SPACES      Init product group code output               
         LA    RE,IOKEY            Point to markt group key                     
         USING MKGKEY,RE                                                        
         OC    MKGKPID(3),MKGKPID  Have product group?                          
         JNZ   *+14                                                             
EDTPRG10 MVC   PRGCHAR(3),=C'ALL'                                               
         J     EXITY                                                            
*                                                                               
         MVC   PRGCHAR(L'MKGKPID),MKGKPID                                       
         OC    MKGKPGRP,MKGKPGRP   Have product group code?                     
         JZ    EXITY                                                            
*                                                                               
         CLC   PRGBIN_,MKGKPID     Same product group code?                     
         JE    EDTPRG40                                                         
         MVC   PRGBIN_,MKGKPID     Save product group code to format            
         DROP  RE                                                               
         MVC   WKIOKEY,IOKEY       Save market group key                        
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING PRGKEY,RE                                                        
         MVI   PRGKTYP+0,X'0D'     Prepare product group key                    
         MVI   PRGKTYP+1,X'01'                                                  
         MVC   PRGKAGMD,WKIOKEY+(MKGKAGMD-MKGKEY)                               
         MVC   PRGKCLT,WKIOKEY+(MKGKCLT-MKGKEY)                                 
         MVC   PRGKID,WKIOKEY+(MKGKPID-MKGKEY)                                  
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JNE   EDTPRG90                                                         
         CLC   IOKEY(L'PRGKEY),IOKEYSAV                                         
         JNE   EDTPRG90                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   EDTPRG90                                                         
         L     R1,IOADDR                                                        
         LA    R1,(PRGEL-PRGRECD)(R1)                                           
         USING PRGEL01,R1                                                       
         CLI   PRGEL01,X'01'       Prd group break description elem?            
         JNE   EDTPRG90                                                         
         LLC   RE,PRGBK1LN                                                      
         LLC   R0,PRGBK2LN                                                      
         AR    RE,R0                                                            
         LLC   R0,PRGBK3LN                                                      
         AR    RE,R0                                                            
         STC   RE,PBRKLEN                                                       
         DROP  R1                                                               
*                                                                               
         MVC   IOKEY,WKIOKEY       Restore market group key and DM seq          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
*                                                                               
EDTPRG40 XC    FULL1,FULL1                                                      
         GOTOR VHEXOUT,DMCB,PRGBIN_+1,FULL1,2,=C'TOG',0                         
         LLC   RE,PBRKLEN                                                       
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   PRGCHAR+L'MKGKPID(0),FULL1                                       
         EX    RE,0(R1)                                                         
         J     EXITY                                                            
*                                                                               
EDTPRG90 MVC   PRGCHAR,=C'!???!'                                                
         MVC   IOKEY,WKIOKEY       Restore market group key and DM seq          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         J     EXITY                                                            
*                                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
NXTMKG   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTMKG50                                                         
         CLI   QMEDX,0             Have media?                                  
         JE    NOMORE                                                           
         MVI   PBRKLEN,0           Init saved product grp break length          
         XC    PRGBIN_,PRGBIN_     Init saved binary product grp code           
         MVI   MBRKLEN,0           Init saved market grp break length           
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         MVI   MKGKTYP+0,X'0D'                                                  
         MVI   MKGKTYP+1,X'02'                                                  
         MVC   MKGKAGMD,QMEDX      Binary agency/media                          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JNE   NOMORE                                                           
         DROP  RE                                                               
*                                                                               
NXTMKG20 LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         CLC   IOKEY(MKGKCLT-MKGKEY),IOKEYSAV                                   
         JNE   NOMORE                                                           
         CLI   MKGKMID,0           Have market group ID?                        
         JE    NXTMKG50                                                         
         OC    MKGKMGRP,MKGKMGRP   Have market group number?                    
         JZ    NXTMKG50                                                         
         DROP  RE                                                               
*                                                                               
NXTMKG40 MVC   WKIOKEY,IOKEY       Save market group key                        
         XC    IOKEY,IOKEY         Find the MGRDEF record                       
         LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         MVC   MKGKEY(MKGKMID-MKGKEY),WKIOKEY                                   
         MVC   MKGKMID,WKIOKEY+(MKGKMID-MKGKEY)                                 
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(L'MKGKEY),IOKEYSAV                                         
         JE    NXTMKG46                                                         
*                                                                               
         XC    IOKEY,IOKEY         Try client and all prd group                 
         LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         MVC   MKGKEY(MKGKPID-MKGKEY),WKIOKEY                                   
         MVC   MKGKMID,WKIOKEY+(MKGKMID-MKGKEY)                                 
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(L'MKGKEY),IOKEYSAV                                         
         JE    NXTMKG46                                                         
*                                                                               
         XC    IOKEY,IOKEY         Try all client                               
         LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         MVC   MKGKEY(MKGKCLT-MKGKEY),WKIOKEY                                   
         MVC   MKGKMID,WKIOKEY+(MKGKMID-MKGKEY)                                 
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(L'MKGKEY),IOKEYSAV                                         
         JE    NXTMKG46                                                         
*                                                                               
NXTMKG44 MVC   IOKEY,WKIOKEY       Restore market group key and DM seq          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     NXTMKG50                                                         
*                                                                               
NXTMKG46 BRAS  RE,GETMBKLN                                                      
*                                                                               
         CLI   MKGOPT1,MKGOPTCQ    Downloading market group for Optica?         
         JE    NXTMKG47                                                         
*                                                                               
         CLI   FORBYTKR,C'Y'       For BuyTracker?                              
         JNE   NXTMKG44            No, skip the market group                    
*                                                                               
NXTMKG47 MVC   IOKEY,WKIOKEY       Restore market group key and DM seq          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
*                                                                               
         CLI   MBRKLEN,0           Have market break length?                    
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         MVC   MKGBIN_,MKGKMID     Save binary market group code                
         DROP  RE                                                               
*                                                                               
         BRAS  RE,EDTMKG                                                        
         SR    RE,RE                                                            
         ICM   RE,7,QAMKG          Filtering on market group codes?             
         JZ    NXTMKG48                                                         
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q                                                       
*                                                                               
         CLC   MKGCHAR,0(RE)       Mkt group code Match that of filter?         
         JE    NXTMKG48                                                         
         AHI   RE,L'MKGCHAR        Next market code in filtering list           
         JCT   RF,*-14                                                          
         J     NXTMKG50            Try next market group code                   
*                                                                               
NXTMKG48 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   NXTMKG50                                                         
         MVC   LP_ADATA,AMKGREC    Point to market group record                 
         J     EXITY                                                            
*                                                                               
NXTMKG50 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   NOMORE                                                           
         LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         CLC   IOKEY(MKGKCLT-MKGKEY),IOKEYSAV                                   
         JNE   NOMORE                                                           
         CLI   MKGKMID,0           Have market group ID?                        
         JE    NXTMKG50                                                         
         OC    MKGKMGRP,MKGKMGRP   Have market group number?                    
         JZ    NXTMKG50                                                         
         J     NXTMKG40                                                         
         DROP  RE                                                               
*                                                                               
GETMBKLN LR    R0,RE                                                            
         MVI   MBRKLEN,0           Init market break length                     
         MVI   FORBYTKR,0          Init For Buytracker                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   GETMBKLX                                                         
         L     R1,IOADDR                                                        
         LA    R1,(MKGEL-MKGRECD)(R1)                                           
         USING MKGEL01,R1                                                       
         CLI   MKGEL01,X'01'       Market group break description elem?         
         JNE   GETMBKLX                                                         
         LLC   RE,MKGBK1LN                                                      
         LLC   RF,MKGBK2LN                                                      
         AR    RE,RF                                                            
         LLC   RF,MKGBK3LN                                                      
         AR    RE,RF                                                            
         STC   RE,MBRKLEN          Save market break length                     
*                                                                               
         MVC   FORBYTKR,MKGBKBTK   Save For Buytracker flag                     
GETMBKLX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
NXTMK#   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTMK#50                                                         
         XC    MKGMKT#,MKGMKT#     Init market# output field                    
         MVC   WKIOKEY,IOKEY       Save market group key                        
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         MVI   MKGPTYP+0,X'0D'     Build market# passive key                    
         MVI   MKGPTYP+1,X'82'                                                  
         MVC   MKGPAGMD,WKIOKEY+(MKGKAGMD-MKGKEY)                               
         MVC   MKGPCLT,WKIOKEY+(MKGKCLT-MKGKEY)                                 
         MVC   MKGPPID,WKIOKEY+(MKGKPID-MKGKEY)                                 
         MVC   MKGPPGRP,WKIOKEY+(MKGKPGRP-MKGKEY)                               
         MVC   MKGPMID,WKIOKEY+(MKGKMID-MKGKEY)                                 
         MVC   MKGPMGRP,WKIOKEY+(MKGKMGRP-MKGKEY)                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JNE   NXTMK#90                                                         
         J     NXTMK#54                                                         
         DROP  RE                                                               
*                                                                               
NXTMK#50 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   NXTMK#90                                                         
NXTMK#54 LA    RE,IOKEY                                                         
         USING MKGKEY,RE                                                        
         CLC   IOKEY(MKGPMKT-MKGKEY),IOKEYSAV                                   
         JNE   NXTMK#90                                                         
         OC    MKGPMKT,MKGPMKT     Have market#?                                
         JZ    NXTMK#50                                                         
         MVC   MKGMKT#,MKGPMKT     Output market#                               
         MVC   LP_ADATA,AMKGREC    Point to market group record                 
         J     EXITY                                                            
         DROP  RE                                                               
*                                                                               
NXTMK#90 XC    MKGMKT#,MKGMKT#     Init market# output field                    
         MVC   IOKEY,WKIOKEY       Restore market group key and DM seq          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J      NOMORE                                                          
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
EDTMK#   LM    R2,R4,LP_AINP       Edit a client code                           
         OC    0(L'MKGMKT#,R2),0(R2)                                            
         JZ    XCOLEN                                                           
         EDIT  (B2,MKGMKT#),(4,0(R4)),FILL=0                                    
         LA    R0,4                Output length                                
         J     SETOLENX                                                         
                                                                                
EDTCL2   LM    R2,R4,LP_AINP       Edit a client code                           
         OC    0(L'MKGKCLT,R2),0(R2)                                            
         JNZ   EDTCL2H                                                          
         MVC   0(3,R4),=C'ALL'                                                  
EDTCL2D  LA    R0,3                Output length                                
         J     SETOLENX                                                         
EDTCL2H  MVC   SVIOVALS,IOVALS                                                  
K        USING CKEY,IOKEY                                                       
         XC    K.CKEY,K.CKEY                                                    
         MVI   K.CKEYTYPE,CKEYTYPQ                                              
         MVC   K.CKEYAM,QMEDX                                                   
         MVC   K.CKEYCLT,0(R2)                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   EDTCL2N                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO4'                           
         JNE   EDTCL2N                                                          
         L     RF,AIO4                                                          
         USING CLTRECD,RF                                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,CKEYCLT),LP_AOUT                           
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EDTCL2D                                                          
EDTCL2N  MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     XCOLEN                                                           
         DROP  RF                                                               
                                                                                
EDTMKG   LR    R0,RE               Edit market group code                       
         MVC   MKGCHAR,SPACES                                                   
         OC    MKGBIN_(1),MKGBIN_                                               
         JZ    EDTMKG90                                                         
         OC    MKGBIN_+1(2),MKGBIN_+1                                           
         JZ    EDTMKG90                                                         
         CLI   MBRKLEN,0           Have market group break length?              
         JNH   EDTMKG90                                                         
         LA    RE,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
EDTMKG12 CLC   2(1,RE),MKGBIN_                                                  
         JE    EDTMKG18                                                         
         LA    RE,3(RE)                                                         
         JCT   RF,EDTMKG12                                                      
         DC    H'0'                                                             
EDTMKG18 MVC   MKGCHAR(2),0(RE)                                                 
         XC    FULL1,FULL1                                                      
         GOTOR VHEXOUT,DMCB,MKGBIN_+1,FULL1,2,=C'TOG',0                         
         LA    RF,MKGCHAR+2                                                     
         CLI   MKGCHAR+1,C' '                                                   
         JH    *+6                                                              
         BCTR  RF,0                                                             
         LLC   RE,MBRKLEN                                                       
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   0(0,RF),FULL1                                                    
         EX    RE,0(R1)                                                         
EDTMKG90 LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* Client group download                                               *         
***********************************************************************         
                                                                                
REQCLTG  LKREQ H,I#CLTGDL,OUTCLG,NEXTREQ=REQTCML                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
ClGCd    LKREQ F,002,(I,B#SAVED,QCLGIND),CHAR,TEXT=(*,CLGCLIT),        +        
               OLEN=L'CLGCHAR,LIST=F,SORT=N                                     
         LKREQ E                                                                
                                                                                
CLGCLIT  DC    C'Client Group Codes'                                            
                                                                                
OUTCLG   LKOUT H                                                                
         LKOUT R,1                                                              
PRout    LKOUT P,,CLGINI           Init market group array values               
Array    LKOUT C,1,(A,ARYCLG)                                                   
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYCLG   LKOUT A,(R,NXTCLG),MULTIROW=Y,ROWNAME=SP$GRPRECD                       
Media    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CGrCd    LKOUT C,002,(D,B#SAVED,CLGCHAR),CHAR                                   
Array    LKOUT C,010,(A,ARYCBKN)                                                
*                                                                               
Array    LKOUT C,2,(A,ARYCLGC)     List of client codes in group                
*                                                                               
         LKOUT E                                                                
                                                                                
ARYCBKN  LKOUT A,(D,B#CLGREC,SP$GRPEL),EOT=EOR,                        +        
               ROWID=(SP$GRPGRPCD,SP$GRPGRPCQ),ROWWIDTH=(V,SP$GRPGRPLN)         
         LKOUT C,010,SP$GRPGNAM1,CHAR,ND=Y                                      
         LKOUT C,011,SP$GRPGNAM2,CHAR,ND=Y                                      
         LKOUT E                                                                
                                                                                
ARYCLGC  LKOUT A,(R,NXTCGC),MULTIROW=Y,ROWNAME=SP$GRPRECD                       
Array    LKOUT C,2,(A,ARYCX30)                                                  
         LKOUT E                                                                
                                                                                
ARYCX30  LKOUT A,(D,B#CLGREC,SP$GRPEL),EOT=EOR,NEWEL=B,                +        
               ROWID=(SP$GRPVALCD,SP$GRPVALCQ),ROWWIDTH=(V,SP$GRPVALLN)         
         LKOUT C,001,SP$GRPVALUE,CHAR                                           
         LKOUT E                                                                
                                                                                
CLGINI   GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   EXITN                                                            
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCLG   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCLG20                                                         
         CLI   QMEDX,0             Have media?                                  
         JE    NOMORE                                                           
         MVI   CBRKLEN,0           Init saved client grp break length           
         XC    CLGBIN_,CLGBIN_     Init saved binary client grp code            
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING SP$GRPKEY,RE                                                     
         MVI   SP$GRPKTYP,SP$GRPKTYPQ                                           
         MVI   SP$GRPKSTYP,SP$GRPKCTYQ                                          
         MVC   SP$GRPKAGMD,QMEDX                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JNE   NOMORE                                                           
         DROP  RE                                                               
*                                                                               
NXTCLG20 LA    RE,IOKEY                                                         
         USING SP$GRPKEY,RE                                                     
         CLC   IOKEY(SP$GRPKID-SP$GRPKEY),IOKEYSAV                              
         JNE   NOMORE                                                           
         CLI   SP$GRPKID,0         Have client group ID?                        
         JE    NXTCLG50                                                         
         OC    SP$GRPKCODE,SP$GRPKCODE  Are we at definition level?             
         JNZ   NXTCLG40                 No, a real client group                 
         DROP  RE                                                               
*                                                                               
NXTCLG30 BRAS  RE,GETCBKLN                                                      
         J     NXTCLG50                                                         
*                                                                               
NXTCLG40 CLI   FORBYTKR,C'Y'       Client group ID for BuyTracker?              
         JNE   NXTCLG50            No                                           
*                                                                               
         LA    RE,IOKEY                                                         
         USING SP$GRPKEY,RE                                                     
         MVC   CLGBIN_,SP$GRPKID   Save binary client group code                
         DROP  RE                                                               
*                                                                               
         BRAS  RE,EDTCLG                                                        
         SR    RE,RE                                                            
         ICM   RE,7,QACLG          Filtering on client group codes?             
         JZ    NXTCLG44                                                         
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q                                                       
*                                                                               
         CLC   CLGCHAR,0(RE)       Clt group code Match that of filter?         
         JE    NXTCLG44                                                         
         AHI   RE,L'CLGCHAR        Next market code in filtering list           
         JCT   RF,*-14                                                          
         J     NXTCLG50            Try next client group code                   
*                                                                               
NXTCLG44 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   NXTCLG50                                                         
         MVC   IOKEYSAV,IOKEY      For nested array (NXTCGC)                    
         MVC   LP_ADATA,ACLGREC    Point to client group record                 
         J     EXITY                                                            
*                                                                               
NXTCLG50 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   NOMORE                                                           
         J     NXTCLG20                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCGC   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCGC50                                                         
         LA    RE,IOKEY                                                         
         USING SP$GRPKEY,RE                                                     
         CLC   IOKEY(SP$GRPKMSQL),IOKEYSAV                                      
         JNE   NOMORE                                                           
         J     NXTCGC46                                                         
         DROP  RE                                                               
*                                                                               
NXTCGC20 LA    RE,IOKEY                                                         
         USING SP$GRPKEY,RE                                                     
         CLC   IOKEY(SP$GRPKMSQL),IOKEYSAV                                      
         JNE   NOMORE                                                           
         DROP  RE                                                               
*                                                                               
NXTCGC40 SR    RE,RE                                                            
         ICM   RE,7,QACLG          Filtering on client group codes?             
         JZ    NXTCGC44                                                         
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN-LW_D(RE)                                            
         AHI   RE,LW_LN2Q                                                       
*                                                                               
         CLC   CLGCHAR,0(RE)       Clt group code Match that of filter?         
         JE    NXTCGC44                                                         
         AHI   RE,L'CLGCHAR        Next market code in filtering list           
         JCT   RF,*-14                                                          
         J     NXTCGC50            Try next client group code                   
*                                                                               
NXTCGC44 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   NXTCGC50                                                         
NXTCGC46 MVC   LP_ADATA,ACLGREC    Point to client group record                 
         J     EXITY                                                            
*                                                                               
NXTCGC50 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   NOMORE                                                           
         J     NXTCGC20                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCBKLN LR    R0,RE                                                            
         MVI   CBRKLEN,0           Init client group break length               
         MVI   FORBYTKR,0                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   GETCBKLX                                                         
         L     R1,IOADDR                                                        
         LA    R1,(SP$GRPEL-SP$GRPRECD)(R1)                                     
         USING SP$GRPBRKD,R1                                                    
         CLI   SP$GRPBRKCD,SP$GRPBRKCQ                                          
         JNE   GETCBKLX                                                         
         LLC   RE,SP$GRPBK1LN                                                   
         LLC   RF,SP$GRPBK2LN                                                   
         AR    RE,RF                                                            
         STC   RE,CBRKLEN          Save client group break length               
         MVC   FORBYTKR,SP$GRPBKBTK                                             
GETCBKLX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
EDTCLG   LR    R0,RE               Edit client group code                       
         MVC   CLGCHAR,SPACES                                                   
         OC    CLGBIN_(1),CLGBIN_                                               
         JZ    EDTCLG90                                                         
         OC    CLGBIN_+1(2),CLGBIN_+1                                           
         JZ    EDTCLG90                                                         
         CLI   CBRKLEN,0           Have client group break length?              
         JNH   EDTCLG90                                                         
         LA    RE,SPCGRTAB                                                      
EDTCLG12 LHI   RF,(SPCGRTBX-SPCGRTAB)/3                                         
         CLC   2(1,RE),CLGBIN_                                                  
         JE    EDTCLG18                                                         
         LA    RE,3(RE)                                                         
         JCT   RF,EDTCLG12                                                      
         DC    H'0'                                                             
EDTCLG18 MVC   CLGCHAR(2),0(RE)                                                 
         LA    RF,CLGCHAR+2                                                     
         CLI   CLGCHAR+1,C' '                                                   
         JH    *+6                                                              
         BCTR  RF,0                                                             
         SR    R1,R1                                                            
         ICM   R1,B'1100',CLGBIN_+1                                             
         SRL   R1,12                  DD DD ?? ??  =>  00 0D DD D?              
         ST    R1,FULL1                                                         
         OI    FULL1+3,X'0F'          00 0D DD DS                               
         UNPK  DUB1(5),FULL1+1(3)                  =>  Z0 ZD ZD ZD ZD           
         LLC   RE,CBRKLEN                                                       
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   0(0,RF),DUB1+1      Client group code                            
         EX    RE,0(R1)                                                         
EDTCLG90 LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* Traffic commercial record download                                  *         
***********************************************************************         
*                                                                               
REQTCML  LKREQ H,I#TCMLDL,OUTTCM,NEXTREQ=REQTHOU                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(D,B#WORKD,QCLTA),CHAR,TEXT=SP#CLI                         
SDate    LKREQ F,003,(D,B#SAVED,STRDATEB),BDAT,TEXT=SP#STDT,COL=*               
EDate    LKREQ F,004,(D,B#SAVED,ENDDATEB),BDAT,TEXT=SP#ENDT,COL=*               
CmlPrefx LKREQ F,005,(D,B#SAVED,CMLPREFX),CHAR,TEXT=SP#FILM,COL=*               
         LKREQ E                                                                
                                                                                
OUTTCM   LKOUT H                   Traffic commercial output maps start         
                                                                                
         LKOUT R,1                                                              
PRout    LKOUT P,,TCMINI           Init traffic commercial arrary               
Array    LKOUT C,1,(A,ARYTCM)                                                   
Array    LKOUT C,1,(A,ARYTHC)      Hi-Def commercials                           
Array    LKOUT C,2,(A,ARYTCL)                                                   
Array    LKOUT C,3,(A,ARYTPR)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                   Traffic commercial output maps end           
                                                                                
ARYTCM   LKOUT A,(R,NXTTCM),MULTIROW=Y,ROWNAME=CMLRECD                          
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#WORKD,IOKEY),(R,EDTCMC)                               
Array    LKOUT C,004,(A,ARYCPRD)                                                
CmlT1    LKOUT C,005,(D,B#SAVED,SVCMLDS1),CHAR,ND=Y                             
CmlT2    LKOUT C,006,(D,B#SAVED,SVCMLDS2),CHAR,ND=Y                             
CmlT3    LKOUT C,007,(D,B#SAVED,SVCMLDS3),CHAR,ND=Y                             
Array    LKOUT C,008,(A,ARYCMDS)                                                
         LKOUT E                                                                
                                                                                
ARYTHC   LKOUT A,(R,NXTTHC),MULTIROW=Y,ROWNAME=CMLRECD                          
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#WORKD,IOKEY),(R,EDTCMC)                               
Array    LKOUT C,004,(A,ARYCPRD)                                                
CmlT1    LKOUT C,005,(D,B#SAVED,SVCMLDS1),CHAR,ND=Y                             
CmlT2    LKOUT C,006,(D,B#SAVED,SVCMLDS2),CHAR,ND=Y                             
CmlT3    LKOUT C,007,(D,B#SAVED,SVCMLDS3),CHAR,ND=Y                             
Array    LKOUT C,008,(A,ARYCMDS)                                                
         LKOUT E                                                                
                                                                                
ARYCPRD  LKOUT A,(D,B#WORKD,ELEM2),NROWS=255,ROWWIDTH=1,ROWNAME=DUMMY_D         
PrdCd    LKOUT C,004,DUM_LIN1,(R,EDTCPR),ND=Y                                   
         LKOUT E                                                                
                                                                                
ARYCMDS  LKOUT A,(D,B#TCMREC,CMLDTAEL),EOT=EOR,ROWID=(CMLDTAEL,X'10'), +        
               ROWWIDTH=(V,CMLDTALN)                                            
         LKOUT C,008,CMLSLN,UBIN                                                
         LKOUT C,009,CMLRLSE,BDAT                                               
         LKOUT C,010,CMLRCL,BDAT,ND=Y                                           
         LKOUT C,011,(D,B#SAVED,SVCMLUFN),CHAR,ND=Y                             
         LKOUT E                                                                
                                                                                
ARYTCL   LKOUT A,(R,NXTTCL),MULTIROW=Y,ROWNAME=CLTRECD                          
Array    LKOUT C,2,(A,ARYTC1)                                                   
         LKOUT E                                                                
                                                                                
ARYTC1   LKOUT A,(D,B#CLTREC,CLTRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
CltCd    LKOUT C,001,(D,B#WORKD,QCLTA),CHAR                                     
CltNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
         LKOUT E                                                                
                                                                                
ARYTPR   LKOUT A,(R,NXTTPR),MULTIROW=Y,ROWNAME=PRDRECD                          
Array    LKOUT C,3,(A,ARYTP1)                                                   
         LKOUT E                                                                
                                                                                
ARYTP1   LKOUT A,(D,B#PRDREC,PRDRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
PrdCd    LKOUT C,001,(D,B#WORKD,QPRDA),CHAR                                     
PrdNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
TalAg    LKOUT C,003,(D,B#WORKD,DUB2),CHAR,ND=Y                                 
         LKOUT E                                                                
                                                                                
TCMINI   OC    MEDCOD,SPACES                                                    
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   EXITN                                                            
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    Get address of TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         MVI   CMLPRFXL,0                                                       
         CLC   CMLPREFX,SPACES     Any commercial prefix?                       
         JNH   TCMINI20            None                                         
         LA    R1,L'CMLPREFX                                                    
         LA    RE,CMLPREFX+L'CMLPREFX-1                                         
*                                                                               
TCMINI10 CLI   0(RE),C' '          Anything here?                               
         JH    TCMINI15                                                         
         BCTR  RE,0                                                             
         JCT   R1,TCMINI10                                                      
*                                                                               
TCMINI15 STC   R1,CMLPRFXL         We know the length now                       
         CLI   CMLPRFXL,8          Input length >= 8  ?                         
         JNL   TCMINI30                                                         
TCMINI20 LA    RE,1(RE)            Point to the first blank                     
         MVI   0(RE),C'A'          1st 8 chars can't be < C'AAAAAAAA'           
         LA    R1,1(R1)            fill with C'A' until first 8 char            
         CHI   R1,8                   are populated                             
         JL    TCMINI20                                                         
*                                                                               
TCMINI30 GOTOR VTRPACK,DMCB,(C'P',CMLPREFX),CMLPRFXS                            
*                                                                               
         CLI   CMLPRFXL,12         FOR A SPECIFIC 12 CHAR FILM?                 
         JE    TCMINI40                                                         
*                                                                               
         LLC   R1,CMLPRFXL                                                      
         LA    R3,CMLPREFX(R1)                                                  
         LA    RE,12                                                            
         SR    RE,R1               RE=# of C'9's to fill to the end             
         BCTR  RE,0                                                             
         BASR  R4,0                                                             
         MVC   0(0,R3),=12C'9'                                                  
         EX    R3,0(R4)                                                         
*                                                                               
TCMINI40 GOTOR VTRPACK,DMCB,(C'P',CMLPREFX),CMLPRFXE                            
*                                                                               
TCMINIX  J     EXITY                                                            
*                                                                               
EDTCPR   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have binary product code?                    
         JE    XCOLEN                                                           
         XC    0(3,R4),0(R4)       Init output                                  
         CLI   0(R2),X'FF'         'All' product?                               
         JNE   *+14                                                             
         MVC   0(3,R4),=C'ALL'                                                  
         J     EDTCPR42                                                         
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
EDTCPR20 CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    EDTCPR40                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,EDTCPR20                                                      
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
EDTCPR24 CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    EDTCPR40                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,EDTCPR24                                                      
         J     XCOLEN              Bad product code                             
EDTCPR40 MVC   0(3,R4),0(RF)       Get character format for prd code            
EDTCPR42 LA    R0,3                Max length for product code                  
         J     SETOLENX                                                         
         DROP  RE                                                               
*                                                                               
EDTCMC   LM    R2,R4,LP_AINP                                                    
         USING CMLKEY,R2                                                        
         OC    CMLKCML,CMLKCML     Have commercial code?                        
         JZ    XCOLEN                                                           
         XC    0(13,R4),0(R4)                                                   
         MVC   0(L'CMLKCML,R4),CMLKCML                                          
*****    TM    CMLKSTAT,CMLKSTA_PCKD   We're going thru the packed              
*****    JZ    EDTCMC10                 films with 0AC1 & 0Ac2 passives         
         XC    0(13,R4),0(R4)                                                   
         GOTOR VTRPACK,DMCB,(C'U',CMLKCML),0(R4)                                
EDTCMC10 CLI   0(R4),C'A'          Lower case letters?                          
         JNL   EDTCMC30                                                         
         OC    SVCMLAID,SVCMLAID   Have saved Ad-ID?                            
         JZ    EDTCMC30                                                         
         XC    0(13,R4),0(R4)                                                   
         MVC   0(L'SVCMLAID,R4),SVCMLAID                                        
EDTCMC30 LA    R0,L'CMLADID        Max length for commerical code               
         J     SETOLENX                                                         
         DROP  R2                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTCM   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTCMSQ                                                         
         XC    SVCMLFLS(SVCMLFLQ),SVCMLFLS   Init saved commercial flds         
         XC    ELEM1,ELEM1         Table of prd codes to be downloaded          
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLPIDAD+0,X'0A'    Passive keys for packed Ad-IDs               
         MVI   CMLPIDAD+1,X'C1'                                                 
         MVC   CMLPADAM,QMEDX                                                   
         MVC   CMLPADCL,QCLTX                                                   
         MVC   CMLPADID,CMLPRFXS   Starting point                               
*                                                                               
NXTTCMHI GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         DROP  RE                                                               
*                                                                               
NXTTCM10 LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         CLC   IOKEY(CMLPADID-CMLKEY),IOKEYSAV  Same upto client?               
         JNE   NOMORE                           No, then we're done             
         OC    CMLPADID,CMLPADID   Have commercial ID?                          
         JZ    NXTTCMSQ            No                                           
         CLC   =X'C5DCC5DCC5B80000',CMLPADID   House for the Client?            
         JE    NXTTCMSQ              Yes, this is TRPACK of C'99999999'         
*                                                                               
         CLC   CMLPADID,CMLPRFXE   Past the last?                               
         JH    NOMORE              Then no more commercials to look for         
         DROP  RE                                                               
*                                                                               
NXTTCM16 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTTCMSQ                                                         
         XC    SVCMLAID,SVCMLAID   Init saved commercial fields                 
         XC    SVCMLDS1,SVCMLDS1                                                
         XC    SVCMLDS2,SVCMLDS2                                                
         XC    SVCMLDS3,SVCMLDS3                                                
         MVI   SVCMLUFN,0                                                       
         XC    ELEM2,ELEM2         List of prds for this commercial             
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
         USING CMLDTAEL,R1                                                      
         TM    CMLSTAT,X'80'       Deleted cml                                  
         JO    NXTTCMSQ                                                         
         MVC   SVCMLDS1(L'CMLTITLE),CMLTITLE                                    
         OC    STRDATEB(L'STRDATEB*2),STRDATEB   Any date filter?               
         JZ    NXTTCM18                          None, accept all cmmls         
         CLC   ENDDATEB,CMLRLSE    End date filter before release date?         
         JL    NXTTCMSQ            Yes, next record then                        
         CLC   STRDATEB,CMLRCL     Start date after recall date?                
         JH    NXTTCMSQ            Yes, next record then                        
*                                                                               
NXTTCM18 CLC   CMLRCL,=X'FFFFFF'   Recall date 'until further notice'?          
         JNE   NXTTCM20                                                         
         XC    CMLRCL,CMLRCL                                                    
         MVI   SVCMLUFN,YESQ       Set 'until further notice' flag to Y         
         DROP  R1                                                               
*                                                                               
NXTTCM20 L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
NXTTCM23 CLI   0(R1),EOR           End of record?                               
         JE    NXTTCM80                                                         
         CLI   0(R1),X'20'         Product list element?                        
         JE    NXTTCM50                                                         
         CLI   0(R1),X'30'         Extra description element?                   
         JE    NXTTCM30                                                         
         CLI   0(R1),X'A0'         Ad-ID element?                               
         JE    NXTTCM40                                                         
NXTTCMNX LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTTCM23            Bump to next element                         
*                                                                               
         USING CMLDSCEL,R1                                                      
NXTTCM30 CLI   CMLDSCSQ,0                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS1,CMLDSC                                                  
         CLI   CMLDSCSQ,1                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS2,CMLDSC                                                  
         CLI   CMLDSCSQ,2                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS3,CMLDSC                                                  
         J     NXTTCMNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLADIEL,R1                                                      
NXTTCM40 MVC   SVCMLAID,CMLADID    Ad-Id                                        
         J     NXTTCMNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLPRDEL,R1                                                      
NXTTCM50 CLI   CMLPRDS,X'FF'       All products?                                
         JNE   *+8                                                              
         OI    SVCMLFLG,CML_ALLQ   Set to download all products                 
*                                                                               
         LLC   RF,CMLPRDLN                                                      
         AHI   RF,-2               Number of binary product codes               
         CHI   RF,0                                                             
         JNH   NXTTCMNX                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),CMLPRDS   Save list of binary prod codes                
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,ELEM2            Current prds for this commercial             
NXTTCM52 CLI   0(RF),X'00'         End of current prds list?                    
         JE    NXTTCMNX                                                         
         CLI   0(RF),X'FF'         All product?                                 
         JE    NXTTCMNX                                                         
*                                                                               
         LA    RE,ELEM1            List of prds to be downloaded later          
NXTTCM54 CLI   0(RE),X'00'         Blank entry?                                 
         JE    NXTTCM56                                                         
         CLC   0(1,RF),0(RE)       Already in table?                            
         JE    NXTTCM58                                                         
         AHI   RE,1                                                             
         J     NXTTCM54                                                         
NXTTCM56 MVC   0(1,RE),0(RF)                                                    
NXTTCM58 AHI   RF,1                Bump to next current prd list                
         J     NXTTCM52                                                         
         DROP  R1                                                               
*                                                                               
NXTTCM80 DS    0H                  To extract more fields if needed             
*                                                                               
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
NXTTCMSQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         J     NXTTCM10                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTHC   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTHCSQ                                                         
         XC    SVCMLFLS(SVCMLFLQ),SVCMLFLS   Init saved commercial flds         
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLHDF+0,X'0A'     Passive for HiDef commercials                 
         MVI   CMLHDF+1,X'C2'                                                   
         MVC   CMLHDFAM,QMEDX                                                   
         MVC   CMLHDFCL,QCLTX                                                   
         MVC   CMLHDFID,CMLPRFXS                                                
*                                                                               
NXTTHCHI GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         DROP  RE                                                               
*                                                                               
NXTTHC10 LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         CLC   IOKEY(CMLHDFID-CMLKEY),IOKEYSAV  Same upto the client?           
         JNE   NOMORE                          No, we're done                   
         OC    CMLHDFID,CMLHDFID   Have commercial ID?                          
         JZ    NXTTHCSQ            No                                           
         CLC   =X'C5DCC5DCC5B80000',CMLHDFID   House for the Client?            
         JE    NXTTCMSQ              Yes, this is TRPACK of C'99999999'         
*                                                                               
         CLC   CMLHDFID,CMLPRFXE   Past the last?                               
         JH    NOMORE              Then no more commercials to look for         
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTTHCSQ                                                         
         XC    SVCMLAID,SVCMLAID   Init saved commercial fields                 
         XC    SVCMLDS1,SVCMLDS1                                                
         XC    SVCMLDS2,SVCMLDS2                                                
         XC    SVCMLDS3,SVCMLDS3                                                
         MVI   SVCMLUFN,0                                                       
         XC    ELEM2,ELEM2         List of prds for this commercial             
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
         USING CMLDTAEL,R1                                                      
         TM    CMLSTAT,X'80'       Deleted cml?                                 
         JO    NXTTHCSQ                                                         
         MVC   SVCMLDS1(L'CMLTITLE),CMLTITLE                                    
         OC    STRDATEB(L'STRDATEB*2),STRDATEB   Any date filter?               
         JZ    NXTTHC15                          None, accept all cmmls         
         CLC   ENDDATEB,CMLRLSE    End date filter before release date?         
         JL    NXTTHCSQ            Yes, next record then                        
         CLC   STRDATEB,CMLRCL     Start date after recall date?                
         JH    NXTTHCSQ            Yes, next record then                        
*                                                                               
NXTTHC15 CLC   CMLRCL,=X'FFFFFF'   Recall date 'until further notice'?          
         JNE   NXTTHC20                                                         
         XC    CMLRCL,CMLRCL                                                    
         MVI   SVCMLUFN,YESQ       Set 'until further notice' flag to Y         
         DROP  R1                                                               
*                                                                               
NXTTHC20 L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
NXTTHC23 CLI   0(R1),EOR           End of record?                               
         JE    NXTTHC80                                                         
         CLI   0(R1),X'20'         Product list element?                        
         JE    NXTTHC50                                                         
         CLI   0(R1),X'24'         Extended data element?                       
         JE    NXTTHC40                                                         
         CLI   0(R1),X'30'         Extra description element?                   
         JE    NXTTHC30                                                         
         CLI   0(R1),X'A0'         Ad-ID element?                               
         JE    NXTTHC40                                                         
NXTTHCNX LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTTHC23            Bump to next element                         
*                                                                               
         USING CMLDSCEL,R1                                                      
NXTTHC30 CLI   CMLDSCSQ,0                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS1,CMLDSC                                                  
         CLI   CMLDSCSQ,1                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS2,CMLDSC                                                  
         CLI   CMLDSCSQ,2                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS3,CMLDSC                                                  
         J     NXTTHCNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLADIEL,R1                                                      
NXTTHC40 MVC   SVCMLAID,CMLADID    Ad-Id                                        
         J     NXTTHCNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLPRDEL,R1                                                      
NXTTHC50 CLI   CMLPRDS,X'FF'       All products?                                
         JNE   *+8                                                              
         OI    SVCMLFLG,CML_ALLQ   Set to download all products                 
*                                                                               
         LLC   RF,CMLPRDLN                                                      
         AHI   RF,-2               Number of binary product codes               
         CHI   RF,0                                                             
         JNH   NXTTHCNX                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),CMLPRDS   Save list of binary prod codes                
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,ELEM2            Current prds for this commercial             
NXTTHC52 CLI   0(RF),X'00'         End of current prds list?                    
         JE    NXTTHCNX                                                         
         CLI   0(RF),X'FF'         All product?                                 
         JE    NXTTHCNX                                                         
*                                                                               
         LA    RE,ELEM1            List of prds to be downloaded later          
NXTTHC54 CLI   0(RE),X'00'         Blank entry?                                 
         JE    NXTTHC56                                                         
         CLC   0(1,RF),0(RE)       Already in table?                            
         JE    NXTTHC58                                                         
         AHI   RE,1                                                             
         J     NXTTHC54                                                         
NXTTHC56 MVC   0(1,RE),0(RF)                                                    
NXTTHC58 AHI   RF,1                Bump to next current prd list                
         J     NXTTHC52                                                         
         DROP  R1                                                               
*                                                                               
NXTTHC80 DS    0H                  To extract more fields if needed             
*                                                                               
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
NXTTHCSQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         J     NXTTHC10                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTCL   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTCL30                                                         
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2         Init client name for output                  
         MVC   WORK2(L'CNAME),CNAME                                             
         J     NXTTCL70                                                         
         DROP  RE                                                               
*                                                                               
NXTTCL30 DS    0H                  To process more client rec if needed         
         J     NOMORE                                                           
*                                                                               
NXTTCL70 DS    0H                  To extract more fields if needed             
*                                                                               
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTTPR   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTPR30                                                         
         TM    SVCMLFLG,CML_ALLQ   Download all products?                       
         JZ    NXTTPR18                                                         
NXTTPR10 L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    ELEM1,ELEM1         Init table of binary product codes           
         LA    R1,CLIST                                                         
         LA    R2,ELEM1                                                         
         LA    RF,220                                                           
NXTTPR12 CLI   3(R1),0             No more binary product code?                 
         JE    NXTTPR18                                                         
         MVC   0(1,R2),3(R1)       Save binary product code to look up          
         AHI   R2,1                                                             
         AHI   R1,4                                                             
         JCT   RF,NXTTPR12                                                      
         LA    R1,CLIST2           2nd product list                             
         LA    RF,35                                                            
NXTTPR14 CLI   3(R1),0             No more binary product code?                 
         JE    NXTTPR18                                                         
         MVC   0(1,R2),3(R1)       Save binary product code to look up          
         AHI   R2,1                                                             
         AHI   R1,4                                                             
         JCT   RF,NXTTPR14                                                      
NXTTPR18 LA    R1,ELEM1                                                         
         MVI   ELEM1+255,X'FF'     Mark end of binary prd codes table           
         ST    R1,FULL1            Address of binary prd codes to proc          
         DROP  RE                                                               
*                                                                               
NXTTPR30 L     R2,FULL1            Point to binary prd table                    
         CLI   0(R2),X'FF'         End of table?                                
         JE    NOMORE                                                           
         CLI   0(R2),X'00'         Blank entry?                                 
         JE    NXTTPR38                                                         
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST            Point to 1st list of products                
         LA    R1,220                                                           
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTTPR36                                                         
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         LA    RF,CLIST2           Point to 2nd list of products                
         LA    R1,35                                                            
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTTPR36                                                         
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         DROP  RE                                                               
*                                                                               
NXTTPR36 XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING PKEY,RE                                                          
         MVI   PKEYTYPE,PKEYTYPQ                                                
         MVC   PKEYAM,QMEDX                                                     
         MVC   PKEYCLT,QCLTX                                                    
         MVC   PKEYPRD,0(RF)       Character format prd code from table         
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(L'PKEY),IOKEYSAV                                           
         JNE   NXTTPR38                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   NXTTPR38                                                         
         AHI   R2,1                Point to next entry in table                 
         ST    R2,FULL1            For next round                               
         J     NXTTPR50            Now extract prd fields for output            
*                                                                               
NXTTPR38 AHI   R2,1                Point to next entry in table                 
         ST    R2,FULL1                                                         
         J     NXTTPR30            Get next product record                      
*                                                                               
NXTTPR50 L     R1,IOADDR                                                        
         USING PRDHDR,R1                                                        
         XC    QCLTA,QCLTA                                                      
         XC    QPRDA,QPRDA         Return prd code - character                  
         XC    WORK2,WORK2         Return prd name                              
         XC    DUB2,DUB2           Return Talent agency code                    
         MVC   QPRDA,PKEYPRD                                                    
         MVC   WORK2(L'PNAME),PNAME                                             
         MVC   DUB2(L'PTALAGY),PTALAGY                                          
*                                                                               
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,QCLTX),QCLTA                               
         DROP  R1,R2                                                            
*                                                                               
NXTTPR70 DS    0H                  To extract more fields if needed             
*                                                                               
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Traffic production house record download                            *         
***********************************************************************         
                                                                                
REQTHOU  LKREQ H,I#THOUDL,OUTTPH,NEXTREQ=REQTRCP                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(D,B#WORKD,QCLTA),CHAR,TEXT=SP#CLI                         
         LKREQ E                                                                
                                                                                
OUTTPH   LKOUT H                   Traffic prod house output maps start         
         LKOUT R,1                                                              
PRout    LKOUT P,,TPHINI           Init traffic prod house arrary               
Array    LKOUT C,1,(A,ARYTPH)                                                   
         LKOUT X                   Traffic prod house output maps end           
                                                                                
TPHINI   OC    MEDCOD,SPACES                                                    
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,AGYMEDX                          
         JNE   EXITN                                                            
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         MVC   TPHRECQ,=X'0A29'    Traffic prod house record code               
         J     EXITY                                                            
                                                                                
ARYTPH   LKOUT A,(R,NXTTPH),ROWNAME=PRHRECD                                     
Media    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
PHoID    LKOUT C,002,PRHKPRH,CHAR                                               
Array    LKOUT C,003,(A,ARYPHAD)                                                
Array    LKOUT C,007,(A,ARYPHFX)                                                
Array    LKOUT C,012,(A,ARYPHOP)                                                
Array    LKOUT C,014,(A,ARYPHCM)                                                
         LKOUT E                                                                
                                                                                
ARYPHAD  LKOUT A,(D,B#TPHREC,PRHDTAEL),EOT=EOR,ROWID=(PRHDTAEL,X'10'), +        
               ROWWIDTH=(V,PRHDTALN)                                            
         LKOUT C,003,PRHLINE1,CHAR,ND=Y                                         
         LKOUT C,004,PRHLINE2,CHAR,ND=Y                                         
         LKOUT C,005,PRHLINE3,CHAR,ND=Y                                         
         LKOUT C,006,PRHLINE4,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPHFX  LKOUT A,(D,B#TPHREC,PRHDTAEL),EOT=EOR,ROWID=(PRHFAXEL,X'20'), +        
               ROWWIDTH=(V,PRHFAXLN)                                            
         LKOUT C,007,PRHFTEL1,CHAR,ND=Y                                         
         LKOUT C,008,PRHFTELA,CHAR,ND=Y                                         
         LKOUT C,009,PRHFTELE,CHAR,ND=Y                                         
         LKOUT C,010,PRHFTELN,CHAR,ND=Y                                         
         LKOUT C,011,PRHFTELX,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYPHOP  LKOUT A,(D,B#TPHREC,PRHDTAEL),EOT=EOR,ROWID=(PRHOPCEL,X'25'), +        
               ROWWIDTH=(V,PRHOPCLN)                                            
         LKOUT C,012,PRHOPC,CHAR,ND=Y                                           
         LKOUT C,013,PRHDELAY,UBIN                                              
         LKOUT E                                                                
                                                                                
ARYPHCM  LKOUT A,(D,B#TPHREC,PRHDTAEL),EOT=EOR,ROWID=(PRHCMTEL,X'40'), +        
               ROWWIDTH=(V,PRHCMTLN)                                            
         LKOUT C,014,PRHCMTNO,UBIN,ND=Y                                         
         LKOUT C,015,PRHCMT,CHAR,LEN=V,ND=Y                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Send production house values                                        *         
***********************************************************************         
NXTTPH   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTTPH50                                                         
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLKID+0,X'0A'                                                   
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,QMEDX                                                     
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLKCML,=C'99999999'  99999999 Commercial tells us house         
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                          No, we're done                   
*                                                                               
         CLC   IOKEY(CMLKSTAT-CMLKEY),IOKEYSAV Exact match?                     
         JNE   NOMORE                          No, we're done                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NOMORE                                                           
*                                                                               
         L     RE,IOADDR                                                        
         USING CMLKEY,RE                                                        
         MVC   HOUSECD,CMLTITLE                                                 
         OC    HOUSECD,SPACES                                                   
         DROP  RE                                                               
*                                                                               
NXTTPH50 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',TPHKEYT),('B#TPHREC',0), +        
               ('$NXTRTRF',SAVED),0,0                                           
         JNE   EXITY                                                            
         L     R2,IOADDR           Put filtering logic here...                  
         USING PRHRECD,R2                                                       
         J     EXITY               Exit to send prod house values               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Traffic Ship Recap record download                                            
***********************************************************************         
*                                                                               
REQTRCP  LKREQ H,I#TRCPDL,OUTRCP,NEXTREQ=REQT0PR                                
MedCd    LKREQ F,001,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,MAXLEN=L'QMEDX,TEXT=SP#MED,COL=*                    
CltCd    LKREQ F,002,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),            +        
               OLEN=L'QCLTX,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                    
PrdCode  LKREQ F,003,(D,B#SAVED,PRDCODE),CHAR,TEXT=SP#PRO                       
SDate    LKREQ F,004,(D,B#SAVED,STRDATEB),BDAT,TEXT=SP#STDT,COL=*               
EDate    LKREQ F,005,(D,B#SAVED,ENDDATEB),BDAT,TEXT=SP#ENDT,COL=*               
         LKREQ E                                                                
                                                                                
OUTRCP   LKOUT H                   Traffic Ship Recap output maps               
                                                                                
         LKOUT R,X'0023'                                                        
PRout    LKOUT P,,TRCPINI          Init traffic Ship Recap arrary               
Array    LKOUT C,X'0023',(A,ARYTRCP)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                  Traffic ship recap output maps end            
                                                                                
ARYTRCP  LKOUT A,(R,NXTRCP),MULTIROW=Y,ROWNAME=SHPRECD                          
ARRAY    LKOUT C,X'0023',(A,ARYTRCE)                                            
Array    LKOUT C,X'0023',(A,ARYSCML),FILTROUT=TSTQEST,SKIPCOLS=CTASKIPS         
PRout    LKOUT P,,SETEST0                                                       
         LKOUT E                                                                
*                                                                               
ARYTRCE  LKOUT A,(D,B#TRCREC,SHPDTAEL),NEWEL=Y,EOT=EOR,                +        
               ROWID=(SHPDTAEL,X'10'),ROWWIDTH=(V,SHPDTALN)                     
PRout    LKOUT P,SHPDTAEL,SETCML                                                
Array    LKOUT C,X'0023',(A,ARYTRCC),FILTROUT=TSTCML1                           
Array    LKOUT C,X'0023',(A,ARYTRCA),FILTROUT=TSTADID                           
Array    LKOUT C,X'0023',(A,ARYTRCH),FILTROUT=TSTHIDF                           
Array    LKOUT C,X'0023',(A,ARYTRC2),FILTROUT=TSTCML2                           
Array    LKOUT C,X'0023',(A,ARYTRCA),FILTROUT=TSTADID                           
Array    LKOUT C,X'0023',(A,ARYTRCH),FILTROUT=TSTHIDF                           
         LKOUT E                                                                
*                                                                               
ARYTRCC  LKOUT A,(D,B#SAVED,REQVALS),NEWEL=Y,NROWS=1                            
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#SAVED,SVSHCML1),(R,EDCML)                             
SDate    LKOUT C,004,(D,B#SAVED,STRDATEB),BDAT                                  
EDate    LKOUT C,005,(D,B#SAVED,ENDDATEB),BDAT                                  
Station  LKOUT C,006,(D,B#SAVED,STATION),CHAR                                   
Estimate LKOUT C,011,(D,B#WORKD,QEST),CHAR,ND=Y                                 
         LKOUT E                                                                
*                                                                               
ARYTRCA  LKOUT A,(D,B#SAVED,REQVALS),NEWEL=Y,NROWS=1                            
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#SAVED,SVCMLAD),CHAR                                   
SDate    LKOUT C,004,(D,B#SAVED,STRDATEB),BDAT                                  
EDate    LKOUT C,005,(D,B#SAVED,ENDDATEB),BDAT                                  
Station  LKOUT C,006,(D,B#SAVED,STATION),CHAR                                   
Estimate LKOUT C,011,(D,B#WORKD,QEST),CHAR,ND=Y                                 
         LKOUT E                                                                
*                                                                               
ARYTRCH  LKOUT A,(D,B#SAVED,REQVALS),NEWEL=Y,NROWS=1                            
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#SAVED,SVCMLHD),CHAR                                   
SDate    LKOUT C,004,(D,B#SAVED,STRDATEB),BDAT                                  
EDate    LKOUT C,005,(D,B#SAVED,ENDDATEB),BDAT                                  
Station  LKOUT C,006,(D,B#SAVED,STATION),CHAR                                   
Estimate LKOUT C,011,(D,B#WORKD,QEST),CHAR,ND=Y                                 
         LKOUT E                                                                
*                                                                               
ARYTRC2  LKOUT A,(D,B#SAVED,REQVALS),NEWEL=Y,NROWS=1                            
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#SAVED,SVSHCML2),(R,EDCML)                             
SDate    LKOUT C,004,(D,B#SAVED,STRDATEB),BDAT                                  
EDate    LKOUT C,005,(D,B#SAVED,ENDDATEB),BDAT                                  
Station  LKOUT C,006,(D,B#SAVED,STATION),CHAR                                   
Estimate LKOUT C,011,(D,B#WORKD,QEST),CHAR,ND=Y                                 
         LKOUT E                                                                
*                                                                               
CTASKIP  EQU   *                                                                
ARYSCML  LKOUT A,(R,NXTEST),MULTIROW=Y,ROWNAME=SHPRECD                          
Array    LKOUT C,X'0023',(A,ARYTRCC),FILTROUT=TSTCML1                           
Array    LKOUT C,X'0023',(A,ARYTRCA),FILTROUT=TSTADID                           
Array    LKOUT C,X'0023',(A,ARYTRCH),FILTROUT=TSTHIDF                           
Array    LKOUT C,X'0023',(A,ARYTRC2),FILTROUT=TSTCML2                           
Array    LKOUT C,X'0023',(A,ARYTRCA),FILTROUT=TSTADID                           
Array    LKOUT C,X'0023',(A,ARYTRCH),FILTROUT=TSTHIDF                           
CTASKIPS EQU   (*-CTASKIP)/LX_COLSL                                             
         LKOUT E                                                                
*                                                                               
TRCPINI  DS    0H                                                               
         MVI   QESTX,0             Init estimate number                         
*                                                                               
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA  Values disappeared          
         GOTOR (#EDTCLT,AEDTCLT),DMCB,QCLTX,,QCLTA                              
*                                                                               
         L     R0,AIO8                                                          
         L     R1,AIO8                                                          
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R1,AIO8                                                          
         ST    R1,ANXTCML          Set cml pointer to start of list             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    Get address of TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         MVC   TRCRECQ,=X'0A25'    Traffic ship recap rec                       
         J     EXITY                                                            
*                                                                               
TSTCML1  DS    0H                                                               
         XC    SVCMLAD,SVCMLAD     Init adid commercial field                   
         XC    SVCMLHD,SVCMLHD     Init hidef commercial field                  
         OC    SVSHCML1,SVSHCML1   Do we have cml1?                             
         J     SETCCC                                                           
*                                                                               
TSTADID  DS    0H                                                               
         OC    SVCMLAD,SVCMLAD     Do we have an AD-ID?                         
         J     SETCCC                                                           
*                                                                               
TSTHIDF  DS    0H                                                               
         OC    SVCMLHD,SVCMLHD     Do we have a HiDef?                          
         J     SETCCC                                                           
*                                                                               
TSTCML2  DS    0H                                                               
         XC    SVCMLAD,SVCMLAD     Init adid commercial field                   
         XC    SVCMLHD,SVCMLHD     Init hidef commercial field                  
         OC    SVSHCML2,SVSHCML2   Do we have cml2?                             
         J     SETCCC                                                           
*                                                                               
TSTQEST  DS    0H                                                               
         CLI   QESTX,0              Traffic by flight?                          
         J     SETCCC                                                           
*                                                                               
*                                                                               
***********************************************************************         
* Send ship recap values                                                        
***********************************************************************         
NXTRCP   XC    SVSHCML1,SVSHCML1                                                
         XC    SVSHCML2,SVSHCML2                                                
         MVI   SVCMLFL1,0                                                       
*                                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTRCP50                                                         
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING SHPKEY,RE                                                        
         MVI   SHPKID+0,X'0A'        Traffic ship recap                         
         MVI   SHPKID+1,X'25'                                                   
         MVC   SHPKAM,QMEDX                                                     
         MVC   SHPKCLT,QCLTX                                                    
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
*                                                                               
NXTRCP10 CLC   IOKEY(SHPKMKT-SHPKEY),IOKEYSAV Match on a/m/clt                  
         JNE   NOMORE              No, we're done                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NOMORE              No, we're done                               
         J     NXTRCPX                                                          
*                                                                               
NXTRCP50 DS    0H                                                               
         MVC   IOKEY,SVKEY                                                      
         LA    RE,IOKEY                                                         
         USING SHPKEY,RE                                                        
                                                                                
         MVC   SVMKSTA,SHPKMKT     Save this market station                     
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3' DUMMY READ HI              
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
*                                                                               
         LA    RE,IOKEY                                                         
         USING SHPKEY,RE                                                        
                                                                                
         CLC   SVMKSTA,SHPKMKT     Same market station?                         
         JE    NXTRCP10                                                         
         DROP  RE                                                               
*                                                                               
         L     R0,AIO8                                                          
         L     R1,AIO8             clear list of cml                            
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,AIO8                                                          
         ST    R1,ANXTCML          Set cml pointer to start of list             
*                                                                               
         J     NXTRCP10                                                         
*                                                                               
NXTRCPX  MVC   SVKEY,IOKEY         Save recap key                               
         MVC   LP_ADATA,ARCPREC    Point to traffic ship recap record           
         J     EXITY               Exit to send net recap values                
         EJECT                                                                  
*                                                                               
*                                                                               
* Set for next estimate in the same record/element                              
NXTEST   L     R3,AIO3                                                          
         MVI   ELCODE,X'10'        Estimate element                             
         BRAS  RE,GETEL                                                         
         JNE   SETCMLX                                                          
*                                                                               
         ST    R3,LP_AINP          Need to fix this for later                   
         J     SETCML                                                           
*                                                                               
* All 255 estimates processed for this record                                   
* Re-init estimate to zero                                                      
SETEST0  MVC   LP_ADATA,ARCPREC    Point to traffic ship recap record           
         MVI   QESTX,0             Re-init the binary estimate                  
         J     EXITY                                                            
*                                                                               
*                                                                               
SETCML   XC    SVSHCML1,SVSHCML1                                                
         XC    SVSHCML2,SVSHCML2                                                
         XC    QEST,QEST                                                        
         MVI   SVCMLFL1,0                                                       
*                                                                               
         L     R1,LP_AINP                                                       
         USING SHPDTAEL,R1                                                      
*                                                                               
* See if within requested dates                                                 
         CLC   SHPFTD,ENDDATEB     FTD to end date                              
         JH    SETCMLX                                                          
         CLC   SHPLTD,STRDATEB     LTD to start date                            
         JL    SETCMLX                                                          
*                                                                               
         MVC   LP_ADATA,ARCPREC    Point to traffic ship recap record           
         CLI   QESTX,X'FF'         Est=255?                                     
         JNL   NOMORE              Yes, done                                    
         BRAS  RE,TESTEST          Test for estimate mask if any                
         JNE   *+12                Estimate found                               
         CLI   0(R3),X'20'         Est not found, but was est element?          
         JE    SETCMLX             Yes, done                                    
         CLI   QESTX,0             Traffic by flight?                           
         JE    SETCML10            Yes, continue w/o estimate                   
*                                                                               
         EDIT  QESTX,QEST,ZERO=BLANK,ALIGN=LEFT  Printable estimate             
*                                                                               
SETCML10 OC    PRDCODE,PRDCODE     Any prd entered?                             
         JNZ   SETCML20                                                         
         MVC   SVSHCML1(16),SHPCMML Cml1 and cml2                               
         MVC   SVCMLFL1,SHPNOSHP   Save flag                                    
         J     SETCMLEQ                                                         
*                                                                               
SETCML20 XC    SVBPRD,SVBPRD                                                    
         BRAS  RE,GETBPRD          Get product binary value                     
         JNE   SETCMLX                                                          
         XC    WORK(16),WORK                                                    
         MVC   WORK(16),SHPCMML    Cml1 and cml2                                
*                                                                               
* Read commercial record see if match on product                                
         LA    R4,WORK                                                          
         BRAS  RE,RDCML            Read commercial record for prod              
         JNE   *+10                                                             
         MVC   SVSHCML1,WORK       Save cml1                                    
                                                                                
         LA    R4,WORK+8                                                        
         OC    0(8,R4),0(R4)                                                    
         JZ    SETCML25                                                         
         BRAS  RE,RDCML            Read commercial record for prod              
         JNE   SETCML25                                                         
         MVC   SVSHCML2,WORK       Save cml2                                    
                                                                                
SETCML25 OC    SVSHCML1(16),SVSHCML1 Any commercials found?                     
         JZ    SETCMLX                                                          
         OC    SVSHCML1,SVSHCML1                                                
         JNZ   SETCML30                                                         
         MVC   SVSHCML1,SVSHCML2                                                
         XC    SVSHCML2,SVSHCML2                                                
*                                                                               
SETCML30 MVC   SVCMLFL1,SHPNOSHP     Save flag                                  
*                                                                               
* Code below keeps track of commercials that were already sent                  
* in order to eliminate duplicates                                              
*                                                                               
* Add cml to list if not already there                                          
SETCMLEQ LA    RE,SVSHCML1                                                      
*                                                                               
SETCML50 L     R1,AIO8                                                          
         C     R1,ANXTCML          start of list = end of list                  
         JE    SETCML58                                                         
*                                                                               
SETCML55 CLC   0(L'SVSHCML1,RE),0(R1)  compare comml                            
         JNE   *+14                                                             
         CLC   8(1,R1),QESTX          and estimate no.                          
         JE    SETCML60                                                         
         AHI   R1,9                Bump to next entry                           
         C     R1,ANXTCML          End of list                                  
         JL    SETCML55                                                         
SETCML58 MVC   0(L'SVSHCML1,R1),0(RE)  Save commercial in list                  
         MVC   8(L'QESTX,R1),QESTX  Save estimate no.                           
         AHI   R1,L'SVSHCML1+1     Bump to next available entry                 
         ST    R1,ANXTCML          Set end of list                              
*                                                                               
         OC    SVSHCML2,SVSHCML2   Any p/b cml?                                 
         JZ    SETCM100                                                         
*                                                                               
         CLC   SVSHCML2,0(RE)      Same cml?                                    
         JE    SETCM100                                                         
*                                                                               
         LA    RE,SVSHCML2                                                      
         J     SETCML50                                                         
*                                                                               
SETCML60 DS    0H                                                               
         CLC   0(L'SVSHCML1,RE),SVSHCML2  Same cmls?                            
         JE    SETCML62                                                         
*                                                                               
         XC    0(L'SVSHCML1,RE),0(RE) Clear cml                                 
         OC    SVSHCML2,SVSHCML2   Any p/b cml?                                 
         JZ    SETCMLX                                                          
         MVC   SVSHCML1,SVSHCML2                                                
         XC    SVSHCML2,SVSHCML2                                                
         J     SETCML50                                                         
*                                                                               
SETCML62 XC    0(L'SVSHCML1,RE),0(RE) Clear cml                                 
         J     SETCMLX             No output                                    
                                                                                
SETCM100 XC    DMCB(24),DMCB                                                    
         XC    DMCB(24),DMCB                                                    
         XC    DUB,DUB                                                          
         MVC   IOKEY,SVKEY         Restore recap key                            
         LA    R4,IOKEY                                                         
         USING SHPKEY,R4                                                        
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,QMEDA                                                    
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,SHPKMKT                                                 
         GOTOR VSTAPACK,STAPACKD                                                
         MVC   STATION,STAPQSTA                                                 
         CLI   QMEDA,C'T'                                                       
         JNE   EXITY                                                            
         CLI   STATION+4,C'T'      Is it TV                                     
         JNE   EXITY               No, leave it in                              
         MVI   STATION+4,C' '      Else remove                                  
         DROP  R4                                                               
         J     EXITY                                                            
*                                                                               
SETCMLX  MVC   LP_ADATA,ARCPREC    Point to traffic ship recap record           
         J     XCOLEN                                                           
*                                                                               
* Test bitmask to see if estimate is there                                      
*                                                                               
TESTEST  NTR1  LABEL=*                                                          
         L     R3,AIO3                                                          
         MVI   ELCODE,X'20'        Estimate element                             
         BRAS  RE,GETEL                                                         
         JNE   ESTNO                                                            
*                                                                               
         LLC   R1,QESTX                                                         
         AHI   R1,1                                                             
         STC   R1,QESTX                                                         
*                                                                               
         USING SHPESTEL,R3                                                      
TEST10   SR    R0,R0                                                            
         LLC   R0,QESTX            Estimate                                     
         LR    RF,R0                                                            
         SR    RE,RE                                                            
         SLDL  RE,29               Get mask byte no. into RE                    
         SRL   RF,29               Get mask bit no. into RF                     
         LA    R1,SHPESMSK(RE)     R1 = A(correct byte in mask)                 
         LA    RF,BITLIST(RF)                                                   
         MVC   BYTE,0(R1)          Test for this estimate                       
         NC    BYTE,0(RF)                                                       
         CLI   BYTE,0              Any estimate?                                
         JNE   ESTYES               Yes                                         
*                                                                               
         CLI   QESTX,X'FF'         Est=255?                                     
         JE    ESTNO               Done                                         
         LLC   R1,QESTX                                                         
         AHI   R1,1                                                             
         STC   R1,QESTX                                                         
         J     TEST10                                                           
*                                                                               
ESTNO    CR    R1,R1               Set cc                                       
ESTYES   XIT1  REGS=(R3)                                                        
*                                                                               
* Get product binary value                                                      
*                                                                               
GETBPRD  NTR1  LABEL=*                                                          
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
GETPRD20 CLC   PRDCODE,0(RF)       Match that of product list?                  
         JE    GETPRD40                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,GETPRD20                                                      
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
GETPRD24 CLC   PRDCODE,0(RF)       Match that of product list?                  
         JE    GETPRD40                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,GETPRD24                                                      
         JNE   EXITN                                                            
                                                                                
GETPRD40 MVC   SVBPRD,3(RF)        Save binary prod code                        
         J     EXITY                                                            
         DROP  RE                                                               
*                                                                               
* See if requested prod is for this commerial                                   
*                                                                               
RDCML    NTR1  LABEL=*                                                          
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLKID+0,X'0A'                                                   
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,QMEDX                                                     
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLKCML,0(R4)         Commercial                                 
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO6'                            
         CLC   IOKEY(L'CMLKEY),IOKEYSAV Exact match?                            
         JE    NXTEL05                                                          
*                                                                               
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   CMLKID+1,X'C1'                                                   
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO6'                            
         CLC   IOKEY(L'CMLKEY),IOKEYSAV Exact match?                            
         JNE   EXITN                           No, we're done                   
*                                                                               
NXTEL05  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO6'                           
         JNE   EXITN                                                            
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
NXTEL10  CLI   0(R1),EOR           End of record?                               
         JE    NXTTCM80                                                         
         CLI   0(R1),X'20'         Binary product list element?                 
         JE    NXTEL30                                                          
         CLI   0(R1),X'29'         3 char product list element?                 
         JE    NXTEL40                                                          
NXTEL20  LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTEL10             Bump to next element                         
*                                                                               
         USING CMLPRDEL,R1                                                      
NXTEL30  CLI   CMLPRDS,X'FF'       All products?                                
         JE    NXTEL80                                                          
*                                                                               
         LLC   RF,CMLPRDLN                                                      
         SHI   RF,2                Number of binary product codes               
         CHI   RF,0                                                             
         JNH   NXTEL20                                                          
*                                                                               
         LA    RE,2(R1)            Point to first prod                          
*                                                                               
NXTEL32  CLC   SVBPRD,0(RE)        Match on prod?                               
         JE    NXTEL80                                                          
*                                                                               
         LA    RE,1(RE)                                                         
         JCT   RF,NXTEL32                                                       
         J     EXITN                                                            
*                                                                               
         USING CMLMPREL,R1                                                      
NXTEL40  CLI   CMLMPRS,X'FF'       All products?                                
         JE    NXTEL80                                                          
*                                                                               
         LLC   RF,CMLMPRLN                                                      
         SHI   RF,2                Number of product codes                      
         CHI   RF,0                                                             
         JNH   NXTEL20                                                          
*                                                                               
         LA    RE,2(R1)            Point to first prod                          
*                                                                               
NXTEL42  CLC   PRDCODE,0(RE)       Match on prod?                               
         JE    NXTEL80                                                          
*                                                                               
         LA    RE,3(RE)                                                         
         SHI   RF,2                                                             
         JCT   RF,NXTEL42                                                       
         J     EXITN                                                            
*                                                                               
NXTEL80  J     EXITY                                                            
*                                                                               
EDCML    LM    R2,R4,LP_AINP                                                    
*                                                                               
         XC    SVCMLAD,SVCMLAD     Init adid commercial field                   
         XC    SVCMLHD,SVCMLHD     Init hidef commercial field                  
*                                                                               
         XC    0(13,R4),0(R4)                                                   
         MVC   0(L'SVSHCML1,R4),0(R2)  Cml                                      
         TM    SVCMLFL1,CMLADSW        Packed commercial ?                      
         JZ    EDCML01                                                          
         XC    0(13,R4),0(R4)                                                   
         GOTOR VTRPACK,DMCB,(C'U',0(R2)),0(R4)                                  
*                                                                               
EDCML01  LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLKID+0,X'0A'     Commercial record                             
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,QMEDX                                                     
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLKCML,0(R2)                                                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO6'                            
         CLC   IOKEY(L'CMLKEY),IOKEYSAV                                         
         JE    EDCML01C                                                         
*                                                                               
         LA    RE,IOKEY                                                         
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   CMLKID+1,X'C1'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO6'                            
         CLC   IOKEY(L'CMLKEY),IOKEYSAV                                         
         JNE   EDCML10                                                          
*                                                                               
         DROP  RE                                                               
*                                                                               
EDCML01C GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO6'                           
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
EDCML02  CLI   0(R1),EOR           End of record?                               
         JE    EDCML10                                                          
         CLI   0(R1),X'24'         Hidef element?                               
         JE    EDCML05                                                          
         CLI   0(R1),X'A0'         Ad-id element?                               
         JE    EDCML04                                                          
EDCMLNXT LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     EDCML02             Bump to next element                         
*                                                                               
         USING CMLADIEL,R1                                                      
EDCML04  CLC   CMLADID,0(R4)       Ad-ID is same as primary film?               
         JNE   EDCML04C             No, go save it                              
         L     R6,IOADDR                                                        
         CLC   CMLADIDP,5(R6)      Compare the packed values                    
         JE    EDCML10             Yes, no need to set SVCMLAD                  
*                                                                               
         MVC   SVCMLAD(8),5(R6)    Save 8 char iscii                            
         XC    SVCMLAD,0(R4)       and swap adid and iscii                      
         XC    0(12,R4),SVCMLAD    in the fields                                
         XC    SVCMLAD,0(R4)                                                    
         J     EDCML10                                                          
*                                                                               
EDCML04C MVC   SVCMLAD,CMLADID     Ad-id                                        
         J     EDCML10                                                          
         DROP  R1                                                               
*                                                                               
         USING CMLXDTEL,R1                                                      
EDCML05  MVC   SVCMLHD,CMLXHDEF    Hidef                                        
         J     EDCMLNXT            bump to next element                         
*                                                                               
EDCML10  LA    R0,12               Max length for commerical code               
         J     SETOLENX                                                         
         DROP  R1                                                               
*                                                                               
***********************************************************************         
* Traffic profile records download                                              
***********************************************************************         
*                                                                               
REQT0PR  LKREQ H,I#T0PRDL,OUTT0P,NEXTREQ=REQCLAS                                
MedCd    LKREQ F,001,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,MAXLEN=L'QMEDX,TEXT=SP#MED,COL=*                    
CltList  LKREQ F,002,(D,B#SAVED,CLTLIST),CHAR,TEXT=SP#CLI                       
         LKREQ E                                                                
                                                                                
OUTT0P   LKOUT H                   Spot Traffic prof output maps                
                                                                                
         LKOUT R,X'0030'                                                        
PRout    LKOUT P,,TPRINI           Init traffic prof array                      
Array    LKOUT C,X'0030',(A,ARYT0PR)                                            
Array    LKOUT C,X'0030',(A,ARYT1PR)                                            
Array    LKOUT C,X'0030',(A,ARYT2PR)                                            
Array    LKOUT C,X'0030',(A,ARYT3PR)                                            
Array    LKOUT C,X'0030',(A,ARYTTPR)                                            
Array    LKOUT C,X'0030',(A,ARYTWPR)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                  Traffic prof output maps end                  
                                                                                
ARYT0PR  LKOUT A,(R,NXTPRT0),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),LBIN,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYT1PR  LKOUT A,(R,NXTPRT1),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),LBIN,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),LBIN,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYT2PR  LKOUT A,(R,NXTPRT2),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYT3PR  LKOUT A,(R,NXTPRT3),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYTTPR  LKOUT A,(R,NXTPRTT),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),LBIN,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),LBIN,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),LBIN,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
ARYTWPR  LKOUT A,(R,NXTPRTW),MULTIROW=Y,ROWNAME=DUMMY_D                         
PROFCD   LKOUT C,001,(D,B#WORKD,FULL),CHAR,LEN=3                                
         LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT C,011,(D,B#WORKD,WORK+00),CHAR,LEN=1,ND=Y                        
         LKOUT C,012,(D,B#WORKD,WORK+01),CHAR,LEN=1,ND=Y                        
         LKOUT C,013,(D,B#WORKD,WORK+02),CHAR,LEN=1,ND=Y                        
         LKOUT C,014,(D,B#WORKD,WORK+03),CHAR,LEN=1,ND=Y                        
         LKOUT C,015,(D,B#WORKD,WORK+04),CHAR,LEN=1,ND=Y                        
         LKOUT C,016,(D,B#WORKD,WORK+05),CHAR,LEN=1,ND=Y                        
         LKOUT C,017,(D,B#WORKD,WORK+06),CHAR,LEN=1,ND=Y                        
         LKOUT C,018,(D,B#WORKD,WORK+07),CHAR,LEN=1,ND=Y                        
         LKOUT C,019,(D,B#WORKD,WORK+08),CHAR,LEN=1,ND=Y                        
         LKOUT C,020,(D,B#WORKD,WORK+09),CHAR,LEN=1,ND=Y                        
         LKOUT C,021,(D,B#WORKD,WORK+10),CHAR,LEN=1,ND=Y                        
         LKOUT C,022,(D,B#WORKD,WORK+11),CHAR,LEN=1,ND=Y                        
         LKOUT C,023,(D,B#WORKD,WORK+12),CHAR,LEN=1,ND=Y                        
         LKOUT C,024,(D,B#WORKD,WORK+13),CHAR,LEN=1,ND=Y                        
         LKOUT C,025,(D,B#WORKD,WORK+14),CHAR,LEN=1,ND=Y                        
         LKOUT C,026,(D,B#WORKD,WORK+15),CHAR,LEN=1,ND=Y                        
         LKOUT E                                                                
                                                                                
TPRINI   DS    0H                                                               
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA  Values disappeared          
*                                                                               
         OC    CLTLIST,CLTLIST                                                  
         JZ    NOMORE                                                           
*                                                                               
         L     R0,AIO8                                                          
         L     R1,AIO8                                                          
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,AIO8                                                          
         LA    R0,L'CLTLIST                                                     
         ST    R1,ACLTLIST                                                      
         AR    R1,R0                                                            
         ST    R1,ACLTLSTX                                                      
*                                                                               
         LA    R2,CLTLIST                                                       
         BRAS  RE,BCLTLIST         Build client list                            
                                                                                
         L     R2,ACLTLIST         Point to 1st client                          
         BRAS  RE,GCLTOFF          Get  client office                           
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* Get Traffic profile for all clients                                           
***********************************************************************         
                                                                                
NXTPRT0  LA    RF,PROFTAB0         Point to Traffic T0 profile table            
         J     NXTPRFT                                                          
                                                                                
NXTPRT1  LA    RF,PROFTBL1         Point to Traffic T1 profile table            
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
         J     NXTPRCLT                                                         
                                                                                
NXTPRT2  LA    RF,PROFTAB2         Point to Traffic T2 profile table            
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
         J     NXTPRCLT                                                         
                                                                                
NXTPRT3  LA    RF,PROFTAB3         Point to Traffic T3 profile table            
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
         J     NXTPRCLT                                                         
                                                                                
NXTPRTW  LA    RF,PROFTABW         Point to Traffic TW profile table            
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
         J     NXTPRCLT                                                         
                                                                                
NXTPRTT  LA    RF,PROFTABT         Point to Traffic TT profile table            
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPRFT                                                          
                                                                                
NXTPRCLT L     R2,ACLTLIST                                                      
         MVC   QCLTA,0(R2)                                                      
         LA    R2,L'QCLTA(R2)                                                   
         ST    R2,ACLTNXT                                                       
                                                                                
NXTPRFT  XC    FULL,FULL           Init output profile name                     
         XC    WORK,WORK           Init output profile values                   
                                                                                
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JE    NXTPFT12                                                         
                                                                                
         L     R2,ACLTNXT                                                       
         L     R0,ACLTLSTX         End of client list                           
         CR    R2,R0                                                            
         JNL   NOMORE                                                           
                                                                                
         CLC   0(L'QCLTA,R2),SPACES                                             
         JNH   NOMORE                                                           
         BRAS  RE,GCLTOFF          Get  client office                           
                                                                                
NXTPFT12 MVC   FULL(4),0(RF)       Set output profile name                      
         XC    TEMP,TEMP                                                        
         MVC   TEMP+00(2),=C'S0'                                                
         MVC   TEMP+02(2),0(RF)                                                 
         MVC   TEMP+04(L'AGENCY),AGENCY                                         
         MVC   TEMP+06(L'QMEDA),QMEDA                                           
         MVC   TEMP+07(L'QCLTA),QCLTA                                           
         MVI   TEMP+10,C'*'                                                     
         MVC   TEMP+11(L'SVCLTOFF),SVCLTOFF                                     
         GOTOR VGETPROF,DMCB,(FULL+3,TEMP),WORK,VDATAMGR                        
         MVC   LP_ADATA,AOFCREC                                                 
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* Traffic COMCLASS record download                                              
***********************************************************************         
*                                                                               
REQCLAS  LKREQ H,I#TCLADL,OUTCLA,NEXTREQ=REQCTXT                                
MedCd    LKREQ F,001,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,MAXLEN=L'QMEDX,TEXT=SP#MED,COL=*                    
CltList  LKREQ F,002,(D,B#SAVED,CLTLIST),CHAR,TEXT=SP#CLI                       
PrdList  LKREQ F,003,(D,B#SAVED,PRDLIST),CHAR,TEXT=SP#PRO                       
Claslist LKREQ F,004,(D,B#SAVED,CLSLIST),CHAR,TEXT=(*,CLASLIT)                  
         LKREQ E                                                                
                                                                                
CLASLIT  DC    C'Class'                                                         
                                                                                
OUTCLA   LKOUT H                   Traffic COMCLASS output maps                 
                                                                                
         LKOUT R,X'0025'                                                        
PRout    LKOUT P,,TCLAINI          Init traffic COMCLASS array                  
Array    LKOUT C,X'0025',(A,ARYTCLA)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                  Traffic ship recap output maps end            
                                                                                
ARYTCLA  LKOUT A,(R,NXTCLA),MULTIROW=Y,ROWNAME=CLSRECD                          
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
ComClas  LKOUT C,002,(D,B#SAVED,SVCLASS),CHAR                                   
CltCd    LKOUT C,003,(D,B#SAVED,SVCLT),CHAR,ND=Y                                
PrdCode  LKOUT C,004,(D,B#SAVED,PRDCODE),CHAR,ND=Y                              
Descrip  LKOUT C,005,(D,B#SAVED,SVDESC),CHAR,ND=Y                               
         LKOUT E                                                                
*                                                                               
TCLAINI  DS    0H                                                               
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA  Values disappeared          
*                                                                               
         OC    CLTLIST,CLTLIST                                                  
         JZ    NOMORE                                                           
*                                                                               
         L     R0,AIO8                                                          
         L     R1,AIO8                                                          
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,AIO8                                                          
         LA    R0,L'CLTLIST                                                     
         ST    R1,ACLTLIST                                                      
         AR    R1,R0                                                            
         ST    R1,ACLTLSTX                                                      
*                                                                               
         LA    R1,2(R1)                                                         
         LA    R0,L'BCLLIST        binary clt list                              
         ST    R1,ABCLLIST                                                      
         AR    R1,R0                                                            
         ST    R1,ABCLLSTX                                                      
*                                                                               
         LA    R1,2(R1)                                                         
         LA    R0,L'PRDLIST                                                     
         ST    R1,APRDLIST                                                      
         AR    R1,R0                                                            
         ST    R1,APRDLSTX                                                      
*                                                                               
         LA    R1,2(R1)                                                         
         ST    R1,ACLSLIST                                                      
         LA    R0,L'CLSLIST                                                     
         AR    R1,R0                                                            
         ST    R1,ACLSLSTX                                                      
*                                                                               
         L     R0,AIO8                                                          
         AHI   R0,IO8LQ                                                         
*                                                                               
         CR    R1,R0                                                            
         JNH   *+6                                                              
         DC    H'0'                tables overflow                              
*                                                                               
         LA    R2,CLTLIST                                                       
         BRAS  RE,BCLTLIST         Build client list                            
                                                                                
         L     R2,ACLTLIST         Point to 1st client                          
         BRAS  RE,BBCLTLST         Build binary client list                     
*                                                                               
         OC    PRDLIST,PRDLIST                                                  
         JZ    TCLA10                                                           
         CLC   =C'PRD=ALL',PRDLIST                                              
         JE    TCLA10                                                           
*                                                                               
         LA    R2,PRDLIST                                                       
         BRAS  RE,BPRODUCT         Build product list                           
*                                                                               
TCLA10   OC    CLSLIST,CLSLIST                                                  
         JZ    TCLA20                                                           
         LA    R2,CLSLIST                                                       
         BRAS  RE,BCLSLIST         Build class list                             
                                                                                
TCLA20   XC    TEMP1,TEMP1                                                      
*                                                                               
         MVC   TCLRECQ,=X'0A44'    COMCLASS record                              
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* Send COMCLASS values                                                          
***********************************************************************         
NXTCLA   XC    SVCLASS,SVCLASS                                                  
         XC    SVDESC,SVDESC                                                    
         XC    SVCLT,SVCLT                                                      
         XC    PRDCODE,PRDCODE                                                  
*                                                                               
         OC    QMEDX,QMEDX         Was media entered?                           
         JZ    NOMORE                                                           
*                                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCLA50                                                         
*                                                                               
NXTCLA04 XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CLSKEY,R4                                                        
         MVI   CLSKID+0,X'0A'        Traffic COMCLASS record (x'0A44')          
         MVI   CLSKID+1,X'44'                                                   
         MVC   CLSKAM,QMEDX                                                     
*                                                                               
NXTCLA05 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
*                                                                               
NXTCLA10 CLC   IOKEY(CLSKCLAS-CLSKEY),IOKEYSAV Match on a/m                     
         JNE   NOMORE              No, we're done                               
*                                                                               
         L     R2,ABCLLIST         1st binary client                            
         OC    CLTLIST,CLTLIST     Any client entered?                          
         JNZ   NXTCLA12                                                         
*                                                                               
         OC    PRDLIST,PRDLIST     Any product                                  
         JZ    NXTCLA14                                                         
         CLC   =C'PRD=ALL',PRDLIST     Prd=all                                  
         JE    NXTCLA14                                                         
         J     EXITN               If product entered, client required          
*                                                                               
NXTCLA12 OC    CLSKCLT,CLSKCLT     Any client                                   
         JZ    NXTCLA14                                                         
*                                                                               
NXTCLA13 CLC   CLSKCLT,0(R2)       Match on client                              
         JE    NXTCLA14                                                         
         LA    R2,L'QCLTX(R2)      Bump in binary list                          
         L     R0,ABCLLSTX         End of client list                           
         CR    R2,R0                                                            
         JNL   NXTCLSEQ            Yes, get next record                         
         OC    0(L'QCLTX,R2),0(R2)                                              
         JZ    NXTCLSEQ                                                         
         J     NXTCLA13                                                         
*                                                                               
NXTCLA14 OC    CLSLIST,CLSLIST     Class entered                                
         JZ    NXTCLA20                                                         
*                                                                               
         L     R2,ACLSLIST                                                      
NXTCLA16 CLC   CLSKCLAS,0(R2)      Match on class                               
         JE    NXTCLA20                                                         
         LA    R2,L'SVCLASS(R2)    Bump in list                                 
         L     R0,ACLSLSTX         End of list                                  
         CR    R2,R0                                                            
         JNL   NXTCLSEQ            Yes, get next record                         
         OC    0(L'SVCLASS,R2),0(R2)                                            
         JZ    NXTCLSEQ                                                         
         J     NXTCLA16                                                         
*                                                                               
NXTCLA20 OC    CLSKPROD,CLSKPROD                                                
         JZ    NXTCLAG                                                          
*                                                                               
         OC    PRDLIST,PRDLIST     Any product                                  
         JZ    NXTCLAG                                                          
*                                                                               
         CLC   =C'PRD=ALL',PRDLIST     Prd=all                                  
         JE    NXTCLAG                                                          
*                                                                               
         L     R2,APRDLIST                                                      
NXTCLA22 CLC   CLSKPROD,0(R2)      Match on prod                                
         JE    NXTCLAG                                                          
         LA    R2,L'PRDCODE(R2)    Bump in list                                 
         L     R0,APRDLSTX         End of list                                  
         CR    R2,R0                                                            
         JNL   NXTCLSEQ            Yes, get next record                         
         OC    0(L'PRDCODE,R2),0(R2)                                            
         JZ    NXTCLSEQ                                                         
         J     NXTCLA22                                                         
*                                                                               
NXTCLA50 DS    0H                                                               
         MVC   IOKEY,SVKEY                                                      
         LA    R4,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3' DUMMY READ HI              
NXTCLSEQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         J     NXTCLA10                                                         
*                                                                               
NXTCLAG  DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NOMORE                                                           
                                                                                
         L     R3,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
                                                                                
         USING CLSDSCEL,R3                                                      
         MVC   SVDESC,CLSDESC                                                   
         DROP  R3                                                               
                                                                                
         MVC   SVKEY,IOKEY         Save COMCLASS key                            
         MVC   SVCLASS,CLSKCLAS    Save class                                   
         OC    CLSKPROD,CLSKPROD   Any prod?                                    
         JZ    *+10                                                             
         MVC   PRDCODE,CLSKPROD    Save product                                 
*                                                                               
         OC    CLSKCLT,CLSKCLT     Any client?                                  
         JZ    NXTCLAX                                                          
                                                                                
         ST    R3,SVR3                                                          
         L     R3,ACLTREC                                                       
         USING CLTRECD,R3                                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,CLSKCLT),SVCLT                             
*****    GOTOR VCLUNPK,DMCB,CLSKCLT,SVCLT                                       
         DROP  R3                                                               
         L     R3,SVR3                                                          
*                                                                               
NXTCLAX  MVC   LP_ADATA,ATCLSREC   Point to COMCLASS record                     
         J     EXITY               Exit to send COMCLASS values                 
*                                                                               
         DROP  R4                                                               
****     GETEL R3,24,ELCODE                                                     
*                                                                               
*----------------------------------------------------                           
* Get client office and save address of next client                             
*----------------------------------------------------                           
*                                                                               
GCLTOFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   QCLTA,0(R2)                                                      
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
                                                                                
         LA    R2,L'QCLTA(R2)                                                   
         ST    R2,ACLTNXT                                                       
                                                                                
         XC    SVCLTOFF,SVCLTOFF                                                
                                                                                
         L     RF,ACLTREC                                                       
         MVC   SVCLTOFF,COFFICE-CLTRECD(RF)                                     
*                                                                               
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* Traffic comment (COMTEXT) record download                                     
***********************************************************************         
*                                                                               
REQCTXT  LKREQ H,I#CTXTDL,OUTCTX,NEXTREQ=REQCML                                 
MedCd    LKREQ F,001,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,MAXLEN=L'QMEDX,TEXT=SP#MED,COL=*                    
CltList  LKREQ F,002,(D,B#SAVED,CLTLIST),CHAR,TEXT=SP#CLI                       
PrdList  LKREQ F,003,(D,B#SAVED,PRDLIST),CHAR,TEXT=SP#PRO                       
Comml    LKREQ F,004,(D,B#SAVED,CMLCODE),CHAR,TEXT=SP#FILM                      
         LKREQ E                                                                
                                                                                
OUTCTX   LKOUT H                   Traffic COMTEXT output maps                  
                                                                                
         LKOUT R,X'0028'                                                        
PRout    LKOUT P,,TCTXINI          Init traffic COMTEXT array                   
Array    LKOUT C,X'0028',(A,ARYTCTX)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                  Traffic ship recap output maps end            
                                                                                
ARYTCTX  LKOUT A,(R,NXTCTX),MULTIROW=Y,ROWNAME=CMTRECD                          
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#SAVED,SVCLT),CHAR                                     
PrdCode  LKOUT C,003,(D,B#SAVED,PRDCODE),CHAR,ND=Y                              
Comml    LKOUT C,004,(D,B#SAVED,CMLCODE),CHAR                                   
Market   LKOUT C,005,(D,B#SAVED,MKTCODE),CHAR,ND=Y                              
Station  LKOUT C,006,(D,B#SAVED,STACODE),CHAR,ND=Y                              
Txtlin1  LKOUT C,007,(D,B#SAVED,SVTXT1),CHAR,ND=Y                               
         LKOUT E                                                                
*                                                                               
TCTXINI  DS    0H                                                               
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA  Values disappeared          
*                                                                               
         OC    CLTLIST,CLTLIST                                                  
         JZ    NOMORE                                                           
*                                                                               
         OC    CMLCODE,CMLCODE                                                  
         JZ    NOMORE                                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    Get address of TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         GOTOR VTRPACK,DMCB,(C'P',CMLCODE),CMLCODEP                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D9000A'      Get address of MSUNPK                  
         MVI   DMCB+7,QMSUNPK                                                   
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VMSUNPK,0(R1)                                                    
*                                                                               
         L     R0,AIO8                                                          
         L     R1,AIO8                                                          
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,AIO8                                                          
         ST    R1,ACLTLIST                                                      
         LA    R0,L'CLTLIST                                                     
         AR    R1,R0                                                            
         ST    R1,ACLTLSTX                                                      
*                                                                               
         LA    R1,2(R1)                                                         
         LA    R0,L'BCLLIST        binary clt list                              
         ST    R1,ABCLLIST                                                      
         AR    R1,R0                                                            
         ST    R1,ABCLLSTX                                                      
*                                                                               
         LA    R1,2(R1)                                                         
         LA    R0,L'PRDLIST                                                     
         ST    R1,APRDLIST                                                      
         AR    R1,R0                                                            
         ST    R1,APRDLSTX                                                      
*                                                                               
         LA    R1,2(R1)                                                         
         LA    R0,PRDNUM           L'binary prd list                            
         ST    R1,ABPRLIST                                                      
         AR    R1,R0                                                            
         ST    R1,ABPRLSTX                                                      
*                                                                               
         L     R0,AIO8                                                          
         AHI   R0,IO8LQ                                                         
*                                                                               
         CR    R1,R0                                                            
         JNH   *+6                                                              
         DC    H'0'                tables overflow                              
*                                                                               
         LA    R2,CLTLIST                                                       
         BRAS  RE,BCLTLIST         Build client list                            
                                                                                
         L     R2,ACLTLIST         Point to 1st client                          
         ST    R2,ACLTNXT                                                       
         BRAS  RE,BBCLTLST         Build binary client list                     
*                                                                               
         L     R2,ACLTLIST                                                      
         BRAS  RE,GCLTREC          Get 1st client record                        
*                                                                               
         OC    PRDLIST,PRDLIST                                                  
         JZ    TCTX10                                                           
         CLC   =C'PRD=ALL',PRDLIST                                              
         JE    TCTX10                                                           
*                                                                               
         LA    R2,PRDLIST                                                       
         BRAS  RE,BPRODUCT         Build product list                           
*                                                                               
         L     R2,APRDLIST                                                      
         BRAS  RE,BBPRODCT         Build binary product list                    
*                                                                               
TCTX10   DS    0H                                                               
         MVC   TCLRECQ,=X'0A35'    COMTEXT record                               
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* Send commercial comments (COMTEXT) values                                     
***********************************************************************         
NXTCTX   DS    0H                                                               
         XC    SVTXT1,SVTXT1       Comment text line 1                          
*                                                                               
         OC    QMEDX,QMEDX         Was media entered?                           
         JZ    NOMORE                                                           
*                                                                               
         OC    CLTLIST,CLTLIST     Was client entered?                          
         JZ    NOMORE                                                           
*                                                                               
NXTCTX02 CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCTX50                                                         
*                                                                               
         L     R2,ABCLLIST         Binary client list                           
         ST    R2,ABCLNXT                                                       
*                                                                               
NXTCTX03 L     R2,ABCLNXT          Point to binary client                       
         BRAS  RE,GCMLSEQ          Get cml seq for this clt/cml                 
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CMTKEY,R4                                                        
         MVI   CMTKID+0,X'0A'        Traffic COMTEXT record (x'0A35')           
         MVI   CMTKID+1,X'35'                                                   
         MVC   CMTKAM,QMEDX                                                     
         MVC   CMTKCLT,0(R2)                                                    
*                                                                               
NXTCTX05 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
*                                                                               
NXTCTX10 CLC   IOKEY(2),=X'0A35'   Match on record type                         
         JNE   NXTCTX13                                                         
*                                                                               
         CLC   CMTKAM,IOKEYSAV+(CMTKAM-CMTKEY) Match on a/m                     
         JNE   NOMORE              No, we're done                               
*                                                                               
         CLC   CMTKCLT,IOKEYSAV+(CMTKCLT-CMTKEY) Match on clt                   
         JE    NXTCTX14                                                         
*                                                                               
NXTCTX13 LA    R2,L'QCLTX(R2)      Bump in binary list                          
         ST    R2,ABCLNXT                                                       
         L     R0,ABCLLSTX         End of client list                           
         CR    R2,R0                                                            
         JNL   NOMORE              Yes, done                                    
         OC    0(L'QCLTX,R2),0(R2)                                              
         JZ    NOMORE                                                           
*                                                                               
         L     R2,ACLTNXT                                                       
         LA    R2,L'QCLTA(R2)                                                   
         ST    R2,ACLTNXT                                                       
         BRAS  RE,GCLTREC          Get client record                            
*                                                                               
         OC    PRDLIST,PRDLIST                                                  
         JZ    NXTCTX03                                                         
         CLC   =C'PRD=ALL',PRDLIST                                              
         JE    NXTCTX03                                                         
*                                                                               
         L     R2,APRDLIST                                                      
         BRAS  RE,BBPRODCT         Build binary product list                    
         J     NXTCTX03                                                         
*                                                                               
NXTCTX14 DS    0H                                                               
         CLC   CMTKSEQ,SVCMLSEQ     Match on cml seq                            
         JNE   NXTCTSEQ                                                         
*                                                                               
         OC    PRDLIST,PRDLIST     Any product                                  
         JZ    NXTCTXG                                                          
*                                                                               
         CLC   =C'PRD=ALL',PRDLIST     Prd=all                                  
         JE    NXTCTXG                                                          
*                                                                               
         CLI   CMTKPRD,0           Any prod on record                           
         JE    NXTCTXG                                                          
*                                                                               
         L     R2,ABPRLIST                                                      
NXTCTX22 CLC   CMTKPRD,0(R2)       Match on prod                                
         JE    NXTCTXG                                                          
         LA    R2,L'SVBPRD(R2)     Bump in list                                 
         L     R0,ABPRLSTX         End of list                                  
         CR    R2,R0                                                            
         JNL   NXTCTSEQ            Yes, get next record                         
         OC    0(L'SVBPRD,R2),0(R2)                                             
         JZ    NXTCTSEQ                                                         
         J     NXTCTX22                                                         
*                                                                               
NXTCTX50 DS    0H                                                               
         MVC   IOKEY,SVKEY                                                      
         L     R2,ABCLNXT                                                       
         LA    R4,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3' DUMMY READ HI              
NXTCTSEQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         J     NXTCTX10                                                         
*                                                                               
NXTCTXG  DS    0H                                                               
         XC    SVCLT,SVCLT         Init client code                             
         XC    PRDCODE,PRDCODE          product code                            
         XC    MKTCODE,MKTCODE          market                                  
         XC    STACODE,STACODE          station                                 
         XC    SVTXT1,SVTXT1            comment line 1                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NOMORE                                                           
                                                                                
         L     R3,AIO3                                                          
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
                                                                                
         USING CMTTXTEL,R3                                                      
         LLC   R1,CMTTXTLN                                                      
         SHI   R1,4                                                             
         MVC   SVTXT1(L'SVTXT1),SPACES                                          
         BASR  RE,0                                                             
         MVC   SVTXT1(0),CMTTXT                                                 
         EX    R1,0(RE)                                                         
         DROP  R3                                                               
                                                                                
         MVC   SVKEY,IOKEY         Save COMTEXT key                             
                                                                                
         CLI   CMTKPRD,0                                                        
         JE    NXTCTG05                                                         
                                                                                
         LA    R2,CMTKPRD                                                       
         BRAS  RE,GPRODC           Get prod code                                
                                                                                
NXTCTG05 DS    0H                                                               
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,CMTKCLT),SVCLT                             
                                                                                
         OC    CMTKMKT,CMTKMKT                                                  
         JZ    NXTCTG08                                                         
         LA    R2,CMTKMKT                                                       
         BRAS  RE,GMKT             Get market                                   
                                                                                
NXTCTG08 OC    CMTKSTA,CMTKSTA                                                  
         JZ    NXTCTXX                                                          
         LA    R2,CMTKSTA                                                       
         BRAS  RE,GSTA             Get station                                  
                                                                                
NXTCTXX  MVC   LP_ADATA,ACTXTREC   Point to COMTEXT record                      
         J     EXITY               Exit to send COMTEXT values                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         GETEL R3,24,ELCODE                                                     
*                                                                               
*----------------------------------------------------                           
* Get commercial sequence number for requested cml                              
*----------------------------------------------------                           
*                                                                               
GCMLSEQ  NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLKID+0,X'0A'                                                   
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,QMEDX                                                     
         MVC   CMLKCLT,0(R2)                                                    
         MVC   CMLKCML,CMLCODEP                                                 
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO6'                            
         CLC   IOKEY(L'CMLKEY),IOKEYSAV Exact match?                            
         JE    GCML05                                                           
*                                                                               
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   CMLKID+1,X'C1'                                                   
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO6'                            
         CLC   IOKEY(L'CMLKEY),IOKEYSAV Exact match?                            
         JNE   NOMORE                          No, we're done                   
*                                                                               
GCML05   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO6'                           
         JNE   EXITN                                                            
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
GCML10   CLI   0(R1),EOR           End of record?                               
         JE    GCMLX                                                            
         CLI   0(R1),X'10'         Binary product list element?                 
         JE    GCML30                                                           
GCML20   LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     GCML10              Bump to next element                         
*                                                                               
         USING CMLDTAEL,R1                                                      
GCML30   MVC   SVCMLSEQ,CMLSEQ+1   Save cml seq                                 
*                                                                               
GCMLX    J     EXITY                                                            
         DROP  R1                                                               
*                                                                               
*----------------------------------------------------                           
* Build client list                                                             
*----------------------------------------------------                           
*                                                                               
BCLTLIST NTR1  BASE=*,LABEL=*                                                   
         XC    TEMP1,TEMP1                                                      
         LA    RF,L'CLTLIST(R2)    Get input length                             
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         SR    RF,R2                                                            
         LA    RF,1(RF)            Actual input len                             
         STC   RF,TEMP1+5          Fake header field                            
                                                                                
         MVC   TEMP1+8(L'CLTLIST),CLTLIST                                       
*                                                                               
         GOTOR VSCANNER,DMCB,TEMP1,AIO7                                         
         LLC   R3,DMCB+4           Get number of blocks                         
         LTR   R3,R3                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R3,CLTCNT           Save number of clients in list               
*                                                                               
* Build list of clients each entry 3 bytes                                      
         L     R2,AIO7                                                          
         L     RF,ACLTLIST         Output area                                  
BCL10    MVC   0(L'QCLTA,RF),12(R2) 2 or 3 bytes clt code                       
         LA    RF,L'QCLTA(RF)                                                   
         LA    R2,32(R2)           Bump to next client                          
         JCT   R3,BCL10                                                         
*                                                                               
         L     R0,ACLTLSTX                                                      
         CR    RF,R0                                                            
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXITY                                                            
*                                                                               
*----------------------------------------------------                           
* Build binary client list                                                      
*----------------------------------------------------                           
*                                                                               
BBCLTLST NTR1  BASE=*,LABEL=*                                                   
         L     R3,ABCLLIST         Binary client list area                      
                                                                                
BBCL10   MVC   QCLTA,0(R2)                                                      
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
                                                                                
         MVC   0(L'QCLTX,R3),QCLTX Save binary client                           
                                                                                
         LA    R3,L'QCLTX(R3)      Bump in binary list                          
         LA    R2,L'QCLTA(R2)      Bump in alpha list                           
         L     R0,ABCLLSTX         End of client list                           
         CR    R3,R0                                                            
         JNL   EXIT                                                             
         L     R0,ACLTLSTX         End of client list                           
         CR    R2,R0                                                            
         JNL   EXIT                                                             
                                                                                
         CLC   0(L'QCLTA,R2),SPACES Any more clts?                              
         JH    BBCL10                                                           
         J     EXIT                                                             
*                                                                               
*----------------------------------------------------                           
* Get client record                                                             
*----------------------------------------------------                           
*                                                                               
GCLTREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   QCLTA,0(R2)                                                      
         OC    QCLTA,SPACES                                                     
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         J     EXIT                                                             
*                                                                               
*----------------------------------------------------                           
* Build comclass list                                                           
*----------------------------------------------------                           
*                                                                               
BCLSLIST NTR1  BASE=*,LABEL=*                                                   
         XC    TEMP1,TEMP1                                                      
         LA    RF,L'CLSLIST(R2)    Get input length                             
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         SR    RF,R2                                                            
         LA    RF,1(RF)            Actual input len                             
         STC   RF,TEMP1+5          Fake header field                            
                                                                                
         MVC   TEMP1+8(L'CLSLIST),CLSLIST                                       
*                                                                               
         GOTOR VSCANNER,DMCB,TEMP1,AIO7                                         
         LLC   R3,DMCB+4           Get number of blocks                         
         LTR   R3,R3                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R3,CLSCNT           Save number of comclass in list              
*                                                                               
* Build list of comclass each entry 4 bytes                                     
         L     R2,AIO7                                                          
         L     RF,ACLSLIST         Output area                                  
BCLS10   MVC   0(L'SVCLASS,RF),12(R2) 4 bytes class code                        
         LA    RF,L'SVCLASS(RF)                                                 
         LA    R2,32(R2)           Bump to next class                           
         JCT   R3,BCLS10                                                        
*                                                                               
         L     R0,ACLSLSTX                                                      
         CR    RF,R0                                                            
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXITY                                                            
*                                                                               
*----------------------------------------------------                           
* Build product list                                                            
*----------------------------------------------------                           
*                                                                               
BPRODUCT NTR1  BASE=*,LABEL=*                                                   
         XC    TEMP1,TEMP1                                                      
         LA    RF,L'PRDLIST-1(R2) Get input length                              
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         SR    RF,R2                                                            
         LA    RF,1(RF)            Actual input len                             
         STC   RF,TEMP1+5          Fake header field                            
                                                                                
         MVC   TEMP1+8(L'PRDLIST),PRDLIST                                       
*                                                                               
         GOTOR VSCANNER,DMCB,TEMP1,AIO7                                         
         LLC   R3,DMCB+4           Get number of blocks                         
         LTR   R3,R3                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R3,PRDCNT           Save number of prods in list                 
*                                                                               
* Build list of products 3 bytes each (or 2 bytes + space)                      
         L     R2,AIO7                                                          
         L     RF,APRDLIST         Output area                                  
BPR10    MVC   0(L'PRDCODE,RF),12(R2)      2 or 3 bytes of prod code            
         LA    RF,L'PRDCODE(RF)                                                 
         LA    R2,32(R2)           Bump to next prod                            
         JCT   R3,BPR10                                                         
*                                                                               
         L     R0,APRDLSTX                                                      
         CR    RF,R0                                                            
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PRDCNT,1            More than one prod request?                  
         JNH   BPRX                                                             
*                                                                               
         GOTOR VXSORT,DMCB,TEMP2,(R3),3,3,0 Sort prods in alpha order           
*                                                                               
BPRX     J     EXITY                                                            
*                                                                               
*----------------------------------------------------                           
* Build binary product list                                                     
*----------------------------------------------------                           
*                                                                               
BBPRODCT NTR1  BASE=*,LABEL=*                                                   
         L     R3,ABPRLIST         Binary product list area                     
*                                                                               
BPRD10   MVC   PRDCODE,0(R2)                                                    
         BRAS  RE,GETBPRD          Get product binary value                     
         MVC   0(L'SVBPRD,R3),SVBPRD                                            
         LA    R3,L'SVBPRD(R3)                                                  
         LA    R2,L'PRDCODE(R2)                                                 
         L     R0,ABPRLSTX         End of product list                          
         CR    R3,R0                                                            
         JL    BPRD10                                                           
*                                                                               
         J     EXIT                                                             
*                                                                               
*----------------------------------------------------                           
* Get product alpha code                                                        
*----------------------------------------------------                           
*                                                                               
GPRODC   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
GETPRD10 CLC   0(L'SVBPRD,R2),3(RF) Match on binary product?                    
         JE    GETPRD15                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,GETPRD10                                                      
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
GETPRD12 CLC   0(L'SVBPRD,R2),3(RF) Match that of product list?                 
         JE    GETPRD15                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,GETPRD12                                                      
         JNE   EXITN                                                            
                                                                                
GETPRD15 MVC   PRDCODE,0(RF)       Save prod apha code                          
         J     EXITY                                                            
*                                                                               
*----------------------------------------------------                           
* Get market number                                                             
*----------------------------------------------------                           
*                                                                               
GMKT     SR    R0,R0                                                            
         ICM   R0,3,CMTKMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTCODE,DUB                                                      
         BR    RE                                                               
*                                                                               
*----------------------------------------------------                           
* Get station call letters                                                      
*----------------------------------------------------                           
*                                                                               
GSTA     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         GOTO1 VMSUNPK,DMCB,CMTKMKT,WORK,DUB                                    
         MVC   STACODE,DUB                                                      
         CLI   QMEDA,C'T'                                                       
         JNE   EXITY                                                            
         CLI   STACODE+4,C'T'      Is it TV                                     
         JNE   EXITY               No, leave it in                              
         MVI   STACODE+4,C' '      No band for TV                               
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Traffic commercial record download for list and display             *         
***********************************************************************         
                                                                                
REQCML   LKREQ H,I#CMLDL,OUTCML,NEXTREQ=REQSLNV                                 
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(I,B#SAVED,CLTIND),CHAR,OLEN=L'QCLTA,LIST=F,      +        
               TEXT=SP#CLI                                                      
CmlPrefx LKREQ F,003,(D,B#SAVED,CMLPREFX),CHAR,TEXT=SP#FILM,COL=*               
SDate    LKREQ F,004,(D,B#SAVED,STRDATEB),BDAT,TEXT=SP#STDT,COL=*               
EDate    LKREQ F,005,(D,B#SAVED,ENDDATEB),BDAT,TEXT=SP#ENDT,COL=*               
RecAct   LKREQ F,100,(D,B#SAVED,RECACT),CHAR,TEXT=SP#ACTN,COL=*                 
         LKREQ E                                                                
                                                                                
OUTCML   LKOUT H                   Traffic commercial output maps start         
                                                                                
         LKOUT R,X'0029'                                                        
PRout    LKOUT P,,CMLINI           Init traffic commercial arrary               
Array    LKOUT C,1,(A,ARYCML),FILTROUT=SHWVLDC                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                   Traffic commercial output maps end           
                                                                                
ARYCML   LKOUT A,(R,NXTCML),MULTIROW=Y,ROWNAME=CMLRECD                          
Array    LKOUT C,X'29',(A,ARYCMLD),FILTROUT=SHWCMLC                             
Array    LKOUT C,2,(A,ARYCCL),FILTROUT=SHWCLTC                                  
         LKOUT E                                                                
                                                                                
ARYCMLD  LKOUT A,(R,SETCMLD),MULTIROW=Y,ROWNAME=CMLRECD                         
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
CmlCd    LKOUT C,003,(D,B#WORKD,IOKEY),(R,EDTCMC)                               
Array    LKOUT C,004,(A,ARYCPR)                                                 
CmlT1    LKOUT C,005,(D,B#SAVED,SVCMLDS1),CHAR,ND=Y                             
CmlT2    LKOUT C,006,(D,B#SAVED,SVCMLDS2),CHAR,ND=Y                             
CmlT3    LKOUT C,007,(D,B#SAVED,SVCMLDS3),CHAR,ND=Y                             
Cksum    LKOUT C,200,(D,B#SAVED,SVCKSM),HEXD,ND=Y                               
Array    LKOUT C,008,(A,ARYCDS)                                                 
         LKOUT E                                                                
                                                                                
ARYCCL   LKOUT A,(R,NXTCL),MULTIROW=Y,ROWNAME=CLTRECD                           
Array    LKOUT C,2,(A,ARYTC2),FILTROUT=TSTCMLC                                  
Array    LKOUT C,3,(A,ARYTPR1),FILTROUT=SHWPRDC                                 
         LKOUT E                                                                
*                                                                               
SHWVLDC  CLI   SHWVLD,C'Y'         Valid request                                
         BR    RE                                                               
*                                                                               
SHWCMLC  CLI   SHWCML,C'Y'         Show commercial?                             
         BR    RE                                                               
*                                                                               
SHWCLTC  CLI   SHWCLT,C'Y'         Show client?                                 
         BR    RE                                                               
*                                                                               
SHWPRDC  CLI   SHWPRD,C'Y'         Show prods?                                  
         BR    RE                                                               
*                                                                               
TSTCMLC  CLI   CMLDLSW1,YESQ       Commercial record downloaded?                
         BR    RE                                                               
*                                                                               
ARYCPR   LKOUT A,(D,B#WORKD,ELEM2),NROWS=255,ROWWIDTH=1,ROWNAME=DUMMY_D         
PrdCd    LKOUT C,004,DUM_LIN1,(R,EDCPRD),ND=Y                                   
         LKOUT E                                                                
*                                                                               
ARYCDS   LKOUT A,(D,B#TCMREC,CMLDTAEL),EOT=EOR,ROWID=(CMLDTAEL,X'10'), +        
               ROWWIDTH=(V,CMLDTALN)                                            
         LKOUT C,008,CMLSLN,UBIN                                                
         LKOUT C,009,CMLRLSE,BDAT                                               
         LKOUT C,010,CMLRCL,BDAT,ND=Y                                           
         LKOUT C,011,(D,B#SAVED,SVCMLUFN),CHAR,ND=Y                             
         LKOUT C,012,CMLSOLO,CHAR,ND=Y                                          
         LKOUT C,013,(D,B#SAVED,SVPARENT),CHAR,ND=Y                             
         LKOUT C,014,(D,B#SAVED,SVMSTIME),CHAR,ND=Y                             
         LKOUT C,015,(D,B#SAVED,SVMETIME),CHAR,ND=Y                             
         LKOUT C,016,(D,B#SAVED,SVDDATE),BDAT,ND=Y                              
         LKOUT C,017,(D,B#SAVED,SVDTIME),CHAR,ND=Y                              
         LKOUT C,018,CMLOVRD1,UBIN,ND=Y                                         
         LKOUT C,019,CMLOVRD2,UBIN,ND=Y                                         
         LKOUT C,020,(D,B#SAVED,SVDAILY),CHAR,ND=Y                              
         LKOUT C,021,(D,B#SAVED,SVMSDATE1),CDAT,ND=Y                            
         LKOUT C,022,(D,B#SAVED,SVMEDATE1),CDAT,ND=Y                            
         LKOUT C,023,(D,B#SAVED,SVMSDATE2),CDAT,ND=Y                            
         LKOUT C,024,(D,B#SAVED,SVMEDATE2),CDAT,ND=Y                            
         LKOUT C,025,(D,B#SAVED,SVMSDATE3),CDAT,ND=Y                            
         LKOUT C,026,(D,B#SAVED,SVMEDATE3),CDAT,ND=Y                            
         LKOUT C,027,(D,B#SAVED,SVMSDATE4),CDAT,ND=Y                            
         LKOUT C,028,(D,B#SAVED,SVMEDATE4),CDAT,ND=Y                            
         LKOUT C,029,(D,B#SAVED,SVMSDATE5),CDAT,ND=Y                            
         LKOUT C,030,(D,B#SAVED,SVMEDATE5),CDAT,ND=Y                            
         LKOUT C,031,(D,B#SAVED,SVMSDATE6),CDAT,ND=Y                            
         LKOUT C,032,(D,B#SAVED,SVMEDATE6),CDAT,ND=Y                            
         LKOUT C,033,(D,B#SAVED,SVCMLCLS),CHAR,ND=Y                             
         LKOUT C,033,CMLCLASS,CHAR,ND=Y                                         
         LKOUT C,034,(D,B#SAVED,SVACTUL1),CHAR,ND=Y                             
         LKOUT C,035,(D,B#SAVED,SVACTUL2),CHAR,ND=Y                             
         LKOUT C,036,(D,B#SAVED,SVACTUL3),CHAR,ND=Y                             
         LKOUT C,037,(D,B#SAVED,SVACTUL4),CHAR,ND=Y                             
         LKOUT C,038,(D,B#SAVED,SVFORMAT),CHAR,ND=Y                             
         LKOUT C,039,CMLTALEX,CHAR,ND=Y                                         
         LKOUT C,040,CMLCLTNO,CHAR,ND=Y                                         
         LKOUT C,050,(D,B#SAVED,CDELETE),CHAR,ND=Y                              
         LKOUT E                                                                
*                                                                               
ARYTC2   LKOUT A,(D,B#CLTREC,CLTRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
CltCd    LKOUT C,001,(D,B#WORKD,QCLTA),CHAR                                     
CltNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
         LKOUT E                                                                
*                                                                               
ARYTPR1  LKOUT A,(R,NXTPR),MULTIROW=Y,ROWNAME=PRDRECD                           
Array    LKOUT C,3,(A,ARYTP2)                                                   
         LKOUT E                                                                
*                                                                               
ARYTP2   LKOUT A,(D,B#PRDREC,PRDRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
PrdCd    LKOUT C,001,(D,B#WORKD,QPRDA),CHAR                                     
PrdNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
TalAg    LKOUT C,003,(D,B#WORKD,DUB2),CHAR,ND=Y                                 
CltCd    LKOUT C,004,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT E                                                                
                                                                                
*                                                                               
SETCMLD  DS    0H                                                               
         CLI   DONESW,C'Y'                                                      
         JE    EXITN                                                            
                                                                                
         MVI   DONESW,C'Y'                                                      
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
CMLINI   MVI   SHWVLD,C'N'         Init request is invalid                      
         OC    MEDCOD,SPACES                                                    
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   EXITN                                                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,ACLT           Have any client code to look up?             
         JZ    NOMORE                                                           
                                                                                
         MVC   NUMCLT,LW_NUMN-LW_D(RE) Number of clients to process             
         AHI   RE,LW_LN2Q          Point to first client                        
         ST    RE,ANXTCLT          Set A(client request)                        
*                                                                               
         XC    QCLTX,QCLTX         Init binary client code                      
         MVC   QCLTA,0(RE)         Get input client code                        
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   EXITN                                                            
*                                                                               
         MVI   CLTCHG,C'N'         Init client changed flag                     
*                                                                               
         L     R0,AIO8                                                          
         L     R1,AIO8                                                          
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,AIO8                                                          
         ST    RE,ABPRLIST         Binary product codes list                    
         ST    RE,ABPRNXT          Current binary product code pointer          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    Get address of TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A11'  Get address of untime                    
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VUNTIME,0(R1)                                                    
*                                                                               
         MVI   CMLPRFXL,0                                                       
         CLC   CMLPREFX,SPACES     Any commercial prefix?                       
         JNH   CMLINI20            None                                         
         LA    R1,L'CMLPREFX                                                    
         LA    RE,CMLPREFX+L'CMLPREFX-1                                         
*                                                                               
CMLINI10 CLI   0(RE),C' '          Anything here?                               
         JH    CMLINI15                                                         
         BCTR  RE,0                                                             
         JCT   R1,CMLINI10                                                      
*                                                                               
CMLINI15 STC   R1,CMLPRFXL         We know the length now                       
         CLI   CMLPRFXL,8          Input length >= 8  ?                         
         JNL   CMLINI30                                                         
*                                                                               
         CLI   RECACT,C'L'         Only for list can have partial cml           
         JNE   NOMORE                                                           
*                                                                               
CMLINI20 LA    RE,1(RE)            Point to the first blank                     
         MVI   0(RE),C'A'          1st 8 chars can't be < C'AAAAAAAA'           
         LA    R1,1(R1)            fill with C'A' until first 8 char            
         CHI   R1,8                   are populated                             
         JL    CMLINI20                                                         
*                                                                               
CMLINI30 GOTOR VTRPACK,DMCB,(C'P',CMLPREFX),CMLPRFXS                            
*                                                                               
         CLI   CMLPRFXL,12         For a specific 12 char film?                 
         JE    CMLINI40                                                         
*                                                                               
         LLC   R1,CMLPRFXL                                                      
         LA    R3,CMLPREFX(R1)                                                  
         LA    RE,12                                                            
         SR    RE,R1               RE=# of C'9's to fill to the end             
         BCTR  RE,0                                                             
         BASR  R4,0                                                             
         MVC   0(0,R3),=12C'9'                                                  
         EX    RE,0(R4)                                                         
*                                                                               
CMLINI40 GOTOR VTRPACK,DMCB,(C'P',CMLPREFX),CMLPRFXE                            
*                                                                               
CMLINIX  MVI   SHWVLD,C'Y'                                                      
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCML   MVI   DONESW,C'N'         For setcmld routine                          
*                                                                               
         CLI   CLTCHG,C'Y'                                                      
         JNE   NXTCML02                                                         
         MVI   CLTCHG,C'N'                                                      
         MVI   CMLDLSW1,NOQ        No commerical rec downloaded yet             
*                                                                               
         L     R0,ABPRLIST         Clear prod list area                         
         L     R1,ABPRLIST         in AIO8                                      
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,ABPRLIST                                                      
         ST    RE,ABPRNXT          Re-init current prod code pointer            
*                                                                               
* Get next client from list                                                     
         DS    0H                                                               
         L     RE,ANXTCLT                                                       
         LA    RE,L'QCLTA(RE)      Point to next clt                            
         ST    RE,ANXTCLT                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
*                                                                               
         MVC   QCLTA,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   NOMORE                                                           
                                                                                
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NOMORE                                                           
         J     *+12                                                             
NXTCML02 CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCMLSQ                                                         
*                                                                               
         MVI   CMLDLSW1,NOQ        No commerical rec downloaded yet             
         MVI   SHWCML,C'Y'         Preset show commercial                       
         MVI   SHWCLT,C'N'         Preset not to show clts                      
         NI    SVCMLFLG,X'FF'-CML_ALLQ  Reset download all products             
*                                                                               
NXTCML03 XC    SVCMLFLS(SVCMLQ2),SVCMLFLS   Init saved commercial flds          
         XC    ELEM1,ELEM1         Table of prd codes to be downloaded          
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLPIDAD+0,X'0A'    Passive keys for packed Ad-IDs               
         MVI   CMLPIDAD+1,X'C1'                                                 
         MVC   CMLPADAM,QMEDX                                                   
         MVC   CMLPADCL,QCLTX                                                   
         MVC   CMLPADID,CMLPRFXS   Starting point                               
*                                                                               
NXTCMLHI GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         DROP  RE                                                               
*                                                                               
NXTCML10 LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         CLC   IOKEY(CMLPADID-CMLKEY),IOKEYSAV  Same upto client?               
         JE    NXTCML14                                                         
         MVI   SHWCML,C'N'         Do not show comml                            
         MVI   SHWCLT,C'Y'         Show client                                  
         MVI   SHWPRD,C'Y'              product                                 
         MVI   CLTCHG,C'Y'         Turn on client changed indicator             
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
NXTCML14 OC    CMLPADID,CMLPADID   Have commercial ID?                          
         JZ    NXTCMLSQ            No                                           
         CLC   =X'C5DCC5DCC5B80000',CMLPADID   House for the Client?            
         JE    NXTCMLSQ              Yes, this is TRPACK of C'99999999'         
*                                                                               
         CLC   CMLPADID,CMLPRFXE   Past the last?                               
         JNH   NXTCML15                                                         
         MVI   SHWCML,C'N'         Do not show commercial                       
         MVI   SHWCLT,C'Y'         Show client                                  
         MVI   SHWPRD,C'Y'              product                                 
         MVI   CLTCHG,C'Y'         Turn on client changed indicator             
         MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
         DROP  RE                                                               
*                                                                               
NXTCML15 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTCMLSQ                                                         
         XC    SVCMLUFN(SVPCMLQ),SVCMLUFN Init saved commercial flds            
         XC    ELEM2,ELEM2         List of prds for this commercial             
*                                                                               
         BRAS  RE,GETCSUM          Get record check sum                         
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
         USING CMLDTAEL,R1                                                      
         MVC   SVCMLDS1(L'CMLTITLE),CMLTITLE                                    
*                                                                               
         TM    CMLSTAT,X'80'                                                    
         JZ    NXTCML17                                                         
         MVI   CDELETE,C'Y'        Deleted cml                                  
*                                                                               
NXTCML17 OC    STRDATEB(L'STRDATEB*2),STRDATEB   Any date filter?               
         JZ    NXTCML18                          None, accept all cmmls         
         CLC   ENDDATEB,CMLRLSE    End date filter before release date?         
         JL    NXTCMLSQ            Yes, next record then                        
         CLC   STRDATEB,CMLRCL     Start date after recall date?                
         JH    NXTCMLSQ            Yes, next record then                        
*                                                                               
NXTCML18 CLC   CMLRCL,=X'FFFFFF'   Recall date 'until further notice'?          
         JNE   NXTCML20                                                         
         XC    CMLRCL,CMLRCL                                                    
         MVI   SVCMLUFN,YESQ       Set 'until further notice' flag to Y         
         DROP  R1                                                               
*                                                                               
NXTCML20 L     R1,IOADDR                                                        
         LA    R1,(CMLDTAEL-CMLRECD)(R1)                                        
NXTCML23 CLI   0(R1),EOR           End of record?                               
         JE    NXTCML90                                                         
         CLI   0(R1),X'20'         Product list element?                        
         JE    NXTCML50                                                         
         CLI   0(R1),X'21'         Comclass                                     
         JE    NXTCML60                                                         
         CLI   0(R1),X'24'         Extended data elem                           
         JE    NXTCML70                                                         
         CLI   0(R1),X'30'         Extra description element?                   
         JE    NXTCML30                                                         
         CLI   0(R1),X'60'         Actual cmls                                  
         JE    NXTCML76                                                         
         CLI   0(R1),X'A0'         Ad-ID element?                               
         JE    NXTCML40                                                         
         CLI   0(R1),X'B0'         Matching data                                
         JE    NXTCML80                                                         
NXTCMLNX LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTCML23            Bump to next element                         
*                                                                               
         USING CMLDSCEL,R1                                                      
NXTCML30 CLI   CMLDSCSQ,0                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS1,CMLDSC                                                  
         CLI   CMLDSCSQ,1                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS2,CMLDSC                                                  
         CLI   CMLDSCSQ,2                                                       
         JNE   *+10                                                             
         MVC   SVCMLDS3,CMLDSC                                                  
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLADIEL,R1                                                      
NXTCML40 MVC   SVCMLAID,CMLADID    Ad-Id                                        
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLPRDEL,R1                                                      
NXTCML50 L     R3,ABPRLIST         Point to binary product table                
         CLI   CMLPRDS,X'FF'       All products?                                
         JNE   *+8                                                              
         OI    SVCMLFLG,CML_ALLQ   Set to download all products                 
*                                                                               
         LLC   RF,CMLPRDLN                                                      
         AHI   RF,-2               Number of binary product codes               
         CHI   RF,0                                                             
         JNH   NXTCMLNX                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),CMLPRDS   Save list of binary prod codes                
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,ELEM2            Current prds for this commercial             
NXTCML52 CLI   0(RF),X'00'         End of current prds list?                    
         JE    NXTCMLNX                                                         
         CLI   0(RF),X'FF'         All product?                                 
         JE    NXTCMLNX                                                         
*                                                                               
NXTCML54 CLI   0(R3),X'00'         Blank entry?                                 
         JE    NXTCML56                                                         
         CLC   0(1,RF),0(R3)       Already in table?                            
         JE    NXTCML58                                                         
         AHI   R3,1                Position to next binary product              
         J     NXTCML54                                                         
NXTCML56 MVC   0(1,R3),0(RF)                                                    
         LA    R3,1(R3)                                                         
         ST    R3,ABPRNXT                                                       
NXTCML58 AHI   RF,1                Bump to next current prd list                
         L     R3,ABPRLIST         Start of binary product table                
         J     NXTCML52                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLCLSEL,R1                                                      
NXTCML60 LA    RF,SVCMLCLS         Comclass                                     
         OC    SVCMLCLS,SVCMLCLS                                                
         JZ    *+8                                                              
NXTCM60C LA    RF,1(RF)                                                         
*                                                                               
NXTCM60F OC    0(6,RF),0(RF)       Any comclass                                 
         JZ    NXTCML62                                                         
         CLI   0(RF),C','                                                       
         JE    NXTCML61                                                         
         J     NXTCM60C                                                         
*                                                                               
NXTCML61 LA    RF,1(RF)                                                         
         J     NXTCM60F                                                         
*                                                                               
NXTCML62 MVC   0(4,RF),CMLCLS                                                   
         LA    RF,4(RF)                                                         
         LA    R2,CMLCLS+3                                                      
*                                                                               
NXTCML65 CLI   0(R2),C' '                                                       
         JH    NXTCML66                                                         
         SHI   RF,1                                                             
         JCT   R2,NXTCML65                                                      
*                                                                               
NXTCML66 MVI   0(RF),C'='                                                       
         EDIT  (B2,CMLCLSPC),(6,1(RF)),2,ALIGN=LEFT                             
         AR    RF,R0                                                            
         MVI   1(RF),C','                                                       
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLXDTEL,R1                                                      
NXTCML70 DS    0H                                                               
         MVC   SVPARENT,CMLXPRNT                                                
                                                                                
         CLI   CMLXDSDT,0                                                       
         JE    NXTCML72                                                         
                                                                                
         MVC   SVDDATE,CMLXDSDT                                                 
         ST    R1,FULL                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLXDSTM                                                 
         GOTOR VUNTIME,DMCB,WORK,SVDTIME                                        
         L     R1,FULL                                                          
                                                                                
NXTCML72 OC    CMLXHDEF,CMLXHDEF                                                
         JZ    *+8                                                              
         MVI   SVFORMAT,C'H'                                                    
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
         USING CMLACTEL,R1                                                      
NXTCML76 LA    RF,SVACTUL1         Actual cml                                   
                                                                                
NXTCM76C OC    0(L'SVACTUL1,RF),0(RF)                                           
         JZ    NXTCML77                                                         
         LA    RF,L'SVACTUL1(RF)                                                
         J     NXTCM76C                                                         
                                                                                
NXTCML77 MVC   0(L'CMLACTID,RF),CMLACTID  Disp 8 char cml                       
         CLI   CMLACTLN,CMLACTL1          Old elem                              
         JE    *+10                                                             
         MVC   0(L'CMLACTCM,RF),CMLACTCM   No, disp 12 char cml                 
         J     NXTCMLNX                                                         
         DROP  R1                                                               
*                                                                               
NXTCML80 LR    R2,R1                                                            
         USING CMLMATCH,R2                                                      
         LA    RF,SVMPERIOD                                                     
         MVC   0(SVMALLPER,RF),CMLMPER1 Move all match dates                    
                                                                                
         ST    R1,FULL                                                          
*                                                                               
         OC    CMLMSTIM,CMLMSTIM   Match start time                             
         JZ    NXTCML85                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLMSTIM                                                 
         GOTOR VUNTIME,DMCB,WORK,SVMSTIME                                       
*                                                                               
NXTCML85 DS    0H                                                               
         OC    CMLMETIM,CMLMETIM   Match end time                               
         JZ    NXTCML87                                                         
         CLC   CMLMETIM,=X'FFFF'                                                
         JE    NXTCML87                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),CMLMETIM                                                 
         GOTOR VUNTIME,DMCB,WORK,SVMETIME                                       
*                                                                               
NXTCML87 DS    0H                  Check times daily                            
         L     R1,FULL             Restore pointer to elem                      
         MVI   SVDAILY,C'N'                                                     
         TM    CMLMFLAG,CMLMFDAY                                                
         JZ    NXTCMLNX                                                         
         MVI   SVDAILY,C'Y'                                                     
         J     NXTCMLNX                                                         
         DROP  R2                                                               
*                                                                               
NXTCML90 DS    0H                  To extract more fields if needed             
         LA    R0,3                3 description fields                         
         LA    RE,SVCMLDS1         1st description                              
NXTCML95 OC    0(L'SVCMLDS1,RE),0(RE)                                           
         JZ    NXTCML97                                                         
         TR    0(L'SVCMLDS1,RE),TRTAB Translate data                            
NXTCML97 LA    RE,L'SVCMLDS1(RE)   Bump to next desc field                      
         JCT   R0,NXTCML95                                                      
*                                                                               
         MVI   CMLDLSW1,YESQ       Commerical rec downloaded                    
         CLI   RECACT,C'D'         Action display                               
         JNE   NXTCML99                                                         
         MVI   SHWCLT,C'Y'         Show client                                  
         MVI   SHWPRD,C'Y'              product                                 
NXTCML99 MVC   LP_ADATA,ATCMREC    Point to traffic commercial record           
         J     EXITY                                                            
*                                                                               
NXTCMLSQ CLI   RECACT,C'D'         Action display                               
         JE    NOMORE              Yes, only one record displayed               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         J     NXTCML10                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCL    CLI   SHWCLT,C'Y'         Show client?                                 
         JNE   EXITN                                                            
*                                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCL30                                                          
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2         Init client name for output                  
         MVC   WORK2(L'CNAME),CNAME                                             
         J     NXTTCL70                                                         
         DROP  RE                                                               
*                                                                               
NXTCL30  DS    0H                  To process more client rec if needed         
         J     NOMORE                                                           
*                                                                               
NXTCL70  DS    0H                  To extract more fields if needed             
*                                                                               
         MVI   SHWCLT,C'N'         Do not show clt                              
*                                                                               
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDCPRD   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Have binary product code?                    
         JE    XCOLEN                                                           
         XC    0(3,R4),0(R4)       Init output                                  
         CLI   0(R2),X'FF'         'All' product?                               
         JNE   EDCPRD10                                                         
                                                                                
         MVC   0(3,R4),=C'ALL'                                                  
         L     RF,ABPRNXT          Current binary product code pointer          
         MVI   0(RF),X'FF'                                                      
         J     EDCPRD42                                                         
                                                                                
EDCPRD10 L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
EDCPRD20 CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    EDCPRD40                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,EDCPRD20                                                      
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
EDCPRD24 CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    EDCPRD40                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,EDCPRD24                                                      
         J     XCOLEN              Bad product code                             
                                                                                
EDCPRD40 MVC   0(3,R4),0(RF)       Get character format for prd code            
                                                                                
         L     RE,ABPRLIST         Start of binary product codes list           
         L     RF,ABPRNXT          Current binary product code pointer          
EDTP40C  CR    RE,RF                                                            
         JE    EDCPRD41                                                         
         CLC   0(1,RE),0(R2)       Same prod                                    
         JE    EDCPRD42                                                         
         LA    RE,1(RE)                                                         
         J     EDTP40C                                                          
                                                                                
EDCPRD41 MVC   0(1,RF),0(R2)       Save binary product in buffer                
         LA    RF,1(RF)                                                         
         ST    RF,ABPRNXT                                                       
                                                                                
EDCPRD42 LA    R0,3                Max length for product code                  
         J     SETOLENX                                                         
         DROP  RE                                                               
*                                                                               
NXTPR    CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTPR30                                                          
         TM    SVCMLFLG,CML_ALLQ   Download all products?                       
         JNZ   NXTPR10                                                          
                                                                                
         L     RE,ABPRLIST         Binary product codes list                    
         MVC   ELEM1(255),0(RE)    Get set of binary prd to process             
         J     NXTPR18                                                          
                                                                                
NXTPR10  L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    ELEM1,ELEM1         Init table of binary product codes           
         LA    R1,CLIST                                                         
         LA    R2,ELEM1                                                         
         LA    RF,220                                                           
NXTPR12  CLI   3(R1),0             No more binary product code?                 
         JE    NXTPR18                                                          
         MVC   0(1,R2),3(R1)       Save binary product code to look up          
         AHI   R2,1                                                             
         AHI   R1,4                                                             
         JCT   RF,NXTPR12                                                       
         LA    R1,CLIST2           2nd product list                             
         LA    RF,35                                                            
NXTPR14  CLI   3(R1),0             No more binary product code?                 
         JE    NXTPR18                                                          
         MVC   0(1,R2),3(R1)       Save binary product code to look up          
         AHI   R2,1                                                             
         AHI   R1,4                                                             
         JCT   RF,NXTPR14                                                       
                                                                                
NXTPR18  LA    R1,ELEM1                                                         
         ST    R1,FULL1            Address of binary prd codes to proc          
         DROP  RE                                                               
*                                                                               
NXTPR30  L     R2,FULL1            Point to binary prd table                    
         CLI   0(R2),X'00'         EOL                                          
         JE    NXTPRX                                                           
         CLI   0(R2),X'FF'         POL?                                         
         JE    NXTPRX                                                           
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST            Point to 1st list of products                
         LA    R1,220                                                           
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTPR36                                                          
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         LA    RF,CLIST2           Point to 2nd list of products                
         LA    R1,35                                                            
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTPR36                                                          
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         DROP  RE                                                               
*                                                                               
NXTPR36  XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING PKEY,RE                                                          
         MVI   PKEYTYPE,PKEYTYPQ                                                
         MVC   PKEYAM,QMEDX                                                     
         MVC   PKEYCLT,QCLTX                                                    
         MVC   PKEYPRD,0(RF)       Character format prd code from table         
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(L'PKEY),IOKEYSAV                                           
         JNE   NXTPR38                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   NXTPR38                                                          
         AHI   R2,1                Point to next entry in table                 
         ST    R2,FULL1            For next round                               
         J     NXTPR50             Now extract prd fields for output            
*                                                                               
NXTPR38  AHI   R2,1                Point to next entry in table                 
         ST    R2,FULL1                                                         
         J     NXTPR30             Get next product record                      
*                                                                               
NXTPR50  L     R1,IOADDR                                                        
         USING PRDHDR,R1                                                        
         XC    QPRDA,QPRDA         Return prd code - character                  
         XC    WORK2,WORK2         Return prd name                              
         XC    DUB2,DUB2           Return Talent agency code                    
         MVC   QPRDA,PKEYPRD                                                    
         MVC   WORK2(L'PNAME),PNAME                                             
         MVC   DUB2(L'PTALAGY),PTALAGY                                          
         J     NXTPR70                                                          
         DROP  R1                                                               
*                                                                               
NXTPRX   MVI   SHWCML,C'Y'         Show commercial                              
         MVI   SHWCLT,C'Y'              client                                  
         MVI   SHWPRD,C'N'         Do not show products                         
         NI    SVCMLFLG,X'FF'-CML_ALLQ Reset all prod switch                    
         J     NOMORE                                                           
*                                                                               
NXTPR70  MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
*                                                                               
GETCSUM  NTR1  BASE=*,LABEL=*                                                   
         L     RE,IOADDR           RE=A(A(RECORD))                              
         SR    RF,RF                                                            
         ICM   RF,3,CMLRLEN-CMLKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,SVCKSM                                                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Spot length verifier                                                *         
***********************************************************************         
                                                                                
REQSLNV  LKREQ H,I#SLENVF,OUTSLV,NEXTREQ=REQSLND                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
SpotLen  LKREQ F,002,(D,B#SAVED,QSPTLN),CHAR,TEXT=SP#SPLEN                      
         LKREQ E                                                                
                                                                                
OUTSLV   LKOUT H                                                                
         LKOUT R,I#SLENVF                                                       
PRout    LKOUT P,,SLNINI           Init spot length verifier request            
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
SpLen    LKOUT C,002,(D,B#WORKD,BYTE2),UBIN                                     
Error    LKOUT C,003,(D,B#WORKD,TEMP2),CHAR,FILTROUT=TSTSLN                     
         LKOUT E                                                                
         LKOUT X                                                                
*                                                                               
SLNINI   MVI   SLNERRSW,0          Init spot length error switch                
         XC    TEMP2,TEMP2         Init error message                           
         OC    MEDCOD,SPACES                                                    
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JE    SLNINI20                                                         
         MVI   SLNERRSW,YESQ       Set spot length error switch                 
         MVC   TEMP2(13),=C'Invalid media'                                      
         J     EXITN                                                            
SLNINI20 OC    QSPTLN,SPACES                                                    
         CLC   QSPTLN,SPACES       Have spot length to validate?                
         JNE   SLNINI26                                                         
         MVI   SLNERRSW,YESQ       Set spot length error switch                 
         MVC   TEMP2(19),=C'Missing spot length'                                
         J     EXITN                                                            
SLNINI26 LA    RF,L'QSPTLN                                                      
         CLI   QSPTLN+2,C' '                                                    
         JNE   *+6                                                              
         BCTR  RF,0                                                             
         CLI   QSPTLN+1,C' '                                                    
         JNE   *+6                                                              
         BCTR  RF,0                                                             
         GOTOR (#VALSLN,AVALSLN),DMCB,QSPTLN,(RF),BYTE2                         
         JE    EXITY                                                            
         MVI   SLNERRSW,YESQ       Set spot length error switch                 
         MVC   TEMP2(19),=C'Invalid spot length'                                
         J     EXITN                                                            
*                                                                               
TSTSLN   CLI   SLNERRSW,YESQ       Error in spot length?                        
         BR    RE                                                               
*                                                                               
                                                                                
***********************************************************************         
* Spot length download                                                *         
***********************************************************************         
                                                                                
REQSLND  LKREQ H,I#SLENDL,OUTSLD,NEXTREQ=REQMSDL                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
SpotLen  LKREQ F,002,(D,B#SAVED,QSPTLN),CHAR,TEXT=SP#SPLEN                      
         LKREQ E                                                                
                                                                                
OUTSLD   LKOUT H                                                                
         LKOUT R,I#SLENDL                                                       
Array    LKOUT C,001,(A,ARYSLN)                                                 
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYSLN   LKOUT A,(R,NXTSLN),MULTIROW=Y,ROWNAME=AGYRECD                          
Media    LKOUT C,001,(D,B#WORKD,BYTE1),CHAR,ND=Y                                
SpLen    LKOUT C,002,(D,B#WORKD,BYTE2),UBIN,ND=Y                                
         LKOUT E                                                                
                                                                                
NXTSLN   NTR1  BASE=*,LABEL=*                                                   
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTSLN30                                                         
         XC    HALF2,HALF2         Init loop counter                            
         MVI   BYTE1,0             Init alpha media code                        
         MVI   BYTE2,0             Init spot length output                      
         OC    QSPTLN,SPACES       Space pad request spot length                
*                                                                               
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   NOMORE                                                           
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A57'  GET A(SLNTAB)                            
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               POINT TO END OF TABLE                        
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMEDA,C'T'                                                       
         JE    NXTSLN14                                                         
         CLI   QMEDA,C'N'                                                       
         JE    NXTSLN14                                                         
         CLI   QMEDA,C'C'                                                       
         JE    NXTSLN14                                                         
*                                                                               
         LA    R0,C'R'                                                          
         CLI   QMEDA,C'R'                                                       
         JE    NXTSLN14                                                         
         CLI   QMEDA,C'X'                                                       
         JE    NXTSLN14                                                         
         DC    H'0'                                                             
*                                                                               
NXTSLN14 CLC   =C'00',0(R1)        Test default table                           
         JE    NXTSLN16                                                         
         CLC   AGENCY,0(R1)        Else match agency                            
         JNE   *+12                                                             
NXTSLN16 CLM   R0,1,2(R1)          And media                                    
         JE    NXTSLN20                                                         
*                                                                               
         BXLE  R1,RE,NXTSLN14      Next entry                                   
         DC    H'0'                                                             
*                                                                               
NXTSLN20 AHI   R1,4+2              Point pass header & 1st zero entry           
         ST    R1,FULL2            Save address of spot length table            
*                                                                               
NXTSLN30 MVI   BYTE1,0             Init alpha media code                        
         MVI   BYTE2,0             Init spot length output                      
         LH    R3,HALF2                                                         
         CHI   R3,254              No more spot length for given media?         
         JH    NOMORE                                                           
         AHI   R3,1                Decrement for loop                           
         STH   R3,HALF2                                                         
         L     R1,FULL2            Point to spot length table                   
         CLI   1(R1),0             Valid length?                                
         JE    NXTSLN40                                                         
         CLC   QSPTLN,SPACES       String matching on spot length?              
         JE    NXTSLN36                                                         
         LA    RF,L'QSPTLN                                                      
         CLI   QSPTLN+2,C' '                                                    
         JNE   *+6                                                              
         BCTR  RF,0                                                             
         CLI   QSPTLN+1,C' '                                                    
         JNE   *+6                                                              
         BCTR  RF,0                                                             
         EDIT  (R3),(3,FULL1),ALIGN=LEFT                                        
         OC    FULL1,SPACES                                                     
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         CLC   QSPTLN(0),FULL1     String match?                                
         EX    RF,0(RE)                                                         
         JNE   NXTSLN40                                                         
*                                                                               
NXTSLN36 STC   R3,BYTE2                                                         
         MVC   BYTE1,QMEDA                                                      
*                                                                               
NXTSLN40 LA    R1,2(R1)            Point to next entry in table                 
         ST    R1,FULL2                                                         
         MVC   LP_ADATA,AAGYREC    Point to client record                       
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Market Group Scheme download                                        *         
***********************************************************************         
                                                                                
REQMSDL  LKREQ H,I#MKGSDL,OUTMSD,NEXTREQ=REQCLPH                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(I,B#SAVED,CLTIND),CHAR,OLEN=L'QCLTA,LIST=F,      +        
               TEXT=SP#CLI                                                      
MkGCd    LKREQ F,003,(D,B#SAVED,QSPTLN),CHAR,OLEN=2,TEXT=(*,MKGCLIT)            
         LKREQ E                                                                
                                                                                
OUTMSD   LKOUT H                                                                
         LKOUT R,I#MKGSDL                                                       
Array    LKOUT C,001,(A,ARYMSD)                                                 
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYMSD   LKOUT A,(R,NXTMSD),MULTIROW=Y,ROWNAME=MKGRECD                          
MedCd    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
MkGCd    LKOUT C,003,(D,B#SAVED,MKGCHAR+0),CHAR,LEN=2,ND=Y                      
MkGNm    LKOUT C,004,(D,B#SAVED,MKGCHAR+2),CHAR,LEN=4,ND=Y                      
         LKOUT E                                                                
                                                                                
NXTMSD   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         USING MKGKEY,R2                                                        
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTMSD20                                                         
*                                                                               
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   NOMORE                                                           
*                                                                               
         MVI   BYTE3,0             Init binary group code                       
         OC    QSPTLN(2),SPACES                                                 
         CLC   QSPTLN(2),SPACES    Have market group code?                      
         JE    NXTMSD18                                                         
         LA    RE,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
NXTMSD12 CLC   0(2,RE),QSPTLN                                                   
         JE    NXTMSD16                                                         
         LA    RE,3(RE)                                                         
         JCT   RF,NXTMSD12                                                      
         J     NOMORE              Invalid mkt group code to look up            
NXTMSD16 MVC   BYTE3,2(RE)         Binary mkt group code                        
*                                                                               
NXTMSD18 XC    IOKEY,IOKEY                                                      
         MVI   MKGKTYP+0,X'0D'                                                  
         MVI   MKGKTYP+1,X'02'                                                  
         MVC   MKGKAGMD,QMEDX      Binary agency/media                          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JNE   NOMORE                                                           
         J     NXTMSD22                                                         
*                                                                               
NXTMSD20 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   NOMORE                                                           
*                                                                               
NXTMSD22 CLC   IOKEY(3),IOKEYSAV   Same record/agency-media?                    
         JNE   NOMORE                                                           
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,ACLT           Have any client code to look up?             
         JZ    NOMORE                                                           
         SR    R4,R4                                                            
         ICM   R4,3,LW_NUMN-LW_D(R3)                                            
         AHI   R3,LW_LN2Q                                                       
*                                                                               
NXTMSD24 XC    QCLTX,QCLTX         Init binary client code                      
         MVC   QCLTA,0(R3)         Get input client code                        
         CLC   =C'ALL',0(R3)                                                    
         JE    NXTMSD26                                                         
         GOTOR VCLPACK,DMCB,QCLTA,QCLTX                                         
*                                                                               
NXTMSD26 CLC   MKGKCLT,QCLTX       Match binary client code?                    
         JE    NXTMSD30                                                         
         LA    R3,L'QCLTA(R3)      Next client code in array                    
         JCT   R4,NXTMSD24                                                      
         J     NXTMSD20            Client code doesn't match, next              
*                                                                               
NXTMSD30 CLI   BYTE3,0             Have market group code?                      
         JNE   NXTMSD36                                                         
         OC    MKGKMGRP,MKGKMGRP   Have market group number?                    
         JNZ   NXTMSD20                                                         
         MVC   MKGCHAR,SPACES      Init market group code output                
         LA    RE,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
NXTMSD32 CLC   2(1,RE),MKGKMID                                                  
         JE    NXTMSD34                                                         
         LA    RE,3(RE)                                                         
         JCT   RF,NXTMSD32                                                      
         J     NXTMSD20            Not in table (how?) - try next rec           
NXTMSD34 MVC   MKGCHAR(2),0(RE)                                                 
         J     NXTMSD90                                                         
*                                                                               
NXTMSD36 OC    MKGKMGRP,MKGKMGRP   Have market group number?                    
         JNZ   *+12                                                             
         BRAS  RE,GETMBKLN                                                      
         J     NXTMSD20                                                         
         CLC   BYTE3,MKGKMID       Market group ID match?                       
         JNE   NXTMSD20                                                         
         MVC   MKGCHAR(2),QSPTLN                                                
         GOTOR VHEXOUT,DMCB,MKGKMGRP,FULL1,2,=C'TOG',0                          
         LLC   RE,MBRKLEN                                                       
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   MKGCHAR+2(0),FULL1                                               
         EX    RE,0(R1)                                                         
         J     NXTMSD90                                                         
*                                                                               
NXTMSD90 MVC   LP_ADATA,AMKGREC                                                 
         J     EXITY                                                            
                                                                                
*                                                                               
***********************************************************************         
* Traffic client production house download                                      
***********************************************************************         
                                                                                
REQCLPH  LKREQ H,I#TCLTPH,OUTCPH,NEXTREQ=REQSXDL                                
MedCd    LKREQ F,001,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),            +        
               OLEN=L'QMEDX,MAXLEN=L'QMEDX,TEXT=SP#MED,COL=*                    
CltCd    LKREQ F,002,(I,B#SAVED,CLTIND),CHAR,LIST=F,                   +        
               OLEN=L'QCLTA,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                    
VndCd    LKREQ F,003,(I,B#SAVED,QVNDIND),CHAR,LIST=F,OLEN=3,           +        
               MAXLEN=3,TEXT=(*,TVENLIT),COL=*                                  
         LKREQ E                                                                
                                                                                
TVENLIT  DC    C'Vendor code(s)'                                                
                                                                                
OUTCPH   LKOUT H                   Traffic prod house output maps start         
         LKOUT R,X'0030'                                                        
PRout    LKOUT P,,CPHINI           Init traffic prod house arrary               
Array    LKOUT C,1,(A,ARYCPH),FILTROUT=SHWVLDH                                  
         LKOUT X                   Traffic prod house output maps end           
*                                                                               
ARYCPH   LKOUT A,(R,NXTCPH),MULTIROW=Y,ROWNAME=PRHRECD                          
Client   LKOUT C,2,(D,B#WORKD,QCLTA),CHAR,ND=Y                                  
PHoID    LKOUT C,3,(D,B#SAVED,HOUSECD),CHAR,ND=Y                                
Vendor   LKOUT C,4,(D,B#SAVED,SVVND),CHAR,ND=Y                                  
CltNm    LKOUT C,5,(D,B#WORKD,WORK2),CHAR,ND=Y                                  
         LKOUT E                                                                
*                                                                               
SHWVLDH  CLI   SHWVLD,C'Y'         Valid request                                
         BR    RE                                                               
*                                                                               
CPHINI   MVI   SHWVLD,C'N'         Init request is invalid                      
         OC    MEDCOD,SPACES                                                    
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         JNE   EXITN                                                            
*                                                                               
         MVC   AGYMEDX,QMEDX       For prod house key driver                    
*                                                                               
         XC    QCLTX,QCLTX         Init if all client request                   
         SR    RE,RE                                                            
         ICM   RE,7,ACLT                                                        
         JZ    CPHINI10            All client request                           
*                                                                               
         MVC   NUMCLT,LW_NUMN-LW_D(RE) Number of clients to process             
         AHI   RE,LW_LN2Q          Point to first client                        
         ST    RE,ANXTCLT          Set A(client request)                        
*                                                                               
         MVC   QCLTA,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2                                                      
         MVC   WORK2(L'CNAME),CNAME                                             
         DROP  RE                                                               
*                                                                               
CPHINI10 SR    RE,RE                                                            
         ICM   RE,7,AVND           Any vendor                                   
         JZ    NOMORE              This should never happen                     
*                                                                               
         MVC   NUMVND,LW_NUMN-LW_D(RE) Number of vendors                        
         AHI   RE,LW_LN2Q          Point to first vendor                        
         ST    RE,ANXTVND          Set A(vendor)                                
*                                                                               
         MVI   SHWVLD,C'Y'         Valid request                                
*                                                                               
         MVC   TPHRECQ,=X'0A29'    Traffic prod house record code               
         J     EXITY                                                            
***********************************************************************         
* Send production house values                                        *         
***********************************************************************         
NXTCPH   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCPH28                                                         
*                                                                               
         MVC   IOKEY(L'CMLKEY),SVKEY Restore last cml key                       
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING CMLKEY,RE                                                        
         MVI   CMLKID+0,X'0A'                                                   
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,QMEDX                                                     
         OC    QCLTX,QCLTX         All client request?                          
         JZ    NXTCPH05            Yes                                          
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLKCML,=C'99999999'  99999999 Commercial tells us house         
         DROP  RE                                                               
*                                                                               
NXTCPH05 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                          No, we're done                   
*                                                                               
         OC    QCLTX,QCLTX         If blank, all client request                 
         JNZ   NXTCPH20                                                         
*                                                                               
NXTCPH10 CLC   IOKEY(3),IOKEYSAV   Same rec type/agency/media                   
         JNE   NOMORE                                                           
*                                 Got the client for all clt request            
         CLC   =C'99999999',IOKEY+(CMLKCML-CMLKEY)                              
         JE    NXTCPH22                                                         
*                                 Got the client for all clt request            
         MVC   IOKEY+(CMLKCML-CMLKEY),=C'99999999'  Chk for house rec           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NXTCPH40                                                         
         J     NXTCPH10                                                         
*                                                                               
NXTCPH20 CLC   IOKEY(CMLKSTAT-CMLKEY),IOKEYSAV Exact match?                     
         JNE   NXTCPH30            Skip to next client                          
*                                                                               
NXTCPH22 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTCPH30            Skip to next client                          
*                                                                               
         L     RE,IOADDR                                                        
         USING CMLKEY,RE                                                        
         MVC   HOUSECD,CMLTITLE                                                 
         OC    HOUSECD,SPACES                                                   
*                                                                               
         MVC   SVKEY,IOKEY         Save commercial key                          
         J     NXTCPH50                                                         
         DROP  RE                                                               
*                                                                               
NXTCPH28 MVC   IOKEY(L'CMLKEY),SVKEY Restore last cml key                       
*                                                                               
NXTCPH30 OC    QCLTX,QCLTX                                                      
         JZ    NXTCPH40                                                         
*                                                                               
* Get next client from list                                                     
         L     RE,ANXTCLT                                                       
         LA    RE,L'QCLTA(RE)      Point to next clt                            
         ST    RE,ANXTCLT                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
*                                                                               
         MVC   QCLTA,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2                                                      
         MVC   WORK2(L'CNAME),CNAME                                             
         DROP  RE                                                               
*                                                                               
         MVC   IOKEY+(CMLKCLT-CMLKEY)(L'QCLTX),QCLTX                            
         MVC   IOKEY+(CMLKCML-CMLKEY)(L'CMLKCML),=C'99999999'                   
         J     NXTCPH05                                                         
*                                                                               
* Skip to next client                                                           
NXTCPH40 MVC   IOKEY+(CMLKCML-CMLKEY)(3),=X'FFFFFF'                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                          No, we're done                   
         J     NXTCPH10                                                         
*                                                                               
* Read production house record for Optica vendors                               
NXTCPH50 XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING PRHKEY,RE                                                        
         MVI   PRHKID+0,X'0A'                                                   
         MVI   PRHKID+1,X'29'                                                   
         MVC   PRHKAM,QMEDX                                                     
         MVC   PRHKPRH,HOUSECD                                                  
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO7'                            
         JNE   NXTCPH28            Skip to next client                          
*                                                                               
         CLC   IOKEY(13),IOKEYSAV Exact match?                                  
         JNE   NXTCPH28            Skip to next client                          
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO7'                           
         JNE   NXTCPH28            Skip to next client                          
                                                                                
         L     R2,IOADDR           Put filtering logic here...                  
         USING PRHRECD,R2                                                       
*                                                                               
         LR    R3,R2                                                            
         MVI   ELCODE,X'25'                                                     
         BRAS  RE,GETEL                                                         
         JNE   NXTCPH28            Skip to next client                          
*                                                                               
         USING PRHOPCEL,R3                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMVND         Vendor count                                 
*                                                                               
         L     RE,ANXTVND                                                       
NXTCPH55 CLC   PRHOPC,0(RE)        Match on vendor                              
         JE    NXTCPHX                                                          
         LA    RE,L'PRHOPC(RE)     Point to next vendor                         
         JCT   R0,NXTCPH55                                                      
         J     NXTCPH28            Skip to next client                          
*                                                                               
NXTCPHX  MVC   SVVND,PRHOPC                                                     
         OC    QCLTX,QCLTX                                                      
         JNZ   NXTCPHX2                                                         
         DROP  R3                                                               
*                                                                               
         MVC   QCLTX,SVKEY+(CMLKCLT-CMLKEY)                                     
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,QCLTX),QCLTA                               
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         XC    WORK2,WORK2                                                      
         MVC   WORK2(L'CNAME),CNAME                                             
         DROP  R2                                                               
         XC    QCLTX,QCLTX         All client request                           
*                                                                               
NXTCPHX2 DS    0H                                                               
         MVC   LP_ADATA,AIO3                                                    
         J     EXITY               Exit to send prod house values               
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* Special text (STEXT) download  M=105                                *         
***********************************************************************         
                                                                                
REQSXDL  LKREQ H,I#STXTDL,OUTSXD,NEXTREQ=REQTPAT                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(I,B#SAVED,CLTIND),CHAR,OLEN=L'QCLTA,LIST=F,     +0        
               MAXLEN=L'QCLTA,TEXT=SP#CLI                                       
TxtTy    LKREQ F,003,(D,B#SAVED,QTXTYP),CHAR,OLEN=1,TEXT=SP#TYPE                
OffCd    LKREQ F,004,(I,B#SAVED,QOFFIND),CHAR,OLEN=L'OFFCDE,LIST=F,    +        
               MAXLEN=L'OFFCDE,TEXT=(*,OFFCLIT)                                 
PageNo   LKREQ F,005,(D,B#SAVED,QPAGENO),CHAR,TEXT=SP#PAGE                      
CallPgm  LKREQ F,006,(D,B#SAVED,QCALLPGM),CHAR,TEXT=(*,CALPLIT)                 
CmtID    LKREQ F,007,(D,B#SAVED,QCMTID),VSTR,TEXT=(*,CMNTLIT)                   
RecAct   LKREQ F,100,(D,B#SAVED,RECACT),CHAR,TEXT=SP#ACTN                       
         LKREQ E                                                                
*                                                                               
OFFCLIT  DC    C'Office Code'                                                   
CALPLIT  DC    C'Calling Program'                                               
CMNTLIT  DC    C'Comment ID'                                                    
*                                                                               
                                                                                
OUTSXD   LKOUT H                                                                
         LKOUT R,I#STXTDL                                                       
Array    LKOUT C,001,(A,ARYSXD)                                                 
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYSXD   LKOUT A,(R,NXTSXD),MULTIROW=Y,ROWNAME=MKGRECD                          
MedCd    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR,ND=Y                                
CmtID    LKOUT C,003,(D,B#WORKD,ELEM2+0),CHAR,LEN=10,ND=Y                       
Title    LKOUT C,004,(D,B#WORKD,ELEM2+10),CHAR,LEN=24,ND=Y                      
OffCde   LKOUT C,005,(D,B#SAVED,OFFCDE),CHAR,ND=Y,FILTROUT=TSTCPG,     +        
               SKIPCOLS=CPGSKIPX                                                
CPGSKIP  EQU   *                                                                
PageNo   LKOUT C,006,(D,B#SAVED,PAGENO),CHAR,ND=Y                               
TxtDel   LKOUT C,009,(D,B#SAVED,CDELETE),CHAR,ND=Y                              
Cksum    LKOUT C,050,(D,B#SAVED,SVCKSM),HEXD,ND=Y                               
Array    LKOUT C,090,(A,ARYSTXT)                                                
CPGSKIPX EQU   (*-CPGSKIP)/LX_COLSL                                             
*                                                                               
Array    LKOUT C,095,(A,ARYTCL),FILTROUT=SHWCNME                                
         LKOUT E                                                                
                                                                                
ARYSTXT  LKOUT A,(R,NXTSTX),MULTIROW=Y,ROWNAME=DTXRECD                          
TxtLiNo  LKOUT C,007,(D,B#SAVED,TXTLINO),CHAR,ND=Y                              
TxtLine  LKOUT C,008,(D,B#SAVED,TXTLIN),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
TSTCPG   CLI   QCALLPGM,C'S'       Calling program STEXT                        
         BR    RE                                                               
*                                                                               
SHWCNME  CLI   SHWCLT,C'Y'         Show client?                                 
         BR    RE                                                               
                                                                                
NXTSXD   NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM2,ELEM2         Init Cmt-ID and Title output                 
         XC    OFFCDE,OFFCDE                                                    
         XC    QCLTA,QCLTA                                                      
         MVI   SHWCLT,C'N'         Preset not to show clts                      
         LA    R2,IOKEY                                                         
         USING DTXKEY,R2                                                        
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTSXDSQ                                                         
*                                                                               
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,ACLT           Have any client code to look up?             
         MVC   NUMCLT,LW_NUMN-LW_D(RE) Number of clients to process             
         AHI   RE,LW_LN2Q          Point to first client                        
         ST    RE,ANXTCLT          Set A(client request)                        
*                                                                               
         CLI   QTXTYP,ALLSTXTQ     All STEXTs?                                  
         JNE   NXTSXD04                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,ACLT           Have any client code to look up?             
         JNZ   NXTSXD04                                                         
*                                                                               
         XC    QOFFCDE,QOFFCDE                                                  
         SR    RE,RE                                                            
         ICM   RE,7,AOFF           Have any office code to look up?             
         JZ    NXTSXD06                                                         
         MVC   NUMOFF,LW_NUMN-LW_D(RE) Number of offices to process             
         SR    R0,R0                                                            
         ICM   R0,3,NUMOFF         R0=remaining office count                    
         BCTR  R0,0                                                             
         STCM  R0,3,NUMOFF                                                      
         AHI   RE,LW_LN2Q          Point to first client                        
         ST    RE,ANXTOFF          Set A(office request)                        
         J     NXTSXD05                                                         
*                                                                               
NXTSXD04 L     RE,ANXTCLT          A(client request)                            
         XC    SVCLT,SVCLT                                                      
         MVC   SVCLT,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,SVCLT,3,QCLTX                             
         JNE   NOMORE                                                           
         MVC   QCLTA,SVCLT                                                      
         SR    RE,RE                                                            
         ICM   RE,7,AOFF           Have any office code to look up?             
         JNZ   NOMORE              Office or client but not both                
         CLI   QTXTYP,PATSTXTQ     Pattern STEXT?                               
         JE    *+8                                                              
         MVI   SHWCLT,C'Y'         Show client                                  
         J     NXTSXD06                                                         
*                                                                               
NXTSXD05 MVC   QOFFCDE,0(RE)                                                    
         CLI   QOFFCDE+1,C' '      2 char office code?                          
         JNE   *+14                                                             
         MVC   SVOFFCDE,QOFFCDE    No, save 1 char office code                  
         J     NXTSXD06                                                         
         BRAS  RE,CNVTO1           Convert 2 char office to 1 byte              
         JNE   NOMORE                                                           
*                                                                               
NXTSXD06 CLI   QTXTYP,C' '         Have STEXT type?                             
         JH    *+8                                                              
         MVI   QTXTYP,PATSTXTQ     Default to Pattern STEXT                     
*                                                                               
         XC    ID,ID                                                            
         OC    QCMTID,QCMTID       Was Comment ID entered?                      
         JZ    NXTSXD07                                                         
         BRAS  RE,VID                                                           
         JNE   NOMORE                                                           
*                                                                               
NXTSXD07 MVI   PPAGENO,0                                                        
         OC    QPAGENO,QPAGENO     Any page number?                             
         JZ    *+8                                                              
         BAS   RE,CPAGENO                                                       
                                                                                
         XC    IOKEY,IOKEY         Prepare STEXT key                            
         MVI   DTXKID+0,X'0A'      X'0A2D'                                      
         MVI   DTXKID+1,X'2D'                                                   
         MVC   DTXKAM,QMEDX        Binary agency/media                          
         MVC   DTXKCLT,QCLTX       Preset to binary client                      
         CLI   QTXTYP,ALLSTXTQ     All STEXTs?                                  
         JNE   NXTSXD09                                                         
         OC    QCLTX,QCLTX         Was client entered?                          
         JNZ   NXTSXD08                                                         
         OC    QOFFCDE,QOFFCDE     Office code                                  
         JZ    NXTSXD08                                                         
         MVI   DTXKCLT,C'*'                                                     
         MVC   DTXKCLT+1(1),SVOFFCDE                                            
*                                                                               
NXTSXD08 OC    ID,ID                                                            
         JZ    NXTSXD18                                                         
         MVC   DTXKDESC,ID                                                      
         MVC   DTXKTYP,PPAGENO                                                  
         J     NXTSXD18                                                         
*                                                                               
NXTSXD09 CLI   QTXTYP,PATSTXTQ     Pattern STEXT?                               
         JNE   NXTSXD10                                                         
         MVI   DTXKDESC,C'-'       Look up all Pattern STEXT records            
         J     NXTSXD18                                                         
*                                                                               
NXTSXD10 DC    H'0'                Add additional STEXT type here               
*                                                                               
NXTSXD18 GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOTRFDIR+IO3'                           
         J     NXTSXD22                                                         
*                                                                               
NXTSXDSQ CLI   RECACT,C'D'         Action display                               
         JE    NOMORE              Yes, only one record displayed               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQD+IOTRFDIR+IO3'                           
*                                                                               
NXTSXD22 CLC   IOKEY(3),IOKEYSAV   Same record/agency-media?                    
         JNE   NOMORE                                                           
         CLC   IOKEY+3(2),IOKEYSAV+3 Same client                                
         JE    NXTSXD23                                                         
         CLI   IOKEY+3,C'*'        Office?                                      
         JE    NXTSXD23                                                         
         CLI   QTXTYP,PATSTXTQ     Pattern STEXT?                               
         JE    *+8                                                              
         MVI   SHWCLT,C'Y'         No, show client                              
*                                                                               
NXTSXD23 OC    QCLTX,QCLTX                                                      
         JZ    NXTSXD24                                                         
         CLC   IOKEY+3(2),QCLTX                                                 
         JE    NXTSXD24                                                         
         MVI   SHWCLT,C'Y'         Show client                                  
*                                                                               
         L     RE,ANXTCLT                                                       
         LA    RE,L'QCLTA(RE)      Point to next clt                            
         ST    RE,ANXTCLT                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
         J     NXTSXD04            Go process next client                       
*                                                                               
NXTSXD24 OC    QOFFCDE,QOFFCDE                                                  
         JZ    NXTSXD26                                                         
         CLC   IOKEY+4(1),SVOFFCDE                                              
         JE    NXTSXD26                                                         
*                                                                               
         L     RE,ANXTOFF                                                       
         LA    RE,L'QOFFCDE(RE)    Point to next office                         
         ST    RE,ANXTOFF                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMOFF         R0=remaining office count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMOFF                                                      
         J     NXTSXD05            Go process next office                       
*                                                                               
NXTSXD26 OC    ID,ID                                                            
         JZ    *+14                                                             
         CLC   DTXKDESC,ID                                                      
         JNE   NOMORE                                                           
*                                                                               
         CLI   DTXKDESC,0          If zero                                      
         JE    SIDX                Office level STEXT                           
*                                                                               
         LA    R1,IDCHARS          Check if STEXT                               
SID10    CLC   DTXKDESC(1),0(R1)                                                
         JE    SIDX                                                             
         LA    R1,1(,R1)                                                        
         CLI   0(R1),X'FF'                                                      
         JNE   SID10                                                            
         J     NXTSXDSQ            Must be dealer text                          
SIDX     DS    0H                                                               
*                                                                               
         CLI   QTXTYP,ALLSTXTQ     All STEXTs?                                  
         JE    NXTSXD70                                                         
         CLI   QTXTYP,PATSTXTQ     Pattern STEXT?                               
         JNE   NXTSXD28                                                         
         CLI   DTXKDESC,C'-'       No more Pattern STEXT records?               
         JNE   NOMORE                                                           
         CLI   DTXKTYP,C'L'        Only show page 1 for pat request             
         JNE   NXTSXDSQ                                                         
         J     NXTSXD70                                                         
*                                                                               
NXTSXD28 DC    H'0'                Add additional STEXT type here               
*                                                                               
NXTSXD70 CLC   =C'TYPE=',DTXKDESC+1                                             
         JE    NXTSXDSQ                                                         
*                                                                               
         CLI   DTXKCLT,C'*'        Office                                       
         JNE   NXTSXD72                                                         
         MVC   SVOFFCDE,DTXKCLT+1                                               
         BRAS  RE,CNVTO2           Convert 1 byte office to 2 char              
         JNE   NOMORE                                                           
         J     NXTSXD75                                                         
*                                                                               
NXTSXD72 OC    DTXKCLT,DTXKCLT                                                  
         JZ    NXTSXD75                                                         
*                                                                               
         GOTOR VCLUNPK,DMCB,DTXKCLT,QCLTA                                       
         MVC   SVKEY(L'DTXKEY),IOKEY                                            
         MVC   HALF,QCLTX                                                       
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,DUB1  Dummy call                  
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'DTXKEY),SVKEY                                            
         MVC   QCLTX,HALF                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOTRFDIR+IO3' Dummy read hi             
*                                                                               
         L     R3,ACLTREC                                                       
         USING CLTRECD,R3                                                       
         GOTOR VCLUNPK,DMCB,(CPROF+6,DTXKCLT),QCLTA                             
*                                                                               
NXTSXD75 CLI   PPAGENO,0           Looking for a specific page no?              
         JE    NXTSXD76            No                                           
         CLC   PPAGENO,DTXKTYP     Is this the page?                            
         JNE   NXTSXDSQ                                                         
*                                                                               
NXTSXD76 LLC   RE,DTXKTYP          Get page number (L=1...)                     
         LA    RF,C'L'-1                                                        
         SR    RE,RF                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'         DROP SIGN                                    
         UNPK  PAGENO(2),DUB                                                    
         CLI   PAGENO,C'0'                                                      
         JNE   *+14                                                             
         MVC   PAGENO(1),PAGENO+1                                               
         MVI   PAGENO+1,C' '                                                    
                                                                                
         MVI   CDELETE,0           Init is deleted flag                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3+IORDEL'                    
         JE    NXTSXD77                                                         
         TM    IOERR,IOEDEL        Deleted record                               
         JZ    NXTSXDSQ                                                         
         MVI   CDELETE,C'Y'        Set deleted to Y                             
*                                                                               
NXTSXD77 BRAS  RE,GETCSUM          Get record check sum                         
*                                                                               
         MVC   ELEM2+0(7),DTXKDESC                                              
         CLI   QTXTYP,ALLSTXTQ     All STEXTs?                                  
         JNE   NXTSXD80                                                         
         BAS   RE,FMTID            Format ID                                    
         J     *+10                                                             
NXTSXD80 MVC   ELEM2+0(6),DTXKDESC+1                                            
         MVC   ELEM2+10(9),=C'* Error *'                                        
         L     R3,IOADDR                                                        
         MVI   ELCODE,X'20'        Title element                                
         BRAS  RE,GETEL                                                         
         JNE   NXTSXD90                                                         
         USING DTXTLEEL,R3                                                      
         MVC   ELEM2+10(L'DTXTITLE),DTXTITLE                                    
         DROP  R3                                                               
*                                                                               
NXTSXD90 CLI   QTXTYP,PATSTXTQ     Pattern STEXT?                               
         JE    NXTSXD99                                                         
         CLI   RECACT,C'D'         Action display                               
         JNE   NXTSXD99                                                         
         OC    DTXKCLT,DTXKCLT                                                  
         JZ    NXTSXD99                                                         
         CLI   DTXKCLT,C'*'        Office                                       
         JE    NXTSXD99                                                         
         MVI   SHWCLT,C'Y'         Show client                                  
*                                                                               
NXTSXD99 MVC   LP_ADATA,AMKGREC                                                 
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
*                                                                               
NXTSTX   XC    TXTLINO,TXTLINO     Line number                                  
         XC    TXTLIN,TXTLIN       Text line                                    
*                                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTSTX30                                                         
*                                                                               
         XC    SVACMT,SVACMT       Address of last text processed               
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(DTXBOXEL-DTXRECD)(R1)                                        
NXTSTX10 CLI   0(R1),EOR           End of record?                               
         JE    EXITN                                                            
         CLI   0(R1),X'40'         Text element?                                
         JE    NXTSTX20                                                         
NXTSTX15 LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTSTX10            Bump to next element                         
*                                                                               
         USING DTXTXTEL,R1                                                      
NXTSTX20 ST    R1,SVACMT           Save address of this comment elem            
         LLC   RE,DTXTXTLN         Get text element length                      
         SHI   RE,4                Get text length -1                           
         BASR  RF,0                                                             
         MVC   TXTLIN(0),DTXTXT                                                 
         EX    RE,0(RF)                                                         
*                                                                               
         EDIT  (B1,2(R1)),(2,TXTLINO),ALIGN=LEFT Text line number               
*                                                                               
         J     NXTSTXX                                                          
*                                                                               
NXTSTX30 DS    0H                                                               
         L     R1,SVACMT           A(last comment elem)                         
         J     NXTSTX15                                                         
*                                                                               
NXTSTXX  MVC   LP_ADATA,APATREC    Point to pattern record                      
         J     EXITY                                                            
*                                                                               
CPAGENO  NTR1                                                                   
         LA    RF,QPAGENO                                                       
         LA    RE,L'QPAGENO-1                                                   
         BRAS  R1,FLEN             Find input len                               
         STC   RE,BYTE             Save input len-1                             
         LA    R2,QPAGENO                                                       
         BRAS  RE,VNUM             Validate numeric                             
                                                                                
         LLC   RE,BYTE                                                          
         LLC   RF,=C'L'                                                         
         LTR   RE,RE               IF ZERO, DON'T SUBTRACT                      
         BZ    CPG10                                                            
         BCTR  RE,0                                                             
CPG10    AR    RE,RF                                                            
         STC   RE,PPAGENO                                                       
         J     EXITY                                                            
*                                                                               
* Format ID                                                                     
FMTID    NTR1                                                                   
         USING DTXKEY,R2                                                        
         CLC   DTXKDESC+1(3),=C'MG='                                            
         JNE   FMTIDX                                                           
         MVC   WORK(10),SPACES                                                  
         MVC   WORK(4),DTXKDESC    Move *MG=                                    
*                                                                               
* Look up 1 char market group in the convert table                              
         SPACE                                                                  
         LA    R1,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
FMTID10  CLC   DTXKDESC+4(1),2(R1) Is this it                                   
         JE    FMTID20                                                          
         LA    R1,3(R1)                                                         
         JCT   RF,FMTID10                                                       
         DC    H'0'                Should not be (not it table)???              
*                                                                               
FMTID20  MVC   WORK+4(2),0(R1)     Move in scheme                               
*                                                                               
         UNPK  DUB(5),DTXKDESC+5(3)                                             
         CLI   WORK+5,X'40'        See if second char is blank                  
         JNH   *+14                                                             
         MVC   WORK+6(4),DUB        Move in the number                          
         J     FMTIDX                                                           
*                                                                               
         MVC   WORK+5(4),DUB        Move in the number                          
         MVC   ELEM2(10),WORK                                                   
FMTIDX   XIT1                                                                   
         DROP  R2                                                               
*                                                                               
*                                                                               
* Convert 2 char office code to 1                                               
*                                                                               
         USING OFFICED,RC                                                       
CNVTO1   NTR1  BASE=*,LABEL=*                                                   
         XC    OFFICED(OFCLENQ),OFFICED                                         
         MVI   OFCSYS,SYSSPTQ                                                   
         MVC   OFCAGY,LP_AGY                                                    
         MVC   OFCOFC2,QOFFCDE                                                  
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(X'01',LP_ACOM)                     
         CLI   0(R1),0                                                          
         JNE   EXITN               Done, error                                  
         MVC   SVOFFCDE,OFCOFC                                                  
*                                                                               
CNVTO1X  J     EXITY                                                            
         DROP  RC                                                               
*                                                                               
*                                                                               
* Convert 1 byte hex office code to 2 char                                      
*                                                                               
         USING OFFICED,RC                                                       
CNVTO2   NTR1  BASE=*,LABEL=*                                                   
         XC    OFFICED(OFCLENQ),OFFICED                                         
         MVI   OFCSYS,SYSSPTQ                                                   
         MVC   OFCAGY,LP_AGY                                                    
         MVC   OFCOFC,SVOFFCDE                                                  
*                                                                               
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(X'01',LP_ACOM)                     
         CLI   0(R1),0                                                          
         JNE   EXITN               Done, error                                  
         MVC   OFFCDE,OFCOFC2                                                   
*                                                                               
CNVTO2X  J     EXITY                                                            
         DROP  RC                                                               
*                                                                               
*                                                                               
VID      NTR1  BASE=*,LABEL=*                                                   
         MVC   ID,QCMTID                                                        
         CLI   QCMTID+1,C' '      If only 1 char, done                          
         JE    VIDX                                                             
*                                                                               
* Test if any special chars special text *                                      
*                                                                               
         LA    R1,IDCHARS                                                       
VID10    CLC   ID(1),0(R1)                                                      
         JE    VID20                                                            
         LA    R1,1(,R1)                                                        
         CLI   0(R1),255                                                        
         JNE   VID10                                                            
         J     VIDX                                                             
*                                                                               
* See if market group                                                           
*                                                                               
VID20    CLC   ID+1(3),=C'MG='                                                  
         JNE   VIDX                                                             
*                                                                               
VID80    CLI   ID+4,C'A'                                                        
         JL    EXITN                                                            
         CLI   ID+4,C'Z'                                                        
         JH    EXITN                                                            
*                                                                               
         MVC   BYTE,ID+4           Market group ID                              
*                                                                               
         LA    RF,ID                                                            
         LA    RE,L'ID-1                                                        
         BRAS  R1,FLEN             Find input len                               
         LR    RF,RE               Len-1                                        
         SHI   RF,4                Minus 4 (*MG=X)                              
         STC   RF,BYTE2            Save len                                     
         LA    R1,ID+5             Point to 2nd char or number                  
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
*                                                                               
         CLI   0(R1),C'A'          See if 2 char market group                   
         JL    VID84                                                            
         CLI   0(R1),C'Z'                                                       
         JH    VID84                                                            
         LA    R1,1(R1)                                                         
         SPACE                                                                  
* CONVERT 2 CHAR MARKET GROUP TO 1 CHAR                                         
         SPACE                                                                  
         LA    RE,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
         SPACE                                                                  
VID81    CLC   ID+4(2),0(RE)       Is this it                                   
         JE    VID82                                                            
         LA    RE,3(RE)                                                         
         JCT   RF,VID81                                                         
         J     EXITN                                                            
*                                                                               
VID82    MVC   BYTE,2(RE)          Move hex value from table                    
*                                                                               
         LA    RE,DUB                                                           
         LLC   RF,BYTE2            len                                          
VID84    CLI   0(R1),C'0'                                                       
         JL    EXITN                                                            
         CLI   0(R1),C'9'                                                       
         JH    EXITN                                                            
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         JCT   RF,VID84                                                         
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   ID+4(1),BYTE                                                     
         MVC   ID+5(2),WORK        MGROUP PWOS                                  
         XC    ID+7(3),ID+7                                                     
VIDX     CLI   ID,C'-'             Pattern stext?                               
         JNE   EXITY                                                            
         OC    ID,SPACES           Space pad                                    
         J     EXITY                                                            
         SPACE                                                                  
IDCHARS  DC    C'*$&&@#/=-'                                                     
         DC    X'FF'                                                            
*                                                                               
*                                                                               
***********************************************************************         
* Traffic pattern record download for list and display                *         
***********************************************************************         
                                                                                
REQTPAT  LKREQ H,I#TPATDL,OUTPAT,NEXTREQ=REQCNTT                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(I,B#SAVED,CLTIND),CHAR,OLEN=L'QCLTA,LIST=F,      +        
               TEXT=SP#CLI                                                      
PrdCode  LKREQ F,003,(D,B#SAVED,PRDCODE),CHAR,TEXT=SP#PRO                       
PrdLen   LKREQ F,004,(D,B#SAVED,QSPTLN),CHAR,TEXT=SP#SPLEN                      
PrdCode2 LKREQ F,005,(D,B#SAVED,QPRDCOD2),CHAR,TEXT=SP#PIGGY                    
PrdLen2  LKREQ F,006,(D,B#SAVED,QPTRLEN),CHAR,TEXT=SP#PTRSL                     
PatCode  LKREQ F,007,(D,B#SAVED,QCODE),CHAR,TEXT=SP#CODE                        
PatRef   LKREQ F,010,(D,B#SAVED,QREF),CHAR,TEXT=SP#SPREF                        
RecAct   LKREQ F,100,(D,B#SAVED,RECACT),CHAR,TEXT=SP#ACTN,COL=*                 
         LKREQ E                                                                
                                                                                
OUTPAT   LKOUT H                   Traffic pattern output maps start            
                                                                                
         LKOUT R,X'0027'                                                        
PRout    LKOUT P,,PATINI           Init traffic pattern arrary                  
Array    LKOUT C,1,(A,ARYPAT),FILTROUT=SHWVLDP                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                   Traffic pattern output maps end              
                                                                                
ARYPAT   LKOUT A,(R,NXTPAT),MULTIROW=Y,ROWNAME=PATRECD                          
Array    LKOUT C,X'27',(A,ARYPATD),FILTROUT=SHWPATC                             
Array    LKOUT C,2,(A,ARYCL2),FILTROUT=SHWCL2C                                  
Array    LKOUT C,3,(A,ARYTPR2),FILTROUT=SHWPR2C                                 
         LKOUT E                                                                
                                                                                
ARYPATD  LKOUT A,(R,SETPATD),MULTIROW=Y,ROWNAME=PATRECD                         
DelPat   LKOUT C,050,(D,B#SAVED,PDELETE),CHAR,ND=Y                              
Cksum    LKOUT C,200,(D,B#SAVED,SVCKSM),HEXD,ND=Y                               
Media    LKOUT C,001,(D,B#WORKD,QMEDA),CHAR                                     
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
PrdCode  LKOUT C,003,(D,B#SAVED,PRDCODE),CHAR                                   
PrdLen   LKOUT C,004,(D,B#SAVED,SVBSLN),UBIN                                    
PrdCode2 LKOUT C,005,(D,B#SAVED,PRDCOD2),CHAR,ND=Y                              
PrdLen2  LKOUT C,006,(D,B#SAVED,SVBSLN2),UBIN,ND=Y                              
PatCode  LKOUT C,007,(D,B#SAVED,CODEOUT),CHAR,ND=Y                              
CodeEst  LKOUT C,008,(D,B#SAVED,CODEEST),CHAR,ND=Y                              
PatUFN   LKOUT C,009,(D,B#SAVED,SVPATUFN),CHAR,ND=Y                             
PatRef   LKOUT C,010,(D,B#SAVED,SVREF),CHAR                                     
PatDesc  LKOUT C,011,(D,B#SAVED,SVPATDSC),CHAR                                  
PatStart LKOUT C,012,(D,B#SAVED,SVPATSTD),CHAR                                  
PatEnd   LKOUT C,013,(D,B#SAVED,SVPATEND),CHAR,ND=Y                             
PatSTime LKOUT C,014,(D,B#SAVED,SVPATSTT),CHAR,ND=Y                             
PatETime LKOUT C,015,(D,B#SAVED,SVPATENT),CHAR,ND=Y                             
PatDaily LKOUT C,016,(D,B#SAVED,SVPATDLY),CHAR,ND=Y                             
Daypart  LKOUT C,017,(D,B#SAVED,SVPATDPT),CHAR,ND=Y                             
Stext    LKOUT C,018,(D,B#SAVED,SVPATSTX),CHAR,ND=Y                             
PrdInv   LKOUT C,019,(D,B#SAVED,SVPATINV),CHAR,ND=Y                             
PatType  LKOUT C,020,(D,B#SAVED,SVPATTYP),CHAR,ND=Y                             
Array    LKOUT C,021,(A,ARYMKT2)                                                
Array    LKOUT C,022,(A,ARYSTA)                                                 
Array    LKOUT C,023,(A,ARYMGRP)                                                
Array    LKOUT C,024,(A,ARYPCML)                                                
         LKOUT C,027,(D,B#SAVED,SVPATROT),CHAR,ND=Y,FILTROUT=SHWROT             
Array    LKOUT C,028,(A,ARYPCMT)                                                
         LKOUT E                                                                
*                                                                               
ARYCL2   LKOUT A,(R,NXTCL2),MULTIROW=Y,ROWNAME=CLTRECD                          
Array    LKOUT C,2,(A,ARYTC3),FILTROUT=TSTPATC                                  
         LKOUT E                                                                
*                                                                               
ARYTPR2  LKOUT A,(R,NXTPR2),MULTIROW=Y,ROWNAME=PRDRECD                          
Array    LKOUT C,3,(A,ARYTP3)                                                   
         LKOUT E                                                                
*                                                                               
ARYTP3   LKOUT A,(D,B#PRDREC,PRDRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
PrdCd    LKOUT C,001,(D,B#WORKD,QPRDA),CHAR                                     
PrdNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
CltCd    LKOUT C,003,(D,B#WORKD,QCLTA),CHAR                                     
         LKOUT E                                                                
*                                                                               
SHWVLDP  CLI   SHWVLD,C'Y'         Valid request                                
         BR    RE                                                               
*                                                                               
SHWPR2C  CLI   SHWPRD,C'Y'         Show prods?                                  
         BR    RE                                                               
*                                                                               
SHWPATC  CLI   SHWPAT,C'Y'         Show pattern?                                
         BR    RE                                                               
*                                                                               
SHWCL2C  CLI   SHWCLT,C'Y'         Show client?                                 
         BR    RE                                                               
*                                                                               
SHWMKT   CLI   SVPATTYP,C'M'       Market specific pattern?                     
         BR    RE                                                               
*                                                                               
SHWMGRP  CLI   SVPATTYP,C'G'       Market group specific pattern?               
         BR    RE                                                               
*                                                                               
SHWSTA   CLI   SVPATTYP,C'S'       Station specific pattern?                    
         BR    RE                                                               
*                                                                               
SHWROT   CLI   SVCPCTL,0           Show rotation only if no %                   
         BR    RE                                                               
*                                                                               
TSTPATC  CLI   PATDLSW1,YESQ       Pattern record downloaded?                   
         BR    RE                                                               
*                                                                               
ARYMKT2  LKOUT A,(D,B#WORKD,ELEM2),NROWS=50,ROWWIDTH=5,ROWNAME=DUMMY_D          
MktCd    LKOUT C,021,DUM_LIN1,(R,EDPMKT),FILTROUT=SHWMKT,ND=Y                   
         LKOUT E                                                                
*                                                                               
ARYSTA   LKOUT A,(D,B#WORKD,ELEM2),NROWS=50,ROWWIDTH=5,ROWNAME=DUMMY_D          
StaCd    LKOUT C,022,DUM_LIN1,(R,EDPSTA),ND=Y,FILTROUT=SHWSTA                   
         LKOUT E                                                                
*                                                                               
ARYMGRP  LKOUT A,(D,B#WORKD,ELEM2),NROWS=50,ROWWIDTH=5,ROWNAME=DUMMY_D          
GrpCd    LKOUT C,023,DUM_LIN1,(R,EDPMGRP),ND=Y,FILTROUT=SHWMGRP                 
         LKOUT E                                                                
*                                                                               
ARYPCML  LKOUT A,(D,B#WORKD,ELEM1),NROWS=15,ROWWIDTH=16,ROWNAME=DUMMY_D         
PatCml   LKOUT C,024,DUM_LIN1,(R,EDPCML),ND=Y                                   
PatCml2  LKOUT C,025,DUM_LIN1,(R,EDPCML2),ND=Y                                  
PatPct   LKOUT C,026,DUM_LIN1,(R,EDPCT),ND=Y                                    
         LKOUT E                                                                
*                                                                               
ARYPCMT  LKOUT A,(R,NXTCMT),MULTIROW=Y,ROWNAME=PATRECD                          
PatCmt   LKOUT C,028,(D,B#SAVED,SVPATCMT),CHAR,ND=Y                             
         LKOUT E                                                                
*                                                                               
ARYTC3   LKOUT A,(D,B#PATREC,PATRECD),NEWEL=Y,NROWS=1,ROWWIDTH=0                
CltCd    LKOUT C,001,(D,B#WORKD,QCLTA),CHAR                                     
CltNm    LKOUT C,002,(D,B#WORKD,WORK2),CHAR                                     
         LKOUT E                                                                
*                                                                               
SETPATD  DS    0H                                                               
         CLI   DONESW,C'Y'                                                      
         JE    EXITN                                                            
                                                                                
         MVI   DONESW,C'Y'                                                      
         MVC   LP_ADATA,APATREC    Point to traffic pattern record              
         J     EXITY                                                            
*                                                                               
PATINI   MVI   SHWVLD,C'N'         Init request is invalid                      
         OC    MEDCOD,SPACES                                                    
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   EXITN                                                            
*                                                                               
         XC    BREF,BREF                                                        
         XC    BREFSUB,BREFSUB                                                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,ACLT           Have any client code to look up?             
         JZ    NOMORE                                                           
                                                                                
         MVC   NUMCLT,LW_NUMN-LW_D(RE) Number of clients to process             
         AHI   RE,LW_LN2Q          Point to first client                        
         ST    RE,ANXTCLT          Set A(client request)                        
*                                                                               
         XC    QCLTX,QCLTX         Init binary client code                      
         MVC   QCLTA,0(RE)         Get input client code                        
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   EXITN                                                            
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   EXITN                                                            
*                                                                               
         MVI   CLTCHG,C'N'         Init client changed flag                     
*                                                                               
         L     R0,AIO8                                                          
         L     R1,AIO8                                                          
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,AIO8                                                          
         ST    RE,ABPRLIST         Binary product codes list                    
         ST    RE,ABPRNXT          Current binary product code pointer          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    Get address of TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A11'  Get address of untime                    
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VUNTIME,0(R1)                                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D9000A'      Get address of MSUNPK                  
         MVI   DMCB+7,QMSUNPK                                                   
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VMSUNPK,0(R1)                                                    
*                                                                               
         MVI   CSVBPRD,0                                                        
         MVI   CSVBPRD2,0                                                       
         MVI   CSVBSLN,0                                                        
         MVI   CSVBSLN2,0                                                       
*                                                                               
         MVI   SVBPRD,0                                                         
         MVI   SVBPRD2,0                                                        
         MVI   SVBSLN,0                                                         
         MVI   SVBSLN2,0                                                        
*                                                                               
         BRAS  RE,VCC              Validate copy code                           
*                                                                               
         CLC   PRDCODE,SPACES                                                   
         JNE   *+12                                                             
         CLI   RECACT,C'L'                                                      
         JE    PATINIX                                                          
*                                                                               
         GOTOR (#VALPRD,AVALPRD),DMCB,PRDCODE,L'PRDCODE,SVBPRD                  
         JNE   NOMORE              Bad product                                  
         MVC   CSVBPRD,SVBPRD                                                   
*                                                                               
         CLC   QSPTLN,SPACES                                                    
         JH    *+12                                                             
         CLI   RECACT,C'L'                                                      
         JE    PATINI04                                                         
*                                                                               
         LA    RF,L'QSPTLN                                                      
         CLI   QSPTLN+2,C' '                                                    
         JNE   *+6                                                              
         BCTR  RF,0                                                             
         CLI   QSPTLN+1,C' '                                                    
         JNE   *+6                                                              
         BCTR  RF,0                                                             
         GOTOR (#VALSLN,AVALSLN),DMCB,QSPTLN,(RF),SVBSLN                        
         MVC   CSVBSLN,SVBSLN                                                   
*                                                                               
PATINI04 CLC   QPRDCOD2,SPACES                                                  
         JNH   PATINI20                                                         
*                                                                               
         GOTOR (#VALPRD,AVALPRD),DMCB,QPRDCOD2,L'QPRDCOD2,SVBPRD2               
         JNE   NOMORE              Bad product                                  
         MVC   CSVBPRD2,SVBPRD2                                                 
*                                                                               
         CLC   QPTRLEN,SPACES                                                   
         JNH   PATINIX                                                          
*                                                                               
         LA    RF,L'QPTRLEN                                                     
         CLI   QPTRLEN+2,C' '                                                   
         JNE   *+6                                                              
         BCTR  RF,0                                                             
         CLI   QPTRLEN+1,C' '                                                   
         JNE   *+6                                                              
         BCTR  RF,0                                                             
         GOTOR (#VALSLN,AVALSLN),DMCB,QPTRLEN,(RF),SVBSLN2                      
         MVC   CSVBSLN2,SVBSLN2                                                 
         MVC   PTRLEN,QPTRLEN                                                   
*                                                                               
PATINI20 OC    QREF,QREF                                                        
         JZ    PATINIX                                                          
*                                                                               
         LA    RF,QREF                                                          
         LA    RE,L'QREF-1                                                      
         BRAS  R1,FLEN             Find input len                               
         LR    R1,RE               Len-1                                        
         BASR  RE,0                                                             
         PACK  DUB,QREF(0)                                                      
         EX    R1,0(RE)                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,BREF                                                        
         X     R0,=XL4'00003FFF'                                                
         SLL   R0,10                                                            
         STCM  R0,7,BREFSUB                                                     
         STCM  R0,7,CBREFSUB                                                    
*                                                                               
PATINIX  MVI   SHWVLD,C'Y'                                                      
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTPAT   MVI   DONESW,C'N'         For setpatd routine                          
*                                                                               
         CLI   CLTCHG,C'Y'                                                      
         JNE   NXTPAT02                                                         
         MVI   CLTCHG,C'N'                                                      
         MVI   PATDLSW1,NOQ        No pattern rec downloaded yet                
*                                                                               
         L     R0,ABPRLIST         Clear prod list area                         
         L     R1,ABPRLIST         in AIO8                                      
         AHI   R1,IO8LQ                                                         
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,ABPRLIST                                                      
         ST    RE,ABPRNXT          Re-init current prod code pointer            
*                                                                               
         MVC   SVBPRD,CSVBPRD                                                   
         MVC   SVBSLN,CSVBSLN                                                   
         MVC   SVBPRD2,CSVBPRD2                                                 
         MVC   SVBSLN2,CSVBSLN2                                                 
         MVC   CCODE,CCCODE                                                     
         MVC   BREFSUB,CBREFSUB                                                 
*                                                                               
* Next client in the list                                                       
         L     RE,ANXTCLT                                                       
         LA    RE,L'QCLTA(RE)      Point to next clt                            
         ST    RE,ANXTCLT                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
*                                                                               
         MVC   QCLTA,0(RE)                                                      
         GOTOR (#VALCLT,AVALCLT),DMCB,QCLTA,3,QCLTX                             
         JNE   NOMORE                                                           
                                                                                
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NOMORE                                                           
         J     *+12                                                             
NXTPAT02 CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTPATSQ                                                         
*                                                                               
         MVI   PATDLSW1,NOQ        No pattern rec downloaded yet                
         MVI   SHWPAT,C'Y'         Preset show pattern                          
         MVI   SHWCLT,C'N'         Preset not to show clts                      
*                                                                               
NXTPAT03 XC    SVPATFLS(SVPATLQ),SVPATFLS Init saved pattern flds               
         LA    R4,IOKEY                                                         
         USING PATKEY,R4                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,QMEDX                                                     
         MVC   PATKCLT,QCLTX                                                    
         MVC   PATKPRD,SVBPRD                                                   
         MVC   PATKSLN,SVBSLN                                                   
         MVC   PATKPRD2,SVBPRD2                                                 
         MVC   PATKSLN2,SVBSLN2                                                 
         MVC   PATKCODE,CCODE                                                   
         MVC   PATKREF,BREFSUB                                                  
                                                                                
         CLI   RECACT,C'D'         Action display                               
         JNE   NXTPATHI                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+#PATREC'                        
         CLC   IOKEY(13),IOKEYSAV  Found exact match                            
         JE    NXTPAT10                                                         
         CLC   IOKEY(PATKREF-PATKEY),IOKEYSAV  Same upto reference              
         JNE   NOMORE                                                           
                                                                                
         SR    R0,R0                                                            
         ICM   R0,7,PATKREF                                                     
         STCM  R0,7,BREFSUB                                                     
         SRDL  R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         CLM   R0,3,BREF                                                        
         JNE   NOMORE                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+#PATREC'                        
         CLC   IOKEY(13),IOKEYSAV  Found exact match                            
         JE    NXTPAT10                                                         
                                                                                
         ICM   R0,7,PATKREF                                                     
         N     R0,=X'FFFFFC00'                                                  
         O     R0,=X'000003FE'     SET FOR HIGHEST SUBLINE                      
         STCM  R0,7,PATKREF                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+#PATREC'                        
         CLC   IOKEY(13),IOKEYSAV  Found exact match                            
         JE    NXTPAT10                                                         
         JNE   NOMORE                                                           
*                                                                               
NXTPATHI GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+#PATREC'                        
         DROP  R4                                                               
*                                                                               
NXTPAT10 LA    RE,IOKEY                                                         
         USING PATKEY,RE                                                        
         CLC   IOKEY(PATKPRD-PATKEY),IOKEYSAV  Same a/m/clt?                    
         JE    NXTPAT11                                                         
         MVI   SHWPAT,C'N'         Do not show pat                              
         MVI   SHWCLT,C'Y'         Show client                                  
         MVI   SHWPRD,C'Y'              product                                 
         MVI   CLTCHG,C'Y'         Turn on client changed indicator             
         MVC   LP_ADATA,APATREC    Point to traffic pattern record              
         J     EXITY                                                            
*                                                                               
NXTPAT11 CLI   RECACT,C'D'                                                      
         JE    NXTPAT12                                                         
*                                                                               
         CLI   CSVBPRD,0                                                        
         JE    *+14                                                             
         CLC   PATKPRD,CSVBPRD                                                  
         JNE   NXTPATSQ                                                         
                                                                                
         CLI   CSVBSLN,0                                                        
         JE    *+14                                                             
         CLC   PATKSLN,CSVBSLN                                                  
         JNE   NXTPATSQ                                                         
                                                                                
*                                                                               
         CLI   CSVBPRD2,0                                                       
         JE    *+14                                                             
         CLC   PATKPRD2,CSVBPRD2                                                
         JNE   NXTPATSQ                                                         
                                                                                
         CLI   CSVBSLN2,0                                                       
         JE    *+14                                                             
         CLC   PATKSLN2,CSVBSLN2                                                
         JNE   NXTPATSQ                                                         
                                                                                
         CLI   CCCODE,0                                                         
         JE    *+14                                                             
         CLC   PATKCODE,CCCODE                                                  
         JNE   NXTPATSQ                                                         
*                                                                               
*NOP     CLI   CBREFSUB,0                                                       
*        JE    *+14                                                             
*        CLC   PATKREF,CBREFSUB                                                 
*****    JNE   NXTPATSQ                                                         
                                                                                
NXTPAT12 TM    IOKEY+13,X'01'      Bpat?                                        
         JO    NXTPATSQ            No bpat records in this release              
*                                                                               
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               Right justify, dropping subline              
         X     R1,=XL4'00003FFF'   Now make positive                            
         LR    R0,R1                                                            
         EDIT  (R0),(5,SVREF),ALIGN=LEFT                                        
                                                                                
         MVC   SVBPRD,PATKPRD      Save binary prod                             
         MVC   SVBSLN,PATKSLN           len                                     
         MVC   SVBPRD2,PATKPRD2         prod 2                                  
         MVC   SVBSLN2,PATKSLN2         len 2                                   
         MVC   CCODE,PATKCODE           copy code                               
*                                                                               
         LA    R2,PATKPRD                                                       
NXTPA12C L     RF,ABPRLIST         Start of prod table                          
         L     R1,ABPRNXT          Current end table                            
NXTPA12F CR    RF,R1                                                            
         JE    NXTPAT13            Add prod to table                            
         CLC   0(1,R2),0(RF)       Match on prod                                
         JE    NXTPAT14                                                         
         LA    RF,1(RF)                                                         
         J     NXTPA12F                                                         
*                                                                               
NXTPAT13 MVC   0(1,R1),0(R2)       Save prod in table                           
         LA    R1,1(R1)                                                         
         ST    R1,ABPRNXT                                                       
*                                                                               
NXTPAT14 LA    RF,PATKPRD2                                                      
         CR    R2,RF               Processed prd2 ?                             
         JE    NXTPA14X                                                         
         LA    R2,PATKPRD2                                                      
         CLI   0(R2),0                                                          
         JNE   NXTPA12C                                                         
         DROP  RE                                                               
*                                                                               
NXTPA14X GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+#PATREC'                       
         JNE   NXTPATSQ                                                         
         XC    SVPATUFN(SVPATLQ),SVPATUFN Init saved pattern flds               
         XC    ELEM1,ELEM1         List of commercials                          
         XC    ELEM2,ELEM2         List of mkts/sta or mgrp                     
*                                                                               
         BRAS  RE,GETCSUM2         Get record check sum                         
*                                                                               
         L     R3,IOADDR                                                        
         LA    R3,(PATDTAEL-PATRECD)(R3)  X'10' element                         
         USING PATDTAEL,R3                                                      
         MVC   SVPATDSC(L'SVPATDSC),PATDESC                                     
         TR    SVPATDSC,TRTAB      Translate data                               
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    PATSTAT1,PATSADID   Cmls are adid?                               
         JO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVI   CCODEST,C'Y'        Preset copy code estimate                    
         TM    PATSTAT,X'10'       Copy code estimate?                          
         JO    *+8                                                              
         MVI   CCODEST,C'N'                                                     
*                                                                               
         TM    PATSTAT,X'80'                                                    
         JZ    NXTPAT15                                                         
         MVI   PDELETE,C'Y'        Deleted pat                                  
*                                                                               
NXTPAT15 GOTOR VDATCON,DMCB,(3,PATSTART),(5,SVPATSTD)                           
*                                                                               
         CLC   PATEND,=X'FFFFFF'   Pat end date until further notice ?          
         JE    NXTPAT16                                                         
*                                                                               
         GOTOR (RF),DMCB,(3,PATEND),(5,SVPATEND)                                
         J     NXTPAT18                                                         
*                                                                               
NXTPAT16 XC    SVPATEND,SVPATEND                                                
         MVI   SVPATUFN,YESQ       Set 'until further notice' flag to Y         
         DROP  R3                                                               
*                                                                               
NXTPAT18 L     R1,IOADDR                                                        
         LA    R1,(PATDTAEL-PATRECD)(R1)                                        
         USING PATDTAEL,R1                                                      
*                                                                               
         LA    R2,PATSTIM                                                       
         LA    R3,SVPATSTT                                                      
         BRAS  RE,GOUNTIME                                                      
*                                                                               
         LA    R2,PATETIM                                                       
         LA    R3,SVPATENT                                                      
         BRAS  RE,GOUNTIME                                                      
*                                                                               
         MVC   SVPATDPT,PATDPT                                                  
*                                                                               
         MVI   SVPATINV,C'N'       Preset inverted prods to no                  
         TM    PATSTAT,X'04'                                                    
         JZ    *+8                                                              
         MVI   SVPATINV,C'Y'                                                    
*                                                                               
         MVI   SVPATDLY,C'N'       Preset times daily to no                     
         TM    PATSTAT1,X'04'                                                   
         JZ    *+8                                                              
         MVI   SVPATDLY,C'Y'                                                    
         DROP  R1                                                               
                                                                                
NXTPAT20 CLI   0(R1),EOR           End of record?                               
         JE    NXTPAT90                                                         
         CLI   0(R1),X'20'         Pattern list element?                        
         JE    NXTPAT40                                                         
         CLI   0(R1),X'30'         Commercial list element?                     
         JE    NXTPAT30                                                         
         CLI   0(R1),X'32'         Rotation element?                            
         JE    NXTPAT50                                                         
         CLI   0(R1),X'34'         Percentage element?                          
         JE    NXTPAT60                                                         
         CLI   0(R1),X'50'         Comclass                                     
         JE    NXTPAT70                                                         
NXTPATNX LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTPAT20            Bump to next element                         
*                                                                               
         USING PATCMLEL,R1                                                      
NXTPAT30 LLC   RF,PATCMLLN                                                      
         SHI   RF,3                Number of cmls                               
         BASR  RE,0                                                             
         MVC   ELEM1(0),PATCML     Save cml list                                
         EX    RF,0(RE)                                                         
         J     NXTPATNX                                                         
         DROP  R1                                                               
*                                                                               
         USING PATLSTEL,R1                                                      
NXTPAT40 MVC   SVPATTYP,PATLSTTY   Pattern type (Mkt,Sta,Grp)                   
                                                                                
         LLC   RF,PATLSTLN                                                      
         SHI   RF,4                Number of mkt/sta or mgrp                    
         CHI   RF,0                                                             
         JNH   NXTPATNX                                                         
         BASR  RE,0                                                             
         MVC   ELEM2(0),PATLST     Save the list                                
         EX    RF,0(RE)                                                         
         OC    PATLST(5),PATLST                                                 
         JNZ   NXTPATNX                                                         
         MVI   ELEM2,X'FF'         M=ALL                                        
         J     NXTPATNX                                                         
         DROP  R1                                                               
*                                                                               
         USING PATPTNEL,R1                                                      
NXTPAT50 LLC   RE,PATPTNLN         Rotation element x'32'                       
         AHI   RE,-3                                                            
         BASR  RF,0                                                             
         MVC   SVPATROT(0),PATPTN                                               
         EX    RE,0(RF)            Move in rotation                             
         J     NXTPATNX                                                         
         DROP  R1                                                               
*                                                                               
         USING PATPCTEL,R1                                                      
NXTPAT60 OC    SVCPCTL,SVCPCTL     Got percentage already?                      
         JNZ   NXTPATNX            Use 1st one (user vs derived)                
         LLC   RE,PATPCTLN         Percentage element x'34'                     
         AHI   RE,-3                                                            
         BASR  RF,0                                                             
         MVC   SVCPCTL(0),PATPCTLT                                              
         EX    RE,0(RF)            Move in cml alpha and percentage             
         XC    SVPATROT,SVPATROT   Do not show rotation if % entered            
         J     NXTPATNX                                                         
         DROP  R1                                                               
*                                                                               
         USING PATTXTEL,R1                                                      
NXTPAT70 MVC   SVPATSTX,PATTXTKY+1 Special text                                 
         J     NXTPATNX                                                         
         DROP  R1                                                               
*                                                                               
NXTPAT90 DS    0H                  To extract more fields if needed             
         LA    R2,SVBPRD                                                        
         LA    R3,PRDCODE                                                       
         BRAS  RE,GCPRD            Get 3 char prod code                         
         LA    R2,SVBPRD2                                                       
         LA    R3,PRDCOD2                                                       
         BRAS  RE,GCPRD            Get 3 char prod code                         
*                                                                               
         XC    CODEOUT,CODEOUT                                                  
         MVI   CODEEST,0                                                        
         MVC   CODEOUT(L'CCODE),CCODE                                           
         CLI   CCODE,0                                                          
         JE    NXTPAT99                                                         
         CLI   CCODEST,C'Y'        This a copy code=est                         
         JNE   NXTPAT99                                                         
         EDIT  (B1,CCODE),(3,CODEOUT),ALIGN=LEFT                                
         MVI   CODEEST,C'Y'        Copy code=est                                
*                                                                               
NXTPAT99 CLI   RECACT,C'D'                                                      
         JNE   *+12                                                             
         MVI   SHWCLT,C'Y'         Show client                                  
         MVI   SHWPRD,C'Y'              product                                 
*                                                                               
         MVI   PATDLSW1,YESQ       Pattern rec downloaded                       
         MVC   LP_ADATA,APATREC    Point to traffic pattern record              
         J     EXITY                                                            
*                                                                               
NXTPATSQ CLI   RECACT,C'D'         Action display                               
         JE    NOMORE              Yes, display only one record                 
*                                                                               
         BRAS  RE,NXK                                                           
         J     NXTPATHI                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCL2   CLI   SHWCLT,C'Y'         Show client?                                 
         JNE   EXITN                                                            
*                                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCL10                                                          
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         XC    WORK2,WORK2         Init client name for output                  
         MVC   WORK2(L'CNAME),CNAME                                             
         J     NXTCL20                                                          
         DROP  RE                                                               
*                                                                               
NXTCL10  DS    0H                  To process more client rec if needed         
         J     NOMORE                                                           
*                                                                               
NXTCL20  DS    0H                  To extract more fields if needed             
*                                                                               
         MVI   SHWCLT,C'N'         Do not show clt                              
         MVI   SHWPRD,C'Y'         Show product                                 
*                                                                               
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDPMKT   LM    R2,R4,LP_AINP                                                    
         CLI   SVPATTYP,C'M'                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         OC    0(5,R2),0(R2)       Any input?                                   
         JZ    XCOLEN                                                           
         XC    0(4,R4),0(R4)       Init output                                  
         CLI   0(R2),X'FF'         'All' mkts?                                  
         JNE   EDPMK02                                                          
         MVI   SVPATTYP,0          Fake it so we only come here once            
                                                                                
         MVC   0(3,R4),=C'ALL'                                                  
         J     EDPMKX                                                           
*                                                                               
EDPMK02  SR    R1,R1                                                            
         ICM   R1,3,3(R2)                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB+5(3)                                                 
         CLI   WORK+1,C'0'         1st a zero                                   
         JNE   EDPMK05                                                          
         MVC   0(3,R4),WORK+2      Only 3 digits of market                      
         J     EDPMKX                                                           
*                                                                               
EDPMK05  MVC   0(4,R4),WORK+1      All 4 of market                              
*                                                                               
EDPMKX   LA    R0,4                Max length for mkt code                      
         J     SETOLENX                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDPSTA   LM    R2,R4,LP_AINP                                                    
         CLI   SVPATTYP,C'S'                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),0             Any input?                                   
         JE    XCOLEN                                                           
*                                                                               
         MVC   0(5,R4),0(R2)       Preset to show WABCA for WABC-AM             
         CLI   MEDCOD,C'T'         Media T                                      
         JNE   *+16                No                                           
         CLI   4(R4),C'T'          Is this T for TV                             
         JNE   *+8                 No, must be DV show WABCD                    
         MVI   4(R4),X'40'                                                      
*                                                                               
         LA    R0,5                Max length for station code                  
         J     SETOLENX                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDPMGRP  LM    R2,R4,LP_AINP                                                    
         CLI   SVPATTYP,C'G'                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),0             Any input?                                   
         JE    XCOLEN                                                           
         XC    0(3,R4),0(R4)       Init output                                  
*                                                                               
         LA    R1,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
EDPMG05  CLC   0(1,R2),2(R1)      Is this it                                    
         JE    EDPMG10                                                          
         LA    R1,3(R1)                                                         
         JCT   RF,EDPMG05                                                       
         DC    H'0'                Should not be (not in table)???              
*                                                                               
EDPMG10  MVC   0(2,R4),0(R1)       Move in letter of market group               
*                                                                               
         UNPK  DUB(5),1(3,R2)                                                   
         CLI   1(R2),X'40'         See if second char is blank                  
         JNH   EDPMG12                                                          
*                                                                               
         MVC   2(4,R4),DUB         Move 4 digitd after 2 char mkt grp           
         J     EDPMGX                                                           
*                                                                               
EDPMG12  MVC   1(4,R4),DUB         Then 4 digits                                
*                                                                               
EDPMGX   LA    R0,6                Max length for mgroup code                   
         J     SETOLENX                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDPCML   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0             Any input?                                   
         JE    XCOLEN                                                           
         XC    0(12,R4),0(R4)      Cml 1                                        
         XC    SVCML2,SVCML2                                                    
                                                                                
         MVI   0(R4),C'*'                                                       
         CLC   =X'5C00',0(R2)                                                   
         JE    EDPCMX                                                           
         MVC   0(8,R4),0(R2)                                                    
         CLC   =C'HIATUS',0(R2)                                                 
         JE    EDPCMX                                                           
*                                                                               
         OC    0(8,R4),0(R4)                                                    
         JZ    *+14                                                             
         MVC   SVCML2,8(R2)                                                     
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         JNE   EDPCM10                                                          
         XC    0(16,R4),0(R4)      Clear cml1 and cml2                          
         GOTO1 VTRPACK,DMCB,(C'U',0(R2)),0(R4)                                  
         OC    8(8,R2),8(R2)                                                    
         JZ    EDPCM10                                                          
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',8(R2)),SVCML2                                 
*                                                                               
EDPCM10  DS    0H                                                               
         OC    SVCPCTL,SVCPCTL                                                  
         JZ    EDPCMX                                                           
*                                                                               
* Find displacement to pct                                                      
         LA    R1,ELEM1            List of cmls                                 
         LR    RE,R2               Current cml                                  
         SR    RE,R1                                                            
                                                                                
         LTR   RE,RE                                                            
         JZ    EDPCM12                                                          
                                                                                
         SRL   RE,4                Div by 16                                    
         MHI   RE,3                % entry is 3 bytes long                      
EDPCM12  LA    R1,SVCPCTL          List of cml alphas and %                     
         AR    R1,RE                                                            
         CLI   2(R1),100           100% ?                                       
         JE    EDPCM15                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(R1)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HALF,DUB                                                         
         MVC   SVCPCT,HALF                                                      
         J     *+10                                                             
EDPCM15  MVC   SVCPCT1,=C'100'                                                  
*                                                                               
EDPCMX   LA    R0,12                                                            
         J     SETOLENX                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDPCML2  LM    R2,R4,LP_AINP                                                    
         CLI   8(R2),0             Any input?                                   
         JE    XCOLEN                                                           
         MVC   0(12,R4),SVCML2                                                  
         LA    R0,12                                                            
         J     SETOLENX                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDPCT    LM    R2,R4,LP_AINP                                                    
         CLI   SVCPCT1,0           Any percentages?                             
         JE    XCOLEN                                                           
         MVC   0(3,R4),SVCPCT1                                                  
         XC    SVCPCT1,SVCPCT1                                                  
         LA    R0,3                                                             
         J     SETOLENX                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCMT   XC    SVPATCMT,SVPATCMT                                                
*                                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCMT30                                                         
*                                                                               
         XC    SVACMT,SVACMT                                                    
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,(PATDTAEL-PATRECD)(R1)                                        
NXTCMT10 CLI   0(R1),EOR           End of record?                               
         JE    EXITN                                                            
         CLI   0(R1),X'40'         Comment element?                             
         JE    NXTCMT20                                                         
NXTCMT15 LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     NXTCMT10            Bump to next element                         
*                                                                               
         USING PATCMTEL,R1                                                      
NXTCMT20 ST    R1,SVACMT           Save address of this comment elem            
         LLC   RE,PATCMTLN         Get comment elem len                         
         SHI   RE,4                GeT actual comment len-1                     
         BASR  RF,0                                                             
         MVC   SVPATCMT(0),PATCMT                                               
         EX    RE,0(RF)            Move in rotation                             
         J     NXTCMTX                                                          
*                                                                               
NXTCMT30 DS    0H                                                               
         L     R1,SVACMT           A(last comment elem)                         
         J     NXTCMT15                                                         
*                                                                               
NXTCMTX  MVC   LP_ADATA,APATREC    Point to pattern record                      
         J     EXITY                                                            
*                                                                               
*                                                                               
NXTPR2   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTPR05                                                          
                                                                                
         XC    ELEM2,ELEM2                                                      
         L     RF,ABPRLIST         Binary product codes                         
         L     R1,ABPRNXT                                                       
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),0(RF)                                                   
         EX    R1,0(RE)                                                         
         LA    R2,ELEM2                                                         
         ST    R2,FULL                                                          
         J     NXTPR07                                                          
                                                                                
NXTPR05  L     R2,FULL                                                          
         LA    R2,1(R2)            Point to next binary prod                    
         ST    R2,FULL                                                          
                                                                                
NXTPR07  CLI   0(R2),X'00'         EOL                                          
         JE    NXTPR2X                                                          
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST            Point to 1st list of products                
         LA    R1,220                                                           
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTPR15                                                          
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         LA    RF,CLIST2           Point to 2nd list of products                
         LA    R1,35                                                            
         CLC   0(1,R2),3(RF)       Binary prd code match that of list?          
         JE    NXTPR15                                                          
         AHI   RF,4                                                             
         JCT   R1,*-14                                                          
         DROP  RE                                                               
*                                                                               
NXTPR15  XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING PKEY,RE                                                          
         MVI   PKEYTYPE,PKEYTYPQ                                                
         MVC   PKEYAM,QMEDX                                                     
         MVC   PKEYCLT,QCLTX                                                    
         MVC   PKEYPRD,0(RF)       Character format prd code from table         
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(L'PKEY),IOKEYSAV                                           
         JNE   NXTPR17                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   NXTPR17                                                          
         J     NXTPR20             Now extract prd fields for output            
*                                                                               
NXTPR17  XC    QPRDA,QPRDA         Return prd code - character                  
         XC    WORK2,WORK2         Return prd name                              
         MVC   QPRDA,=C'???'       Bad product                                  
         MVC   WORK2(3),=C'???'                                                 
         J     NXTPR25                                                          
*                                                                               
NXTPR20  L     R1,IOADDR                                                        
         USING PRDHDR,R1                                                        
         XC    QPRDA,QPRDA         Return prd code - character                  
         XC    WORK2,WORK2         Return prd name                              
         MVC   QPRDA,PKEYPRD                                                    
         MVC   WORK2(L'PNAME),PNAME                                             
         J     NXTPR25                                                          
         DROP  R1                                                               
*                                                                               
NXTPR2X  DS    0H                                                               
         MVI   SHWPRD,C'N'         Do not show products                         
         J     NOMORE                                                           
*                                                                               
NXTPR25  DS    0H                                                               
         MVC   LP_ADATA,ACLTREC    Point to client record                       
         J     EXITY                                                            
*                                                                               
GOUNTIME NTR1  BASE=*,LABEL=*                                                   
         XC    0(5,R3),0(R3)       Init output area                             
*                                                                               
         OC    0(2,R2),0(R2)       Test no time present                         
         JZ    EXITY                                                            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R2)                                                    
         XC    WORK,WORK                                                        
         GOTO1 VUNTIME,DMCB,FULL,WORK                                           
         MVC   0(5,R3),WORK                                                     
         J     EXITY                                                            
*                                                                               
*                                                                               
* Get 3 char product code                                                       
*                                                                               
GCPRD    NTR1  BASE=*,LABEL=*                                                   
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
GCPRD20  CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    GCPRD40                                                          
         LA    RF,4(RF)                                                         
         JCT   R1,GCPRD20                                                       
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
GCPRD24  CLC   0(1,R2),3(RF)       Match that of product list?                  
         JE    GCPRD40                                                          
         LA    RF,4(RF)                                                         
         JCT   R1,GCPRD24                                                       
         JNE   EXITN                                                            
                                                                                
GCPRD40  MVC   0(3,R3),0(RF)        3 char prod code                            
         J     EXITY                                                            
         DROP  RE                                                               
*                                                                               
*                                                                               
NXK      NTR1  BASE=*,LABEL=*                                                   
         LA    R4,IOKEY                                                         
         USING PATKEY,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               Right justify, dripping subline              
         X     R1,=XL4'00003FFF'   Now make positive                            
         STCM  R1,3,BREF                                                        
         SHI   R1,1                Build next key                               
         JP    NXK20               If still not zero, ok                        
         LLC   R1,PATKCODE         Bump code                                    
         LA    R1,1(,R1)                     By 1                               
         STC   R1,PATKCODE                                                      
         CLI   PATKCODE,0          If over 255, bump spot len                   
         JNE   NXK10                                                            
         LLC   R1,PATKSLN2        Bump spot len2                                
         LA    R1,1(,R1)                     By 1                               
         STC   R1,PATKSLN2                                                      
NXK10    SR    R1,R1               Set bref                                     
         J     *+8                         Zeto and leave it zero               
NXK20    X     R1,=XL4'00003FFF'             Reset ref to 1's compl             
         SLL   R1,10                         And subline zero                   
         STCM  R1,7,PATKREF                                                     
         J     EXITY                                                            
         DROP  R4                                                               
*                                                                               
*                                                                               
GETCSUM2 NTR1  BASE=*,LABEL=*                                                   
         L     RE,IOADDR           RE=A(A(RECORD))                              
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,SVCKSM                                                     
         J     EXITY                                                            
*                                                                               
*                                                                               
*------------------------------------------------------------                   
* VALIDATE COPY CODE (MAY BE ESTIMATE IF T1 PROFILE 12 ON) *                    
*------------------------------------------------------------                   
*                                                                               
VCC      NTR1  BASE=*,LABEL=*                                                   
         MVI   CCODE,0                                                          
*                                                                               
         CLI   QCODE,0             Any entry                                    
         JE    VCC30                                                            
         MVC   CODEOUT,QCODE                                                    
*                                                                               
         LA    RF,QCODE                                                         
         LA    RE,L'QCODE-1                                                     
         BRAS  R1,FLEN             Find input len                               
         STC   RE,BYTE             Save input len-1                             
         LA    R2,QCODE                                                         
         BRAS  RE,VNUM             Validate numeric                             
*                                                                               
         MVC   CCCODE,BYTE         Move actual len                              
         MVC   CCODE,BYTE                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING ESTRECD,R4                                                       
*                                                                               
         MVC   EKEYAM,QMEDX                                                     
         MVC   EKEYCLT,QCLTX                                                    
         MVC   EKEYPRD,PRDCODE                                                  
         MVC   EKEYEST,CCCODE                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   NOMORE              Estimate not found                           
*                                                                               
         L     R4,AIO7                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
*                                                                               
         CLI   ECOPY,0             Must not be copy code                        
         JNE   NOMORE                                                           
*                                                                               
         J     VCCX                                                             
         DROP  R4                                                               
*                                                                               
VCC30    CLI   RECACT,C'L'         For list need only media clt                 
         JE    VCCX                                                             
*                                                                               
         CLI   QCODE+1,X'40'                                                    
         JH    NOMORE                                                           
*                                                                               
         MVC   CCODE,QCODE                                                      
VCCX     J     EXITY                                                            
*                                                                               
*---------------------------------------------------                            
* Find input len-1                                                              
* (on entry RF=field, RE=max field len                                          
*---------------------------------------------------                            
FLEN     AR    RE,RF               Point to the end of field                    
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         SR    RE,RF               Len -1                                       
         BR    R1                                                               
*                                                                               
*                                                                               
*----------------------------------                                             
* VALIDATE NUMERIC                                                              
*----------------------------------                                             
*                                                                               
VNUM     NTR1  BASE=*,LABEL=*                                                   
         LLC   R1,BYTE             Len-1 of input                               
         MVC   WORK(6),=6X'F0'                                                  
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVZ   WORK(0),0(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         JNE   NOMORE              Invalid number                               
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         JZ    NOMORE                                                           
         CHI   R1,255                                                           
         JH    NOMORE                                                           
         STC   R1,BYTE             Save actual                                  
         J     EXITY                                                            
*                                                                               
*                                                                               
***********************************************************************         
* Traffic contact record download                                     *         
***********************************************************************         
                                                                                
REQCNTT  LKREQ H,I#CNTTDL,OUTCNT,NEXTREQ=REQFLGH                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(D,B#SAVED,SVCLT),CHAR,TEXT=SP#CLI                         
         LKREQ E                                                                
                                                                                
OUTCNT   LKOUT H                   Traffic contact output start                 
         LKOUT R,I#CNTTDL                                                       
Array    LKOUT C,001,(A,ARYCNT)                                                 
         LKOUT E                                                                
         LKOUT X                   Traffic contact output maps end              
                                                                                
ARYCNT   LKOUT A,(R,NXTCNT),MULTIROW=Y,ROWNAME=CNTRECD                          
MedCd    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR,ND=Y                                
CntNm    LKOUT C,003,CNTKNAME,CHAR                                              
Array    LKOUT C,004,(A,ARYCN1)                                                 
Array    LKOUT C,006,(A,ARYCN2)                                                 
Array    LKOUT C,007,(A,ARYCN3)                                                 
         LKOUT E                                                                
                                                                                
ARYCN1   LKOUT A,(D,B#CNTREC,CNTDTAEL),EOT=EOR,ROWID=(CNTDTAEL,X'10'), +        
               ROWWIDTH=(V,CNTDTALN)                                            
         LKOUT C,004,CNTNAME,CHAR                                               
         LKOUT C,005,CNTTEL,(R,EDTCNX)                                          
         LKOUT E                                                                
                                                                                
ARYCN2   LKOUT A,(D,B#CNTREC,CNTDTAEL),EOT=EOR,ROWID=(CNTFAXEL,X'20'), +        
               ROWWIDTH=(V,CNTFAXLN)                                            
         LKOUT C,006,CNTFTEL,(R,EDTCNX),ND=Y                                    
         LKOUT E                                                                
                                                                                
ARYCN3   LKOUT A,(D,B#CNTREC,CNTDTAEL),EOT=EOR,ROWID=(CNTEMLEL,X'30'), +        
               ROWWIDTH=(V,CNTEMLLN)                                            
         LKOUT C,007,CNTEMLAD,CHAR,LEN=50,ND=Y                                  
         LKOUT E                                                                
                                                                                
EDTCNX   LM    R2,R4,LP_AINP                                                    
         MVC   0(L'CNTTEL,R4),0(R2)                                             
         LHI   R0,L'CNTTEL                                                      
         CLC   (CNTTELEX-CNTTEL)(L'CNTTELEX,R2),SPACES                          
         JNH   SETOLENX                                                         
         MVI   ((CNTTELN-CNTTEL)+L'CNTTELN)(R4),C'X'                            
         J     SETOLENX                                                         
                                                                                
NXTCNT   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         USING CNTKEY,R2                                                        
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCNTSQ                                                         
*                                                                               
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   NOMORE                                                           
         XC    HALF1,HALF1                                                      
         CLC   SVCLT,SPACES                                                     
         JNH   NXTCNT16                                                         
         GOTOR (#VALCLT,AVALCLT),DMCB,SVCLT,3,HALF1                             
         JNE   NOMORE                                                           
*                                                                               
NXTCNT16 XC    IOKEY,IOKEY         Prepare Contact key                          
         MVC   CNTKID(2),=X'0A36'                                               
         MVC   CNTKAM,QMEDX        Binary agency/media                          
         MVC   CNTKCLT,HALF1       Binary client                                
         XC    QCLTX,QCLTX                                                      
         MVC   QCLTA,SPACES        Init output alpha client code                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                                                           
         J     NXTCNT22                                                         
*                                                                               
NXTCNTSQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         JNE   NOMORE                                                           
*                                                                               
NXTCNT22 CLC   CNTKID(2),=X'0A36'                                               
         JNE   NOMORE                                                           
                                                                                
         CLC   CNTKAM,QMEDX        If Media does not match                      
         JE    NXTCNT25                                                         
         OC    HALF1,HALF1         and filtering on client code                 
         JZ    NOMORE                                                           
         CLI   LP_RMODE,LP_RFRST   and nothing was found                        
         JNE   NOMORE                                                           
         XC    HALF1,HALF1         go try again without client code             
         OI    PROSTAT,PSMEDLEV    and set to only return media                 
         J     NXTCNT16            level contacts                               
*                                                                               
NXTCNT25 OC    HALF1,HALF1         Have filtering client code?                  
         JZ    *+14                                                             
         CLC   CNTKCLT,HALF1       Match client code?                           
         JNE   NXTCNTSQ                                                         
*                                                                               
         TM    PROSTAT,PSMEDLEV    If returning only media level                
         JZ    *+14                contacts                                     
         OC    CNTKCLT,CNTKCLT     reject all client-specifics                  
         JNZ   NXTCNTSQ                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTCNTSQ                                                         
*                                                                               
         OC    CNTKCLT,CNTKCLT     Have client code?                            
         JZ    NXTCNT60                                                         
         CLC   QCLTX,CNTKCLT       Same as previous client code?                
         JE    NXTCNT60                                                         
         MVC   QCLTA,SPACES        Init output alpha client code                
         MVC   QCLTX,CNTKCLT                                                    
         MVC   SVIOVALS,IOVALS                                                  
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NXTCNT32                                                         
         L     RF,ACLTREC                                                       
         MVC   BYTE1,(CPROF+6)-CLTRECD(RF)                                      
         GOTOR VCLUNPK,DMCB,(BYTE1,QCLTX),QCLTA                                 
NXTCNT32 MVC   IOVALS,SVIOVALS                                                  
*                                                                               
NXTCNT60 MVC   LP_ADATA,ACNTTREC                                                
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* Traffic flight record download                                      *         
***********************************************************************         
                                                                                
REQFLGH  LKREQ H,I#FLGHDL,OUTFLG,NEXTREQ=REQMLST                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(D,B#SAVED,SVCLT),CHAR,TEXT=SP#CLI                         
         LKREQ E                                                                
                                                                                
OUTFLG   LKOUT H                   Traffic flight output start                  
         LKOUT R,I#FLGHDL                                                       
Array    LKOUT C,001,(A,ARYFLG)                                                 
         LKOUT E                                                                
         LKOUT X                   Traffic flight output maps end               
                                                                                
ARYFLG   LKOUT A,(R,NXTFLG),MULTIROW=Y,ROWNAME=FLTRECD                          
MedCd    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
PRdCd    LKOUT C,003,(D,B#WORKD,QPRDA),CHAR,ND=Y                                
EndDt    LKOUT C,004,FLTKEDT,BDAT                                               
Array    LKOUT C,005,(A,ARYFL1)                                                 
         LKOUT E                                                                
                                                                                
ARYFL1   LKOUT A,(D,B#FLTREC,FLTDTAEL),EOT=EOR,ROWID=(FLTDTAEL,X'10'), +        
               ROWWIDTH=(V,FLTDTALN)                                            
         LKOUT C,005,FLTSTART,BDAT                                              
         LKOUT C,006,FLTEND,BDAT                                                
         LKOUT E                                                                
                                                                                
NXTFLG   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         USING FLTKEY,R2                                                        
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTFLGSQ                                                         
*                                                                               
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   NOMORE                                                           
         XC    HALF1,HALF1                                                      
         CLC   SVCLT,SPACES                                                     
         JNH   NXTFLG16                                                         
         GOTOR (#VALCLT,AVALCLT),DMCB,SVCLT,3,HALF1                             
         JNE   NOMORE                                                           
*                                                                               
NXTFLG16 XC    IOKEY,IOKEY         Prepare Flight key                           
         MVC   FLTKID(2),=X'0A27'                                               
         MVC   FLTKAM,QMEDX        Binary agency/media                          
         MVC   FLTKCLT,HALF1       Binary client                                
         XC    QCLTX,QCLTX                                                      
         MVI   QPRDX,0             Inir binary product code                     
         MVC   QCLTA,SPACES        Init output alpha client code                
         MVC   QPRDA,SPACES        Init output alpha product code               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                                                           
         J     NXTFLG22                                                         
*                                                                               
NXTFLGSQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         JNE   NOMORE                                                           
*                                                                               
NXTFLG22 CLC   FLTKID(2),=X'0A27'                                               
         JNE   NOMORE                                                           
         CLC   FLTKAM,QMEDX                                                     
         JNE   NOMORE                                                           
         OC    HALF1,HALF1         Have filtering client code?                  
         JZ    *+14                                                             
         CLC   FLTKCLT,HALF1       Match client code?                           
         JNE   NXTFLGSQ                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTFLGSQ                                                         
*                                                                               
         MVC   SVIOVALS,IOVALS                                                  
         MVC   QPRDX,FLTKPRD                                                    
         MVC   QPRDA,SPACES        Init output alpha product code               
         CLC   QCLTX,FLTKCLT       Same as previous client code?                
         JE    NXTFLG32                                                         
         MVC   QCLTA,SPACES        Init output alpha client code                
         MVC   QCLTX,FLTKCLT                                                    
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NXTFLG48                                                         
         L     RF,ACLTREC                                                       
         MVC   BYTE1,(CPROF+6)-CLTRECD(RF)                                      
         GOTOR VCLUNPK,DMCB,(BYTE1,FLTKCLT),QCLTA                               
NXTFLG32 CLI   QPRDX,0             Have product code?                           
         JE    NXTFLG48                                                         
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
NXTFLG40 CLC   QPRDX,3(RF)         Match that of product list?                  
         JE    NXTFLG46                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,NXTFLG40                                                      
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
NXTFLG44 CLC   QPRDX,3(RF)         Match that of product list?                  
         JE    NXTFLG46                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,NXTFLG44                                                      
         MVC   QPRDA,=C'???'       Bad product code                             
         J     NXTFLG48                                                         
NXTFLG46 MVC   QPRDA,0(RF)         Get character format for prd code            
NXTFLG48 MVC   IOVALS,SVIOVALS                                                  
*                                                                               
         MVC   LP_ADATA,AFLGHREC                                                
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* Traffic market list record download                                 *         
***********************************************************************         
                                                                                
REQMLST  LKREQ H,I#MLSTDL,OUTMLS,NEXTREQ=TRAUTIN                                
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
CltCd    LKREQ F,002,(D,B#SAVED,SVCLT),CHAR,TEXT=SP#CLI                         
         LKREQ E                                                                
                                                                                
OUTMLS   LKOUT H                   Traffic market list output record            
         LKOUT R,I#MLSTDL                                                       
Array    LKOUT C,001,(A,ARYMLS)                                                 
         LKOUT E                                                                
         LKOUT X                   Traffic flight output maps end               
                                                                                
ARYMLS   LKOUT A,(R,NXTMLS),MULTIROW=Y,ROWNAME=MKLRECD                          
MedCd    LKOUT C,001,(D,B#SAVED,MEDCOD),CHAR                                    
CltCd    LKOUT C,002,(D,B#WORKD,QCLTA),CHAR                                     
PRdCd    LKOUT C,003,(D,B#WORKD,QPRDA),CHAR,ND=Y                                
EstCd    LKOUT C,004,MKLKBEST,UBIN,ND=Y                                         
MLsNm    LKOUT C,005,MKLKLNAM,CHAR                                              
Array    LKOUT C,006,(A,ARYML1)                                                 
         LKOUT E                                                                
                                                                                
ARYML1   LKOUT A,(D,B#MLSREC,MKLDTAEL),EOT=EOR,ROWID=(MKLDTAEL,X'10'), +        
               ROWWIDTH=(V,MKLDTALN)                                            
         LKOUT C,006,MKLMKT,(R,EDTMKCD)                                         
         LKOUT E                                                                
                                                                                
EDTMKCD  LM    R2,R4,LP_AINP                                                    
         EDIT  (B2,0(R2)),(4,0(R4)),0,ALIGN=RIGHT,ZERO=NOBLANK,FILL=0           
         LHI   R0,4                                                             
         J     SETOLENX                                                         
                                                                                
NXTMLS   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         USING MKLKEY,R2                                                        
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTMLSSQ                                                         
*                                                                               
         GOTOR (#VALMED,AVALMED),DMCB,MEDCOD,0,QMEDX                            
         JNE   NOMORE                                                           
         XC    HALF1,HALF1                                                      
         CLC   SVCLT,SPACES                                                     
         JNH   NXTMLS16                                                         
         GOTOR (#VALCLT,AVALCLT),DMCB,SVCLT,3,HALF1                             
         JNE   NOMORE                                                           
*                                                                               
NXTMLS16 XC    IOKEY,IOKEY         Prepare Market List key                      
         MVC   MKLKEY(2),=X'0A38'                                               
         MVC   MKLKAM,QMEDX        Binary agency/media                          
         MVC   MKLKCLT,HALF1       Binary client                                
         XC    QCLTX,QCLTX                                                      
         MVI   QPRDX,0             Inir binary product code                     
         MVC   QCLTA,SPACES        Init output alpha client code                
         MVC   QPRDA,SPACES        Init output alpha product code               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE   NOMORE                                                           
         J     NXTMLS22                                                         
*                                                                               
NXTMLSSQ GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO3'                            
         JNE   NOMORE                                                           
*                                                                               
NXTMLS22 CLC   MKLKID(2),=X'0A38'                                               
         JNE   NOMORE                                                           
         CLC   MKLKAM,QMEDX                                                     
         JNE   NOMORE                                                           
         OC    HALF1,HALF1         Have filtering client code?                  
         JZ    *+14                                                             
         CLC   MKLKCLT,HALF1       Match client code?                           
         JNE   NXTMLSSQ                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO3'                           
         JNE   NXTMLSSQ                                                         
*                                                                               
         MVC   SVIOVALS,IOVALS                                                  
         MVC   QPRDX,MKLKBPRD                                                   
         MVC   QPRDA,SPACES        Init output alpha product code               
         CLC   QCLTX,MKLKCLT       Same as previous client code?                
         JE    NXTMLS32                                                         
         MVC   QCLTA,SPACES        Init output alpha client code                
         MVC   QCLTX,MKLKCLT                                                    
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   NXTMLS48                                                         
         L     RF,ACLTREC                                                       
         MVC   BYTE1,(CPROF+6)-CLTRECD(RF)                                      
         GOTOR VCLUNPK,DMCB,(BYTE1,MKLKCLT),QCLTA                               
NXTMLS32 CLI   QPRDX,0             Have product code?                           
         JE    NXTMLS48                                                         
         L     RE,ACLTREC                                                       
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         LA    R1,220              220 entries in CLIST                         
NXTMLS40 CLC   QPRDX,3(RF)         Match that of product list?                  
         JE    NXTMLS46                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,NXTMLS40                                                      
         LA    RF,CLIST2                                                        
         LA    R1,35               Additional 35 entries in CLIST2              
NXTMLS44 CLC   QPRDX,3(RF)         Match that of product list?                  
         JE    NXTMLS46                                                         
         LA    RF,4(RF)                                                         
         JCT   R1,NXTMLS44                                                      
         MVC   QPRDA,=C'???'       Bad product code                             
         J     NXTMLS48                                                         
NXTMLS46 MVC   QPRDA,0(RF)         Get character format for prd code            
NXTMLS48 MVC   IOVALS,SVIOVALS                                                  
*                                                                               
         MVC   LP_ADATA,AMLSTREC                                                
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
WSSVRLN  EQU   8*1024                                                           
*                                                                               
*                                                                               
***********************************************************************         
* TRAFFIC AUTOGEN                                                     *         
* This transfers control to SPTRA07 to build list of prds/stations    *         
***********************************************************************         
*                                                                               
TRAUTIN  LKREQ H,I#AUTODL,OUT109,NEXTREQ=TRAUTOT                                
MedCd    LKREQ F,001,(D,B#WORKD,QMEDA),CHAR,TEXT=SP#MED                         
CltCd    LKREQ F,002,(D,B#WORKD,QCLTA),CHAR,TEXT=SP#CLI                         
PrdCd    LKREQ F,003,(D,B#WORKD,QPRDA),CHAR,TEXT=SP#PRD                         
PtrCd    LKREQ F,004,(D,B#WORKD,QPRD2),CHAR,TEXT=SP#PRTNR                       
ESTCD    LKREQ F,005,(D,B#WORKD,QEST),CHAR,TEXT=SP#EST                          
StDate   LKREQ F,007,(D,B#WORKD,QSTRDATE),EDAT,TEXT=SP#STDT                     
EndDate  LKREQ F,008,(D,B#WORKD,QENDDATE),EDAT,TEXT=SP#ENDT                     
OptShip  LKREQ F,009,(D,B#WORKD,QOPTSHIP),CHAR,TEXT=(*,SHIPLIT)                 
OptFax   LKREQ F,010,(D,B#WORKD,QOPTFAX),CHAR,TEXT=(*,FAXLIT)                   
Options  LKREQ F,011,(D,B#WORKD,QOPTIONS),CHAR,TEXT=SP#OPTNS                    
Questor  LKREQ F,012,(D,B#WORKD,QOPTREQ),CHAR,TEXT=SP#RQSTR    INITS            
ConName  LKREQ F,013,(D,B#WORKD,QNAME),CHAR,TEXT=(*,CONTLIT)   NAME             
         LKREQ E                                                                
*                                                                               
SHIPLIT  DC    C'Ship?'                                                         
FAXLIT   DC    C'Fax?'                                                          
CONTLIT  DC    C'Contact'                                                       
*                                                                               
OUT109   LKOUT H                   DUMMY OUTPUT FOR GLOBBER CALL                
*                                                                               
         LKOUT R,X'002A'           Unique reply number                          
         LKOUT P,,AUTOINI          INIT AUTOGEN ARRAY                           
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
TRAUTOT  LKREQ H,I#AUTOUP,OUT10A,NEXTREQ=TRAURUN                                
MedCd    LKREQ F,001,(D,B#WORKD,QMEDA),CHAR,TEXT=SP#MED                         
         LKREQ E                                                                
*                                                                               
OUT10A   LKOUT H                   Traffic autogen output starts                
                                                                                
         LKOUT R,X'002A'           UNIQUE REPLY NUMBER                          
         LKOUT C,1,(A,ARYAUTO)                                                  
         LKOUT C,13,(D,B#WORKD,QSAVEKEY),CHAR                                   
         LKOUT C,98,(D,B#WORKD,QERRCODE),CHAR                                   
         LKOUT C,99,(D,B#WORKD,QERRTEXT),CHAR                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYAUTO  LKOUT A,(R,AUTONXT),MULTIROW=Y                                         
         LKOUT C,001,(D,B#SAVED,RECOUT),CHAR                                    
         LKOUT C,002,(D,B#SAVED,MKTCODE),CHAR                                   
         LKOUT C,022,(D,B#SAVED,MKTNM),CHAR                                     
         LKOUT C,003,(D,B#SAVED,QSTA),CHAR                                      
         LKOUT C,004,(D,B#WORKD,QPRDA),CHAR                                     
         LKOUT C,006,(D,B#SAVED,QSPTLN),CHAR                                    
         LKOUT C,007,(D,B#WORKD,QPRD2),CHAR,ND=Y                                
         LKOUT C,009,(D,B#SAVED,QPTRLEN),CHAR,ND=Y                              
         LKOUT C,010,(D,B#WORKD,QSTR8),CHAR                                     
         LKOUT C,011,(D,B#WORKD,QEND8),CHAR                                     
         LKOUT C,012,(D,B#WORKD,QCOPY),CHAR,ND=Y                                
         LKOUT E                                                                
         EJECT                                                                  
*============================================================                   
* THIS ROUTINE JUST PASSES THE GLOBALS TO TRAFFIC AND BUILDS                    
* A XFRCTL ELEMENT                                                              
*============================================================                   
                                                                                
AUTOINI  DS    0H                                                               
         LA    R0,I#AUTOUP                                                      
         MVI   BYTE1,C'B'          SET MODE=BLD                                 
         BRAS  RE,AUTOGLOB                                                      
         J     EXITN                                                            
                                                                                
*================================================================               
* FIRST GET KEY OF CLTHDR FROM GLOBBER (FROM SPTRA07 GLOBBER CALL)              
* AND THEN READ CLIENT RECORD                                                   
*================================================================               
                                                                                
AUTONXT  ICM   R4,15,SVNXTTAB       ANY RECORDS SENT YET?                       
         JNZ   AUNX10                                                           
*                                                                               
         XC    QERRTEXT,QERRTEXT                                                
         BRAS  RE,AUTOERR          CHECK FOR STR ERRORS                         
         JNE   NOMORE                                                           
*                                                                               
         XC    IOKEY,IOKEY                                                      
         GOTO1 VGLOBBER,DMCB,=C'GETD',IOKEY,18,GLVPRKEY                         
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO2'                            
         JNE    *+2                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO2'                           
         JNE    *+2                                                             
                                                                                
                                                                                
*================================================================               
* THIS ROUTINE IS CALLED TO DOWNLOAD PRD-STA LINES TO THE PC                    
* RECORDS REQUIRE A LITTLE CONVERSION FOR OUTPUT FORMAT                         
* FILE REFNUM IS SENT TO THE PC SINCE THE SESSION IS STATELESS                  
*===============================================================                
                                                                                
* READ THE OPTICA SAVE RECORD(S) TO GET THE SVTABLE                             
                                                                                
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,24,GLVSPTRF                          
*                                                                               
         LA    R4,IOKEY                                                         
         USING XSAVKEY,R4                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVI   XSAVTYPE,XSAVTYPQ   X'0A'                                        
         MVI   XSAVSBTY,XSAVSBTQ   X'7F'                                        
         MVC   XSAVSEQ(8),ELEM+TRFSVKEY-GLVTRFD  MOVE SEQ/DATE/A-M/CLT          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,XSAVSEQ                                                     
         X     R0,=X'0000FFFF'     GET FF COMPLEMENT                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSAVEKEY(4),DUB                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(3,XSAVDTB),(X'20',QSAVEKEY+4)                      
         DROP  R4                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO5'                            
         JNE    *+2                                                             
         CLC   IOKEY(XSAVAGMD-XSAVRECD),IOKEYSAV   MATCH SEQ/DATE               
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO5'                           
         JNE    *+2                                                             
                                                                                
         ZAP   RECNUM,=P'0'                                                     
         L     R4,AIO5                                                          
         AHI   R4,XSAVFRST-XSAVRECD                                             
         USING SVTABLED,R4                                                      
         OC    SVTBLINE+2(2),SVTBLINE+2   TEST ANY DATA AT ALL                  
         JZ    AUNXERR                                                          
*                                                                               
AUNX10   OC    SVTBLINE,SVTBLINE   TEST MORE DATA                               
         JNZ   AUNX12                                                           
                                                                                
* SEE IF THERE IS ANOTHER RECORD                                                
                                                                                
         MVC   QERRCODE,=C'0000'   PRESET EXIT CODE                             
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO5'                            
         JNE    *+2                                                             
         CLC   IOKEY(XSAVAGMD-XSAVRECD),IOKEYSAV   MATCH SEQ/DATE               
         JNE   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO5'                           
         JNE    *+2                                                             
*                                                                               
         L     R4,AIO5                                                          
         AHI   R4,XSAVFRST-XSAVRECD    POINT TO FIRST LINE IN REC               
         USING SVTABLED,R4                                                      
         OC    SVTBLINE+2(2),SVTBLINE+2   TEST ANY DATA AT ALL                  
         JZ    NOMORE                                                           
*                                                                               
AUNX12   AP    RECNUM,=P'1'        RECORD NUMBER IN BUFFER                      
         OI    RECNUM+3,X'0F'                                                   
         UNPK  RECOUT,RECNUM                                                    
*                                                                               
         L     RE,AIO2             POINT TO CLTHDR                              
         LLC   RF,1(RE)            GET AGYMD                                    
         LA    R0,15                                                            
         NR    RF,R0               DROP AGY                                     
*                                                                               
         LARL  RE,MDTAB                                                         
         LA    RE,1(RE)                                                         
         JCT   RF,*-4              INDEXING INTO TABLE DOESN'T WORK!            
         MVC   QMEDA,0(RE)                                                      
*                                                                               
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMED,QMEDA                                                    
         MVC   STAPMKST,SVTBMKST                                                
*                                                                               
         GOTO1 VSTAPACK,STAPACKD                                                
*                                                                               
         MVC   QSTA,STAPQSTA                                                    
         CLI   QSTA,C'0'           TEST CABLE STATION                           
         JL    AUNX13                                                           
         CLI   STAPQNET,C' '       TEST FOR A NETWORK                           
         JNH   AUNX13              NO- SKIP THE /                               
         MVI   QSTA+4,C'/'                                                      
*                                                                               
AUNX13   MVC   MKTCODE,STAPQMKT                                                 
*                                                                               
         LA    R1,IOKEY            GET THE MARKET NAME                          
         USING MKTREC,R1                                                        
*                                                                               
         CLC   MKTKMKT,STAPQMKT    TEST HAVE IT ALREADY                         
         JE    AUNX14                                                           
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMEDA                                                    
         MVC   MKTKMKT,STAPQMKT                                                 
         MVC   MKTKAGY,STAPAGY                                                  
         MVC   MKTKFILL,EZEROS                                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO1'                            
         JE    AUNX14                                                           
         L     R1,AIO1                                                          
         MVC   MKTKEY,IOKEYSAV                                                  
         MVI   MKTNAME,C'?'                                                     
         MVC   MKTNAME+1(L'MKTNAME-1),MKTNAME                                   
*                                                                               
AUNX14   L     R1,AIO1                                                          
         MVC   MKTNM,MKTNAME       MOVE TO OUTPUT AREA                          
*                                                                               
         LA    R1,SVTBPRD                                                       
         BRAS  RE,GETPRD                                                        
         MVC   QPRDA,0(RF)                                                      
         UNPK  QPR#,DUB                                                         
*                                                                               
         LLC   R1,SVTBSLN                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSPTLN,DUB                                                       
*                                                                               
         MVC   QPRD2,SPACES                                                     
         MVC   QPR2#,SPACES                                                     
         MVC   QPTRLEN,SPACES                                                   
*                                                                               
         CLI   SVTBPRD2,0                                                       
         JE    AUNX20                                                           
         LA    R1,SVTBPRD2                                                      
         BRAS  RE,GETPRD                                                        
         MVC   QPRD2,0(RF)                                                      
         UNPK  QPR2#,DUB                                                        
*                                                                               
         LLC   R1,SVTBSLN2                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QPTRLEN,DUB                                                      
*                                                                               
AUNX20   GOTO1 VDATCON,DMCB,(2,SVTBSTR),(11,QSTR8)  MMMDD/YY                    
         GOTO1 (RF),(R1),(2,SVTBEND),(11,QEND8)                                 
*                                                                               
         MVC   QCOPY,SVTBCOPY                                                   
         MVC   QAFFL,SVTBAFFL                                                   
         MVC   QSTATYP,SVTBTYPE                                                 
*                                                                               
         MVI   QCABLE,C' '                                                      
         TM    SVTBIND2,X'80'                                                   
         JZ    *+8                                                              
         MVI   QCABLE,C'Y'                                                      
*                                                                               
         MVI   QCBLGRP,C' '                                                     
         TM    SVTBIND2,X'40'                                                   
         JZ    *+8                                                              
         MVI   QCBLGRP,C'Y'                                                     
*                                                                               
         LA    R4,L'SVTBDATA(R4)                                                
         ST    R4,SVNXTTAB                                                      
*                                                                               
AUNX30   MVC   LP_ADATA,LP_ABLK1                                                
         J     EXITY                                                            
*                                                                               
AUNXERR  MVC   QERRCODE,=C'9999'                                                
         MVC   QERRTEXT(25),=C'NO INSTRUCTIONS TO BE RUN'                       
         J     NOMORE                                                           
*                                                                               
GETPRD   L     RF,AIO2                                                          
         LA    RF,CLIST-CLTHDR(RF)                                              
*                                                                               
GETPRD2  CLI   0(RF),C'A'                                                       
         JL    *+2                                                              
         CLC   0(1,R1),3(RF)                                                    
         JE    GETPRD4                                                          
         LA    RF,4(RF)                                                         
         J     GETPRD2                                                          
*                                                                               
GETPRD4  CVD   R1,DUB              RETURN NUMERIC PRD IN DUB                    
         OI    DUB+7,X'0F'                                                      
         BR    RE                                                               
*                                                                               
MDTAB    DC    C' TRNX '                                                        
         EJECT                                                                  
*============================================================                   
* UPLOAD FIELDS FOR AUTO/GEN RUN.                                               
* UPLOAD SELECTED PRODUCT-STATION ROW NUMBERS                                   
* IF RUNALL=Y, THEN NO RECORD NUMBERS SHOULD BE RECEIVED                        
* AND BUILD A NEW TABLE IN IO2                                                  
*============================================================                   
*                                                                               
TRAURUN  LKREQ H,I#AUTRUN,OUT10B,NEXTREQ=TRAUOUT                                
         LKREQ F,001,(D,B#WORKD,QMEDA),CHAR,TEXT=SP#MED                         
         LKREQ F,002,(D,B#WORKD,QCLTA),CHAR,TEXT=SP#CLI                         
         LKREQ F,003,(D,B#WORKD,QPRDA),CHAR,TEXT=SP#PRD                         
         LKREQ F,004,(D,B#WORKD,QPRD2),CHAR,TEXT=SP#PRTNR                       
         LKREQ F,005,(D,B#WORKD,QEST),CHAR,TEXT=SP#EST                          
         LKREQ F,007,(D,B#WORKD,QSTRDATE),EDAT,TEXT=SP#STDT                     
         LKREQ F,008,(D,B#WORKD,QENDDATE),EDAT,TEXT=SP#ENDT                     
         LKREQ F,009,(D,B#WORKD,QOPTSHIP),CHAR,TEXT=(*,SHIPLIT)                 
         LKREQ F,010,(D,B#WORKD,QOPTFAX),CHAR,TEXT=(*,FAXLIT)                   
         LKREQ F,011,(D,B#WORKD,QOPTIONS),CHAR,TEXT=SP#OPTNS                    
         LKREQ F,012,(D,B#WORKD,QOPTREQ),CHAR,TEXT=SP#RQSTR    INITS            
         LKREQ F,013,(D,B#WORKD,QNAME),CHAR,TEXT=(*,CONTLIT)   NAME             
         LKREQ F,014,(I,B#SAVED,RECNUMI),UBIN,TEXT=(*,RECTXT),OLEN=4,  X        
               LIST=F                                                           
         LKREQ F,015,(D,B#WORKD,QUNIQID),CHAR,TEXT=(*,UNIQID),OLEN=60           
         LKREQ F,016,(D,B#WORKD,QRUNALL),CHAR,TEXT=(*,RUNALL),OLEN=1            
         LKREQ F,017,(D,B#WORKD,QSAVEKEY),CHAR,TEXT=(*,SAVEKEY),OLEN=10         
         LKREQ E                                                                
*                                                                               
OUT10B   LKOUT H                   DUMMY OUTPUT FOR GLOBBER CALL                
                                                                                
         LKOUT R,X'002B'           UNIQUE REPLY NUMBER                          
         LKOUT P,,RUN10C                                                        
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
* 'F' CARD BELOW IS JUST A DUMMY                                                
                                                                                
TRAUOUT  LKREQ H,I#AUTOUP,OUT10C,NEXTREQ=TRAUEND                                
                                                                                
         LKREQ F,002,(I,B#SAVED,RECNUMI),UBIN,TEXT=(*,RECTXT),OLEN=4,  X        
               LIST=F                                                           
         LKREQ E                                                                
RECTXT   DC    C'Recnum'                                                        
RUNALL   DC    C'Runall'                                                        
UNIQID   DC    C'Uniqid'                                                        
SAVEKEY  DC    C'Savekey'                                                       
*                                                                               
OUT10C   LKOUT H                   DUMMY OUTPUT FOR GLOBBER CALL                
                                                                                
         LKOUT R,X'002C'           UNIQUE REPLY NUMBER                          
         LKOUT P,,RUN10C                                                        
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
*                                                                               
RUN10C   DS    0H                                                               
         BRAS  RE,AUTOERR                                                       
         JNE   NOMORE                                                           
*                                                                               
         LA    R0,I#AUTEND                                                      
         MVI   BYTE1,C'S'          SET MODE=SUB                                 
         BRAS  RE,AUTOGLOB                                                      
*                                                                               
         MVC   QERRCODE,=C'0000'                                                
         XC    QERRTEXT,QERRTEXT                                                
*                                                                               
         CLI   QRUNALL,C'Y'           IF RUN=ALL                                
         JE    RUN10CX                                                          
                                                                                
* MAKE SURE CONNECT TO TRAFFIC SYSTEM FOR GETREC/PUTREC                         
                                                                                
RUN10C2  MVI   BYTE1,C'N'           SET DID NOT SWITCH SYSTEMS                  
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'80',0),F#UTLD                                       
         L     R1,0(R1)                                                         
         USING F@UTLD,R1                                                        
         MVI   BYTE1,C'Y'          SET SYSTEM SWITCH FLAG                       
         CLI   F@TOVSYS,X'0D'      CONNECTED TO A TRAFFIC SYSTEM                
         JE    RUN10C4                                                          
         DROP  R1                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         LA    RE,=C'STR'                                                       
         ST    RE,DMCB                                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         JNE   *+2                                                              
                                                                                
RUN10C4  LA    R4,IOKEY                                                         
         USING XSAVKEY,R4                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVI   XSAVTYPE,XSAVTYPQ   X'0A'                                        
         MVI   XSAVSBTY,XSAVSBTQ   X'7F'                                        
*                                                                               
         PACK  DUB,QSAVEKEY(4)     GET SEQNUM                                   
         CVB   R0,DUB                                                           
         X     R0,=X'FFFFFFFF'     COMPLEMENT                                   
         STCM  R0,3,XSAVSEQ                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,QSAVEKEY+4,(3,XSAVDTB)                              
         DROP  R4                                                               
                                                                                
*==========================================================                     
* READ RECORDS INTO IO3/IO5/IO6 - ALL ARE 6K                                    
*==========================================================                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO3'                            
         JNE    *+2                                                             
         CLC   IOKEY(XSAVAGMD-XSAVKEY),IOKEYSAV   FIND SEQ/DATE                 
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+IO3'                        
         JNE    *+2                                                             
*                                                                               
         XC    DUB,DUB                                                          
         L     RF,AIO3                                                          
         BRAS  RE,RUN10CNT                                                      
         STH   R0,DUB                                                           
*                                                                               
         L     RE,AIO5             CLEAR TOP OF IOAREAS                         
         XC    0(24,RE),0(RE)                                                   
         L     RE,AIO6                                                          
         XC    0(24,RE),0(RE)                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO5'                            
         JNE    *+2                                                             
         CLC   IOKEY(XSAVAGMD-XSAVKEY),IOKEYSAV   FIND SEQ/DATE                 
         JNE   RUN10C10                           NO (MORE) RECS                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+IO5'                        
*                                                                               
         L      RF,AIO5                                                         
         BRAS   RE,RUN10CNT                                                     
         STH    R0,DUB+2                                                        
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTRFDIR+IO6'                            
         JNE    *+2                                                             
         CLC   IOKEY(XSAVAGMD-XSAVKEY),IOKEYSAV   FIND SEQ/DATE                 
         JNE   RUN10C10                           NO (MORE) RECS                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOTRFFIL+IO6'                        
*                                                                               
         L      RF,AIO6                                                         
         BRAS   RE,RUN10CNT                                                     
         STH    R0,DUB+4                                                        
         J      RUN10C10                                                        
                                                                                
RUN10CNT DS    0H                                                               
         LA    RF,24(RF)           POINT TO FIRST ELEMENT                       
         SR    R0,R0               CLEAR COUNTER                                
*                                                                               
RUN10CN2 CLI   0(RF),0             TEST ELEM PRESENT                            
         JE    RUN10CN4                                                         
         LLC   R4,1(RF)                                                         
         AR    RF,R4                                                            
         JCT   R0,RUN10CN2                                                      
*                                                                               
RUN10CN4 LPR   R0,R0               R0 NOW HAS HIGH RECORD NUMBER                
         JZ    *+2                 AND THERE SHOULD BE AT LEAST ONE!            
         BR    RE                                                               
*                                                                               
RUN10C10 ICM   RE,7,ARECNUM        GET ARRAY ADDRESS                            
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN-LW_D(RE) NUMBER OF ENTRIES                          
         JZ    RUN10ERR                                                         
         AHI   RE,LW_LN2Q                                                       
*                                                                               
RUN10C12 L     R4,0(RE)            GET RECORD NUMBER                            
*                                                                               
         L     R0,AIO3             FIND WHICH RECORD IT IS IN                   
         CH    R4,DUB                                                           
         JNH   RUN10C14                                                         
         L     R0,AIO5                                                          
         CH    R4,DUB+2                                                         
         JNH   RUN10C14                                                         
         L     R0,AIO6                                                          
         CH    R4,DUB+4                                                         
         JH    *+2                                                              
*                                                                               
RUN10C14 BCTR  R4,0                                                             
         MHI   R4,L'SVTBDATA                                                    
         AR    R4,R0                    DSPL FROM FIRST ELEM                    
         LA    R4,XSAVFRST-XSAVRECD(R4) ADJUST FOR DSPL TO FIRST ROW            
         MVI   3(R4),C'S'               INDICATE SELECTED                       
*                                                                               
         LA    RE,4(RE)            NEXT RECORD NUMBER                           
         JCT   RF,RUN10C12                                                      
                                                                                
* ALWAYS WRITE FIRST REC                                                        
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOTRFFIL+IO3'                           
         JNE    *+2                                                             
*                                                                               
         L     RE,AIO5                                                          
         CLI   0(RE),0             TEST FOR SECOND REC                          
         JE    RUN10C20                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOTRFFIL+IO5'                           
         JNE    *+2                                                             
*                                                                               
         L     RE,AIO6                                                          
         CLI   0(RE),0             TEST FOR THIRD REC                           
         JE    RUN10C20                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOTRFFIL+IO6'                           
         JNE    *+2                                                             
                                                                                
* SWITCH BACK TO SPOT IF NECESSARY                                              
                                                                                
RUN10C20 CLI   BYTE1,C'Y'          TEST DID A SWITCH                            
         JNE   RUN10CX                                                          
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         LA    RE,=C'SPOT'                                                      
         ST    RE,DMCB                                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         JNE   *+2                                                              
                                                                                
RUN10CX  J     EXITN                                                            
         EJECT                                                                  
TRAUEND  LKREQ H,I#AUTEND,OUT10D,NEXTREQ=TRCJRUN                                
         LKREQ F,001,(D,B#WORKD,QMEDA),CHAR,TEXT=SP#MED                         
         LKREQ E                                                                
*                                                                               
OUT10D   LKOUT H                   DUMMY OUTPUT FOR GLOBBER CALL                
                                                                                
         LKOUT R,X'002D'           UNIQUE REPLY NUMBER                          
         LKOUT P,,RUN10D                                                        
         LKOUT C,98,(D,B#WORKD,QERRCODE),CHAR                                   
         LKOUT C,99,(D,B#WORKD,QERRTEXT),CHAR                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
*                                                                               
RUN10D   BRAS  RE,AUTOERR          CHECK FOR ERRORS ON RETURN                   
         JNE   NOMORE                                                           
         MVC   QERRCODE,=C'0000'                                                
         MVC   QERRTEXT(18),=C'SOON JOB SUBMITTED'                              
         J     EXITY                                                            
*                                                                               
RUN10ERR MVC   QERRCODE,=C'9999'                                                
         MVC   QERRTEXT(25),=C'NO INSTRUCTIONS TO BE RUN'                       
         J     EXITY                                                            
         EJECT                                                                  
*============================================================                   
* PUT GLOBALS TO TRAFFIC FOR AUTOGEN                                            
* ON ENTRY R0 HAS ROUTINE NUMBER FOR RETURN                                     
* BYTE1 HAS B FOR BLD CALL OR S FOR SUB CALL                                    
*============================================================                   
                                                                                
* SUMMARY OF GLOBALS USED                                                       
* GLVXREC                                                                       
* GLVXACT                                                                       
* GLVSPRNT = NOW/SOON,XXX                                                       
* GLVSPMD                                                                       
* GLVSPCLT                                                                      
* GLVSPPRD                                                                      
* GLVSPPR2(7) = FOR EXTENDED VALUES FOR POL PRDS                                
* GLVSPEST                                                                      
* GLVSPPER                                                                      
* GLVSPREQ = 16 CHAR NAME ON INSTRUCTIONS                                       
* GLVSPTRF = COVERED BY DDGLVSPTRF                                              
* GLVBUY1  = OPTIONS FIELD (69) ON INPUT, ERROR FIELD ON OUTPUT                 
*            IF FIRST CHARACTER IS <                                            
* GLVBUY2  = UNIQUE ID FOR PDF                                                  
*============================================================                   
                                                                                
AUTOGLOB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'CLEAR'  CLEAR ALL GLOBAL VALUES                 
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(4),=C'AUTO'                                                 
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,4,GLVXREC                            
*                                                                               
         MVC   ELEM(4),=C'GEN '                                                 
         GOTO1 (RF),(R1),,ELEM,4,GLVXACT                                        
*                                                                               
         MVC   ELEM(7),=C'NOW,PDF'                                              
         GOTO1 (RF),(R1),,ELEM,7,GLVSPRNT                                       
*                                                                               
         GOTO1 (RF),(R1),=C'PUTD',QMEDA,1,GLVSPMD                               
*                                                                               
         GOTO1 (RF),(R1),,QCLTA,3,GLVSPCLT                                      
*                                                                               
         GOTO1 (RF),(R1),,QPRDA,3,GLVSPPRD                                      
*                                                                               
         CLI   QPRD2,C' '          TEST THERE IS A PRD2                         
         JNH   AUTOGL10                                                         
         GOTO1 (RF),(R1),,QPRD2,7,GLVSPPR2                                      
*                                                                               
AUTOGL10 GOTO1 (RF),(R1),,QEST,3,GLVSPEST                                       
*                                                                               
AUTOGL12 XC    ELEM,ELEM                                                        
         GOTO1 VDATCON,DMCB,QSTRDATE,(11,ELEM)                                  
         MVI   ELEM+8,C'-'                                                      
         GOTO1 (RF),(R1),QENDDATE,(11,ELEM+9)                                   
*                                                                               
AUTOGL14 GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,17,GLVSPPER                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',QNAME,16,GLVSPREQ  NAME ON INST           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING GLVTRFD,R4                                                       
         MVC   TRFACT,=C'BLD'                                                   
         CLI   BYTE1,C'B'                                                       
         JE    *+10                                                             
         MVC   TRFACT,=C'SUB'                                                   
*                                                                               
         STH   R0,HALF                                                          
         GOTO1 VHEXOUT,DMCB,(2,HALF),WORK,2,=C'TOG'                             
         MVC   TRFROUT,WORK+1      3 CHAR HEX CODE IN EBCDIC                    
*                                                                               
         MVC   TRFSHIP,QOPTSHIP                                                 
         MVC   TRFFAX,QOPTFAX                                                   
         MVC   TRFDOALL,QRUNALL                                                 
         MVC   TRFWHO,QOPTREQ                                                   
*                                                                               
         CLI   BYTE1,C'B'                                                       
         JE    AUTOGL20                                                         
         CLI   QRUNALL,C'Y'                                                     
         JE    AUTOGL20                                                         
*                                                                               
* ONLY DO THIS FOR 10B AND RUNALL=N                                             
*                                                                               
         PACK  DUB,QSAVEKEY(4)     PACK THE SEQNUM                              
         CVB   R0,DUB                                                           
         X     R0,=X'FFFFFFFF'     COMPLEMENT IT YOU IDIOT!                     
         STCM  R0,3,TRFSVKEY                                                    
*                                                                               
         GOTOR VDATCON,DMCB,QSAVEKEY+4,(3,TRFSVKEY+2)                           
         DROP  R4                                                               
*                                                                               
AUTOGL20 GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,24,GLVSPTRF                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',QOPTIONS,69,GLVBUY1                       
*                                                                               
         GOTO1 (RF),(R1),,QUNIQID,60,GLVBUY2                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING GLVXFRSY,R4                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'LIN'                                                 
         MVC   GLVXTOSY,=C'STR'                                                 
         MVC   GLVXTOPR,=C'TRA'                                                 
         OI    GLVXFLG1,GLV1SEPS+GLV1SEPD                                       
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
         J     EXITY                                                            
         DROP  R4                                                               
*                                                                               
       ++INCLUDE DDGLVSPTRF                                                     
*                                                                               
SVRDEF   CSECT ,                                                                
                                                                                
*==========================================================                     
* CHECK FOR AN ERROR RETURN FROM TRAFFIC                                        
*==========================================================                     
                                                                                
AUTOERR  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,69,GLVBUY1                           
         CLI   8(R1),0                                                          
         JNE   EXITY               IF NO ELEM, CLEAN RETURN                     
         CLI   ELEM,C'<'           ERRORS START WITH THIS                       
         JNE   EXITY                                                            
         MVC   QERRCODE,ELEM+1                                                  
         MVC   QERRTEXT,ELEM+6                                                  
         GOTO1 VGLOBBER,DMCB,=C'CLEAR'                                          
         J     EXITN                                                            
         EJECT                                                                  
*============================================================                   
* UPLOAD FIELDS FOR AMS/GEN RUN.                                                
*============================================================                   
* AMS/GEN                                                                       
TRCJRUN  LKREQ H,I#AMSRUN,OUT11A,NEXTREQ=TRCJEND                                
         LKREQ F,001,(D,B#WORKD,QMEDA),CHAR,TEXT=SP#MED                         
         LKREQ F,002,(D,B#WORKD,QCLTA),CHAR,TEXT=SP#CLI                         
         LKREQ F,003,(D,B#WORKD,QPRDA),CHAR,TEXT=SP#PRD                         
         LKREQ F,004,(D,B#WORKD,QPRD2),CHAR,TEXT=SP#PRTNR                       
         LKREQ F,005,(D,B#WORKD,QEST),CHAR,TEXT=SP#EST                          
         LKREQ F,007,(D,B#WORKD,QSTRDATE),EDAT,TEXT=SP#STDT                     
         LKREQ F,008,(D,B#WORKD,QENDDATE),EDAT,TEXT=SP#ENDT                     
         LKREQ F,010,(D,B#WORKD,QOPTFAX),CHAR,TEXT=(*,FAXLIT)                   
         LKREQ F,011,(D,B#WORKD,QOPTIONS),CHAR,TEXT=SP#OPTNS                    
         LKREQ F,012,(D,B#WORKD,QOPTREQ),CHAR,TEXT=SP#RQSTR    INITS            
         LKREQ F,013,(D,B#WORKD,QNAME),CHAR,TEXT=(*,CONTLIT)   NAME             
         LKREQ F,015,(D,B#WORKD,QUNIQID),CHAR,TEXT=(*,UNIQID),OLEN=60           
         LKREQ E                                                                
*                                                                               
OUT11A   LKOUT H                   DUMMY OUTPUT FOR GLOBBER CALL                
                                                                                
         LKOUT R,X'0030'           UNIQUE REPLY NUMBER                          
         LKOUT P,,RUN11C                                                        
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
*                                                                               
RUN11C   DS    0H                                                               
         MVI   QRUNALL,C'Y'        SET FOR NO SAVEKEY                           
         LA    R0,I#AMSEND                                                      
         MVI   BYTE1,C'S'          SET MODE=SUB                                 
         BRAS  RE,AUTOGLOB                                                      
         JNE   *+2                 EXIT IF ERROR RETURNED                       
                                                                                
* NOW FIX THE REC AND WHEN FIELDS                                               
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(4),=C'AMS '                                                 
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,4,GLVXREC                            
*                                                                               
         MVC   ELEM(8),=C'SOON,PDF'                                             
         GOTO1 (RF),(R1),,ELEM,8,GLVSPRNT                                       
*                                                                               
         MVC   QERRCODE,=C'0000'                                                
         XC    QERRTEXT,QERRTEXT                                                
         J     EXITN               THIS EXIT CAUSES XFRCTL TO OCCUR             
*                                                                               
TRCJEND  LKREQ H,I#AMSEND,OUT10FD,NEXTREQ=REQOFFC                               
         LKREQ F,001,(D,B#WORKD,QMEDA),CHAR,TEXT=SP#MED                         
         LKREQ E                                                                
*                                                                               
OUT10FD  LKOUT H                   DUMMY OUTPUT FOR GLOBBER CALL                
                                                                                
         LKOUT R,X'002D'           UNIQUE REPLY NUMBER                          
         LKOUT P,,RUN10FD                                                       
         LKOUT C,98,(D,B#WORKD,QERRCODE),CHAR                                   
         LKOUT C,99,(D,B#WORKD,QERRTEXT),CHAR                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
*                                                                               
RUN10FD  BRAS  RE,AUTOERR                                                       
         JNE   EXITY                                                            
*                                                                               
         MVC   QERRCODE,=C'0000'                                                
         MVC   QERRTEXT(18),=C'SOON JOB SUBMITTED'                              
         J     EXITY                                                            
*                                                                               
*                                                                               
***********************************************************************         
* Media office code download                                          *         
***********************************************************************         
                                                                                
REQOFFC  LKREQ H,I#OFFCDL,OUTOFFC,NEXTREQ=REQUESTX                              
MedCd    LKREQ F,001,(D,B#SAVED,MEDCOD),CHAR,TEXT=SP#MED                        
         LKREQ E                                                                
                                                                                
OUTOFFC  LKOUT H                                                                
OFFC2    LKOUT R,X'0120'                                                        
Array    LKOUT C,X'0120',(A,ARY2OF)                                             
Array    LKOUT C,X'0120',(A,ARY1OF),FILTROUT=TST1OF                             
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
                                                                                
REQUESTX LKREQ X                                                                
         EJECT                                                                  
         LKARY T                                                                
         EJECT                                                                  
SETCCC   JE    *+8                 Set converse condition code                  
         CR    RE,RE               CC=Equal if not equal                        
         BR    RE                                                               
         LTR   RE,RE               CC=Not equal if equal                        
         BR    RE                                                               
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   Set no more records and exit                 
         J     EXITY                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     Clear output field length & exit             
         J     EXITY                                                            
                                                                                
SETOLENX STCM  R0,15,LP_OLEN       Set output field length & exit               
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1  ,                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** Global literals **                        
                                                                                
CANAGYQ  EQU   C'C'                Canadian agency identifier                   
RADMEDQ  EQU   C'R'                Radio media                                  
EOR      EQU   0                   End of record element                        
                                                                                
         LTORG                                                                  
*                                                                               
TRTAB    DC    X'000102030405060708090A0B0C0D0E0F'     00-0F                    
         DC    X'101112131415161718191A1B1C1D1E1F'     10-1F                    
         DC    X'202122232425262728292A2B2C2D2E2F'     20-2F                    
         DC    X'303132333435363738393A3B3C3D3E3F'     30-3F                    
         DC    X'404142434445464748494A4B4C4D4E4F'     40-4F                    
         DC    X'505152535455565758595A5B5C5D5E40'     50-5F  <<<               
         DC    X'606162636465666768696A6B6C6D6E6F'     60-6F                    
         DC    X'707172737475767778797A7B7C7D7E7F'     70-7F                    
         DC    X'808182838485868788898A8B8C8D8E8F'     80-8F                    
         DC    X'909192939495969798999A9B9C9D9E9F'     90-9F                    
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'     A0-AF                    
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'     B0-BF                    
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'     C0-CF                    
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'     D0-DF                    
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'     F0-FF                    
*                                                                               
         LTORG                                                                  
                                                                                
ONEOFCTB DC    C'ABCDEFGH'         One character office code table              
         DC    C'IJKLMNOP'                                                      
         DC    C'QRSTUVWX'                                                      
         DC    C'YZ123456'         0 is removed                                 
         DC    C'789'                                                           
         DC    C'<>?/`~!@'         = - , not allowed                            
         DC    C'#$%^*()_'                                                      
         DC    C'+{}º|\"'                                                      
         DC    C'.'                                                             
         DC    C'&&'                                                            
         DC    C''''                                                            
         DC    X'FF'                                                            
ONEOFCLN EQU   *-ONEOFCTB                                                       
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* Profile table                                                                 
* Byte 1-3 = Profile name                                                       
* Byte 4   = Profile parameter indicator byte                                   
* Byte 5   = X'01' means agency level only (media & client ignored)             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
PROFTAB  DC    C'C2 ',X'C0',X'01'  PROFILE TABLE AND PARAM FOR GETPROF          
PROFTAB1 DC    C'C3 ',X'C0',X'01'    C2, C3, C4  USES X'C0' AS PER              
         DC    C'C4 ',X'C0',X'01'    SPSFM08                                    
         DC    C'B1X',X'00',X'00'  B1X uses X'00' as per SPSFM50                
         DC    X'0000'                                                          
PROFTABL EQU   PROFTAB1-PROFTAB                                                 
*                                                                               
* Traffic profiles                                                              
PROFTAB0 DC    C'T0 ',X'C0'          T0 profile (Comclass)                      
PROFTBL1 DC    C'T1 ',X'C0'          T1 profile (UFN invalid)                   
PROFTAB2 DC    C'T2 ',X'C0'          T2 profile (1 prd/cml)                     
PROFTAB3 DC    C'T3 ',X'C0'          T3 exclude bands in traffic                
PROFTABT DC    C'TT ',X'C0'          TT profile (Talent)                        
PROFTABW DC    C'TW ',X'C0'          TW profile                                 
                                                                                
MFMMKD#  DC    AL2(I#MFMMKT)       MFM market download                          
                                                                                
       ++INCLUDE SPCGRTAB          1 byte to 2 bytes Client Groups              
       ++INCLUDE SPMGRTAB          1 byte to 2 bytes Market Groups              
                                                                                
BITLIST  DC    X'8040201008040201'                                              
EZEROS   DC    C'00000000'                                                      
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
SPTDIR   DC    C'SPTDIR '                                                       
SPTFIL   DC    C'SPTFILE'                                                       
STAFIL   DC    C'STATION'                                                       
GENDIR   DC    C'GENDIR '                                                       
                                                                                
CLTKEYT  LKKEY H,CKEY,SAVED        ** Client key driver table **                
         LKKEY LIT,CKEYTYPE,CKEYTYPQ                                            
         LKKEY WMP,CKEYAM,AMED                                                  
         LKKEY NZR,CKEYCLT                                                      
         LKKEY LIT,CKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
PRDKEYT  LKKEY H,PKEY              ** Product key driver table **               
         LKKEY LIT,PKEYTYPE,PKEYTYPQ                                            
         LKKEY WMP,PKEYAM,AAMC,L'PKEYAM+L'PKEYCLT                               
         LKKEY NZR,PKEYPRD                                                      
         LKKEY LIT,PKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
CFMPRKT  LKKEY H,PKEY,SAVED        ** CFM Product key driver table **           
         LKKEY LIT,PKEYTYPE,PKEYTYPQ                                            
         LKKEY SIN,PKEYAM,SVCLTKEY+(CKEYAM-CKEY)                                
         LKKEY SIN,PKEYCLT,SVCLTKEY+(CKEYCLT-CKEY)                              
         LKKEY RNG,PKEYPRD,PRDRNGE                                              
         LKKEY LIT,PKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
STAKEYT  LKKEY H,STAKEY            ** Station key driver table **               
         LKKEY LIT,STAKTYPE,STAKTYPQ                                            
         LKKEY LST,STAKMED,MEDIA#                                               
         LKKEY ALL,STAKCALL                                                     
         LKKEY SIN,STAKAGY,AGENCY                                               
         LKKEY LIT,STAKCLT,C'0'                                                 
         LKKEY E                                                                
                                                                                
TPHKEYT  LKKEY H,PRHKEY,SAVED      ** Traffic prod house driver table *         
         LKKEY SIN,PRHKID,TPHRECQ                                               
         LKKEY SIN,PRHKAM,AGYMEDX                                               
         LKKEY SIN,PRHKPRH,HOUSECD                                              
         LKKEY E                                                                
                                                                                
LVALUES  DS    0F                  ** Literals, see WVALUES **                  
         DC    AL1(0,8,10,0)       ASSOCIATED WITH LABEL VS081000               
         DS    0H                                                               
MYUNIQID DC    CL60'374FA-9404-6A71-4547-AB78-50DF-6D3I8'                       
         EJECT                                                                  
*                                                                               
*====================================================================           
* DSECT FOR GLVNOTE GLOBALS RETURNED BY SPOT/TRAFFIC                            
*====================================================================           
GLVIND   DS    XL1                                                              
GLVMKT   DS    XL2                                                              
GLVSTA   DS    CL5                                                              
GLVQPRD  DS    CL3                                                              
GLVPRD   DS    XL1                                                              
GLVSLN   DS    XL1                                                              
GLVQPR2  DS    CL3                                                              
GLVPRD2  DS    XL1                                                              
GLVSLN2  DS    XL1                                                              
GLVSTR   DS    XL3                                                              
GLVEND   DS    XL3                                                              
GLVCOPY  DS    CL1                                                              
GLVAFFL  DS    CL3                                                              
GLVTYPE  DS    CL1                                                              
GLVIND2  DS    XL1                                                              
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SAVED    DSECT ,                   ** Dsect to cover saved storage **           
WVALUES  DS    0X                  ** LITERAL VALUES **                         
VS081000 DS    XL4                 PC VERSION 0.8.10.0                          
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
GETUID   DS    A                   A(GETUID)                                    
VTRPACK  DS    A                   A(TRPACK)                                    
VMSUNPK  DS    A                   A(MSUNPK)                                    
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
                                                                                
BUYMED   DS    C                   Media code                                   
                                                                                
REQVALS  DS    0F                  ** Request values **                         
                                                                                
ADVIND   DS    X                   Advertiser node array                        
AADV     DS    AL3                                                              
                                                                                
MEDIND   DS    X                   Media array                                  
AMED     DS    AL3                                                              
                                                                                
CLTIND   DS    X                   Client array                                 
ACLT     DS    AL3                                                              
                                                                                
STAIND   DS    X                   Station array                                
ASTA     DS    AL3                                                              
                                                                                
SMCIND   DS    X                   System/Media/Client array                    
ASMC     DS    AL3                                                              
                                                                                
AMCIND   DS    X                   Agency/Media/Client array                    
AAMC     DS    AL3                                                              
                                                                                
QAGYIND  DS    X                   Agency connections array                     
QAAGY    DS    AL3                                                              
                                                                                
QMKGIND  DS    X                   List of market group code to filter          
QAMKG    DS    AL3                                                              
                                                                                
QCLGIND  DS    X                   List of client group code to filter          
QACLG    DS    AL3                                                              
*                                                                               
QVNDIND  DS    X                                                                
AVND     DS    AL3                 Array of Optica vendors                      
NUMVND   DS    XL(L'LW_NUMN)       N'vendors to process                         
*                                                                               
QOFFIND  DS    X                                                                
AOFF     DS    AL3                 Array of office codes                        
NUMOFF   DS    XL(L'LW_NUMN)       N'office codes to process                    
*                                                                               
RECNUMI  DS    X                                                                
ARECNUM  DS    AL3                 ARRAY OF INPUT RECNUMS                       
*                                                                               
QONEOFC  DS    C                   Agy using ONE char media office?             
QANYREQ  DS    C                   Any request processed for this sys?          
QCLTOPT  DS    C                   Download client? Y/N option                  
QPRDOPT  DS    C                   Download product? Y/N option                 
QOPTION  DS    C                   Download options not used in Spot            
                                                                                
AGENCY   DS    CL(L'LP_AGY)        Agency code                                  
MEDIA#   DS    XL2                 Number of entries in media list              
MEDIA    DS    CL8                 Media list (vendor download)                 
MARKET#  DS    XL2                 Market number (market download)              
MARKET$  DS    X                   Market media index (market download)         
                                                                                
STRDATEB DS    XL3                 Request start date (binary)                  
ENDDATEB DS    XL3                 Request end date (binary)                    
CMLPRFXS DS    XL8                 Packed of commercial search start            
CMLPRFXE DS    XL8                 Packed of commercial search end              
CMLPREFX DS    CL12                Prefix for commercial search                 
CMLPRFXL DS    XL1                 L(Prefix for commercial)                     
                                                                                
STRDATEC DS    XL2                 Request start date (compressed)              
ENDDATEC DS    XL2                 Request end date (compressed)                
STRDMONC DS    XL2                 Request start Monday (compressed)            
                                                                                
SUPNODE  DS    XL(L'CFMVENN)       Supplier node                                
                                                                                
CALOPTN  DS    C                   Calendar option                              
SPLOPTN  DS    C                   Split option                                 
SUMOPTN  DS    C                   Summarize by... option                       
PNTOPTN  DS    C                   Program name/time option                     
ESTOPTN  DS    C                   Estimate option                              
ESNOPTN  DS    C                   Estimate name option                         
                                                                                
AGYMEDX  DS    XL(L'QMEDX)         Binary Agency/media                          
HOUSECD  DS    CL(L'PRHKPRH)       house code for media/client                  
TPHRECQ  DS    XL(L'PRHKID)        Traffic prod house record code               
TRCRECQ  DS    XL(L'SHPKID)        Traffic ship recap record code               
TCLRECQ  DS    XL(L'CLSKID)        Traffic COMCLASS record code                 
                                                                                
SVVND    DS    CL3                 Vendor code                                  
                                                                                
STACODE  DS   0CL5                 Station                                      
STATION  DS    CL5                 Station                                      
         ORG   STATION                                                          
QSTA     DS    CL8                                                              
MKTCODE  DS    CL4                 Market code                                  
                                                                                
ELCODE   DS    X                                                                
                                                                                
ACLTNXT  DS    A                                                                
ACLTLIST DS    A                                                                
ACLTLSTX DS    A                                                                
                                                                                
ABCLNXT  DS    A                   Binary client                                
ABCLLIST DS    A                                                                
ABCLLSTX DS    A                                                                
                                                                                
APRDNXT  DS    A                                                                
APRDLIST DS    A                                                                
APRDLSTX DS    A                                                                
                                                                                
ABPRNXT  DS    A                   Binary product                               
ABPRLIST DS    A                                                                
ABPRLSTX DS    A                                                                
                                                                                
ACLSNXT  DS    A                                                                
ACLSLIST DS    A                                                                
ACLSLSTX DS    A                                                                
                                                                                
AMKTNXT  DS    A                                                                
AMKTLIST DS    A                                                                
AMKTLSTX DS    A                                                                
                                                                                
ASTANXT  DS    A                                                                
ASTALIST DS    A                                                                
ASTALSTX DS    A                                                                
                                                                                
PRDCODE  DS    CL3                 Product code                                 
PRDLIST  DS    CL(PRDNUM*PRDLEN)   Product List                                 
PRDNUM   EQU   9                   Number of prods                              
PRDLEN   EQU   4                   Entry len (3 clt + 1 comma)                  
                                                                                
BCLLIST  DS   0CL(CLTNUM*L'QCLTX)  Binary client                                
CLTLIST  DS    CL(CLTNUM*CLTLEN)   Alpha client list                            
CLTCNT   DS    C                                                                
CLTNUM   EQU   10                  Number of clients                            
CLTLEN   EQU   4                   Entry len (3 clt + 1 comma)                  
                                                                                
SVCLASS  DS    CL(L'CLSKCLAS)      Class code                                   
CLSLIST  DS    CL(CLSNUM*CLSLEN)   Class list                                   
CLSNUM   EQU   4                   Number of classes                            
CLSLEN   EQU   (L'SVCLASS+1)       Entry len (class + 1 comma)                  
                                                                                
SVDESC   DS    CL(L'CLSDESC)       Description                                  
                                                                                
QCMTID   DS    CL10                                                             
QOFFCDE  DS    CL2                                                              
QPAGENO  DS    CL3                                                              
QCALLPGM DS    C                                                                
                                                                                
PRDCNT   DS    X                   Number of prods to process                   
CLSCNT   DS    X                   Number of classes to process                 
SVCLT    DS    CL(L'QCLTA)         Client code from comclass rec                
                                                                                
CMLCODE  DS    CL(L'CMLADID)       Commercial code                              
CMLCODEP DS    XL8                 Packed commercial                            
                                                                                
SVR3     DS    F                   Save R3                                      
RECACT   DS    C                   Record Action (L/D/C/X/R)                    
                                                                                
ENVIRO   DS    X                   Enviroment to use                            
                                                                                
PROSTAT  DS    X                   Program Status                               
PSMEDLEV EQU   X'80'               Return only media-level contacts             
REQVALL  EQU   *-REQVALS                                                        
                                                                                
ANXTVND  DS    A                   A(next vendor code in work map pool)         
ANXTOFF  DS    A                   A(next office code in work map pool)         
                                                                                
ANXTCLT  DS    A                   A(next client code in work map pool)         
NUMCLT   DS    XL(L'LW_NUMN)       N'client codes left to process               
SVRFPID  DS    XL(L'AGYPRNID)      RFP Userid# from SPGENAGY                    
PRLPGRP  DS    CL(L'GRPKGRP)       Previous RLP group code                      
                                                                                
SVPBLDAT DS    XL(L'PBILLDT)                                                    
SVPBLBAS DS    XL(L'PBILLBAS)                                                   
SVPBLCOM DS    XL(L'PBILLCOM)                                                   
                                                                                
QSPTLN   DS    CL3                 Request spot length                          
QPTRLEN  DS    CL3                 Request spot length 2                        
QPRDCOD2 DS    CL3                 Product code                                 
SLNERRSW DS    X                   Spot length error switch                     
CMLDLSW1 DS    C                   Commercial download switch 1                 
                                                                                
QTXTYP   DS    CL1                 Request - STEXT type                         
PATSTXTQ EQU   C'P'                Pattern special test text (default)          
ESTSTXTQ EQU   C'E'                Instructions by estimate STEXT               
ALLSTXTQ EQU   C'A'                All STEXTs                                   
                                                                                
SHWCML   DS    C                   Show commercial(s)                           
SHWCLT   DS    C                        client                                  
SHWPRD   DS    C                        product(s)                              
SHWVLD   DS    C                   Valid request                                
CLTCHG   DS    C                   Client changed indicator                     
                                                                                
OUTVALS  DS    0F                  ** Output values **                          
                                                                                
SYSLET   DS    C                   ** System letter **                          
SYSSPTQ  EQU   C'S'                Spot system letter                           
                                                                                
MEDCOD   DS    CL(L'AGYMEDCD)      Media code                                   
                                                                                
MKGOPT1  DS    C                   Market Group download option 1               
MKGOPTCQ EQU   C'O'                Market Group download for Optica             
                                                                                
CBRKLEN  DS    XL1                 Client group break length                    
CLGBIN_  DS    XL3                 Client group code - binary                   
CLGCHAR  DS    CL6                 Client group code - character                
PBRKLEN  DS    XL1                 Product group break length                   
PRGBIN_  DS    XL3                 Product group code - binary                  
PRGCHAR  DS    CL5                 Product group code - character               
MBRKLEN  DS    XL1                 Market group break length                    
FORBYTKR DS    CL1                 For BuyTracker                               
MKGBIN_  DS    XL3                 Market group code - binary                   
MKGCHAR  DS    CL6                 Market group code - character                
MKGMKT#  DS    XL(L'MKGPMKT)       Mkt# (used multiple times in array)          
WKIOKEY  DS    XL(L'IOKEY)                                                      
                                                                                
SVCMLFLS DS    0X                  Start of saved commercial rec fields         
SVCMLFLG DS    XL1                 Saved flag                                   
CML_ALLQ EQU   X'80'               Need to download all products                
SVCMLUFN DS    CL1                 Saved traffic commercial UFN flag            
SVCMLAID DS    CL(L'CMLADID)       Saved traffic commercial Ad-ID               
SVCMLDS1 DS    CL(L'CMLDSC)        SAVED TRAFFIC COMMERCIAL TITLE 1             
SVCMLDS2 DS    CL(L'CMLDSC)        Saved traffic commercial title 2             
SVCMLDS3 DS    CL(L'CMLDSC)        Saved traffic commercial title 3             
SVCMLFLX DS    0X                  End of saved commercial rec fields           
SVCMLFLQ EQU   *-SVCMLFLS          Length of saved commercial rec flds          
                                                                                
SVPARENT  DS   CL12                Parent commercial                            
SVMSTIME  DS   CL6                 Match start time                             
SVMETIME  DS   CL6                 Match end time                               
SVDDATE   DS   CL8                 Destroy date                                 
SVDTIME   DS   CL5                 Destroy time (2400=12A,0=NONE)               
SVDAILY   DS   C                   Check times daily                            
SVFORMAT  DS   C                   Format H for hidef                           
CDELETE   DS   C                   CML is deleted                               
SVCKSM    DS   XL4                 Record check sum                             
*                                                                               
SVMPERIOD DS  0XL4                 Match period                                 
SVMSDATE1 DS   XL2                 Match start dates                            
SVMEDATE1 DS   XL2                 And end dates                                
SVMSDATE2 DS   XL2                                                              
SVMEDATE2 DS   XL2                                                              
SVMSDATE3 DS   XL2                                                              
SVMEDATE3 DS   XL2                                                              
SVMSDATE4 DS   XL2                                                              
SVMEDATE4 DS   XL2                                                              
SVMSDATE5 DS   XL2                                                              
SVMEDATE5 DS   XL2                                                              
SVMSDATE6 DS   XL2                                                              
SVMEDATE6 DS   XL2                                                              
SVMALLPER EQU  *-SVMPERIOD                                                      
*                                                                               
SVCMLCLS DS   CL50                 Class and percentage allocation              
*                                                                               
SVACTUL1 DS   CL12                 8-12 char actual cml (space padded)          
SVACTUL2 DS   CL12                                                              
SVACTUL3 DS   CL12                                                              
SVACTUL4 DS   CL12                                                              
SVPCMLQ  EQU   *-SVCMLUFN          Cml flds to clear nxt time around            
SVCMLQ2  EQU   *-SVCMLFLS          Length of all saved cml rec flds             
*                                                                               
SVCMLFL1 DS    XL1                 Saved flag                                   
CMLADSW  EQU   X'01'               Cml codes packed adids (SHPISADI)            
*                                                                               
SVBPRD   DS    X                   Binary product                               
SVMKSTA  DS    XL5                 Save market station                          
SVCMLAD  DS    CL(L'CMLADID)       Commercial adid                              
SVCMLHD  DS    CL(L'CMLXHDEF)      Commercial hidef                             
SVKEY    DS    CL(L'SHPKEY)        Save recap key                               
ANXTCML  DS    A                   A(next commercial in list)                   
SVNXTTAB DS    A                                                                
                                                                                
SVSHCML1 DS    CL(L'SHPCMML)       Ship commercial 1                            
SVSHCML2 DS    CL(L'SHPCMML2)      Ship commercial2                             
                                                                                
DONESW   DS    C                                                                
*                                                                               
SVCMLSEQ DS    XL(L'CMTKSEQ)       Comml sequence number                        
SVTXT1   DS    CL58                                                             
         ORG   SVTXT1                                                           
RECNUM   DS    PL4                 USED BY AUTOGEN OUTPUT                       
RECOUT   DS    CL4                                                              
MKTNM    DS    CL24                                                             
         ORG   SVTXT1+L'SVTXT1                                                  
*                                                                               
PPAGENO  DS    XL1                                                              
SVOFFCDE DS    XL1                 1 byte office number                         
OFFCDE   DS    CL2                                                              
ID       DS    CL10                Comment id                                   
PAGENO   DS    CL3                                                              
TXTLINO  DS    CL2                                                              
TXTLIN   DS    CL58                                                             
*                                                                               
*Pattern fields                                                                 
*                                                                               
SVACMT   DS    A                   A(comment in elem)                           
BREF     DS    XL2                                                              
*                                                                               
SVPATFLS DS    0X                  Start of saved pattern rec fields            
QCODE    DS    CL3                 Copy code (estimate/daypart)                 
QREF     DS    CL5                 Pattern reference number                     
SVREF    DS    CL5                                                              
SVPATUFN DS    CL1                 Saved traffic pattern UFN flag               
PRDCOD2  DS    CL3                 Product code                                 
PTRLEN   DS    CL3                 Product partner code                         
CODEOUT  DS    CL3                 Copy code (estimate/daypart)                 
CODEEST  DS    CL1                 Estimate = Y                                 
PDELETE  DS    CL1                                       deleted pat            
SVPATDSC DS    CL16                                      description            
SVPATSTD DS    CL11                                      start date             
SVPATEND DS    CL11                                      end date               
SVPATSTT DS    CL6                                       start time             
SVPATENT DS    CL6                                       end time               
SVPATDPT DS    CL1                 Daypart                                      
SVPATSTX DS    CL6                 Stext                                        
SVPATINV DS    CL1                 Invert prods Y/N                             
SVPATDLY DS    CL1                 Times daily Y/N                              
SVPATTYP DS    CL1                 Type (s)tation, (m)arket, (g)roup            
SVPATROT DS    CL78                Rotation                                     
SVCPCTL  DS    XL45                Save percentage elem (15x3)                  
SVCPCT1  DS   0CL3                                                              
SVCPCT   DS    CL2                                                              
         DS    C                                                                
SVCML1   DS    CL12                                                             
SVCML2   DS    CL12                                                             
SVPATFLX DS    0X                  End of saved pattern rec fields              
SVPATLQ EQU    *-SVPATFLS          Length of saved commercial rec flds          
                                                                                
SVPATCMT DS    CL53                Comment                                      
ADIDFLAG DS    C                                                                
PATDLSW1 DS    C                   Pattern download switch 1                    
SHWPAT   DS    C                   Show patterns                                
*                                                                               
BREFSUB  DS    XL3                 Ref num (14 bits)/subline (10 bits)          
CCODE    DS    X                                                                
SVBSLN   DS    X                   len 1                                        
SVBPRD2  DS    X                   Binary product 2                             
SVBSLN2  DS    X                   len 2                                        
*                                                                               
CCODEST  DS    C                   Copy code est                                
*                                                                               
* Saved values for compare                                                      
CBREFSUB DS    XL3                 Ref num (14 bits)/subline (10 bits)          
CCCODE   DS    X                                                                
CSVBSLN  DS    X                   len 1                                        
CSVBPRD2 DS    X                   Binary product 2                             
CSVBPRD  DS    X                   Binary product 2                             
CSVBSLN2 DS    X                   len 2                                        
*                                                                               
                                                                                
DUMMYCLI DS    C                   Current client is 'dummy'                    
SVCLTKEY DS    CL(L'CKEY)          Client record key                            
SVREQTOK DS    CL(L'CA_TOKEN)      Client array token                           
SVCLTOFF DS    CL(L'COFFICE)       Client office code                           
PREVPROF DS    XL4                 Previous profile value in table              
                                                                                
PRDRNGE  DS    0XL(3*2)            ** Product key reading range **              
PRDRSTR  DS    XL3                 Start of range                               
PRDREND  DS    XL3                 End of range                                 
                                                                                
PRVVALS  DS    0X                  ** Previous values **                        
PAGYCOD  DS    CL(L'CA_AGYA)       Last (previous) agency                       
PMEDCOD  DS    CL(L'AGYMEDCD)      Last media code                              
PCLTCOD  DS    CL(L'QCLTA)         Last client code                             
PRVVALL  EQU   *-PRVVALS                                                        
                                                                                
         ORG   OUTVALS                                                          
CFMIOC   DS    XL(CFMIOL)          CFMIO control block                          
                                                                                
       ++INCLUDE GESTATSPT                                                      
                                                                                
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
AUQVALS  ORG   OUTVALS                                                          
*                                                                               
AUQMKT   DS    CL4                 SVTABLE INPUT VALUES FOR AUTOGEN             
AUQSTA   DS    CL5                                                              
AUQPRD   DS    CL3                                                              
AUQPR#   DS    CL3                                                              
AUQSLN   DS    CL3                                                              
AUQPRD2  DS    CL3                                                              
AUQPR2#  DS    CL3                                                              
AUQSLN2  DS    CL3                                                              
AUQFTD   DS    CL8                                                              
AUQLTD   DS    CL8                                                              
AUQCOPY  DS    CL1                                                              
AUQAFFL  DS    CL3                                                              
AUQSTYP  DS    CL1                                                              
AUQCBLHD DS    CL1                                                              
AUQCBLGR DS    CL1                                                              
*                                                                               
AUVALX   EQU   *                                                                
*                                                                               
NETLIST# DS    H                   N'entries in NETLIST                         
NETLIST  DS    0CL(L'STAPQNET)     Cable network list                           
                                                                                
CLTTAB   DS    0XL5                Client table                                 
         EJECT                                                                  
       ++INCLUDE SPLNKWRK                                                       
                                                                                
DUMMY_D  DSECT                                                                  
DUM_LIN1 DS    XL1                                                              
                                                                                
CLTRECD  DSECT ,                                                                
         ORG   CKEYCLT+L'CKEYCLT                                                
CKEYREST DS    XL(L'CKEY-(*-CKEY))                                              
                                                                                
PRDRECD  DSECT ,                                                                
         ORG   PKEYPRD+L'PKEYPRD                                                
PKEYREST DS    XL(L'PKEY-(*-PKEY))                                              
                                                                                
ESTRECD  DSECT ,                                                                
         ORG   EKEYEST+L'EKEYEST                                                
EKEYREST DS    XL(L'EKEY-(*-EKEY))                                              
                                                                                
WORKD    DSECT ,                   ** Redefine OVERWORK **                      
         ORG   OVERWORK                                                         
VRECUP   DS    V                   RECUP FROM COMFACS                           
                                                                                
SVIOVALS DS    XL(IOVALL)          Saved I/O values                             
                                                                                
* Included books follow                                                         
         PRINT OFF                                                              
       ++INCLUDE GEGENOFF                                                       
       ++INCLUDE GEGENSDR                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENRFP                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE GECFMIOD                                                       
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
*                                                                               
       ++INCLUDE SPGENMKG          MARKET GROUP RECORD                          
       ++INCLUDE SPGENPRG          PRODUCT GROUP RECORD                         
       ++INCLUDE SPTRDTXT          Special text (STEXT) record                  
       ++INCLUDE SPTRAGYCON        Contact record                               
       ++INCLUDE SPTRFLT           Flight record                                
       ++INCLUDE SPTRMKL           Market list record                           
                                                                                
*PREFIX=SP$                                                                     
       ++INCLUDE SPGENGRP          CLIENT/STATION GROUP RECORD                  
*PREFIX=                                                                        
                                                                                
       ++INCLUDE SPTRPRH           Traffic prod house record                    
       ++INCLUDE SPTRSHIP          Traffic ship recap record                    
       ++INCLUDE SPTRCMLCLS        Traffic comml class record                   
       ++INCLUDE SPTRCMML          Traffic commercial record                    
       ++INCLUDE SPTRPAT           Traffic pattern record                       
*                                                                               
       ++INCLUDE SPGENBUY                                                       
STASTAL  EQU   L'STAPQSTA+L'STAPQNET                                            
STARECD  DSECT ,                                                                
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
SVTABLED DSECT                                                                  
                                                                                
SVTBDATA DS    0XL34                                                            
SVTBLINE DS    AL4                 A(SCREEN INPUT LINE)                         
SVTBAPTN DS    AL4                 PATTERN LIST ADDRESS                         
SVTBIND  DS    XL1                 X'80'=SAME AS LAST INST                      
*                                  X'40'=PATTERN CHANGE                         
*                                  X'20'=SCHEDULE CHANGE                        
*                                  X'10'=NEW PATTERN (WAS TBA)                  
*                                  X'08'=MORE THAN 27 PATS/AUTO P/B ERR         
*                                        MORE THAN 371 DAYS IN PERIOD           
*                                  X'04'=COPY CODE FROM EST HDR                 
*                                  X'02'=CHANGE IN COMMERCIAL TEXT              
*                                  X'01'=NO PATTERN (ERROR)                     
SVTBSTA  DS    CL5                 STATION CALL LETTERS                         
SVTBPRD  DS    XL1                 PRODUCT CODE                                 
SVTBSLN  DS    XL1                 SPOT LENGTH                                  
SVTBPRD2 DS    XL1                 PARTNER PRODUCT CODE                         
SVTBSLN2 DS    XL1                 PARTNER SPOT LENGTH                          
SVTBDTS  DS    0XL4                                                             
SVTBSTR  DS    XL2                 FIRST TELECAST DATE                          
SVTBEND  DS    XL2                 LAST TELECAST DATE                           
SVTBCOPY DS    CL1                 COPY CODE - IS EST IF T0 PROF 11=E           
*                                              IS ADJACENCY CODE IF=A           
*                                              IS DAYPART CODE   IF=D           
SVTBMKST DS    XL5                 PACKED MARKET/STATION                        
SVTBAFFL DS    CL3                 AFFILIATE CODE                               
SVTBTYPE DS    CL1                 STATION TYPE                                 
SVTBIND2 DS    XL1                                                              
SVTBICAB EQU   X'80'               STATION IS CABLE HEAD STATION                
SVTBICGR EQU   X'40'               STATION IS CABLE GROUP                       
         DS    CL1                 SPARE                                        
SVTBLEN  EQU   *-SVTBDATA                                                       
*                                                                               
SVTBNEXT EQU   *,L'SVTBDATA                                                     
*                                                                               
SVTBNXLN EQU   SVTBLINE+L'SVTBDATA,4                                            
SVTBNXST EQU   SVTBSTA+L'SVTBDATA                                               
SVTBNXMS EQU   SVTBMKST+L'SVTBDATA                                              
SVTBNXPR EQU   SVTBPRD+L'SVTBDATA                                               
SVTBNXP2 EQU   SVTBPRD2+L'SVTBDATA                                              
SVTBNXCP EQU   SVTBCOPY+L'SVTBDATA                                              
       ++INCLUDE GEMAPEQUS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE SPTROPSAV                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040SPLNK10   04/17/18'                                      
         END                                                                    
