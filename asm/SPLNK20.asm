*          DATA SET SPLNK20    AT LEVEL 015 AS OF 11/02/20                      
*PHASE T21E20B                                                                  
SPLNK19  TITLE '- DESKTOP FUA FOR MM DOWNLOADS'                                 
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,                                                 *        
               SERVERTYPE=TSTSPOT,                                     *        
               WORKERKEY=SPFU,                                         *        
               SEGMENT=Y,                                              *        
               APPEND=Y,                                               *        
               REQUEST=*,                                              *        
               CODE=CODE,                                              *        
               SYSPHASE=SYSPHASE,                                      *        
               SYSTEM=SPTSYSQ,                                         *        
               IDF=Y,                                                  *        
               BLOCKS=(B#WORKD,WORKD,                                  *        
               B#SAVED,SAVED,                                          *        
               B#LP_D,LP_D,                                            *        
               B#TWAD,TWAD,                                            *        
               B#ESTREC,ESTRECD,                                       *        
               B#INSREC,INSRECD,                                       *        
               B#PATREC,PATRECD,                                       *        
               B#SNVREC,SNVKEYD,                                       *        
               B#CMLREC,CMLRECD)                                                
         EJECT                                                                  
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-43536   11/02/20 INCREASE PROGRAM NAME TO 40 CHARACTERS   *         
***********************************************************************         
CODE     NMOD1 0,**SL20**,RR=RE                                                 
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
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
         LA    R8,WORKD(R8)                                                     
         USING SAVED,R8            R8=A(SAVE W/S)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         DROP  R6,R7                                                            
                                                                                
         ST    R5,ALP              SAVE A(LP_D)                                 
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR02            NO                                           
                                                                                
         MVC   AROUTS1,LP_AUIR1    SET LOCAL INDEX ROUTINE ADDRESSES            
         MVC   AROUTS2,LP_AUIR2    WHICH WERE LOADED BY MASTER SERVER           
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE SERVER WORKING STORAGE            
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
         MVC   LP_BLKS+((B#LP_D-1)*L'LP_BLKS)(L'LP_BLKS),ALP                    
         MVC   LP_BLKS+((B#INSREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                 
         MVC   LP_BLKS+((B#PATREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO4                 
         MVC   LP_BLKS+((B#ESTREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO4                 
         MVC   LP_BLKS+((B#SNVREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                 
         MVC   LP_BLKS+((B#CMLREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO4                 
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         LTR   R0,R0                                                            
         JZ    EXITY                                                            
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
                                                                                
         GOTOR VCALLOV,DMCB,0,X'D9000AFE'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTRPACK,0(R1)       A(TRPACK)                                    
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    QVALUES(QVALUEL),QVALUES                                         
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
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
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
                                                                                
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JE    RUNREQ10                                                         
         AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
                                                                                
RUNREQ10 MVC   MAPNUM,LP_QMAPN     EXTRACT MAP NUMBER                           
         CLC   MAPNUM,=X'0410'     GET PROGRAM NAMES?                           
         JE    RUNREQ20            YES                                          
                                                                                
         MVC   AGENCY,LP_AGY       SET AGENCY ALPHA ID                          
                                                                                
         GOTOR VDATCON,DMCB,(0,QMOS),(2,REQSTART)                               
                                                                                
***      GOTOR VADDAY,DMCB,(C'Y',QMOS),(X'80',WORK),0                           
                                                                                
***      GOTOR VDATCON,DMCB,(0,WORK),(2,REQEND)                                 
         MVC   INVMON,REQSTART                                                  
         XC    INVMON,=X'FFFF'                                                  
                                                                                
***      CLI   QBROAD,C'Y'         WANT BROADCAST MONTH?                        
***      JNE   RUNREQ15            NO                                           
         MVC   WORK(6),QMOS                                                     
         GOTOR VGETBRD,DMCB,(1,WORK),WORK+6,VGETDAY,VADDAY                      
         GOTOR VDATCON,DMCB,WORK+6,(2,REQSTART)                                 
         GOTOR VDATCON,DMCB,WORK+12,(2,REQEND)                                  
                                                                                
RUNREQ15 TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQ20                                                         
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,SPTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,TRFDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,TRFFIL,(4,0),0                               
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
RUNREQ20 GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
***********************************************************************         
* Film Usage Analysis Download                                        *         
***********************************************************************         
                                                                                
FUADLD   LKREQ H,I#SDFUAD,OUTFUA                                                
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            *        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            *        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,3,(I,B#SAVED,QPPEIND),(U,#VALPRD,$VALPRD),ARRAY=S,    *        
               SORT=NO,OLEN=L'INSKPRD,TEXT=SP#PRDL,COL=*                        
PigCd    LKREQ F,4,,(U,#VALPRD,$VALPRD),                               *        
               OLEN=L'INSKPRD,TEXT=SP#PIGL,COL=*                                
EstNo    LKREQ F,5,,LBIN,OLEN=L'EKEYEST,                               *        
               TEXT=SP#ESTLS,COL=*,SORT=NO,ARRAY=E                              
MktNo    LKREQ F,6,(I,B#SAVED,QMKTIND),LBIN,TEXT=SP#MKT,               *        
               OLEN=L'INSKMKT,COL=*                                             
Station  LKREQ F,7,(I,B#SAVED,QSTAIND),(U,#VALSTA,$VALSTA),TEXT=SP#STA,*        
               OLEN=L'INSKSTA,COL=*                                             
MOS      LKREQ F,8,(D,B#SAVED,QMOS),EDAT,TEXT=SP#STDT,COL=*                     
InvList  LKREQ F,9,(I,B#SAVED,QINVLST),CHAR,LIST=Y,SORT,               *        
               OLEN=L'SNVKINV,TEXT=SP#INV,COL=*                                 
         LKREQ E                                                                
                                                                                
PRGDLD   LKREQ H,I#SDPGMD,OUTPRG                                                
Invoice  LKREQ F,1,(I,B#SAVED,QPRGIND),CHAR,MAXLEN=10,OLEN=10,         +        
               TEXT=SP#INV,ARRAY=S                                              
UserID   LKREQ F,2,,HEXD,OLEN=2,TEXT=SP#USER                                    
Station  LKREQ F,3,,CHAR,OLEN=5,TEXT=SP#STA                                     
Date     LKREQ F,4,,CDAT,OLEN=2,TEXT=SP#DATE,ARRAY=E                            
         LKREQ E                                                                
         LKREQ X                                                                
                                                                                
***********************************************************************         
* FILM USAGE ANALYSIS DOWNLOAD FOR SPOT                               *         
***********************************************************************         
                                                                                
OUTFUA   LKOUT H                                                                
                                                                                
RDINST   LKOUT R,X'0401'           FILM USAGE ANALYSIS DOWNLOAD                 
Prout    LKOUT P,,SETCLT           READ CLIENT RECORD                           
*Prout    LKOUT P,,SETCOPY          SET COPY CODES                              
Array    LKOUT C,1,(A,ARYINS)      PATTERN RECORDS                              
         LKOUT E                                                                
                                                                                
RDSNV    LKOUT R,X'0406'           FILM USAGE ANALYSIS DOWNLOAD                 
Array    LKOUT C,1,(A,ARYCML)      COMMERCIAL TABLE                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYINS   LKOUT A,(R,NXTINS),MULTIROW=Y                                          
         LKOUT C,X'0401',(A,ARYPAT)                                             
         LKOUT E                                                                
                                                                                
ARYPAT   LKOUT A,(D,B#PATREC,PATRECD),NEWEL=Y,NROWS=1,ROWWIDTH=1000             
PatPrd   LKOUT C,1,(D,B#SAVED,PATPRD),CHAR,ND=Y                                 
PatSln   LKOUT C,2,PATKSLN,LBIN,ND=Y                                            
PatPrd2  LKOUT C,3,(D,B#SAVED,PATPRD2),CHAR,ND=Y                                
PatSln2  LKOUT C,4,PATKSLN2,LBIN,ND=Y                                           
PatRef   LKOUT C,5,PATKREF,LBIN,ND=Y                                            
*PCopyCd  LKOUT C,6,(D,B#SAVED,COPYCODE),CHAR,ND=Y                              
*PatCopyE LKOUT C,6,PATKCODE,LBIN,FILTROUT=TSTCOPYE                             
PatCopyE LKOUT C,6,PATKCODE,LBIN                                                
PatCopyE LKOUT C,7,(D,B#SAVED,NETWORK),CHAR,ND=Y                                
*PatCopy  LKOUT C,7,PATKCODE,CHAR,FILTROUT=TSTCOPY                              
Array    LKOUT C,X'0402',(A,ARYPDATA)  X'10' ELEM DATA                          
Array    LKOUT C,X'0403',(A,ARYPCML)   COMMERCIAL LIST                          
Array    LKOUT C,X'0404',(A,ARYPCMLB)  COMMERCIAL LIST FOR BPAT REC             
Array    LKOUT C,X'0405',(A,ARYPROT)   ROTATION LIST                            
         LKOUT E                                                                
********                                                                        
******** Pattern x'10' Element Data                                             
********                                                                        
ARYPDATA LKOUT A,(D,B#PATREC,PATDTAEL),EOT=EOR,NEWEL=Y,                *        
               ROWID=(PATDTAEL,X'10'),ROWWIDTH=(V,PATDTALN)                     
PatDscrp LKOUT C,1,PATDESC,CHAR,ND=Y                                            
PatStDt  LKOUT C,2,PATSTART,BDAT,ND=Y                                           
PatEndDt LKOUT C,3,PATEND,BDAT,ND=Y                                             
PatStTm  LKOUT C,4,PATSTIM,LBIN,ND=Y                                            
PatEndTm LKOUT C,5,PATETIM,LBIN,ND=Y                                            
         LKOUT E                                                                
********                                                                        
******** Pattern x'30' Element Data (Commercial List)                           
********                                                                        
ARYPCML  LKOUT A,(D,B#PATREC,PATDTAEL),EOT=EOR,NEWEL=Y,                *        
               ROWID=(PATCMLEL,X'30'),ROWWIDTH=(V,PATCMLLN)                     
                                                                                
ARRAY    LKOUT C,1,(A,ARYCMLP)                                                  
         LKOUT E                                                                
                                                                                
ARYCMLP  LKOUT A,(*,PATCML),ROWNAME=PATCMLEL,NROWS=*,ROWWIDTH=16                
                                                                                
PRout    LKOUT P,PATCML,EDTCML                                                  
CMLLST1  LKOUT C,1,(D,B#WORKD,COMML),CHAR,LEN=16                                
CMLLST2  LKOUT C,2,(D,B#WORKD,COMML2),CHAR,LEN=12,FILTROUT=FILTCML              
         LKOUT E                                                                
********                                                                        
******** Pattern x'31' Element Data (Commercial List For Bpat)                  
********                                                                        
ARYPCMLB LKOUT A,(D,B#PATREC,PATDTAEL),EOT=EOR,NEWEL=Y,                *        
               ROWID=(PATBCMEL,X'31'),ROWWIDTH=(V,PATBCMLN)                     
                                                                                
ARRAY    LKOUT C,1,(A,ARYCMLB)                                                  
         LKOUT E                                                                
                                                                                
ARYCMLB  LKOUT A,(*,PATBCML),ROWNAME=PATBCMEL,NROWS=*,ROWWIDTH=16               
                                                                                
PRout    LKOUT P,PATBCML,EDTCML                                                 
CMLBLST1 LKOUT C,1,(D,B#WORKD,COMML),CHAR,LEN=16                                
CMLBLST2 LKOUT C,2,(D,B#WORKD,COMML2),CHAR,LEN=12,FILTROUT=FILTCML              
         LKOUT E                                                                
********                                                                        
******** Pattern x'32' Element Data (Rotation List)                             
********                                                                        
ARYPROT  LKOUT A,(D,B#PATREC,PATDTAEL),EOT=EOR,NEWEL=Y,                *        
               ROWID=(PATPTNEL,X'32'),ROWWIDTH=(V,PATPTNLN)                     
                                                                                
ARRAY    LKOUT C,1,(A,ARYROT)                                                   
         LKOUT E                                                                
                                                                                
ARYROT   LKOUT A,(*,PATPTN),ROWNAME=PATPTNEL,NROWS=*,ROWWIDTH=1                 
                                                                                
CMLROT   LKOUT C,1,PATPTN,CHAR,ND=Y                                             
         LKOUT E                                                                
                                                                                
ARYCML   LKOUT A,(R,NXTINV),MULTIROW=Y                                          
         LKOUT C,X'0406',(A,ARYTCML)                                            
         LKOUT E                                                                
                                                                                
ARYTCML  LKOUT A,(D,B#CMLREC,CMLRECD),NEWEL=Y,NROWS=1,ROWWIDTH=1000             
Array    LKOUT C,X'0406',(A,ARYSEQ)    CMML SEQ NUM                             
CmlIscii LKOUT C,2,(D,B#SAVED,CMLCODE),CHAR,ND=Y                                
Array    LKOUT C,X'0406',(A,ARYADID)   AD-ID                                    
Array    LKOUT C,X'0406',(A,ARYHDEF)   HDEF/CENTERCUT                           
         LKOUT E                                                                
********                                                                        
******** Commercial x'10' element for seq num                                   
********                                                                        
ARYSEQ   LKOUT A,(D,B#CMLREC,CMLDTAEL),EOT=EOR,NEWEL=Y,                *        
               ROWID=(CMLDTAEL,X'10'),ROWWIDTH=(V,CMLDTALN)                     
Seq      LKOUT C,1,CMLSEQ,HEXD,ND=Y                                             
         LKOUT E                                                                
********                                                                        
******** X'A0' Element for Ad-ID                                                
********                                                                        
ARYADID  LKOUT A,(D,B#CMLREC,CMLDTAEL),EOT=EOR,                        *        
               ROWID=(CMLADIEL,X'A0'),ROWWIDTH=(V,CMLADILN)                     
Ad-ID    LKOUT C,3,CMLADID,CHAR,ND=Y                                            
         LKOUT E                                                                
********                                                                        
******** X'A0' Element for Hdef/Centercut                                       
********                                                                        
ARYHDEF  LKOUT A,(D,B#CMLREC,CMLDTAEL),EOT=EOR,                        *        
               ROWID=(CMLXDTEL,X'24'),ROWWIDTH=(V,CMLXDTLN)                     
Hdef     LKOUT C,4,CMLXHDEF,CHAR,ND=Y                                           
Cntrcut  LKOUT C,5,CMLXCNTR,CHAR,ND=Y                                           
         LKOUT E                                                                
                                                                                
SETCLT   NTR1                                                                   
         SR    RE,RE               GET CLIENT RECORD                            
         ICM   RE,7,QAMED                                                       
         MVC   QMEDX,LW_DATA1-LW_D(RE)                                          
         ICM   RE,7,QACLT                                                       
         MVC   QCLTX,LW_DATA1-LW_D(RE)                                          
         GOTOR (#GETCLT,AGETCLT)                                                
         J     EXITY                                                            
                                                                                
FILTCML  NTR1                                                                   
         CLC   LP_VRSN,=X'04000007'                                             
         JH    EXITY                                                            
         J     EXITN                                                            
                                                                                
EDTCML   NTR1                                                                   
         L     R2,LP_AINP          A(CMML/AD-ID ENTRY)                          
         XC    COMML,COMML                                                      
         XC    COMML2,COMML2                                                    
         MVC   COMML(8),0(R2)      MOVE IN CMML                                 
         CLC   LP_VRSN,=X'04000007'                                             
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
*&&DO                                                                           
SETCOPY  NTR1                                                                   
                                                                                
         XC    WORK(12),WORK       BUILD PROFILE KEY & GET PROFILES             
         MVC   WORK+00(4),=C'S0T0'                                              
         MVC   WORK+04(L'AGENCY),AGENCY                                         
         MVC   WORK+06(L'QMEDA),QMEDA                                           
         MVC   WORK+07(L'QCLTA),QCLTA                                           
         L     RF,ACLTREC                                                       
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         JNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),0(RF)                                                 
                                                                                
         GOTOR VGETPROF,DMCB,(X'C0',WORK),WORK+16,VDATAMGR                      
                                                                                
         CLI   WORK+26,C'E'        COPY CODE=EST?                               
         JE    EXITY               YES - ESTIMATE IS FILTER                     
                                                                                
         XR    R1,R1                                                            
         ICM   R1,7,QAPPE          HAVE PRD/PIGGY/EST TABLE?                    
         JZ    EXITY               NO                                           
         USING LW_D,R1                                                          
         XR    R2,R2                                                            
         ICM   R2,3,LW_NUMN        NUMBER OF PRD/PIGGY/EST ENTRIES              
         JZ    EXITY                                                            
         LA    R3,LW_DATA2         LIST STARTS HERE                             
         DROP  R1                                                               
                                                                                
SE10     CLI   2(R3),0             HAVE AN ESTIMATE                             
         JNE   SE20                NO                                           
SE15     LA    R3,3(R3)            NEXT PRD/PIG/EST ENTRY                       
         JCT   R2,SE10                                                          
         J     EXITY                                                            
                                                                                
         USING ESTRECD,R4                                                       
SE20     XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         MVC   EKEYAM,QMEDX        AGENCY/MEDIA                                 
         MVC   EKEYCLT,QCLTX       CLIENT                                       
                                                                                
         L     RF,ACLTREC          A(CLIENT RECORD)                             
         LA    RE,CLIST-CLTHDR(RF) POINT TO CLIST                               
         LA    RF,220              220 PRODUCTS MAX                             
                                                                                
SE25     CLC   3(1,RE),0(R3)       BINARY PRODUCTS MATCH?                       
         JE    SE30                YES                                          
         LA    RE,4(RE)                                                         
         JCT   RF,SE25             NO - BUMP CLIST POINTER                      
         DC    H'0'                                                             
                                                                                
SE30     MVC   EKEYPRD,0(RE)       3 CHAR PRODUCT CODE                          
         MVC   EKEYEST,2(R3)       ESTIMATE                                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO4'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO4             A(EST RECORD)                                
         MVC   2(1,R3),ECOPY       REPLACE ESTIMATE WITH COPY CODE              
         J     SE15                PROCESS NEXT PRD/PIG/EST ENTRY               
         DROP  R4                                                               
                                                                                
TSTCOPYE CLI   COPYCODE,C'E'       COPY CODE=EST                                
         BR    RE                  YES - SEND MAPCODE AS LBIN                   
                                                                                
TSTCOPY  CLI   COPYCODE,0          COPY CODE EST?                               
         BR    RE                  NO - SEND MAPCODE AS CHAR                    
*&&                                                                             
***  Next Instruction Recap Record                                              
NXTINS   OC    SVPATREF,SVPATREF   PROCESSING A PATTERN?                        
         JZ    NXTINS15            NO                                           
NXTINS10 BRAS  RE,GETPAT           GET PATTERN RECORD                           
         JE    EXITY                                                            
                                                                                
NXTINS15 GOTOR (#NXTREC,ANXTREC),DMCB,INSKEYT,('B#INSREC',0),          *        
               ('$NXTRTRF',SAVED),AFLTINSK,0                                    
         JNE   NOMORE                                                           
                                                                                
         XC    SVPATREF,SVPATREF                                                
         XC    SVPATNUM,SVPATNUM                                                
         XC    SVPAT10,SVPAT10                                                  
         J     NXTINS10                                                         
***********************************************************************         
*               Instruction Recap Record Filter Routine               *         
***********************************************************************         
         USING INSKEY,R2                                                        
GETPAT   NTR1                                                                   
         L     R2,IOADDR           A(RECAP RECORD)                              
         MVC   SVIOVALS,IOVALS     SAVE I/O VALUES FROM RECAP RECORD            
         ICM   R3,15,SVPATREF      ANY PATTERN REF ENTRY?                       
         JZ    GETPAT05            NO                                           
         L     R7,SVPATNUM         NUMBER OF PATTERNS LEFT                      
         L     R6,SVPAT10          A(CURRENT X'10' ELEMENT)                     
         J     GETPAT21                                                         
                                                                                
GETPAT05 LR    R6,R2               INST RECAP RECORD                            
         MVC   DATADISP,=H'24'                                                  
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         J     *+12                                                             
                                                                                
GETPAT10 MVI   ELCODE,X'10'        INST DATA ELEMENT                            
         BRAS  RE,NEXTEL           ANY MORE X'10' ELEMENTS?                     
         JNE   GETPAT30            NO - DONE                                    
                                                                                
         USING INSDTAEL,R6         1NST DATA ELEM                               
         OC    VALPIGS,VALPIGS     HAVE PIGGY FILTERS?                          
         JZ    GETPAT12            NO                                           
                                                                                
         LA    R1,VALPIGS          PIGGY FILTER LIST                            
         LA    RE,L'VALPIGS                                                     
                                                                                
GETPAT11 CLC   INSPRD2,0(R1)       YES - PIG PRD MATCH?                         
         JE    GETPAT12            YES                                          
         LA    R1,1(R1)            TRY NEXT PIGGY FILTER                        
         JCT   RE,GETPAT11                                                      
         J     GETPAT10            NO - NEXT X'10' ELEMENT                      
                                                                                
GETPAT12 MVI   COPYCODE,0                                                       
***      TM    INSFLAG,X'20'       COPY CODE = ESTIMATE?                        
***      JZ    *+8                 NO                                           
***      JZ    GETPAT10            NO - FILTER THIS OUT!                        
***      MVI   COPYCODE,C'E'       YES                                          
                                                                                
         LA    R3,INSPTTN          FIRST PATTERN IN X'10' ELEMENT               
         LLC   R7,INSDTALN         ELEMENT LENGTH                               
         SHI   R7,(INSPTTN-INSDTAEL)       GET NUMBER OF PATTERNS               
         LR    R0,R6               SAVE OFF R6                                  
         XR    R6,R6                                                            
         D     R6,=F'7'            R7 HAS NO. OF PTTNS                          
         LR    R6,R0               RESTORE R6                                   
                                                                                
GETPAT20 OC    0(3,R3),0(R3)       TEST ANY PATTERN                             
         JZ    GETPAT21            SKIP HIATUS                                  
         CLC   5(2,R3),REQSTART    IF ENDS BEFORE REQ START                     
         JL    GETPAT21            REJECT                                       
         CLC   3(2,R3),REQEND      OR IF STARTS AFTER REQ END                   
         JNH   GETPAT22                                                         
                                                                                
GETPAT21 LA    R3,7(R3)            NEXT PATTERN                                 
         JCT   R7,GETPAT20                                                      
         J     GETPAT10            NEXT X'10' ELEMENT                           
                                                                                
         USING PATRECD,R4                                                       
GETPAT22 ST    R3,SVPATREF         SAVE PATTERN REF ENTRY                       
         ST    R7,SVPATNUM         SAVE NUMBER OF PATTERNS LEFT                 
         ST    R6,SVPAT10          SAVE A(CURRENT X'10' ELEMENT)                
                                                                                
         LA    R4,IOKEY                                                         
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,INSKAM       A/M FROM KEY                                 
         MVC   PATKCLT,INSKCLT     CLIENT FROM KEY                              
         MVC   PATKPRD,INSPRD1     PRD1 FROM X'10' ELEM                         
         MVC   PATKSLN,INSSLN1     SLN1 FROM X'10' ELEM                         
         MVC   PATKPRD2,INSPRD2    PRD2 FROM X'10' ELEM                         
         MVC   PATKSLN2,INSSLN2    SLN2 FROM X'10' ELEM                         
         MVC   PATKCODE,INSKCOPY   COPY CODE FROM KEY                           
         MVC   PATKREF,0(R3)       PATTERN REFERENCE FROM X'10' ELEM            
         DROP  R6                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO4'                            
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(13),IOKEYSAV  MATCH ON KEY?                                
         JNE   GETPAT21                                                         
***      JE    GETPAT23            YES                                          
***      CLI   COPYCODE,C'E'       COPYCODE=EST?                                
***      JNE   GETPAT21            NO, GET NEXT PATTERN FROM RECAP REC          
***      MVC   IOKEY(13),IOKEYSAV  LOOK UP THE SAVE RECORD WITH...              
***      MVI   PATKCODE,0          COPY CODE=0                                  
                                                                                
***      GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO4'                            
***      JE    *+6                                                              
***      DC    H'0'                                                             
***      CLC   IOKEY(13),IOKEYSAV  MATCH ON KEY?                                
***      JNE   GETPAT21            NO                                           
         DROP  R4                                                               
                                                                                
GETPAT23 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO4'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PATRECD,R3                                                       
         L     R3,AIO4                                                          
         ST    R3,LP_ADATA                                                      
                                                                                
         TM    15(R3),X'02'        INCOMPLETE BPAT RECORD?                      
         JO    GETPAT21            YES - REJECT                                 
         MVI   BPATREC,C'N'                                                     
         TM    15(R3),X'01'        BPAT RECORD?                                 
         JZ    *+8                 NO                                           
         MVI   BPATREC,C'Y'        YES - INDICATE BPAT RECORD                   
         CLC   PATSTIM,=H'2400'    START TIME 2400?                             
         JNE   *+10                NO                                           
         XC    PATSTIM,PATSTIM     YES - 12M IS X'0000'                         
         XC    PATKREF,=3X'FF'     UN-COMPLEMENT                                
         SR    RF,RF                                                            
         ICM   RF,7,PATKREF                                                     
         SRL   RF,10               USE ONLY 14 BITS                             
         STCM  RF,7,PATKREF                                                     
                                                                                
         MVI   PACKED,C'N'         DEFAULT IS CMML ARE NOT PACKED               
         TM    PATSTAT1,PATSADID   ALL CMML PACKED?                             
         JZ    *+8                 NO                                           
         MVI   PACKED,C'Y'         YES                                          
                                                                                
         LA    R1,PATPRD           PUT 3 CHAR PRD HERE                          
         LA    R4,PATKPRD          BINARY PRD IN PATTERN KEY                    
                                                                                
GETPAT24 L     RF,ACLTREC          A(CLIENT RECORD)                             
         LA    RE,CLIST-CLTHDR(RF) POINT TO CLIST                               
         LA    RF,220              220 PRODUCTS MAX                             
                                                                                
GETPAT25 CLC   3(1,RE),0(R4)       BINARY PRODUCTS MATCH?                       
         JE    GETPAT26            YES                                          
         LA    RE,4(RE)                                                         
         JCT   RF,GETPAT25         NO - BUMP CLIST POINTER                      
         DC    H'0'                                                             
                                                                                
GETPAT26 MVC   0(3,R1),0(RE)       MOVE THE 3 CHAR PRODUCT CODE                 
         LA    RF,PATKPRD2         POINT TO SECOND PRODUCT                      
         CR    RF,R4               DID WE JUST PROCESS THE PIGGY?               
         JE    GETPAT27            YES - DONE                                   
         XC    PATPRD2,PATPRD2     CLEAR THE 3 CHAR PIGGY CODE                  
         CLI   PATKPRD2,0          IS THERE A BINARY PIGGY CODE?                
         JE    GETPAT27            NO - DON'T BOTHER LOOKING FOR ONE            
         LA    R1,PATPRD2          PUT 3 CHAR PRD2 HERE                         
         LA    R4,PATKPRD2         BINARY PRD2 IN PATTERN KEY                   
         J     GETPAT24                                                         
         DROP  R3                                                               
                                                                                
GETPAT27 LR    R6,R3               PATTERN RECORD                               
         MVI   ELCODE,X'10'        GET PATTERN DATA ELEMENT                     
         BRAS  RE,GETEL            HAVE PATTERN DATA ELEMENT?                   
         JNE   GETPAT28            NO - DONE                                    
                                                                                
         USING PATDTAEL,R6                                                      
         L     R3,SVPATREF         USE INSTRUCTION RECAP RECORD FOR...          
         GOTOR VDATCON,DMCB,(2,3(R3)),(3,PATSTART) START DATE                   
         GOTOR (RF),(R1),(2,5(R3)),(3,PATEND)      AND END DATE                 
         DROP  R6                                                               
                                                                                
GETPAT28 MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
                                                                                
GETPAT30 MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITN                                                            
***********************************************************************         
*                Instruction Recap Key Filter Routine                 *         
***********************************************************************         
F        USING INSKEY,IOKEY                                                     
FLTINSK  NTR1                                                                   
         XR    R1,R1                                                            
         ICM   R1,7,QASTA          HAVE PRD/PIGGY/EST TABLE?                    
         JZ    FLTI05              NO                                           
         USING LW_D,R1                                                          
         MVC   FULL(3),LW_DATA1    REQ STATION                                  
         MVC   STATION,F.INSKSTA   INST RECAP STATION                           
         CLI   FULL,X'E8'          CABLE?                                       
         JL    *+12                NO                                           
         NI    FULL+2,X'80'                                                     
         NI    STATION+2,X'80'                                                  
         CLC   STATION,FULL                                                     
         JNE   EXITN               NO - FILTER THIS RECORD OUT!                 
         XC    NETWORK,NETWORK     CLEAR THE NETWORK                            
         CLI   FULL,X'E8'          CABLE?                                       
         JL    FLTI05              NO                                           
         USING STAPACKD,VSSTAPKD                                                
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVI   STAPMED,C'T'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPSTA,F.INSKSTA                                                
         DROP  R1                                                               
         GOTOR VSTAPACK,STAPACKD                                                
         MVC   NETWORK,STAPQNET                                                 
                                                                                
FLTI05   XC    VALPIGS,VALPIGS                                                  
         XR    R1,R1                                                            
         ICM   R1,7,QAPPE          HAVE PRD/PIGGY/EST TABLE?                    
         JZ    EXITY               NO                                           
         USING LW_D,R1                                                          
         XR    R2,R2                                                            
         ICM   R2,3,LW_NUMN        NUMBER OF PRD/PIGGY/EST ENTRIES              
         JZ    EXITY                                                            
         LA    R3,LW_DATA2         LIST STARTS HERE                             
         DROP  R1                                                               
*                                                                               
FLTI10   CLI   0(R3),X'FF'         POL?                                         
         JE    FLTI15              YES - ALL VALID                              
         CLC   F.INSKPRD,0(R3)     MATCH ON PRODUCT?                            
         JE    FLTI15              YES                                          
FLTI11   LA    R3,3(R3)            BUMP TO NEXT ENTRY                           
         JCT   R2,FLTI10                                                        
                                                                                
         J     EXITN               NO - FILTER THIS RECORD OUT!                 
                                                                                
FLTI15   CLC   F.INSKCOPY,2(R3)    MATCH ON ESTIMATE?                           
         JNE   FLTI11              NO                                           
                                                                                
FLTI20   MVC   HALF(1),0(R3)       PRD                                          
         MVC   HALF+1(1),2(R3)     EST                                          
         LA    R1,VALPIGS          PIGGY LIST TO VALIDATE                       
         LA    R4,L'VALPIGS+1      L'PIGGY LIST                                 
*                                                                               
FLTI25   CLC   0(1,R3),HALF        PRD MATCH?                                   
         JNE   FLTI30              NO                                           
         CLC   2(1,R3),HALF+1      EST MATCH?                                   
         JNE   FLTI30              NO                                           
         CLI   1(R3),0             HAVE A PIGGY?                                
         JE    FLTI30              NO                                           
         JCT   R4,*+6              HAVE ROOM LEFT IN PIGGY LIST?                
         DC    H'0'                NO - NEEDS EXPANSION!                        
         MVC   0(1,R1),1(R3)       MOVE PIGGY TO THE LIST                       
         LA    R1,1(R1)            BUMP PIGGY LIST                              
*                                                                               
FLTI30   LA    R3,3(R3)            BUMP TO NEXT ENTRY                           
         JCT   R2,FLTI25                                                        
         J     EXITY                                                            
                                                                                
***  Next Invoice Record                                                        
NXTINV   CLI   LP_RMODE,LP_RFRST   FIRST TIME IN?                               
         JNE   *+10                NO                                           
         XC    SEQLIST,SEQLIST     YES - CLEAR SEQ NUMBER LIST                  
         OC    SV30ELEM,SV30ELEM   CONTINUE PROCESSING AN INVOICE?              
         JZ    NINV15              NO - GET NEXT INVOICE                        
NINV10   BRAS  RE,GETCML           CONTINUE GETTING X'30' ELEMENTS              
         JE    EXITY                                                            
                                                                                
NINV15   GOTOR (#NXTREC,ANXTREC),DMCB,INVKEYT,('B#SNVREC',0),          *        
               ('$NXTRXSP',SAVED),0,0                                           
         JNE   NOMORE                                                           
                                                                                
         XC    SV30ELEM,SV30ELEM   SAVE A(LAST X'30' ELEM PROCESSED)            
         J     NINV10                                                           
***********************************************************************         
*               Get commercial codes from invoice x'30' elem          *         
***********************************************************************         
         USING SNVKEYD,R2                                                       
GETCML   NTR1                                                                   
         L     R2,IOADDR           A(INV RECORD)                                
         MVC   SVIOVALS,IOVALS     SAVE I/O VALUES FROM RECAP RECORD            
         XC    CMLCODE,CMLCODE     CLEAR THE LAST COMMERCIAL RECORD             
                                                                                
         ICM   R6,15,SV30ELEM      HAVE A(LAST X'30' ELEM PROCESSED)?           
         JZ    INV05               NO - START FROM THE 1ST X'30' ELEM           
         CLI   READCNTR,C'Y'       READ THE CENTERCUT ALREADY?                  
         JE    INV10               YES, GET NEXT X'30' ELEM                     
***                                                                             
* READ THE CENTERCUT COMMERCIAL                                                 
***                                                                             
         USING SNVCMELD,R6         CMML ELEMENT                                 
         MVI   READCNTR,C'Y'       FLAG CENTERCUT AS READ                       
         LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY         CLEAR THE KEY                                
         USING CMLRECD,R4                                                       
         MVC   CMLCNT,=X'0AC3'     PASSIVE KEY BY CENTERCUT                     
         MVC   CMLCNTAM,SNVKAM     A/M                                          
         MVC   CMLCNTCL,SNVKCLT    CLIENT                                       
         DROP  R6                                                               
                                                                                
         L     R6,AIO4             CMML RECORD                                  
         MVC   DATADISP,=H'24'                                                  
         MVI   ELCODE,X'A0'        AD-ID ELEMENT                                
         BRAS  RE,GETEL            HAVE ONE?                                    
         JNE   INV03               NO                                           
         USING CMLADIEL,R6                                                      
         MVC   CMLCNTID,CMLADIDP   LOOK UP CENTERCUT ENTRY                      
         DROP  R6                                                               
         ICM   R6,15,SV30ELEM      RESTORE A(LAST X'30' ELEM)                   
         J     INV15                                                            
                                                                                
INV03    ICM   R6,15,SV30ELEM      A(LAST X'30' ELEM PROCESSED)                 
         J     INV10                                                            
                                                                                
INV05    MVC   DATADISP,=H'42'                                                  
         LR    R6,R2               INV RECORD                                   
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         J     *+12                                                             
                                                                                
INV10    MVI   ELCODE,X'30'        CMML ELEMENT                                 
         BRAS  RE,NEXTEL           ANY MORE X'30' ELEMENTS?                     
         JNE   INV20               NO - DONE                                    
         USING SNVCMELD,R6         CMML ELEMENT                                 
                                                                                
         ST    R6,SV30ELEM         SAVE A(CURRENT X'10' ELEMENT)                
         OC    SNVCMSEQ,SNVCMSEQ   HAVE A SEQ NUM?                              
         JNZ   INV12               YES                                          
                                                                                
         L     R0,AIO4             CLEAR COMMERCIAL RECORD AREA                 
         LA    R1,4000             AS WERE ONLY PASSING BACK                    
         XR    RE,RE               THE INVALID COMMERCIAL CODE                  
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   CMLCODE,SNVCMCD     PASS BACK INVALID CMML                       
         MVI   READCNTR,C'Y'       DON'T READ CENTERCUT FOR THIS ONE!           
         J     INV19                                                            
                                                                                
INV12    LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         USING CMLRECD,R4                                                       
         MVC   CMLPID,=X'0AA1'     READ COMMERCIAL REC BY SEQ NUMBER            
         MVC   CMLPAM,SNVKAM       A/M                                          
         MVC   CMLPCLT,SNVKCLT     CLIENT                                       
         MVC   CMLPSEQ+1(2),SNVCMSEQ                                            
         MVI   READCNTR,C'N'       INDICATE THAT CENTERCUT WAS NOT READ         
         DROP  R6                                                               
                                                                                
INV15    GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTRFDIR+IO4'                            
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(13),IOKEYSAV  MATCH ON KEY?                                
         JNE   INV10               NO, GET NEXT X'30' ELEMENT                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTRFFIL+IO4'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO4             A(COMMERCIAL RECORD)                         
         LA    RE,SEQLIST          TRAFFIC SEQ LIST                             
         LA    RF,L'SEQLIST-2/2    ROOM FOR 124 SEQ NUMBERS                     
                                                                                
INV17    OC    0(2,RE),0(RE)       EMPTY SLOT?                                  
         JZ    INV18               YES - ADD NEW SEQ NUMBER                     
         CLC   0(2,RE),CMLSEQ+1    ALREADY SENT THIS COMMERCIAL?                
         JE    INV10               YES - GET NEXT X'30' ELEMENT                 
         LA    RE,2(RE)            BUMP TABLE                                   
         JCT   RF,INV17                                                         
         DC    H'0'                                                             
                                                                                
INV18    MVC   0(2,RE),CMLSEQ+1    ADD SEQ NUMBER TO TABLE                      
         MVC   CMLCODE(8),CMLKCML  PASS BACK COMMERCIAL CODE                    
         TM    CMLRSTAT,CMLKSTA_PCKD    PACKED AD-ID?                           
         JZ    INV19                    NO                                      
         GOTOR VTRPACK,DMCB,(C'U',CMLKCML),CMLCODE                              
                                                                                
INV19    MVC   LP_ADATA,AIO4                                                    
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITY                                                            
                                                                                
INV20    MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXITN                                                            
         DROP  R4                                                               
***********************************************************************         
* PROGRAM NAMES DOWNLOAD                                                        
***********************************************************************         
                                                                                
OUTPRG   LKOUT H                                                                
RDPROG   LKOUT R,X'0411'           FILM USAGE ANALYSIS DOWNLOAD                 
ARRAY    LKOUT C,1,(A,ARYPRG)      EXTRACT PROGRAM NAME                         
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYPRG   LKOUT A,(R,NXTPRG),MULTIROW=Y                                          
INVOICE  LKOUT C,1,(D,B#SAVED,PRGINV),CHAR,ND=Y                                 
PRGNAME  LKOUT C,2,(D,B#SAVED,PRGPROG),CHAR,ND=Y                                
DATE     LKOUT C,3,(D,B#SAVED,PRGDATE),CHAR,ND=Y                                
TIME     LKOUT C,4,(D,B#SAVED,PRGTIME),CHAR,ND=Y                                
LENGTH   LKOUT C,5,(D,B#SAVED,PRGLEN),CHAR,ND=Y                                 
COST     LKOUT C,6,(D,B#SAVED,PRGCOST),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
NXTPRG   CLI   LP_RMODE,LP_RFRST   FIRST TIME IN?                               
         JNE   NXTP05              NO                                           
                                                                                
         GOTOR VCALLOV,DMCB,0,X'D9000A2C'                                       
         MVC   EZWRKIO,0(R1)       SET ADDRESS OF WORKIO                        
                                                                                
         GOTOR VCALLOV,DMCB,0,X'D9000A2E'                                       
         MVC   EZPARSE,0(R1)       SET ADDRESS OF EZPARSE                       
                                                                                
         XR    R1,R1               CLEAR R1                                     
         ICM   R1,7,QPRG           A(INVOICE ARRAY) SET?                        
         JZ    NOMORE              NO - DONE                                    
                                                                                
         XR    R0,R0               CLEAR R0                                     
         ICM   R0,3,LW_NUMN-LW_D(R1)                                            
         STC   R0,QPRGIND          # OF ENTRIES                                 
         LA    R0,LW_DATA2-LW_D(R1)                                             
         STCM  R0,7,QPRG           A(PROGRAM NAME LIST)                         
         MVI   PRGFLAG,0           CLEAR FLAG                                   
                                                                                
NXTP05   XC    PRGDATA,PRGDATA     CLEAR OUTPUT FIELDS                          
         LA    R3,PRGINV           SET LP_ADATA TO DUMMY STORAGE                
         ST    R3,LP_ADATA         SO DDLINK DOESN'T COMPLAIN                   
         LA    R4,EZWRKIOB         WORK AREA                                    
         USING WRKIOB,R4           WORK AREA DSECT                              
         XR    R3,R3               CLEAR R3                                     
         ICM   R3,7,QPRG           PROGRAM NAME INPUT                           
         USING PGMNMIN,R3          PROGRAM NAME INPUT DSECT                     
         TM    PRGFLAG,X'40'       PROCESSING INVOICE DETAILS?                  
         JO    NXTP20              YES - GET THE NEXT INVOICE DETAIL            
         CLI   LP_RMODE,LP_RFRST   FIRST TIME IN?                               
         JE    NXTP06              YES                                          
                                                                                
         LLC   R0,QPRGIND          PREV NUM OF ENTRIES LEFT TO PROCESS          
         BCTR  R0,0                CURRENT NUM OF ENTRIES LEFT                  
         LTR   R0,R0               ZERO ENTRIES LEFT?                           
         JZ    NOMORE              YES - WE ARE DONE                            
         STC   R0,QPRGIND          SAVE NEW COUNTER                             
         LA    R3,PGMNMINQ(R3)     BUMP TO NEXT ENTRY                           
         STCM  R3,7,QPRG           A(NEXT PROGRAM NAME LIST)                    
                                                                                
NXTP06   LA    RE,PINV+L'PINV-1    LAST CHARACTER OF INVOICE                    
         LA    RF,L'PINV           LENGTH OF INVOICE                            
                                                                                
NXTP07   CLI   0(RE),X'40'         SPACE?                                       
         JNE   NXTP08              NO - LAST CHARACTER OF INVOICE               
         BCTR  RE,0                BUMP BACK ONE BYTE                           
         JCT   RF,NXTP07           DECREMENT INVOICE LENGTH COUNTER             
         J     EXITY                                                            
                                                                                
NXTP08   STC   RF,PINVLEN          INVOICE LENGTH                               
         XC    EZWRKIOB,EZWRKIOB   CLEAR WORK AREA                              
         MVC   WRKEZUID,PUSERID    USER-ID                                      
         MVC   WRKIACOM,ACOMFACS   A(COMFACS)                                   
         LA    RF,EZIO             IO AREA                                      
         ST    RF,WRKIAREC         A(IO AREA)                                   
         LAY   RF,WKBUFF           IO BUFFER                                    
         ST    RF,WRKIABUF         A(IO BUFFER)                                 
         MVI   WRKIFTYP,WRKIFTEZ   EZ WORKER FILE                               
                                                                                
NXTP10   MVI   WRKIACTN,WRKIANDX   GET FIRST/NEXT RECORD INDEX                  
         GOTO1 EZWRKIO,WRKIOB      CALL WORKIO                                  
         CLI   WRKIERRS,0          ANY ERRORS?                                  
         JE    NXTP15              NO                                           
         MVI   PRGFLAG,0           CLEAR FLAG                                   
         CLI   WRKIERRS,WRKIEEOF   EOF?                                         
         JE    EXITY               YES - DONE                                   
         DC    H'0'                ANY OTHER ERROR = DEATH                      
                                                                                
NXTP15   CLI   WRKEZDAY,WRKEZDYQ   IS IT AN INVOICE?                            
         JNE   NXTP10              NO                                           
         CLC   WRKEZSCL,PSTATION   STATION CALL LETTERS MATCH?                  
         JNE   NXTP10              NO                                           
         CLI   WRKEZMED,C'C'       WORKER FILE MEDIA IN KEY = 'C'?              
         JNE   *+8                 NO                                           
         MVI   WRKEZMED,C'N'       YES - CHANGE IT TO MEDIA 'N'                 
         CLC   WRKEZMED,PSTATION+4 MATCH ON MEDIA?                              
         JNE   NXTP10              NO                                           
         CLC   WRKEZBDT,PDATE      MATCH ON BATCH DATE?                         
         JNE   NXTP10              NO                                           
                                                                                
NXTP20   MVI   WRKIACTN,WRKIAGET   GET THE WORKER FILE                          
         GOTO1 EZWRKIO,WRKIOB      CALL WORKIO                                  
         CLI   WRKIERRS,0          ANY ERRORS?                                  
         JE    NXTP25              NO                                           
         MVI   PRGFLAG,0           CLEAR FLAG                                   
         CLI   WRKIERRS,WRKIEEOF   EOF?                                         
         JE    NXTP10              YES - GET NEXT INDEX                         
         DC    H'0'                ANY OTHER ERROR = DEATH                      
                                                                                
NXTP25   CLC   EZIO+4(2),=C'31'    INVOICE HEADER INFO?                         
         JE    NXTP30              YES                                          
         CLC   EZIO+4(2),=C'51'    INVOICE DETAIL INFO?                         
         JNE   NXTP20              NO - GET THE NEXT RECORD                     
         TM    PRGFLAG,X'80'       PROCESSING THIS INVOICE?                     
         JZ    NXTP20              NO - GET THE NEXT RECORD                     
         J     NXTP35              YES - START PROCESSING DETAILS               
                                                                                
NXTP30   MVI   PRGFLAG,0           CLEAR THE FLAG                               
         TM    EZIO+7,X'40'        CONVERTED INVOICE?                           
         JZ    NXTP20              NO - SKIP                                    
*                                                                               
         LA    R2,EZIO             WORKER FILE INVOICE LINE                     
         GOTOR EZPARSE,DMCB,(R2),('FTMAXENT',FLDTAB)                            
         JE    *+6                 NO ERROR RETURNED                            
         DC    H'0'                ELSE DEATH                                   
                                                                                
         GOTOR GETFDISP,10         HAVE INVOICE NUMBER?                         
         JZ    NXTP20              NO                                           
         CLM   RF,1,PINVLEN        INVOICE LENGTHS ARE THE SAME?                
         JNE   NXTP20              NO - GET NEXT WORKER FILE ENTRY              
         BCTR  RF,0                INVOICE LENGTH                               
         BASR  RE,0                A(INSTRUCTION) FOR EX                        
         CLC   PINV(0),0(R1)       INVOICE WERE LOOKING FOR?                    
         EX    RF,0(RE)            EXECUTE                                      
         JNE   NXTP20              NO                                           
                                                                                
         OI    PRGFLAG,X'80'       PROCESSING THIS INVOICE                      
         J     NXTP20              START LOOKING FOR DETAILS                    
                                                                                
NXTP35   LA    R2,EZIO             WORKER FILE INVOICE LINE                     
         GOTOR EZPARSE,DMCB,(R2),('FTMAXENT',FLDTAB)                            
         JE    *+6                 NO ERROR RETURNED                            
         DC    H'0'                ELSE DEATH                                   
                                                                                
         XC    PRGDATA,PRGDATA     CLEAR OUTPUT FIELDS                          
                                                                                
         GOTOR GETFDISP,18         HAVE PROGRAM NAME?                           
         JZ    NXTP40              NO - GET NEXT WORKER FILE ENTRY              
         BCTR  RF,0                LENGTH                                       
         CHI   RF,L'PRGPROG-1      PROGRAM NAME > 40 CHARACTERS?                
         JNH   *+8                 NO                                           
         LA    RF,L'PRGPROG-1      YES - ONLY MOVE 40 CHARACTERS                
         BASR  RE,0                A(INSTRUCTION) FOR EX                        
         MVC   PRGPROG(0),0(R1)    PROGRAM NAME                                 
         EX    RF,0(RE)            EXECUTE                                      
                                                                                
         GOTOR GETFDISP,3          HAVE DETAIL DATE?                            
         JZ    NXTP40              NO - GET NEXT WORKER FILE ENTRY              
         BCTR  RF,0                LENGTH                                       
         BASR  RE,0                A(INSTRUCTION) FOR EX                        
         MVC   PRGDATE(0),0(R1)    DETAIL DATE                                  
         EX    RF,0(RE)            EXECUTE                                      
                                                                                
         GOTOR GETFDISP,5          HAVE DETAIL TIME?                            
         JZ    NXTP40              NO - GET NEXT WORKER FILE ENTRY              
         BCTR  RF,0                LENGTH                                       
         BASR  RE,0                A(INSTRUCTION) FOR EX                        
         MVC   PRGTIME(0),0(R1)    DETAIL TIME                                  
         EX    RF,0(RE)            EXECUTE                                      
                                                                                
         GOTOR GETFDISP,6          HAVE DETAIL LENGTH?                          
         JZ    NXTP40              NO - GET NEXT WORKER FILE ENTRY              
         BCTR  RF,0                LENGTH                                       
         BASR  RE,0                A(INSTRUCTION) FOR EX                        
         MVC   PRGLEN(0),0(R1)     DETAIL LENGTH                                
         EX    RF,0(RE)            EXECUTE                                      
                                                                                
         GOTOR GETFDISP,8          HAVE DETAIL COST?                            
         JZ    NXTP40              NO - GET NEXT WORKER FILE ENTRY              
         BCTR  RF,0                LENGTH                                       
         BASR  RE,0                A(INSTRUCTION) FOR EX                        
         MVC   PRGCOST(0),0(R1)    DETAIL COST                                  
         EX    RF,0(RE)            EXECUTE                                      
*                                                                               
         MVC   PRGINV,PINV         INVOICE                                      
         OI    PRGFLAG,X'40'       PROCESSED INVOICE DETAIL                     
         J     EXITY               UPLOAD DETAIL DATA                           
                                                                                
NXTP40   XC    PRGDATA,PRGDATA     CLEAR OUTPUT FIELDS                          
         J     NXTP20              KEEP LOOKING FOR DETAILS                     
                                                                                
GETFDISP LR    RF,R1               FIELD NUMBER                                 
         BCTR  RF,0                DEC FOR INDEXING (FIELD 1 = INDEX 0)         
         MHI   RF,FTLENQ           TIMES TABLE LENGTH                           
         LA    RF,FLDTAB(RF)       INDEX INTO THE TABLE                         
         XR    R1,R1               CLEAR R1                                     
         ICM   R1,3,FTFLDDSP-FLDTABD(RF) FLD DISPLACEMENT                       
         BZR   RE                  EXIT IF ZERO                                 
         AR    R1,R2               INDEX INTO EZIO                              
         LLC   RF,FTFLDLEN-FLDTABD(RF) FLD LENGTH                               
         LTR   RF,RF               RETURN CONDITION CODE                        
         BR    RE                  RETURN TO CALLER                             
                                                                                
RTRNYES  CR    RE,RE               APPLIED TO AT LEAST ONE FILTER               
         BR    RE                                                               
RTRNNO   LTR   RE,RE               DOES NOT MATCH ANY FILTER                    
RTRNCC   BR    RE                                                               
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,0                SET CONDITION CODE TO NOT EQUAL              
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CONDITION CODE TO EQUAL                  
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   GENERAL EXIT POINT                           
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
B#INSREC EQU   3                   IO3 - INSTRUCTION RECAP RECORD               
AINSREC  EQU   LP_BLKS+((B#INSREC-1)*L'LP_BLKS)                                 
B#PATREC EQU   4                   IO4 - PATTERN RECORD                         
APATREC2 EQU   LP_BLKS+((B#PATREC-1)*L'LP_BLKS)                                 
B#ESTREC EQU   4                   IO4 - PATTERN RECORD                         
APATRECN EQU   LP_BLKS+((B#ESTREC-1)*L'LP_BLKS)                                 
B#SNVREC EQU   3                   IO3 - INVOICE RECORDS                        
ASNVREC  EQU   LP_BLKS+((B#SNVREC-1)*L'LP_BLKS)                                 
B#CMLREC EQU   4                   IO4 - CMML RECORDS                           
ACMLREC2 EQU   LP_BLKS+((B#CMLREC-1)*L'LP_BLKS)                                 
B#SVRDEF EQU   12                  SPLNK22                                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   14                  LP_D                                         
EOR      EQU   0                                                                
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
FUADL#   DC    AL2(I#SDFUAD)       FILM USAGE ANALYSIS                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
FUAPATK# DC    AL2(I#SDFUPK)       FUA PATTERN KEY RESPONSE                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
FUA10EL# DC    AL2(I#SDFU10)       FUA PATTERN X'10' ELEM RESPONSE              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
FUACML#  DC    AL2(I#SDFUCL)       FUA PATTERN CMML LIST RESPONSE               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
FUACMLB# DC    AL2(I#SDFUCB)       FUA PATTERN CMML LIST BPAT RESPONSE          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
FUAROT#  DC    AL2(I#SDFURL)       FUA PATTERN ROT LIST RESPONSE                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
FUACMLT# DC    AL2(I#SDFUCT)       FUA MASTER CMML TABLE RESPONSE               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
PNMDL#   DC    AL2(I#SDPGMD)       PROGRAM NAME REQUEST                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
PNMRS#   DC    AL2(I#SDPGMR)       PROGRAM NAME RESPONSE                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
SPTDIR   DC    C'SPTDIR '                                                       
SPTFIL   DC    C'SPTFILE'                                                       
XSPDIR   DC    C'XSPDIR '                                                       
XSPFIL   DC    C'XSPFIL '                                                       
TRFDIR   DC    C'TRFDIR '                                                       
TRFFIL   DC    C'TRFFILE'                                                       
         EJECT                                                                  
LVALUES  DS    0F                  ** LITERALS MOVED TO SAVED **                
         DC    A(FLTINSK)                                                       
*                                                                               
INSKEYT  LKKEY H,INSKEY,SAVED      ** INSTRUCTION RECAP KEY DRIVER **           
         LKKEY LIT,INSKID,X'0A',1                                               
         LKKEY LIT,INSKID+1,X'24',1                                             
         LKKEY WMP,INSKAM,QAMED                                                 
         LKKEY WMP,INSKCLT,QACLT                                                
         LKKEY ALL,INSKPRD                                                      
         LKKEY WMP,INSKMKT,QAMKT                                                
         LKKEY ALL,INSKSTA                                                      
         LKKEY ALL,INSKCOPY                                                     
         LKKEY ALL,INSKDPT                                                      
         LKKEY E                                                                
*                                                                               
INVKEYT  LKKEY H,SNVKEY,SAVED      ** INVOICE KEY DRIVER **                     
         LKKEY LIT,SNVKTYPE,SNVKTYPQ                                            
         LKKEY LIT,SNVKSUB,SNVKSUBQ                                             
         LKKEY WMP,SNVKAM,QAMED                                                 
         LKKEY WMP,SNVKCLT,QACLT                                                
         LKKEY WMP,SNVKSTA,QASTA                                                
         LKKEY SIN,SNVKMOS,INVMON                                               
         LKKEY WMP,SNVKINV,QAINVLST                                             
         LKKEY ALL,SNVKMINK                                                     
         LKKEY E                                                                
                                                                                
SAVED    DSECT ,                   ** DSECT TO COVER SAVED STORAGE **           
                                                                                
WVALUES  DS    0X                  ** LITERAL VALUES **                         
                                                                                
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AFLTINSK DS    A                   A(INST RECAP KEY FILTER ROUTINE)             
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
                                                                                
MAPNUM   DS    XL(L'LP_QMAPN)      RECORD MAP NUMBER                            
*                                                                               
         DS    0F                                                               
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
                                                                                
QMEDIND  DS    X                   MEDIA                                        
QAMED    DS    AL3                                                              
                                                                                
QCLTIND  DS    X                   CLIENT                                       
QACLT    DS    AL3                                                              
                                                                                
QPPEIND  DS    X                   PRODUCT-PIGGY-ESTIMATE LIST                  
QAPPE    DS    AL3                                                              
                                                                                
QMKTIND  DS    X                   MARKET                                       
QAMKT    DS    AL3                                                              
                                                                                
QSTAIND  DS    X                   STATION                                      
QASTA    DS    AL3                                                              
                                                                                
QINVLST  DS    X                   INVOICE LIST                                 
QAINVLST DS    AL3                                                              
                                                                                
QPRGIND  DS    X                   INV, USER ID, STATION, DATE                  
QPRG     DS    AL3                                                              
                                                                                
QMOS     DS    CL6                 MONTH OF SERVICE                             
                                                                                
SVPATREF DS    F                   SAVED PATTERN REF                            
SVPATNUM DS    F                   SAVED NUM OF PATTERNS TO PROCESS             
SVPAT10  DS    F                   SAVED PATTERN X'10' ELEMENT                  
SV30ELEM DS    F                   SAVED X'30' ELEMENT ON INVOICE               
                                                                                
         ORG   QVALUES+L'QVALUES                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
INVMON   DS    XL2                 INV MOS X'FFFF' COMPLIMENTED                 
REQSTART DS    XL2                                                              
REQEND   DS    XL2                                                              
VALPIGS  DS    XL25                PIGGY LIST                                   
DATADISP DS    H                                                                
CMLCODE  DS    CL12                                                             
SEQLIST  DS    XL250                                                            
PINVLEN  DS    XL1                 PROGRAM INVOICE LENGTH                       
*                                                                               
PRGDATA  DS    0CL(PRGLNQ)                                                      
PRGINV   DS    CL10                                                             
PRGPROG  DS    CL40                                                             
PRGDATE  DS    CL6                                                              
PRGTIME  DS    CL4                                                              
PRGLEN   DS    CL3                                                              
PRGCOST  DS    CL10                                                             
PRGLNQ   EQU   *-PRGINV                                                         
         DS    0F                                                               
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
ELCODE   DS    CL1                                                              
COPYCODE DS    XL1                                                              
BPATREC  DS    XL1                                                              
READCNTR DS    XL1                                                              
PATPRD   DS    CL3                                                              
PATPRD2  DS    CL3                                                              
NETWORK  DS    CL3                                                              
STATION  DS    CL3                                                              
STIME    DS    CL11                                                             
ETIME    DS    CL11                                                             
AGENCY   DS    CL(L'LP_AGY)        AGENCY CODE                                  
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
                                                                                
FTMAXENT EQU   50                                                               
FLDTAB   DS    0X                                                               
         DS    (FTMAXENT*FTLENQ)X                                               
                                                                                
OVALUES  DS    0D                  ** OUTPUT VALUES **                          
         ORG   OVALUES                                                          
         EJECT                                                                  
*                                                                               
PGMNMIN  DSECT                     PROGRAM NAMES INPUT DSECT                    
PINV     DS    CL10                INVOICE                                      
PUSERID  DS    XL2                 USER-ID                                      
PSTATION DS    CL5                 STATION                                      
PDATE    DS    XL2                 DATE                                         
PGMNMINQ EQU   *-PGMNMIN                                                        
                                                                                
       ++INCLUDE SPLNKWRK                                                       
                                                                                
WORKD    DSECT ,                   ** REDEFINE OVERWORK **                      
         ORG   OVERWORK                                                         
SVIOVALS DS    XL(IOVALL)          SAVED I/O VALUES                             
VSSTAPKD DS    XL(STAPACKL)        STAPACK BLOCK                                
VTRPACK  DS    A                   A(TRPACK)                                    
COMML    DS    CL16                COMMERCIAL/AD-ID                             
COMML2   DS    CL12                PIGGY COMMERCIAL/AD-ID                       
PACKED   DS    CL1                 PACKED FLAG                                  
         DS    XL2                 SPARE FOR ALIGNMENT                          
PRGFLAG  DS    XL1                 PROGRAM NAME FLAG                            
EZWRKIO  DS    A                   A(EZWORKIO)                                  
EZPARSE  DS    A                   A(EZPARSE)                                   
EZWRKIOB DS    XL256               DMWRKIO CONTROL BLOCK (194+56 SPARE)         
EZIO     DS    XL500               DMWRKIO IO AREA                              
WKBUFF   DS    14336C                                                           
         ORG                                                                    
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPTRINST                                                       
       ++INCLUDE SPTRPAT                                                        
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE EZFLDTAB                                                       
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPLNK20   11/02/20'                                      
         END                                                                    
