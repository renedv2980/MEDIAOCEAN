*          DATA SET SPLNK19    AT LEVEL 023 AS OF 11/21/19                      
*PHASE T21E19B                                                                  
SPLNK19  TITLE '- DESKTOP REVISION DOWNLOADS'                                   
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,                                                 *        
               SERVERTYPE=TSTSPOT,                                     *        
               WORKERKEY=SPRD,                                         *        
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
               B#REVREC,DRVRECD)                                                
         EJECT                                                                  
CODE     NMOD1 0,**SL19**,RR=RE                                                 
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
         MVC   ABUFFRIN,RBUFFRIN                                                
         DROP  R6,R7                                                            
                                                                                
         ST    R5,ALP              SAVE A(LP_D)                                 
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         USING STABLKD,WORK                                                     
         USING STAPACKD,WORK                                                    
         EJECT                                                                  
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
         MVC   LP_BLKS+((B#AGYREC-1)*L'LP_BLKS)(AIOLAST-AIO1),AIO1              
                                                                                
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
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    QVALUES(QVALUEL),QVALUES                                         
         XC    DVALUES(DVALUEL),DVALUES                                         
         LA    R0,QVALS                                                         
         LHI   R1,L'QVALS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    RUNI,RUNI                                                        
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
                                                                                
         XC    MAPI,MAPI           INITIALIZE MAP INDICATORS                    
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JE    RUNREQ10                                                         
         AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
                                                                                
RUNREQ10 MVC   MAPNUM,LP_QMAPN     EXTRACT MAP NUMBER                           
         MVC   MAPI,L'LP_QMAPN(RF) EXTRACT MAP INDICATOR                        
         MVC   AGENCY,LP_AGY       SET AGENCY ALPHA ID                          
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQ20                                                         
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,SPTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPFIL,(4,0),0                               
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
RUNREQ20 GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
***********************************************************************         
* Revision download - Request for Revsheet                            *         
***********************************************************************         
                                                                                
REVDLD   LKREQ H,I#SDREVD,POINTTO=REQDLD     0300-REVISION DOWNLOAD             
WRKDLD   LKREQ H,I#SDWRKD,POINTTO=REQDLD     0320-WORK DOWNLOAD                 
                                                                                
REQDLD   LKREQ H,0,OUTREV                                                       
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,QMEDIND),(U,#VALMED,$VALMED),            *        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,QCLTIND),(U,#VALCLT,$VALCLT),            *        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,3,(I,B#SAVED,QPRDIND),CHAR,TEXT=SP#PRO,               *        
               OLEN=L'DRVKPRD,COL=*                                             
EstNo    LKREQ F,4,(I,B#SAVED,QESTIND),LBIN,TEXT=SP#EST,               *        
               OLEN=L'DRVKEST,COL=*                                             
MktNo    LKREQ F,5,(I,B#SAVED,QMKTIND),LBIN,TEXT=SP#MKT,               *        
               OLEN=L'DRVKMKT,COL=*                                             
RevS#    LKREQ F,6,(D,B#SAVED,QREVSEQ),LBIN,TEXT=(*,REVSLIT),COL=*              
D/LType  LKREQ F,7,(D,B#SAVED,QDLTYPE),CHAR,TEXT=(*,REVSHIS),COL=*              
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Revision Response Link Overlays                                     *         
***********************************************************************         
                                                                                
REQRSR   LKREQ H,I#SDRSDR,POINTTO=REQULR     03FF-REVSHEET RESPONSE             
REQRLR   LKREQ H,I#SDRLDR,POINTTO=REQULR     03FE-REVLINE RESPONSE              
REQWSR   LKREQ H,I#SDWSDR,POINTTO=REQULR     03FC-WORKSHEET RESPONSE            
REQALR   LKREQ H,I#SDALDR,POINTTO=REQULR     03FD-AVAILLINE RESPONSE            
                                                                                
REQULR   LKREQ H,0,OUTREV                                                       
                                                                                
RecDA    LKREQ F,D#DA,(I,B#SAVED,RDAIND),HEXD,OLEN=L'DRVKDA,           *        
               TEXT=SP#SDBDA,COL=*                                              
PCKey    LKREQ F,D#PCKEY,(D,B#SAVED,PCKEY),CHAR,TEXT=SP#KEY,COL=*               
                                                                                
TxtResp  LKREQ F,D#UPLERR,(D,B#SAVED,ERRTEXT),VSTR,TEXT=SP#ERR,COL=*            
                                                                                
NumResp  LKREQ F,D#UPLERN,(D,B#SAVED,ERRNUM),LBIN,TEXT=SP#ERR,COL=*             
                                                                                
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
         EJECT                                                                  
***********************************************************************         
* Revision download                                                   *         
***********************************************************************         
                                                                                
OUTREV   LKOUT H                                                                
                                                                                
REVREV   LKOUT R,X'03FF'           REVISION/WORK RECORDS                        
Array    LKOUT C,1,(A,ARYRWP)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYRWP   LKOUT A,(R,NXTRWP),MULTIROW=Y                                          
         LKOUT C,255,(A,ARYSHT),FILTROUT=TSTSHT                                 
         LKOUT C,255,(A,ARYLIN),FILTROUT=TSTLINE                                
         LKOUT C,I#SDRSDR,(A,ARYPCKY),FILTROUT=TST03FF                          
         LKOUT C,I#SDRLDR,(A,ARYPCKY),FILTROUT=TST03FE                          
         LKOUT C,I#SDALDR,(A,ARYPCKY),FILTROUT=TST03FD                          
         LKOUT C,I#SDWSDR,(A,ARYPCKY),FILTROUT=TST03FC                          
*                                                                               
         LKOUT E                                                                
*                                                                               
TST03FF  CLC   MAPNUM,=AL2(I#SDRSDR)                                            
         BR    RE                                                               
TST03FE  CLC   MAPNUM,=AL2(I#SDRLDR)                                            
         BR    RE                                                               
TST03FD  CLC   MAPNUM,=AL2(I#SDALDR)                                            
         BR    RE                                                               
TST03FC  CLC   MAPNUM,=AL2(I#SDWSDR)                                            
         BR    RE                                                               
                                                                                
                                                                                
******   Test to download the sheet                                             
TSTSHT   L     RF,IOADDR                                                        
         OC    DRVKREVL-DRVKEY(L'DRVKREVL,RF),DRVKREVL-DRVKEY(RF)               
         BR    RE                                                               
                                                                                
******   TEST TO DOWNLOAD THE LINE                                              
TSTLINE  L     RF,IOADDR                                                        
         OC    DRVKREVL-DRVKEY(L'DRVKREVL,RF),DRVKREVL-DRVKEY(RF)               
SETMAPCC JZ    *+8                 SET MAP INDICATOR CC                         
         CR    RE,RE               CC=EQUAL IF INDICATOR ON                     
         BR    RE                                                               
         LTR   RE,RE               CC=NOT EQUAL IF INDICATOR OFF                
         BR    RE                                                               
*                                                                               
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
*                                                                               
ARYPCKY  LKOUT A,(D,B#SAVED,PCKEY),NEWEL=Y,NROWS=1                              
PCKey    LKOUT C,1,(D,B#SAVED,PCKEY),CHAR,ND=Y                                  
ErrTxt   LKOUT C,2,(D,B#SAVED,ERRTEXT),CHAR,ND=Y                                
ErrNum   LKOUT C,3,(D,B#SAVED,ERRNUM),LBIN,ND=Y                                 
         LKOUT E                                                                
*                                                                               
***  Revsheet RECORD DATA                                                       
***  Worksheet RECORD DATA                                                      
                                                                                
ARYSHT   LKOUT A,(D,B#REVREC,DRVRECD),NROWS=1,ROWWIDTH=1000                     
PRout    LKOUT P,DRVKREVS,COMPSEQ      Complement Revsheet Seq No               
Array    LKOUT C,X'0308',(A,ARYNAM)    SHEET NAME                               
Array    LKOUT C,X'0301',(A,ARYSDT)    SHEET DATA                               
Array    LKOUT C,X'0302',(A,ARYSDM)    SHEET DEMOS                              
Array    LKOUT C,X'0303',(A,ARYSBK)    SHEET BOOKS                              
Array    LKOUT C,X'0304',(A,ARYSUP)    SHEET UPGRADE FORMULA                    
ARRAY    LKOUT C,X'030B',(A,ARYSCM)    SHEET CUMES                              
Array    LKOUT C,X'0309',(A,ARYDTP)    SHEET DAY/TIME PERIOD OVERRIDES          
Array    LKOUT C,X'0309',(A,ARYNDTP)   SHEET NEW DTP OVERRIDES                  
Array    LKOUT C,X'0306',(A,ARYSAC)    SHEET ACTIVITY                           
Array    LKOUT C,X'0307',(A,ARYSCY)    SHEET COPIED FROM                        
Array    LKOUT C,X'0305',(A,ARYSNT)    SHEET NOTES                              
         LKOUT E                                                                
                                                                                
***  Revline RECORD DATA                                                        
***  Avail RECORD DATA                                                          
                                                                                
ARYLIN   LKOUT A,(D,B#REVREC,DRVRECD),NROWS=1,ROWWIDTH=1000                     
PRout    LKOUT P,DRVKREVL,COMPSEQ      Complement RevLine Seq No                
Array    LKOUT C,X'0310',(A,ARYLDT)    LINE DATA                                
Array    LKOUT C,X'0311',(A,ARYLCM)    LINE COMMENTS                            
Array    LKOUT C,X'0312',(A,ARYLDM)    LINE ESTIMATE DEMOS                      
Array    LKOUT C,X'0312',(A,ARYLNDM)   NEW LINE ESTIMATE DEMOS                  
Array    LKOUT C,X'031B',(A,ARYLDL)    LINE ESTIMATE SPILL DEMOS                
Array    LKOUT C,X'031B',(A,ARYLNDL)   NEW LINE ESTIMATE SPILL DEMOS            
Array    LKOUT C,X'0313',(A,ARYLUP)    LINE UPGRADE FORMULA                     
Array    LKOUT C,X'0314',(A,ARYLSP)    LINE SPOTS                               
Array    LKOUT C,X'0315',(A,ARYLRT)    LINE EFFECTIVE RATES                     
Array    LKOUT C,X'0316',(A,ARYLHI)    LINE HIATUS                              
Array    LKOUT C,X'0317',(A,ARYLOR)    LINE ORBITS                              
Array    LKOUT C,X'0317',(A,ARYLNOR)    LINE ORBITS                             
Array    LKOUT C,X'0318',(A,ARYLPK)    LINE PACKAGE                             
Array    LKOUT C,X'031A',(A,ARYLNT)    LINE NOTES                               
Array    LKOUT C,X'031C',(A,ARYAAU)    LINE AUTO-AVAIL UUID                     
Array    LKOUT C,X'031D',(A,ARYXFR)    LINE TRANSFER                            
Array    LKOUT C,X'0319',(A,ARYLAC)    LINE ACTIVITY                            
                                                                                
         LKOUT E                                                                
******   Complement Seq Number                                                  
COMPSEQ  L     R1,LP_AINP          COMPLEMENT REVSHEET/LINE SEQ NUM             
         MVC   OSEQNO,0(R1)                                                     
         OC    OSEQNO,OSEQNO       LEAVE BLANK IF NOT THERE                     
         JZ    EXITY                                                            
         XC    OSEQNO,EFFS                                                      
         J     EXITY                                                            
                                                                                
******   Header Record                                                          
ARYHDR   LKOUT A,(D,B#SAVED,KEYTYP),NEWEL=Y,NROWS=1                             
HdrTyp   LKOUT C,1,(D,B#SAVED,KEYTYP),HEXD                                      
History  LKOUT C,2,(D,B#SAVED,HISTFLG),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
******   Name and Seq No                                                        
ARYNAM   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RSNELD,RSNELQ),ROWWIDTH=(V,RSNLEN)                        
                                                                                
Array    LKOUT C,X'0321',(A,ARYHDR)                                             
RevS#    LKOUT C,1,(D,B#SAVED,OSEQNO),LBIN,ND=Y                                 
RSName   LKOUT C,2,RSNAME,CHAR,LEN=V,ND=Y                                       
         LKOUT E                                                                
                                                                                
******** Revsheet Data                                                          
ARYSDT   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RSDELD,RSDELQ),ROWWIDTH=(V,RSDLEN)                        
                                                                                
DSKADDR  LKOUT C,10,(D,B#SAVED,RECDA),HEXD,ND=Y                                 
PutCS    LKOUT C,1,(D,B#SAVED,DCHKSUM),HEXD                                     
StDate   LKOUT C,2,RSDFLTST,CDAT,ND=Y                                           
EnDate   LKOUT C,3,RSDFLTEN,CDAT,ND=Y                                           
*djCd    LKOUT C,4,RSDADJ,VSTR,ND=Y                                             
AdjCd    LKOUT C,4,RSDADJ,(R,EDTADJ),LEN=2,ND=Y                                 
RepCd    LKOUT C,5,RSDREP,VSTR,ND=Y                                             
PurpCd   LKOUT C,6,RSDPURP,VSTR,ND=Y                                            
BuyerCd  LKOUT C,7,RSDBYR,VSTR,ND=Y                                             
PrdCd    LKOUT C,8,RSDPRD,VSTR,ND=Y                                             
AudDem   LKOUT C,9,RSDAUDEM,(R,EDTDCD),ND=Y                                     
Daily    LKOUT C,20,RSDFDALY,CHAR,ND=Y                                          
CombDly  LKOUT C,21,RSDFCMBD,CHAR,ND=Y                                          
AutAdj   LKOUT C,22,RSDFAADJ,CHAR,ND=Y                                          
AARhome  LKOUT C,23,RSDFAAD2,CHAR,ND=Y                                          
AATarget LKOUT C,24,RSDFAAD3,CHAR,ND=Y                                          
HiLinE#  LKOUT C,25,RSDHILN#,LBIN,ND=Y                                          
cSBkType LKOUT C,26,RSCSBKTY,CHAR,ND=Y                                          
cSSrvDat LKOUT C,27,RSCSSRDT,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
******   Revsheet Demo Categories                                               
ARYSDM   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RSCELD,RSCELQ),ROWWIDTH=(V,RSCLEN)                        
                                                                                
Array    LKOUT C,1,(A,ARYDEMC)     DEMO CATEGORY/INDICATORS                     
         LKOUT E                                                                
                                                                                
ARYDEMC  LKOUT A,(*,RSCDEMO),ROWNAME=RSCEL,NROWS=*,ROWWIDTH=RSCLEN2             
                                                                                
DemCd    LKOUT C,1,RSCDEMO,(R,EDTDCD),FILTROUT=TSTNODMO,SKIPCOLS=3              
BcastFlg LKOUT C,2,RSCDFBCS,CHAR,ND=Y                                           
CableFlg LKOUT C,3,RSCDFCBL,CHAR,ND=Y                                           
DisCat   LKOUT C,4,RSCDFDIS,CHAR,ND=Y                                           
         LKOUT E                                                                
                                                                                
TSTNODMO L     R1,LP_AINP                                                       
         OC    0(L'RSCDEMO,R1),0(R1)                                            
         JNZ   RTRNYES                                                          
         J     RTRNNO              RTE if we send an empty mapcode 1            
                                                                                
******   Revsheet Demo Books                                                    
ARYSBK   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RSBELD,RSBELQ),ROWWIDTH=(V,RSBLEN)                        
                                                                                
ARRAY    LKOUT C,1,(A,ARYBOOK),FILTROUT=TSTBOOK                                 
         LKOUT E                                                                
                                                                                
ARYBOOK  LKOUT A,(*,RSBBOOK),ROWNAME=RSBEL,NROWS=*,ROWWIDTH=RSBLEN2             
                                                                                
BOOK     LKOUT C,1,RSBBOOK,CHAR,ND=Y                                            
         LKOUT E                                                                
                                                                                
TSTBOOK  L     R1,LP_AINP                                                       
         CLI   RSBBOOK-RSBEL(R1),C'A'                                           
         JNL   RTRNYES                                                          
         J     RTRNNO              DON'T D/L OLD STYLE BOOK ELEMENTS            
                                                                                
******   Revsheet Upgrade Formula                                               
ARYSUP   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RSUELD,RSUELQ),ROWWIDTH=(V,RSULEN)                        
                                                                                
SttDt    LKOUT C,1,RSUSTTDT,CDAT,ND=Y                                           
EndDt    LKOUT C,2,RSUENDDT,CDAT,ND=Y                                           
Bcastflg LKOUT C,3,RSUTBRD,CHAR,ND=Y                                            
Cableflg LKOUT C,4,RSUTCBL,CHAR,ND=Y                                            
Upgrade  LKOUT C,5,RSUFORM,VSTR,LEN=V,ND=Y                                      
         LKOUT E                                                                
                                                                                
******   Revsheet CUME                                                          
ARYSCM   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RSCUELD,RSCUELQ),ROWWIDTH=(V,RSCULEN)                     
                                                                                
TimeZone LKOUT C,1,RSCUTZ,CHAR,ND=Y                                             
CUMEForm LKOUT C,2,RSCUME,VSTR,LEN=V,ND=Y                                       
         LKOUT E                                                                
                                                                                
******   Revsheet NEW DAY/TIME PERIOD OVERRIDES                                 
ARYNDTP  LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=B,                   *        
               ROWID=(RSPNELD,RSPNELQ),ROWWIDTH=(V,RSPNLEN)                     
                                                                                
Station  LKOUT C,1,RSPNSTA,VSTR,ND=Y                                            
Days     LKOUT C,2,RSPNDAYS,LBIN,ND=Y                                           
StaTime  LKOUT C,3,RSPNSTIM,LBIN,ND=Y                                           
EndTime  LKOUT C,4,RSPNETIM,LBIN,ND=Y                                           
Upgrade  LKOUT C,5,RSPNUFRM,VSTR,LEN=V,FILTROUT=TSTNUPG                         
Array    LKOUT C,1,(A,ARYNDTPD),FILTROUT=TSTNDEM                                
         LKOUT E                                                                
                                                                                
TSTNUPG  L     R1,LP_AINP                                                       
         CLI   RSPNTYPE-RSPNEL(R1),RSPNUPG                                      
         BR    RE                                                               
                                                                                
TSTNDEM  L     R1,LP_AINP                                                       
         CLI   RSPNTYPE-RSPNEL(R1),RSPNDEM                                      
         BR    RE                                                               
                                                                                
ARYNDTPD LKOUT A,(*,RSPNDEMO),ROWNAME=RSPNEL,NROWS=*,ROWWIDTH=RSPNLNQ2          
DemCd    LKOUT C,6,RSPNCAT,(R,EDTDCD),FILTROUT=TSTNODMO,SKIPCOLS=6              
Prout    LKOUT P,RSPNRAW,RD2DRPL5                                               
DemVal   LKOUT C,7,RSPNRAW,SPAK,ND=Y                                            
DemOver  LKOUT C,8,RSPNFP,MB80,ND=Y                                             
PRout    LKOUT P,,STDFDTPP                                                      
DemPrec  LKOUT C,13,RSPNFP,(R,EDTLON),ND=Y,PCVERSION=4.7.0.37                   
SHRVal   LKOUT C,9,RSPNSHR,SPAK,ND=Y                                            
SHROver  LKOUT C,10,RSPNSFP,MB80,ND=Y                                           
SHRPrec  LKOUT C,14,RSPNSFP,(R,EDTLON),ND=Y,PCVERSION=4.7.0.37                  
PUTVal   LKOUT C,11,RSPNPUT,SPAK,ND=Y                                           
PUTOver  LKOUT C,12,RSPNPFP,MB80,ND=Y                                           
PUTPrec  LKOUT C,15,RSPNPFP,(R,EDTLON),ND=Y,PCVERSION=4.7.0.37                  
         LKOUT E                                                                
                                                                                
******   Revsheet DAY/TIME PERIOD OVERRIDES                                     
ARYDTP   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=B,                   *        
               ROWID=(RSPHELD,RSPHELQ),ROWWIDTH=(V,RSPHLEN)                     
                                                                                
Station  LKOUT C,1,RSPHSTA,VSTR,ND=Y                                            
Days     LKOUT C,2,RSPHDAYS,LBIN,ND=Y                                           
StaTime  LKOUT C,3,RSPHSTIM,LBIN,ND=Y                                           
EndTime  LKOUT C,4,RSPHETIM,LBIN,ND=Y                                           
Upgrade  LKOUT C,5,RSPHUFRM,VSTR,LEN=V,FILTROUT=TSTUPG                          
Array    LKOUT C,1,(A,ARYDTPD),FILTROUT=TSTDEM                                  
         LKOUT E                                                                
                                                                                
TSTUPG   L     R1,LP_AINP                                                       
         CLI   RSPHTYPE-RSPHEL(R1),RSPHUPG                                      
         BR    RE                                                               
                                                                                
TSTDEM   L     R1,LP_AINP                                                       
         CLI   RSPHTYPE-RSPHEL(R1),RSPHDEM                                      
         BR    RE                                                               
                                                                                
ARYDTPD  LKOUT A,(*,RSPHDEMO),ROWNAME=RSPHEL,NROWS=*,ROWWIDTH=RSPHLNQ2          
DemCd    LKOUT C,6,RSPHCAT,(R,EDTDCD),FILTROUT=TSTNODMO,SKIPCOLS=6              
PRout    LKOUT P,RSPHRAW,RD2DRPL3                                               
DemVal   LKOUT C,7,RSPHRAW,SPAK,ND=Y                                            
DemOver  LKOUT C,8,RSPHDOVR,CHAR,ND=Y                                           
PRout    LKOUT P,,STDFDTPP                                                      
DemPrec  LKOUT C,13,(D,B#WORKD,BYTE1),LBIN,ND=Y,PCVERSION=4.7.0.37              
SHRVal   LKOUT C,9,RSPHSHR,SPAK,ND=Y                                            
SHROver  LKOUT C,10,RSPHSOVR,CHAR,ND=Y                                          
SHRPrec  LKOUT C,14,(D,B#WORKD,BYTE2),LBIN,ND=Y,PCVERSION=4.7.0.37              
PUTVal   LKOUT C,11,RSPHPUT,SPAK,ND=Y                                           
PUTOver  LKOUT C,12,RSPHPOVR,CHAR,ND=Y                                          
PUTPrec  LKOUT C,15,(D,B#WORKD,BYTE3),LBIN,ND=Y,PCVERSION=4.7.0.37              
         LKOUT E                                                                
*********                                                                       
* SET DEFAULT DEMO PRECISION                                                    
*********                                                                       
STDFDTPP MVI   BYTE1,2             2 DEC PREC RAW                               
         MVI   BYTE2,2             2 DEC PREC SHR                               
         MVI   BYTE3,2             2 DEC PREC PUT                               
         CLI   DEMOTYPE,C'R'       HAVE RATING?                                 
         JE    EXITY                YES, LEAVE PRECISION AS IS                  
         CLI   DEMOTYPE,C'E'       OR EXTENDED?                                 
         JE    EXITY                YES, LEAVE PRECISION AS IS                  
         MVI   BYTE3,1             2-DEC PREC RAW                               
         J     EXITY                                                            
                                                                                
******   Copied From Revsheet                                                   
ARYSCY   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RSRELD,RSRELQ),ROWWIDTH=(V,RSRLEN)                        
                                                                                
CpyDt    LKOUT C,1,RSRDATE,CDAT,ND=Y                                            
CpyWho   LKOUT C,2,RSRPID,(R,EDTPID),ND=Y                                       
CpyMed   LKOUT C,3,RSRMED,CHAR,ND=Y                                             
CpyClt   LKOUT C,4,RSRCLT,(U,#EDTCLT,$EDTCLT),ND=Y                              
CpyPrd   LKOUT C,5,RSRPRD,CHAR,ND=Y                                             
CpyEst   LKOUT C,6,RSREST,LBIN,ND=Y                                             
CpyMkt   LKOUT C,7,RSRMKT,LBIN,ND=Y                                             
PRout    LKOUT P,RSRSEQ,COMPSEQ            Complement Revsheet Seq No           
CpySeq   LKOUT C,8,(D,B#SAVED,OSEQNO),LBIN,ND=Y                                 
CpyNM    LKOUT C,9,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
******   Revsheet Comments = Notes                                              
ARYSNT   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=B,                   *        
               ROWID=(RSNTELD,RSNTELQ),ROWWIDTH=(V,RSNTLEN)                     
                                                                                
Seq      LKOUT C,1,RSNTSEQ,LBIN,ND=Y                                            
Comnt    LKOUT C,2,RSNTTXT,CHAR,LEN=V,ND=Y                                      
PutCS    LKOUT C,4,(D,B#SAVED,DCHKSUM),HEXD                                     
         LKOUT E                                                                
                                                                                
******   Revsheet Activity                                                      
ARYSAC   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RSAELD,RSAELQ),ROWWIDTH=(V,RSALEN)                        
                                                                                
AddDt    LKOUT C,1,RSAADD,CDAT,ND=Y                                             
AddTime  LKOUT C,2,RSAATIME,HEXD,ND=Y                                           
AddPid   LKOUT C,3,RSAAPID,(R,EDTPID),ND=Y                                      
AddNM    LKOUT C,4,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
ChgDt    LKOUT C,5,RSACHG,CDAT,ND=Y                                             
ChgTime  LKOUT C,6,RSACTIME,HEXD,ND=Y                                           
ChgPid   LKOUT C,7,RSACPID,(R,EDTPID),ND=Y                                      
ChgNM    LKOUT C,8,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
LXfDt    LKOUT C,9,RSAXFR,CDAT,ND=Y                                             
LXfTim   LKOUT C,10,RSAXTIME,HEXD,ND=Y                                          
LXfPid   LKOUT C,11,RSAXPID,(R,EDTPID),ND=Y                                     
LXfNM    LKOUT C,12,(D,B#SAVED,PERNAME),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
******   Revline Downloads   ******                                             
                                                                                
******   Revline Data                                                           
ARYLDT   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLDELD,RLDELQ),ROWWIDTH=(V,RLDLEN)                        
                                                                                
Array    LKOUT C,X'0322',(A,ARYHDR)                                             
RevL#    LKOUT C,1,(D,B#SAVED,OSEQNO),LBIN,ND=Y    always send first            
ByLin    LKOUT C,2,RLDBLINE,LBIN,ND=Y                                           
Statn    LKOUT C,3,RLDSTA,(R,EDTCBL),CHAR,ND=Y                                  
Buyer    LKOUT C,4,RLDBYR,CHAR,ND=Y                                             
SDate    LKOUT C,5,RLDSTDT,CDAT,ND=Y                                            
EDate    LKOUT C,6,RLDENDT,CDAT,ND=Y                                            
Days     LKOUT C,7,RLDDAYS,LBIN,ND=Y                                            
StDay    LKOUT C,8,RLDSTDAY,HEXD,ND=Y                                           
STime    LKOUT C,9,RLDSTIME,SPAK,ND=Y                                           
ETime    LKOUT C,10,RLDETIME,SPAK,ND=Y                                          
SecLn    LKOUT C,11,RLDSLN,LBIN,ND=Y                                            
SptNPW   LKOUT C,12,RLDNPW,SPAK,ND=Y                                            
DayPt    LKOUT C,13,RLDDAYPT,CHAR,ND=Y                                          
SubDP    LKOUT C,14,RLDSDYPT,CHAR,ND=Y                                          
ProgN    LKOUT C,15,RLDPROG,(R,EDTCHAR),ND=Y                                    
AdjCd    LKOUT C,16,RLDADJ,(R,EDTADJ),LEN=2,ND=Y                                
Rate     LKOUT C,17,RLDCOST,SPAK,ND=Y                                           
*RateTy1  LKOUT C,18,RLDCIND,CHAR,ND=Y                                          
RateTy1  LKOUT C,18,RLDCIND,(R,EDTRTYP),ND=Y                                    
RepCd    LKOUT C,19,RLDREP,(R,EDTCHAR),ND=Y                                     
PrdMS    LKOUT C,20,RLDPRD,CHAR,ND=Y                                            
PigMS    LKOUT C,21,RLDPRD2,CHAR,ND=Y                                           
PigLn    LKOUT C,22,RLDPRDLN,(R,EDTSPAK),ND=Y                                   
**PigLn  LKOUT C,22,(D,B#WORKD,WORK),CHAR,ND=Y                                  
MGcd     LKOUT C,23,RLDMGCD,CHAR,ND=Y                                           
BuyID    LKOUT C,24,RLDBYID,(R,EDTCHAR),ND=Y                                    
Reasn    LKOUT C,25,RLDREASN,CHAR,ND=Y                                          
TaxRt    LKOUT C,26,RLDTAX,(R,EDTSPAK),ND=Y                                     
**TaxRt  LKOUT C,26,(D,B#WORKD,WORK),CHAR,ND=Y                                  
PurpC    LKOUT C,27,RLDPURP,(R,EDTCHAR),ND=Y                                    
HISQ#    LKOUT C,28,RLDHISQ#,HEXD,ND=Y                                          
Cost2    LKOUT C,29,RLCOST2,SPAK,ND=Y,FILTROUT=TSTCOS2                          
NoCost2  LKOUT C,29,(D,B#WORKD,WORK),CHAR,ND=Y                                  
Dailyflg LKOUT C,30,RLDFDALY,CHAR,ND=Y                                          
PrvSchd  LKOUT C,31,RLDFPSCD,CHAR,ND=Y                                          
DelBuy   LKOUT C,32,RLDFDELX,CHAR,ND=Y                                          
Cos2flg  LKOUT C,33,RLCOS2TP,CHAR,ND=Y                                          
LineID#  LKOUT C,36,RLLINEID,LBIN,ND=Y                                          
PutCS    LKOUT C,34,(D,B#SAVED,DCHKSUM),HEXD                                    
DskAddr  LKOUT C,35,(D,B#SAVED,RECDA),HEXD,ND=Y                                 
DelSpot  LKOUT C,37,RLDDLSPT,CHAR,ND=Y                                          
Cos2RTyp LKOUT C,38,RLC2RTYP,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
TSTCOS2  L     R1,LP_AINP                                                       
         LA    R1,RLCOST2+5-RLDEL(R1)                                           
         J     TSTEFF                                                           
*                                                                               
TSTEFF   XC    WORK,WORK                                                        
         CLI   0(R1),X'FF'                                                      
         JNE   RTRNYES                                                          
         MVC   WORK(6),=CL6'NOCOS2'                                             
         J     RTRNNO                                                           
                                                                                
******   Revline Comments                                                       
ARYLCM   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLCMELD,RLCMELQ),ROWWIDTH=(V,RLCMLEN)                     
                                                                                
Comnt    LKOUT C,0,RLCMTXT,CHAR,LEN=V,MAPMOD1=RLCMNUM,ND=Y                      
         LKOUT E                                                                
                                                                                
******   Revline NEW Estimate Demo Categories and Values                        
ARYLNDM  LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=B,                   *        
               ROWID=(RLNEDELD,RLNEDELQ),ROWWIDTH=(V,RLNEDLEN)                  
                                                                                
Book     LKOUT C,1,RLNEDBK,BMON,ND=Y                                            
BType    LKOUT C,2,RLNEDBT,(R,TRNSBKT),ND=Y                                     
ARRAY    LKOUT C,3,(A,ARYLNDM2)                                                 
         LKOUT E                                                                
                                                                                
ARYLNDM2 LKOUT A,(*,RLNEDDEM),ROWNAME=RLNEDEL,ROWWIDTH=RLNEDLN2Q,      *        
               NROWS=*                                                          
                                                                                
DemCd    LKOUT C,3,RLNEDCAT,(R,EDTDCD)                                          
PRout    LKOUT P,RLNEDRAW,RD2DRPL5                                              
DemVl    LKOUT C,4,RLNEDRAW,SPAK,ND=Y                                           
DemOv    LKOUT C,5,RLNEDFP,MB80,ND=Y                                            
DemPrec  LKOUT C,14,RLNEDFP,(R,EDTLON),ND=Y,PCVERSION=4.7.0.37                  
PRout    LKOUT P,RLNEDSHR,RD2DRPL5                                              
SHRVl    LKOUT C,6,RLNEDSHR,SPAK,ND=Y                                           
SHROV    LKOUT C,7,RLNEDSFP,MB80                                                
SHRPrec  LKOUT C,15,RLNEDSFP,(R,EDTLON),ND=Y,PCVERSION=4.7.0.37                 
PRout    LKOUT P,RLNEDPUT,RD2DRPL5                                              
PUTVl    LKOUT C,8,RLNEDPUT,SPAK,ND=Y                                           
PUTOV    LKOUT C,9,RLNEDPFP,MB80                                                
PUTPrec  LKOUT C,16,RLNEDPFP,(R,EDTLON),ND=Y,PCVERSION=4.7.0.37                 
NtDemRLk LKOUT C,12,(D,B#SAVED,NTDEMFLG),MB80,FILTROUT=TSTNTRD                  
NtDemSLk LKOUT C,13,(D,B#SAVED,NTDEMFLG),MB40,FILTROUT=TSTNTRD                  
         LKOUT E                                                                
                                                                                
EDTLON   LM    R2,R4,LP_AINP       EDIT OUT LOW NIBBLE                          
         OI    0(R2),X'F0'         CONVERT TO EBCDIC                            
         CLI   0(R2),C'2'          IF <2, OUTPUT NOTHING                        
         JL    XCOLEN                                                           
         MVC   0(1,R4),0(R2)                                                    
         MVI   LP_OLEN+3,1                                                      
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
******   Revline Estimate Demo Categories and Values                            
ARYLDM   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=B,                   *        
               ROWID=(RLEDELD,RLEDELQ),ROWWIDTH=(V,RLEDLEN)                     
                                                                                
Book     LKOUT C,1,RLEDBOOK,BMON,ND=Y                                           
BType    LKOUT C,2,RLEDBTYP,(R,TRNSBKT),ND=Y                                    
Array    LKOUT C,3,(A,ARYLDM2)                                                  
         LKOUT E                                                                
                                                                                
ARYLDM2  LKOUT A,(*,RLEDDEMO),ROWNAME=RLEDEL,NROWS=*,ROWWIDTH=RLEDLN2Q          
                                                                                
DemCd    LKOUT C,3,RLEDCAT,(R,EDTDCD)                                           
PRout    LKOUT P,RLEDRAW,RD2DRPL3                                               
DemVl    LKOUT C,4,RLEDRAW,SPAK,ND=Y                                            
DemOV    LKOUT C,5,RLEDOVR,CHAR,ND=Y                                            
PRout    LKOUT P,,STDEFPRC                                                      
DemPrec  LKOUT C,14,(D,B#WORKD,BYTE),LBIN,ND=Y,PCVERSION=4.7.0.37               
SHRVl    LKOUT C,6,RLEDSHR,SPAK,ND=Y                                            
SHROV    LKOUT C,7,RLEDSOVR,CHAR,ND=Y                                           
SHRPrec  LKOUT C,15,(D,B#WORKD,BYTE),LBIN,ND=Y,PCVERSION=4.7.0.37               
PUTVl    LKOUT C,8,RLEDPUT,SPAK,ND=Y                                            
PUTOV    LKOUT C,9,RLEDPOVR,CHAR,ND=Y                                           
PUTPrec  LKOUT C,16,(D,B#WORKD,BYTE),LBIN,ND=Y,PCVERSION=4.7.0.37               
NtDemRLk LKOUT C,12,(D,B#SAVED,NTDEMFLG),MB80,FILTROUT=TSTNTRD                  
NtDemSLk LKOUT C,13,(D,B#SAVED,NTDEMFLG),MB40,FILTROUT=TSTNTRD                  
         LKOUT E                                                                
                                                                                
TSTNTRD  OC    LP_VRSN,LP_VRSN     IF NO VERSION                                
         JZ    TSTNTRD5             THEN GO TEST IF NON-TRAD DEMO               
         CLC   LP_VRSN,V460050     IF VERSION LT 4.6.0.50                       
         JL    RTRNNO               THEN DON'T RETURN                           
TSTNTRD5 L     R1,LP_AINP          LETS TEST IF NON-TRAD DEMO                   
         LLC   RF,0(R1)                                                         
         ICM   RF,B'0010',2(R1)    BYTE1&3 ZERO, NON-TRADITIONAL?               
         LTR   RF,RF               HAVE NON-TRADITIONAL DEMO CAT?               
         BR    RE                                                               
                                                                                
*********                                                                       
* SET DEFAULT DEMO PRECISION                                                    
*********                                                                       
STDEFPRC MVI   BYTE,1              1-DEC PREC                                   
         CLI   DEMOTYPE,C'R'       HAVE RATING?                                 
         JE    SET2DECP             YES, SET 2-DEC PREC                         
         CLI   DEMOTYPE,C'E'       OR EXTENDED?                                 
         JNE   EXITY                NO, LEAVE PRECISION AS IS                   
SET2DECP MVI   BYTE,2              2-DEC PREC RAW                               
         J     EXITY                                                            
*                                                                               
*********                                                                       
* ROUND 2-DEC PREC RADIO RATING/EXTENDED DEMO VALUES TO NEAREST 10TH            
*                                        HWON - SPEC-25154  9/15/2019           
*********                                                                       
RD2DRPL5 LA    R3,L'RLNEDRAW-1     INPUT DATA IS A PL5                          
         OC    LP_VRSN,LP_VRSN     HAVE VERSION?                                
         JZ    RR2DT005             NO, DO NOT ROUND IMPS                       
         CLC   LP_VRSN,V470037     OR VERSION 4.7.0.37 AND HIGHER               
         JNL   RR2DT005             YES, DO NOT ROUND IMPS                      
         CLI   DEMOTYPE,C'R'       HAVE RATING?                                 
         JE    RR2DT005             YES, NOT IMPS, DO NOT ROUND                 
         CLI   DEMOTYPE,C'E'       OR EXTENDED?                                 
         JE    RR2DT005             YES, NOT IMPS, DO NOT ROUND                 
         L     RE,LP_AINP                                                       
         EXRL  R3,TSTNULL          TEST HAVE NULLS?                             
         JZ    EXITY               YES, LEAVE ALONE                             
         LAY   RF,-1(RE)           RF = A(DEMO FLAG AND PRECISION)              
         MVC   BYTE,0(RF)                                                       
         NI    BYTE,X'0F'          ISOLATE DEMO PRECISION                       
         CLI   BYTE,X'02'          HAVE 2-DEC PRECISION VALUE?                  
         JNE   EXITY                NO, DO NOT ROUND TO 1-DEC                   
         SLL   R3,4                MOVE LN TO ZONE, SETUP FOR EX ON SRP         
         EXRL  R3,RNDAD1DP         ROUND & ADJUST TO 1 DEC PREC                 
         J     EXITY                                                            
*                                                                               
RD2DRPL3 LA    R3,L'RLEDRAW-1      INPUT DATA IS A PL3                          
*                                                                               
RR2DT005 L     RF,AREVREC                                                       
         MVC   BYTE,DRVKAM-DRVRECD(RF)                                          
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'02'          MEDIA R?                                     
         JNE   EXITY                NO, LEAVE ALONE                             
         CLI   DEMOTYPE,C'R'       HAVE RATING?                                 
         JE    RR2DT010             YES                                         
         CLI   DEMOTYPE,C'E'       OR EXTENDED?                                 
         JNE   EXITY                NO, LEAVE ALONE                             
*                                                                               
RR2DT010 L     RE,LP_AINP                                                       
         EXRL  R3,TSTNULL          TEST HAVE NULLS?                             
         JZ    EXITY               YES, LEAVE ALONE                             
         SLL   R3,4                MOVE LN TO ZONE, SETUP FOR EX ON SRP         
         EXRL  R3,RNDAD1DP         ROUND & ADJUST TO 1 DEC PREC                 
         EXRL  R3,ADJTO2DP         ADJUST BACK TO 2-DEC PREC                    
         J     EXITY                                                            
*                                                                               
TSTNULL  OC    0(0,RE),0(RE)       TEST NULLS                                   
RNDAD1DP SRP   0(0,RE),64-1,5      ROUND & ADJUST TO 1 DEC PREC                 
ADJTO2DP SRP   0(0,RE),1,0         ADJUST BACK TO 2-DEC PREC                    
*                                                                               
******   Revline NEW Estimate Spill Demo Categories and Values                  
ARYLNDL  LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLNLDELD,RLNLDELQ),ROWWIDTH=(V,RLNLDLEN)                  
                                                                                
Mkt      LKOUT C,1,RLNLDMKT,LBIN,ND=Y                                           
MktName  LKOUT C,5,RLNLDMKT,(R,EDTMKTN),ND=Y,PCVERSION=3.1.0.31                 
Array    LKOUT C,2,(A,ARYLNDL2)                                                 
         LKOUT E                                                                
                                                                                
ARYLNDL2 LKOUT A,(*,RLNLDDEM),ROWNAME=RLNLDEL,ROWWIDTH=RLNLDLN2Q,      *        
               NROWS=*                                                          
                                                                                
DemCd    LKOUT C,2,RLNLDCAT,(R,EDTDCD)                                          
PRout    LKOUT P,RLNLDRAW,RD2DRPL5                                              
DemVl    LKOUT C,3,RLNLDRAW,SPAK,ND=Y                                           
DemOV    LKOUT C,4,RLNLDFP,MB80                                                 
DemPrec  LKOUT C,8,RLNLDFP,(R,EDTLON),ND=Y,PCVERSION=4.7.0.37                   
NtDemRLk LKOUT C,6,(D,B#SAVED,NTDEMFLG),MB80,FILTROUT=TSTNTRD                   
NtDemSLk LKOUT C,7,(D,B#SAVED,NTDEMFLG),MB40,FILTROUT=TSTNTRD                   
         LKOUT E                                                                
                                                                                
******   Revline Estimate Spill Demo Categories and Values                      
ARYLDL   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLLDELD,RLLDELQ),ROWWIDTH=(V,RLLDLEN)                     
                                                                                
Mkt      LKOUT C,1,RLLDMKT,LBIN,ND=Y                                            
MktName  LKOUT C,5,RLLDMKT,(R,EDTMKTN),ND=Y,PCVERSION=3.1.0.31                  
Array    LKOUT C,2,(A,ARYLDL2)                                                  
         LKOUT E                                                                
                                                                                
ARYLDL2  LKOUT A,(*,RLLDDEMO),ROWNAME=RLLDEL,NROWS=*,ROWWIDTH=RLLDLN2Q          
                                                                                
DemCd    LKOUT C,2,RLLDCAT,(R,EDTDCD)                                           
PRout    LKOUT P,RLLDRAW,RD2DRPL3                                               
DemVl    LKOUT C,3,RLLDRAW,SPAK,ND=Y                                            
DemOV    LKOUT C,4,RLLDOVR,CHAR,ND=Y                                            
PRout    LKOUT P,,STDEFPRC                                                      
DemPrec  LKOUT C,8,(D,B#WORKD,BYTE),LBIN,ND=Y,PCVERSION=4.7.0.37                
NtDemRLk LKOUT C,6,(D,B#SAVED,NTDEMFLG),MB80,FILTROUT=TSTNTRD                   
NtDemSLk LKOUT C,7,(D,B#SAVED,NTDEMFLG),MB40,FILTROUT=TSTNTRD                   
         LKOUT E                                                                
                                                                                
******   RevLine Upgrade Formula                                                
ARYLUP   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLUELD,RLUELQ),ROWWIDTH=(V,RLULEN)                        
                                                                                
SttDate  LKOUT C,1,RLUSTTDT,CDAT,ND=Y                                           
endDate  LKOUT C,2,RLUENDDT,CDAT,ND=Y                                           
UpgForm  LKOUT C,3,RLUFORM,CHAR,LEN=V,ND=Y                                      
         LKOUT E                                                                
                                                                                
******   RevLine Spots                                                          
ARYLSP   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=B,                   *        
               ROWID=(RLSELD,RLSELQ),ROWWIDTH=(V,RLSLEN)                        
                                                                                
Date     LKOUT C,1,RLSDATE,CDAT                                                 
DtSeq    LKOUT C,2,RLSREF#,LBIN,ND=Y                                            
SpotNPW  LKOUT C,3,RLSNOWK,SPAK,ND=Y                                            
Prd      LKOUT C,4,RLSPRD,CHAR,ND=Y                                             
Pig      LKOUT C,5,RLSPRD2,CHAR,ND=Y                                            
PigLn    LKOUT C,6,RLSPRDLN,SPAK,ND=Y                                           
Cost     LKOUT C,7,RLSCOST,SPAK,ND=Y                                            
XSched   LKOUT C,8,RLSXSCHD,CHAR,ND=Y                                           
         LKOUT E                                                                
                                                                                
******   Revline Effective Rates                                                
ARYLRT   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLMELD,RLMELQ),ROWWIDTH=(V,RLMLEN)                        
Array    LKOUT C,255,(A,ARYLRT2)                                                
         LKOUT E                                                                
                                                                                
ARYLRT2  LKOUT A,(*,RLMEFFRT),ROWNAME=RLMEL,NROWS=*,ROWWIDTH=RLMLENQ2           
                                                                                
SttDt    LKOUT C,1,RLMSTTDT,CDAT,ND=Y                                           
EndDt    LKOUT C,2,RLMENDDT,CDAT,ND=Y                                           
Rate     LKOUT C,3,RLMRATE,SPAK,ND=Y                                            
         LKOUT E                                                                
                                                                                
******   RevLine Hiatus Periods                                                 
ARYLHI   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLHELD,RLHELQ),ROWWIDTH=(V,RLHLEN)                        
                                                                                
Array    LKOUT C,1,(A,ARYHIAT)     HIATUS PERIODS                               
         LKOUT E                                                                
                                                                                
ARYHIAT  LKOUT A,(*,RLHIAST),ROWNAME=RLHEL,NROWS=*,ROWWIDTH=RLHLEN2             
                                                                                
HiStDt   LKOUT C,1,RLHIAST,CDAT,ND=Y                                            
HiEnDt   LKOUT C,2,RLHIAEND,CDAT,ND=Y                                           
         LKOUT E                                                                
                                                                                
******   RevLine Orbits                                                         
ARYLNOR  LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLNOELD,RLNOELQ),ROWWIDTH=(V,RLNOLEN)                     
                                                                                
Array    LKOUT C,1,(A,ARYNORB)     NEW ORBITS                                   
         LKOUT C,6,RLNODEL,CHAR,ND=Y                                            
         LKOUT E                                                                
                                                                                
ARYNORB  LKOUT A,(*,RLNODAY),ROWNAME=RLNOEL,NROWS=*,ROWWIDTH=RLNOLN2Q           
                                                                                
OrbDy    LKOUT C,1,RLNODAY,LBIN                                                 
OrbSTm   LKOUT C,2,RLNOSTIM,SPAK                                                
OrbETm   LKOUT C,3,RLNOETIM,SPAK                                                
OrdDs    LKOUT C,4,RLNODESC,CHAR                                                
OrbDm    LKOUT C,5,RLNODEM,SPAK,ND=Y                                            
OrbPrec  LKOUT C,7,RLNOFP,(R,EDTLON),ND=Y,PCVERSION=4.7.0.37                    
         LKOUT E                                                                
                                                                                
******   RevLine Orbits                                                         
ARYLOR   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLOELD,RLOELQ),ROWWIDTH=(V,RLOLEN)                        
                                                                                
Array    LKOUT C,1,(A,ARYORBS)     ORBITS                                       
         LKOUT C,6,RLODEL,CHAR,ND=Y                                             
         LKOUT E                                                                
                                                                                
ARYORBS  LKOUT A,(*,RLODAY),ROWNAME=RLOEL,NROWS=*,ROWWIDTH=RLOLN2Q              
                                                                                
OrbDy    LKOUT C,1,RLODAY,LBIN                                                  
OrbSTm   LKOUT C,2,RLOSTIM,SPAK                                                 
OrbETm   LKOUT C,3,RLOETIM,SPAK                                                 
OrdDs    LKOUT C,4,RLODESC,CHAR                                                 
OrbDm    LKOUT C,5,RLODEM,SPAK,ND=Y                                             
PRout    LKOUT P,,SET2DECP                                                      
OrbPrec  LKOUT C,7,(D,B#WORKD,BYTE),LBIN,ND=Y,PCVERSION=4.7.0.37                
         LKOUT E                                                                
                                                                                
******   RevLine Packages                                                       
ARYLPK   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLPELD,RLPELQ),ROWWIDTH=(V,RLPLEN)                        
                                                                                
PkgTy    LKOUT C,1,RLPIND,UBIN,ND=Y         PACKAGE TYPE                        
Array    LKOUT C,2,(A,ARYPLIN)              PACKAGE LINES                       
         LKOUT E                                                                
                                                                                
ARYPLIN  LKOUT A,(*,RLPLINES),ROWNAME=RLPEL,NROWS=*,ROWWIDTH=RLPLN2Q            
                                                                                
PLines   LKOUT C,2,RLPLINES,LBIN,ND=Y                                           
         LKOUT E                                                                
                                                                                
******   Revline Notes                                                          
ARYLNT   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLNTELD,RLNTELQ),ROWWIDTH=(V,RLNTLEN)                     
                                                                                
COMNT    LKOUT C,1,RLNTTXT,CHAR,LEN=V,ND=Y                                      
         LKOUT E                                                                
                                                                                
******   RevLine Upgrade Formula                                                
ARYAAU   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLAUTAVD,RLAAELQ),ROWWIDTH=(V,RLAALEN)                    
                                                                                
AA-UUID  LKOUT C,1,RLAAUUID,CHAR,LEN=V,ND=Y                                     
         LKOUT E                                                                
                                                                                
******   Revline Transfer                                                       
ARYXFR   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLXELD,RLXELQ),ROWWIDTH=(V,RLXLEN)                        
                                                                                
BuyLine  LKOUT C,1,RLXBUYLN,LBIN,ND=Y                                           
SttDate  LKOUT C,2,RLXFLTST,BDAT,ND=Y                                           
EndDate  LKOUT C,3,RLXFLTEN,BDAT,ND=Y                                           
Rotation LKOUT C,4,RLXDAYS,LBIN,ND=Y                                            
Rate     LKOUT C,5,RLXRATE,LBIN,ND=Y                                            
ChgType  LKOUT C,6,RLXACT,CHAR,ND=Y                                             
         LKOUT E                                                                
                                                                                
******   RevLine Activity                                                       
ARYLAC   LKOUT A,(D,B#REVREC,DRVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(RLAELD,RLAELQ),ROWWIDTH=(V,RLALEN)                        
                                                                                
AddDt    LKOUT C,1,RLAADD,CDAT,ND=Y                                             
AddTime  LKOUT C,2,RLAATIME,HEXD,ND=Y                                           
AddPid   LKOUT C,3,RLAAPID,(R,EDTPID),ND=Y                                      
AddNM    LKOUT C,4,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
ChgDt    LKOUT C,5,RLACHG,CDAT,ND=Y                                             
ChgTime  LKOUT C,6,RLACTIME,HEXD,ND=Y                                           
ChgPid   LKOUT C,7,RLACPID,(R,EDTPID),ND=Y                                      
ChgNM    LKOUT C,8,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
LXfDt    LKOUT C,9,RLAXFR,CDAT,ND=Y                                             
LXfTim   LKOUT C,10,RLAXTIM,HEXD,ND=Y                                           
LXfPid   LKOUT C,11,RLAXPID,(R,EDTPID),ND=Y                                     
LXfNM    LKOUT C,12,(D,B#SAVED,PERNAME),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
         LKARY T                                                                
                                                                                
***  Next Revision/Work/Proposal Record                                         
NXTRWP   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTRWP40                                                         
                                                                                
         MVI   KEYTYP,DRVKSUBQ     REVISION DOWNLOAD                            
         TM    MAPI1,MAPIREV                                                    
         JO    NXTRWP10                                                         
         MVI   KEYTYP,DWKKSUBQ     WORK DOWNLOAD                                
         TM    MAPI1,MAPIWRK                                                    
         JZ    *+2                  UNKNOWN MAP                                 
                                                                                
NXTRWP10 ST    R0,LP_ADATA         SET A(DUMMY OUTPUT) IN CASE OF EXIT          
         XC    QREVSEQ,EFFS        COMPLEMENT SEQUENCE NUMBER                   
         TM    MAPI1,MAPIRESP                                                   
         JZ    NXTRWP50                                                         
NXTRWP30 ICM   RE,7,QADA           TEST DISK ADDRESS PASSED                     
         JZ    NXTRWP50                                                         
         CLI   LW_TYPE-LW_D(RE),LW_TSINQ                                        
         JNE   *+2                 NOT A SINGLE VALUE                           
         MVC   IODAOVER,LW_DATA1-LW_D(RE)                                       
         OC    IODAOVER,IODAOVER                                                
         JZ    *+2                                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOXSPFIL+B#REVREC'                     
         JNE   *+2                                                              
                                                                                
         L     R1,AREVREC                                                       
         MVC   IOKEY,0(R1)                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOXSPDIR+B#REVREC'                      
         JNE   *+2                                                              
                                                                                
         BRAS  RE,FLTREVK                                                       
         J     NXTRWP60                                                         
                                                                                
NXTRWP40 TM    MAPI1,MAPIRESP      RESPONSE DONWLOAD?                           
         JZ    NXTRWP50                                                         
         CLC   MAPNUM,=AL2(I#SDRSDR)                                            
         JE    NXTRWP45                                                         
         CLC   MAPNUM,=AL2(I#SDWSDR)                                            
         JNE   NOMORE                                                           
NXTRWP45 MVC   LP_ADATA,AREVREC                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOBSQ+IOXSPDIR+B#REVREC'                      
         JNE   NOMORE                                                           
         CLC   IOKEY(DRVKREVL-DRVKEY),IOKEYSAV                                  
         JNE   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOXSPFIL+B#REVREC'                     
         JE    NXTRWP60                                                         
         DC    H'0'                                                             
                                                                                
NXTRWP50 GOTOR (#NXTREC,ANXTREC),DMCB,REVKEYT,('B#REVREC',0),          *        
               ('$NXTRXSP',SAVED),AFLTREVK,0                                    
NXTRWP60 BRAS  RE,GETCSUM                                                       
         MVC   RECDA,IODA          SET DISK ADDRESS                             
         J     EXITY                                                            
                                                                                
GETCSUM  NTR1  LABEL=NO                                                         
         L     RE,IOADDR           RE=A(RECORD))                                
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,DCHKSUM                                                    
         XIT1                                                                   
                                                                                
**********************************************************************          
* Edit/Translate book type                                                      
**********************************************************************          
TRNSBKT  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0                                                          
         JE    EXITN                                                            
         MVI   LP_OLEN+3,7                                                      
         MVC   0(7,R4),=CL7'~#CLEAR'                                            
         CLI   0(R2),X'FF'                                                      
         JE    EXITY                                                            
                                                                                
         GOTOR (#TRNSBT,ATRNSBT),DMCB,(R2),1,(R4),12                            
         MVI   LP_OLEN+3,2                                                      
         J     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* Edit Station                                                                  
**********************************************************************          
                                                                                
EDTCBL   LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),C'0'          Cable syscode?                               
         JL    EDTCBLBR            No, broadcast station                        
         CLI   4(R2),C'/'          Have / in 5 character?                       
         JE    EDTCBL20                                                         
*                                                                               
         SR    RF,RF               Clear RF                                     
         LA    RE,L'RLDSTA(R2)     Check NNNN/CCC Cable Sta format              
EDTCBL10 BCTR  RE,0                                                             
         CR    RE,R2               Just a network?                              
         JNH   EDTCBL20             yes,                                        
         LTR   RF,RF               Did we find network code?                    
         JNZ   EDTCBL15             Yes                                         
         CLI   0(RE),C'/'           No, did we find '/'?                        
         JNE   EDTCBL10              No, keep going                             
         LA    RF,1(RE)              Yes, set RF = A(Network code)              
         J     EDTCBL10                                                         
EDTCBL15 CLI   0(RE),C' '          HAVE space, remove it                        
         JE    EDTCBL10                                                         
         CLI   0(RE),C'0'          Have numeric?                                
         JL    *+2                                                              
         CLI   0(RE),C'9'                                                       
         JH    *+2                                                              
         MVI   1(RE),C'/'          ok, lets move in '/'                         
         MVC   2(L'STAPQNET,RE),0(RF)       followed by network                 
         XC    8(2,R2),8(R2)       and clear last 2 chars of garbage            
*                                                                               
EDTCBL20 XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'P'        If we pack and then unpk, we                 
         MVC   STAPAGY,LP_AGY       will always get the newest network          
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVI   STAPMED,C'T'        Cable is TV only                             
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA,0(R2)      Copy the syscode                             
         MVI   STAPQSTA+4,C'T'     Fill in the band                             
         MVC   STAPQNET,5(R2)      Cable network code could be old              
*                                **DUH, SBTK saves network in**                 
*                                **lowercase, using the copy/paste**            
         OC    STAPQNET,SPACES   **Uppercase it, just in case**                 
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0           We should never get an error here            
         JE    EDTCBL30                                                         
         MVC   5(3,R2),=C'???'      but in case we do, send ???                 
         J     EDTCBLBR                                                         
*                                                                               
EDTCBL30 MVI   STAPACT,C'U'        Now for the unpk part                        
         XC    STAPQNET,STAPQNET                                                
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0           We should never get an error here            
         JNE   *+2                 But in case we do, die                       
*                                                                               
         MVC   5(3,R2),STAPQNET    SHOULD BE THE NEW NETWORK, IF ANY            
*                                                                               
EDTCBLBR LR    R0,R3               BROADCAST STATION                            
         BCTR  R3,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    R3,0(RF)                                                         
                                                                                
EDTCBLX  ST    R0,LP_OLEN                                                       
         J     EXITY                                                            
                                                                                
**********************************************************************          
* Edit Rate Type                                                                
**********************************************************************          
                                                                                
EDTRTYP  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),C' '          ANYTHING TO SEND?                            
         JNH   EXITY                                                            
         LHI   R0,10                                                            
         MVC   0(10,R4),=CL10'NORATETYPE'                                       
         CLI   0(R2),X'FF'         IS IT CLEAR?                                 
         JE    EDTRTYPX            YES, EXIT                                    
         LR    R0,R3                                                            
         BCTR  R3,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    R3,0(RF)                                                         
                                                                                
EDTRTYPX ST    R0,LP_OLEN                                                       
         J     EXITY                                                            
                                                                                
**********************************************************************          
* Edit Character string                                                         
**********************************************************************          
                                                                                
EDTCHAR  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),C' '          ANYTHING TO SEND?                            
         JNH   EXITY                                                            
         LHI   R0,7                                                             
         MVC   0(7,R4),=CL7'~#CLEAR'                                            
         CLI   0(R2),X'FF'         IS IT CLEAR?                                 
         JE    EDTCHARX            YES, EXIT                                    
         LR    R0,R3                                                            
         BCTR  R3,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R4),0(R2)                                                    
         EX    R3,0(RF)                                                         
                                                                                
EDTCHARX ST    R0,LP_OLEN                                                       
         J     EXITY                                                            
                                                                                
**********************************************************************          
* Edit Packed field                                                             
**********************************************************************          
EDTSPAK  LM    R2,R4,LP_AINP                                                    
         LHI   RE,7                                                             
         MVC   0(7,R4),=CL7'~#CLEAR'                                            
         CLI   0(R2),X'FF'         IS IT CLEAR?                                 
         JE    EDTSPAKX            YES, EXIT                                    
         BCTR  R3,0                                                             
         LARL  RF,EDTSPNUL                                                      
         EX    R3,0(RF)            TEST NULLS                                   
         JNE   EDSPK010                                                         
         SR    RE,RE                                                            
         J     EDTSPAKX                                                         
                                                                                
EDSPK010 LARL  RF,EDTSPTST                                                      
         SLL   R3,4                                                             
         EX    R3,0(RF)            Test for valid packed field                  
         JNE   *+2                 Invalid packed field                         
                                                                                
         MVI   BYTE,0                                                           
         SRL   R3,4                                                             
         LARL  RF,EDTSPSET                                                      
         EX    R3,0(RF)                                                         
         CP    DUB,PZERO           Test for zero PACK VALUE                     
         JNL   EDSPK030                                                         
         MVI   BYTE,C'-'           SET FLOAT FLAG IN BYTE                       
                                                                                
EDSPK030 OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK(14),DUB                                                     
         LA    R1,WORK             POINT TO FIRST DIGIT                         
         LA    RE,14-1                                                          
EDSPK040 CLI   0(R1),C'0'          LOOK FOR FIRST SIGNIFICANT DIGIT             
         JNE   EDSPK050                                                         
         AHI   R1,1                                                             
         JCT   RE,EDSPK040                                                      
EDSPK050 SR    R0,R0               SET NO BYTE                                  
         CLI   BYTE,0              HAVE FLOAT?                                  
         JE    EDSPK060                                                         
         MVC   0(L'BYTE,R4),BYTE                                                
         AHI   R4,L'BYTE                                                        
         MVI   BYTE,0              RESET BYTE WHEN USED                         
         LA    R0,L'BYTE           R0=LENGTH OF BYTE                            
EDSPK060 BASR  RF,0                                                             
         MVC   0(0,R4),0(R1)       MOVE VALUE TO OUTPUT STRING                  
         EX    RE,0(RF)                                                         
         AHI   RE,1                ADD BACK ONE FOR EX ABOVE                    
         AR    RE,R0               ADD LENGTH OF BYTE IF ANY                    
EDTSPAKX ST    RE,LP_OLEN          SET SIGNIFICANT DATA LENGTH                  
         LTR   RE,RE               SET CC=EQUAL IF NO DATA                      
         J     EXIT                                                             
                                                                                
EDTSPNUL CLC   0(0,R2),=XL10'00'                                                
EDTSPTST TP    0(0,R2)             Test for valid packed field                  
EDTSPSET ZAP   DUB,0(0,R2)         Set packed value                             
**********************************************************************          
* Edit adjacency code                                                           
**********************************************************************          
                                                                                
EDTADJ   LM    R2,R4,LP_AINP                                                    
         LHI   R0,7                                                             
         MVC   0(7,R4),=CL7'~#CLEAR'                                            
         CLI   0(R2),X'FF'         IS IT CLEAR?                                 
         JE    EDTADJX             YES, EXIT                                    
         MVC   0(1,R4),0(R2)                                                    
         SR    R0,R0                                                            
         CLI   0(R2),X'00'                                                      
         JE    EDTADJX                                                          
         LHI   R0,L'RSDADJ                                                      
         CLI   0(R2),X'99'         IS IT 2-CHAR NUMERIC ADJ CODE?               
         JH    EDTADJX             NO, EXIT                                     
                                                                                
         ICM   RF,8,0(R2)                                                       
         SLDL  RE,4                                                             
         SLL   RE,4                                                             
         SLDL  RE,4                                                             
         STCM  RE,3,0(R4)                                                       
         OI    0(R4),C'0'                                                       
         OI    1(R4),C'0'                                                       
         LHI   R0,L'RSDADJ+1                                                    
EDTADJX  ST    R0,LP_OLEN                                                       
         J     EXITY                                                            
                                                                                
**********************************************************************          
* Edit person ID and extract person name from PID record             *          
**********************************************************************          
                                                                                
         USING EPWORKD,RC                                                       
         USING SA0REC,EPIO                                                      
EDTPID   MVC   EPIOSAVE,IOVALS     SAVE CURRENT I/O VALUES                      
         XC    PERNAME,PERNAME                                                  
         LHI   R0,L'SAPALPID                                                    
         ST    R0,LP_OLEN                                                       
         L     R1,LP_AINP          R1=A(PASSWORD NUMBER)                        
         OC    0(L'SA0KNUM,R1),0(R1)                                            
         JZ    EDTPIDN                                                          
                                                                                
         L     RF,LP_AOUT                                                       
         MVC   0(L'SAPALPID,RF),PPERPID                                         
         MVC   PERNAME,PPERNAME                                                 
EDTPID10 CLC   0(L'SA0KNUM,R1),PPERNUM                                          
         JE    EDTPIDY                                                          
         MVC   PPERNUM,0(R1)                                                    
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
         SR    R0,R0                                                            
EDTPID30 CLI   SAPALEL,EOR         TEST END OF RECORD                           
         JE    EDTPIDN                                                          
         CLI   SAPALEL,SAPALELQ    TEST NEW SECURITY PERSON ELEMENT             
         JE    *+14                                                             
         IC    R0,SAPALLN          BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         J     EDTPID30                                                         
         L     RF,LP_AOUT                                                       
         MVC   0(L'SAPALPID,RF),SAPALPID                                        
         MVC   PPERPID,SAPALPID                                                 
                                                                                
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
                                                                                
         XC    PERNAME,PERNAME                                                  
         LA    R1,SAPEDATA         LOCATE PERSONNEL DETAILS ELEMENT             
         SR    R0,R0                                                            
         USING SANAMD,R1                                                        
EDTPID50 CLI   SANAMEL,EOR                                                      
         JE    EDTPIDY                                                          
         CLI   SANAMEL,SANAMELQ                                                 
         JE    *+14                                                             
         IC    R0,SANAMLN                                                       
         AR    R1,R0                                                            
         J     EDTPID50                                                         
                                                                                
         MVC   EPNAMIND,SANAMIND                                                
         LA    R1,SANAMES                                                       
         LA    R2,PERNAME                                                       
         SR    RF,RF                                                            
         USING SANAMES,R1                                                       
                                                                                
         TM    EPNAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         JZ    EDTPID70                                                         
         IC    RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R2),SANAME                                                   
         LA    R2,1(R2,RF)                                                      
         MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
         LA    R1,2(RF,R1)                                                      
                                                                                
EDTPID70 TM    EPNAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT                     
         JZ    EDTPID90                                                         
         IC    RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R2),SANAME                                                   
         LA    R2,1(R2,RF)                                                      
         MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
         LA    R1,2(RF,R1)                                                      
                                                                                
EDTPID90 TM    EPNAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         JZ    EDTPIDY                                                          
         IC    RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     EDTPIDY                                                          
         MVC   0(0,R2),SANAME                                                   
         DROP  R1                                                               
                                                                                
EDTPIDY  MVC   PPERNAME,PERNAME                                                 
         MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     EXITY                                                            
                                                                                
EDTPIDN  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     XCOLEN                                                           
         DROP  RC                                                               
                                                                                
EPWORKD  DSECT ,                   ** EDTPID LOCAL WORKING STORAGE **           
EPIOSAVE DS    XL(IOVALL)          SAVED I/O VALUES                             
EPNAMIND DS    XL(L'SANAMIND)      NAME INDICATORS                              
EPIO     DS    XL1000              I/O AREA                                     
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
**********************************************************************          
* Edit Market Name                                                              
**********************************************************************          
                                                                                
         USING EMWORKD,RC                                                       
EDTMKTN  LM    R2,R4,LP_AINP                                                    
         LHI   R0,L'MKTNAME                                                     
         ST    R0,LP_OLEN                                                       
         MVC   EMIOVALS,IOVALS                                                  
                                                                                
         SR    R0,R0               CONVERT BINARY MARKET TO EBCDIC              
         ICM   R0,3,0(R2)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
                                                                                
         LA    R1,IOKEY                                                         
         USING MKTREC,R1           READ MARKET RECORD                           
         XC    IOKEY,IOKEY                                                      
         MVI   MKTKTYPE,MKTKTYPQ                                                
         L     RF,AREVREC                                                       
         MVC   BYTE,DRVKAM-DRVKEY(RF)                                           
         NI    BYTE,X'0F'                                                       
         MVI   MKTKMED,C'T'                                                     
         CLI   BYTE,X'01'          TV?                                          
         JE    EDTMKT10                                                         
         MVI   MKTKMED,C'R'                                                     
         CLI   BYTE,X'02'          RADIO?                                       
         JE    EDTMKT10                                                         
         MVI   MKTKMED,C'X'                                                     
         CLI   BYTE,X'04'          NET RADIO?                                   
         JNE   *+2                  UNKNOWN MEDIA                               
*                                                                               
EDTMKT10 UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,AGENCY                                                   
         MVC   MKTKFILL,EZEROS                                                  
         LA    R1,EMIO                                                          
         ST    R1,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL'                                
         L     R1,IOADDR                                                        
         JE    EDTMKT20                                                         
         MVI   MKTNAME,C'?'                                                     
         MVC   MKTNAME+1(L'MKTNAME-1),MKTNAME                                   
         XC    MKTALF,MKTALF                                                    
         MVC   MKTLPMDT,MKTLPMDT                                                
         XC    MKTCDEM,MKTCDEM                                                  
                                                                                
EDTMKT20 MVC   0(L'MKTNAME,R4),MKTNAME                                          
                                                                                
         MVC   IOVALS(IOVALL),EMIOVALS                                          
         J     EXITY                                                            
         DROP  R1,RC                                                            
                                                                                
EMWORKD  DSECT                    ** EDTMKTN LOCAL WORKING STORAGE **           
EMIOVALS DS    XL(IOVALL)                                                       
EMIO     DS    XL1000                                                           
EMWORKL  EQU   *-EMWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT DEMO CODE FROM BUY DEMO ELEMENT                                          
***********************************************************************         
EDTDCD   MVI   NTDEMFLG,0          RESET FLAG                                   
         MVI   DEMOTYPE,0                                                       
         L     R2,LP_AINP          R2 = A(INPUT 3-BT DEMO CODE)                 
         OC    0(3,R2),0(R2)       HAVE VALUE?                                  
         JZ    EXITN                NO                                          
         LLC   RF,0(R2)                                                         
         ICM   RF,B'0010',2(R2)    BYTE1&3 ZERO, NON-TRADITIONAL?               
         LTR   RF,RF               HAVE NON-TRADITIONAL DEMO CAT?               
         JNZ   EDTDCD10             NO                                          
*                                                                               
         OC    LP_VRSN,LP_VRSN     ONLY IF NO VERSION                           
         JZ    GETNTDEM                                                         
         CLC   LP_VRSN,V460050     OR VERSION 4.6.0.50 AND HIGHER               
         JNL   GETNTDEM             GET THE NON-TRAD NAME                       
         L     R2,LP_AINP          R1 = A(SCDDEMO)                              
         LLC   RF,1(R2)             YES, GET INDEX AND                          
         MVI   1(R2),X'21'         TRANSFORM TO USER DEF                        
         LA    R3,10(RF)           AND START AT 11                              
         STC   R3,2(R2)                                                         
*                                                                               
EDTDCD10 GOTOR (#EDTDCD,AEDTDCD),DMCB,LP_AINP,,LP_AOUT                          
         MVC   LP_OLEN,DMCB+4      SET OUTPUT LENGTH                            
         MVC   DEMOTYPE,1(R2)                                                   
         J     EXIT                                                             
*                                                                               
GETNTDEM DS    0H                                                               
         L     R3,AREVREC          GET THE SHORT NAME AND FLAG                  
         LA    R3,DRVEL-DRVRECD(R3)                                             
GETNTDM2 CLI   0(R3),EOR           EOR?                                         
         JE    GETNTDMN             YES                                         
         CLI   0(R3),RSNDELQ       X'38' SHEET NON-TRAD DEMO LIST               
         JE    GETNTDMS              YES                                        
         CLI   0(R3),RLNDELQ       X'24' LINE NON-TRAD DEMO LIST                
         JE    GETNTDML              YES                                        
         LLC   RF,1(R3)            BUMP TO NEXT                                 
         LA    R3,0(RF,R3)                                                      
         J     GETNTDM2                                                         
*                                                                               
GETNTDMS L     R2,LP_AINP                R1 = A(SCDDEMO)                        
         LLC   RF,1(R2)                  GET INDEX                              
         BCTR  RF,0                      DEC BY 1                               
         MHI   RF,RSNDLEN2               MULT BY L'ENTRY                        
         LA    RF,RSNDLENQ(RF)           GET OFFSET                             
         CLM   RF,1,1(R3)                BEYOND ELEM LIST?                      
         JNL   GETNTDMN                   YES, DON'T SEND ANYTHING              
         LA    R3,0(R3,RF)               ADD OFFSET, R3=A(ENTRY)                
         J     GETNTDM9                                                         
*                                                                               
GETNTDML L     R2,LP_AINP                R1 = A(SCDDEMO)                        
         LLC   RF,1(R2)                  GET INDEX                              
         BCTR  RF,0                      DEC BY 1                               
         MHI   RF,RLNDLEN2               MULT BY L'ENTRY                        
         LA    RF,RLNDLENQ(RF)           GET OFFSET                             
         CLM   RF,1,1(R3)                BEYOND ELEM LIST?                      
         JNL   GETNTDMN                   YES, DON'T SEND ANYTHING              
         LA    R3,0(R3,RF)               ADD OFFSET, R3=A(ENTRY)                
         MVC   NTDEMFLG,L'RLNDDEMO(R3)                                          
*&&DO                                                                           
         MVI   NTDEMRLK,C'N'                                                    
         TM    L'RLNDDEMO(R3),X'80'      DEMO DOES NOT REQ LKUP?                
         JZ    *+8                                                              
         MVI   NTDEMRLK,C'Y'                                                    
         MVI   NTDEMSLK,C'N'                                                    
         TM    L'RLNDDEMO(R3),X'40'      SPILL DOES NOT REQ LKUP?               
         JZ    *+8                                                              
         MVI   NTDEMSLK,C'Y'                                                    
*&&                                                                             
GETNTDM9 L     RF,LP_AOUT                UPDATE OUTPUT TO                       
         MVC   0(L'RLNDDEMO,RF),0(R3)    BE THE SHORT FORM NAME                 
         LA    RF,L'RLNDDEMO             GET LENTH OF OUTPUT                    
         ST    RF,LP_OLEN                AND SET OUTPUT LENGTH                  
         MVC   DEMOTYPE,0(R3)                                                   
GETNTDMY J     EXITY                                                            
GETNTDMN J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR (online) or BUFFERIN (offline)                    *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows:-                   *         
*                                                                     *         
*        P1/0   - Buffer number                                       *         
*          /1-3 - TSAR action code                                    *         
*        P2/0   - Key length (initialization call)                    *         
*          /2-3 - Record length (initialization call)                 *         
***********************************************************************         
                                                                                
BUFFER   NTR1  LABEL=NO                                                         
         LR    R2,R1               R2=A(PARAMETER LIST)                         
*&&UK                                                                           
         LA    RE,ALLBLK                                                        
         LA    RF,ALLBUF                                                        
         LA    R1,ALLREC                                                        
         CLI   0(R2),ALLBUFQ       TEST ALLOCATION BUFFER                       
         JE    BUFFER02                                                         
         LA    RE,XPLBLK                                                        
         LA    RF,XPLBUF                                                        
         LA    R1,XPLREC                                                        
         CLI   0(R2),XPLBUFQ       TEST CUTIN BUFFER                            
         JE    BUFFER02                                                         
         LA    RE,GPWBLK                                                        
         LA    RF,GPWBUF                                                        
         LA    R1,GPWREC                                                        
         CLI   0(R2),GPWBUFQ       TEST GVP REPORT BUFFER                       
         JNE   *+2                  NO                                          
*&&                                                                             
                                                                                
BUFFER02 ST    R1,BUFFREC          SET A(INPUT RECORD)                          
         MVC   TSARKSAV,0(R1)      SAVE CURRENT RECORD KEY                      
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   BUFFER06             YES                                         
                                                                                
         LTR   R3,RE                                                            
         JZ    *+2                                                              
         USING TSARD,R3            R3=A(TSAR CONTROL BLOCK)                     
                                                                                
         MVC   TSACTN,3(R2)        SET ACTION CODE                              
         MVC   TSAREC,BUFFREC      SET A(RECORD)                                
                                                                                
         CLI   TSACTN,TSAINI       TEST INITIALIZATION CALL                     
         JNE   BUFFER04                                                         
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSKEYL,4(R2)                                                     
         MVC   TSRECL,6(R2)                                                     
         MVI   TSPAGN,8                                                         
         OI    TSINDS,TSINODSK+TSIXTTWA                                         
         CLI   0(R2),1             TEST BUFFER 1                                
         JE    BUFFER04                                                         
         OI    TSIND2,TSI2BUF2     SET TSAR BUFFER 2 FLAG                       
                                                                                
BUFFER04 GOTOR LP_ATSAR,TSARD                                                   
         MVC   BUFFRET,TSERRS                                                   
         TM    BUFFRET,TSEINIF     TEST INITIALIZATION FAILURE                  
         JZ    BUFFER12                                                         
         DC    H'0'                YES - TAKE A HIT                             
         DROP  R3                                                               
                                                                                
BUFFER06 LR    R3,RF                                                            
         USING BUFFD,R3            R3=A(BUFFERIN CONTROL BLOCK)                 
         LHI   R0,BUFFAINI         CONVERT TSAR ACTION TO BUFFERIN              
         CLI   3(R2),TSAINI                                                     
         JE    BUFFER08                                                         
         LHI   R0,BUFFAPUT                                                      
         CLI   3(R2),TSAADD                                                     
         JE    BUFFER08                                                         
         CLI   3(R2),TSAWRT                                                     
         JE    BUFFER08                                                         
         CLI   3(R2),TSAPUT                                                     
         JE    BUFFER08                                                         
         LHI   R0,BUFFASEQ                                                      
         CLI   3(R2),TSANXT                                                     
         JE    BUFFER08                                                         
         LHI   R0,BUFFARDH                                                      
         CLI   3(R2),TSARDH                                                     
         JNE   *+2                 UNKNOWN ACTION                               
                                                                                
BUFFER08 CHI   R0,BUFFAINI         TEST INITIALIZATION CALL                     
         JNE   BUFFER10                                                         
         SR    RE,RE                                                            
         IC    RE,4(R2)            RE=KEY LENGTH                                
         LH    RF,6(R2)            RF=RECORD LENGTH                             
         SR    RF,RE                                                            
         STCM  RE,3,BUFFLKEY       SET KEY LENGTH                               
         STCM  RF,3,BUFFLCOM       SET COMMENT LENGTH                           
                                                                                
BUFFER10 GOTOR ABUFFRIN,DMCB,((R0),BUFFD),BUFFREC,ACOMFACS                      
         MVC   BUFFRET,BUFFERRS-BUFFPARM(R1)                                    
         CHI   R0,BUFFARDH         EMULATE TSAR NOT FOUND ON READ HIGH          
         JNE   BUFFER12                                                         
         L     RF,BUFFREC                                                       
         LH    R1,BUFFLKEY                                                      
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         JE    BUFFER12                                                         
         CLC   TSARKSAV(0),0(RF)                                                
         OI    BUFFRET,BUFFERNF                                                 
                                                                                
BUFFER12 CLI   BUFFRET,0           SET CONDITION CODE FOR CALLER                
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         J     EXITY                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
                                                                                
F        USING DRVKEY,IOKEY                                                     
FLTREVK  TM    F.DRVKST0,DRVKSDEL  X'80' DELETED?                               
         JNZ   RTRNNO                                                           
         MVI   HISTFLG,0                                                        
         TM    F.DRVKST1,DRVKSXFR  X'40' TRANSFERRED?                           
         JZ    *+8                                                              
         MVI   HISTFLG,C'T'                                                     
         TM    F.DRVKST1,DRVKSRPL  X'80' REPLACED?                              
         JZ    RTRNYES                                                          
         MVI   HISTFLG,C'H'                                                     
         CLI   QDLTYPE,C'H'        DO THEY WANT REPLACED/HISTORY?               
         J     RTRNCC                                                           
RTRNYES  CR    RE,RE               APPLIED TO AT LEAST ONE FILTER               
         BR    RE                                                               
RTRNNO   LTR   RE,RE               DOES NOT MATCH ANY FILTER                    
RTRNCC   BR    RE                                                               
         DROP  F                                                                
                                                                                
EXITN    LHI   RE,0                SET CONDITION CODE TO NOT EQUAL              
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CONDITION CODE TO EQUAL                  
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   GENERAL EXIT POINT                           
         EJECT                                                                  
EOR      EQU   0                                                                
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
SDREVD#  DC    AL2(I#SDREVD)       0300-REVISION DOWNLOAD                       
         DC    AL1(MAPIREV)                                                     
         DC    AL1(0)                                                           
                                                                                
SDREVS#  DC    AL2(I#SDRSDR)       03FF-REVSHEET DOWNLOAD RESPONSE              
         DC    AL1(MAPIREV+MAPIRESP)                                            
         DC    AL1(0)                                                           
                                                                                
SDREVL#  DC    AL2(I#SDRLDR)       03FE-REVLINE DOWNLOAD RESPONSE               
         DC    AL1(MAPIREV+MAPIRESP)                                            
         DC    AL1(0)                                                           
                                                                                
SDWRKD#  DC    AL2(I#SDWRKD)       0320-WORK DOWNLOAD                           
         DC    AL1(MAPIWRK)                                                     
         DC    AL1(0)                                                           
                                                                                
SDWRKS#  DC    AL2(I#SDWSDR)       03FC-WORKSHEET DOWNLOAD RESPONSE             
         DC    AL1(MAPIWRK+MAPIRESP)                                            
         DC    AL1(0)                                                           
                                                                                
SDAVLL#  DC    AL2(I#SDALDR)       03FD-AVAILLINE DOWNLOAD RESPONSE             
         DC    AL1(MAPIWRK+MAPIRESP)                                            
         DC    AL1(0)                                                           
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
EZEROS   DC    C'00000000'                                                      
PZERO    DC    P'0'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
SPTDIR   DC    C'SPTDIR '                                                       
SPTFIL   DC    C'SPTFILE'                                                       
XSPDIR   DC    C'XSPDIR '                                                       
XSPFIL   DC    C'XSPFIL '                                                       
         EJECT                                                                  
LVALUES  DS    0F                  ** LITERALS MOVED TO SAVED **                
         DC    A(FLTREVK)                                                       
                                                                                
         DC    AL1(4,6,0,50)       ASSOCIATED WITH LABEL V460050                
         DC    AL1(4,7,0,37)       ASSOCIATED WITH LABEL V470037                
         EJECT                                                                  
                                                                                
REVKEYT  LKKEY H,DRVKEY,SAVED      ** REVISION SHEET KEY DRIVER **              
         LKKEY LIT,DRVKTYP,DRVKTYPQ                                             
         LKKEY SIN,DRVKSUB,KEYTYP                                               
         LKKEY LIT,DRVKSPR1,0                                                   
         LKKEY WMP,DRVKAM,QAMED                                                 
         LKKEY WMP,DRVKCLT,QACLT                                                
         LKKEY WMP,DRVKPRD,QAPRD                                                
         LKKEY WMP,DRVKEST,QAEST                                                
         LKKEY WMP,DRVKMKT,QAMKT                                                
         LKKEY SIN,DRVKREVS,QREVSEQ                                             
         LKKEY ALL,DRVKREVL                                                     
         LKKEY ALL,DRVKFLG                                                      
         LKKEY LIT,DRVKSPR3,0                                                   
         LKKEY E                                                                
                                                                                
REVSLIT  DC    C'Revision sequence number'                                      
REVSHIS  DC    C'Download History?'                                             
         EJECT                                                                  
SAVED    DSECT ,                   ** DSECT TO COVER SAVED STORAGE **           
                                                                                
WVALUES  DS    0X                  ** LITERAL VALUES **                         
                                                                                
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AFLTREVK DS    A                   A(REV KEY FILTER ROUTINE)                    
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
                                                                                
V460050  DS    XL4                 PC VERSION 4.6.0.50                          
V470037  DS    XL4                 PC VERSION 4.7.0.37                          
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
                                                                                
MAPNUM   DS    XL(L'LP_QMAPN)      RECORD MAP NUMBER                            
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
                                                                                
MAPI1    DS    X                   ** MAP INDICATOR BYTE1 1 **                  
MAPIREV  EQU   X'80'               DOWNLOAD REVISION                            
MAPIWRK  EQU   X'40'               DOWNLOAD WORK                                
MAPIRESP EQU   X'01'               DOWNLOAD RESPONSE                            
                                                                                
MAPI2    DS    X                   ** MAP INDICATOR BYTE1 2 **                  
                                                                                
RUNI     DS    0XL(L'RUNI1)                                                     
                                                                                
RUNI1    DS    X                   ** RUN INDICATOR BYTE1 1 **                  
*RUN#CUTI EQU   RUNI1                                                           
*RUNSCUTI EQU   X'80'               BUILD CUTIN RECORDS                         
                                                                                
BUFFREC  DS    A                   BUFFER S/R A(RECORD)                         
BUFFRET  DS    X                   BUFFER S/R RETURN VALUES                     
                                                                                
         DS    0F                                                               
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
                                                                                
QMEDIND  DS    X                   MEDIA                                        
QAMED    DS    AL3                                                              
                                                                                
QCLTIND  DS    X                   CLIENT                                       
QACLT    DS    AL3                                                              
                                                                                
QPRDIND  DS    X                   PRODUCT                                      
QAPRD    DS    AL3                                                              
                                                                                
QESTIND  DS    X                   ESTIMATE                                     
QAEST    DS    AL3                                                              
                                                                                
RDAIND   DS    0X                  D/A                                          
QMKTIND  DS    X                   MARKET                                       
QADA     DS    0AL3                                                             
QAMKT    DS    AL3                                                              
                                                                                
RECDA    DS    XL(L'IODA)          DISK ADDRESS                                 
QREVSEQ  DS    XL(L'DRVKREVL)      REVISION SEQUENCE NUMBER                     
QDLTYPE  DS    C                                                                
                                                                                
PCKEY    DS    CL20                PC KEY                                       
ERRTEXT  DS    CL(L'WORK)                                                       
ERRNUM   DS    XL2                                                              
                                                                                
         ORG   QVALUES+L'QVALUES                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
         DS    0F                                                               
DVALUES  DS    0X                  ** DERIVED VALUES **                         
                                                                                
DNROWS   DS    H                                                                
DCHKSUM  DS    F                                                                
                                                                                
DVALUEL  EQU   *-DVALUES                                                        
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
AGENCY   DS    CL(L'LP_AGY)        AGENCY CODE                                  
                                                                                
PPERNUM  DS    XL(L'SA0KNUM)       PREV PERSON AUTH NUMBER                      
PPERPID  DS    CL(L'SAPALPID)      PREV 8 CHARACTER PID                         
PPERNAME DS    CL62                PREV PERSON NAME                             
PERNAME  DS    CL62                PERSON NAME                                  
                                                                                
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
                                                                                
OVALUES  DS    0D                  ** OUTPUT VALUES **                          
         ORG   OVALUES                                                          
KEYTYP   DS    X                                                                
HISTFLG  DS    C                                                                
OSEQNO   DS    CL(L'DRVKREVS)                                                   
DEMOTYPE DS    C                   DEMO RATING TYPE                             
NTDEMFLG DS    C                                                                
*NTDEMRLK DS    C                                                               
*NTDEMSLK DS    C                                                               
         EJECT                                                                  
       ++INCLUDE SPLNKWRK                                                       
*                                                                               
B#B3IO1  EQU   3                   IO1                                          
B#AGYREC EQU   3                   - AGENCY RECORD                              
B#B4IO2  EQU   4                   IO2 -                                        
B#CLTREC EQU   3                   - CLIENT RECORD                              
B#B5IO3  EQU   5                   IO3 -                                        
B#B6IO4  EQU   6                   IO4 -                                        
B#B7IO5  EQU   7                   IO5 -                                        
B#REVREC EQU   7                   - REVISION RECORDS                           
AREVREC  EQU   LP_BLKS+((B#REVREC-1)*L'LP_BLKS)                                 
B#B8IO6  EQU   8                   IO6 -                                        
B#B9IO7  EQU   9                   IO7 -                                        
B#B10IO8 EQU   10                  IO8 -                                        
B#SVRDEF EQU   12                  SERVER SPLNK19                               
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   14                  LP_D                                         
                                                                                
WORKD    DSECT ,                   ** REDEFINE OVERWORK **                      
         ORG   OVERWORK                                                         
*                                                                               
GPWBLK   DS    0X                  TSAR BLOCK FOR GVP WEEKLY BUFFER             
*ALLBLK   DS    XL(TSPNEWL)         TSAR BLOCK FOR ALLOCATION BUFFER            
*XPLBLK   DS    XL(TSPNEWL)         TSAR BLOCK FOR EXPLODED BUY BUFFER          
                                                                                
TSARKSAV DS    XL32                TSAR RECORD KEY SAVE AREA                    
                                                                                
SAVERE   DS    A                   SAVE RE                                      
SAVERF   DS    A                   SAVE RF                                      
SAVER0   DS    A                   SAVE R0                                      
SAVER1   DS    A                   SAVE R1                                      
SAVER2   DS    A                   SAVE R2                                      
SAVER3   DS    A                   SAVE R3                                      
SAVER4   DS    A                   SAVE R4                                      
                                                                                
SVIOVALS DS    XL(IOVALL)          SAVED I/O VALUES                             
                                                                                
         DS    0F                                                               
GDBLOCK  DS    XL(GDEMBLKL)        GETDEM INTERFACE BLOCK                       
         ORG                                                                    
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE SPGENDREV                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPSTABLK                                                       
STARECD  DSECT ,                                                                
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPLNK19   11/21/19'                                      
         END                                                                    
