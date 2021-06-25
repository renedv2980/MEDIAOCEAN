*          DATA SET SPLNK11    AT LEVEL 003 AS OF 09/12/17                      
*PHASE T21E11A                                                                  
SPLNK11  TITLE '- Canadian Steward downloads'                                   
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,BIG=Y,     +        
               SLOWLIST=SLOWS,WORKERKEY=SPCS,ABENDLIST=FAILS,          +        
               SYSPHASE=SYSPHASE,SYSTEM=SPTSYSQ,APPEND=Y,              +        
               SERVERTYPE=TSTSPOT,SEGMENT=Y,AUTOCLEAR=Y,               +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#TWAD,TWAD),       +        
               BLOCKS2=(B#BUYREC,BUYREC,                               +        
               B#AGYREC,AGYHDR,B#ESTREC,ESTHDR,B#GOLREC,GOALRECD)               
                                                                                
CODE     NMOD1 0,**SL11**,RR=RE                                                 
         LARL  RA,GLOBALS                                                       
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
         USING SAVED,R8            R8=A(SAVE W/S)                               
         LA    R8,WORKD(R8)                                                     
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
         USING TSARD,TSARBLK                                                    
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
                                                                                
         MVC   AMKSTAB,AIO7        USE IO7/8 FOR MKSTAB ONLINE                  
         LHI   R0,MKSTMAX1                                                      
         STCM  R0,15,MKSMAXN       SET MAXIMUM N'MKSTAB ENTRIES ONLINE          
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR02                                                         
                                                                                
         LA    R0,MKSTAB           USE W/S FOR MKSTAB OFFLINE                   
         ST    R0,AMKSTAB                                                       
         LHI   R0,MKSTMAX2                                                      
         STCM  R0,15,MKSMAXN       SET MAXIMUM N'MKSTAB ENTRIES OFFLINE         
                                                                                
         L     RF,ACOMFACS         LOAD FACILITIES OVERLAYS                     
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#AGYREC-1)*L'LP_BLKS)(AIOLAST-AIO1),AIO1              
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
                                                                                
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
         BNE   EXITY                                                            
                                                                                
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
         GOTOR VDATAMGR,DMCB,DMKEY,GENDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,STAFIL,(4,0),0                               
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
RUNREQ02 TM    MAPI2,MAPIAMCE      TEST WANT AGY/MED/CLT/EST RESOLVED           
         BZ    RUNREQ08                                                         
                                                                                
         ICM   RF,7,AMED                                                        
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ICM   RF,7,ACLT           RF=A(CLIENT REQUEST VALUE)                   
         BZ    RUNREQ04                                                         
         MVC   QCLTX,LW_DATA1-LW_D(RF)                                          
         GOTOR RESCLT              RESOLVE CLIENT VALUES                        
                                                                                
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
         BNZ   RUNREQ08                                                         
         MVI   XSPILL,YESQ         NO - TURN OFF SPILL                          
                                                                                
RUNREQ08 GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         B     EXITY                                                            
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
         EJECT                                                                  
***********************************************************************         
* STEWARD INITIAL DOWNLOAD                                            *         
***********************************************************************         
                                                                                
REQSINI  LKREQ *,I#INIDLD,OUTINI,NEXTREQ=REQWIZ                                 
                                                                                
OUTINI   LKOUT H                   ** STEWARD INITIAL DOWNLOAD **               
                                                                                
AGYC     LKOUT R,X'0012'                                                        
Array    LKOUT C,X'0012',(A,ARYAGY)                                             
         LKOUT E                                                                
                                                                                
DEMC     LKOUT R,X'0014'                                                        
Array    LKOUT C,X'0014',(A,ARYDEM)                                             
         LKOUT E                                                                
                                                                                
MKTC     LKOUT R,X'0016'                                                        
Array    LKOUT C,X'0016',(A,ARYMKT)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD AGENCY DOWNLOAD                        *         
***********************************************************************         
                                                                                
ARYAGY   LKOUT A,(D,B#AGYREC,AGYHDR),NEWEL=Y,NROWS=1,ROWWIDTH=0                 
                                                                                
AgyNm    LKOUT C,71,AGYNAME,CHAR                                                
AgyAd    LKOUT C,72,AGYADDR,CHAR                                                
PRout    LKOUT P,,GETUID                                                        
UsrNm    LKOUT C,73,(D,B#WORKD,WORK),CHAR,ND=Y,LEN=L'CTORGNAM                   
Cntry    LKOUT C,74,AGYPCNDA,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* READ ID RECORD AND SET WORK TO ORIGIN NAME                          *         
***********************************************************************         
                                                                                
         USING GUWORKD,RC                                                       
         USING CTIREC,GUIO                                                      
GETUID   XC    WORK(L'CTORGNAM),WORK                                            
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LP_USRID                                                 
         LA    R0,GUIO                                                          
         ST    R0,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE'                                
         BNE   EXITY                                                            
         LA    R2,CTIDATA                                                       
         USING CTORGD,R2                                                        
         SR    R0,R0                                                            
GETUID02 CLI   CTORGEL,0                                                        
         BE    EXITY                                                            
         CLI   CTORGEL,CTORGELQ                                                 
         JE    *+14                                                             
         IC    R0,CTORGLEN                                                      
         AR    R2,R0                                                            
         J     GETUID02                                                         
         MVC   WORK(L'CTORGNAM),CTORGNAM                                        
         B     EXITY                                                            
         DROP  R2,RC                                                            
                                                                                
GUWORKD  DSECT                     ** GETUID LOCAL WORKING STORAGE **           
GUIO     DS    XL2000                                                           
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD DEMO TABLE DOWNLOAD                    *         
***********************************************************************         
                                                                                
ARYDEM   LKOUT A,(R,BLDDEM),NEWEL=R,EOT=EOR,ROWNAME=DEMTABD,           +        
               ROWWIDTH=DEMTABL                                                 
                                                                                
DemCd    LKOUT C,14,DEMTCOD,CHAR                                                
DemNm    LKOUT C,70,DEMTNAM,CHAR                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* BUILD TABLE OF DEMO CODES AND NAMES                                 *         
***********************************************************************         
                                                                                
BLDDEM   MVI   DBSELMED,C'C'       SET CANADIAN MEDIA                           
         MVC   DBCOMFCS,ACOMFACS                                                
                                                                                
         L     R2,AIO6             R2=A(DEMO TABLE BUILD AREA)                  
         ST    R2,LP_ADATA                                                      
         USING DEMTABD,R2                                                       
         LA    R3,MODLCAN                                                       
         SR    R4,R4               R4=N'TABLE ENTRIES                           
BLDDEM02 CLI   0(R3),0             TEST END OF MODIFIER LIST                    
         JE    BLDDEM06                                                         
         SR    R0,R0                                                            
BLDDEM04 AHI   R0,1                BUMP DEMO NUMBER                             
         CHI   R0,255              TEST ALL CATEGORIES PROCESSED                
         JNH   *+12                                                             
         AHI   R3,1                YES - BUMP TO NEXT MODIFIER                  
         J     BLDDEM02                                                         
                                                                                
         MVI   WORK+0,0            BUILD DEMO LOOKUP EXPRESSION                 
         MVC   WORK+1(1),0(R3)                                                  
         STC   R0,WORK+2                                                        
         GOTOR VDEMOCON,DMCB,WORK,(2,DEMTNAM),DBLOCK                            
         CLI   DEMTNNAM,C'*'       TEST BAD DEMO                                
         JE    BLDDEM04                                                         
         CLI   DEMTNMOD,C' '       TEST MODIFIER IS A SPACE (IMPS)              
         JNE   *+14                                                             
         MVC   DEMTMOD(L'DEMTNNAM),DEMTNNAM                                     
         MVI   DEMTNNAM+L'DEMTNNAM-1,C' '                                       
         GOTOR (#EDTDCD,AEDTDCD),DMCB,WORK,0,DEMTCOD                            
                                                                                
         AHI   R2,DEMTABL          BUMP TO NEXT OUTPUT TABLE ENTRY              
         AHI   R4,1                BUMP N'OUTPUT ENTRIES                        
         J     BLDDEM04                                                         
                                                                                
BLDDEM06 MVI   DEMTCOD,EOR         SET END OF DEMO TABLE                        
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD MARKET DOWNLOAD                        *         
***********************************************************************         
                                                                                
ARYMKT   LKOUT A,(R,NXTMKT),MULTIROW=Y,ROWNAME=MKTREC                           
MktNo    LKOUT C,16,MKTKMKT,CHAR                                                
MktNm    LKOUT C,17,MKTNAME,CHAR                                                
         LKOUT E                                                                
                                                                                
NXTMKT   GOTOR (#NXTREC,ANXTREC),DMCB,MKTKEYT,('B#MKTREC',0),          +        
               ('$NXTRSTA',SAVED),0,0                                           
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* STEWARD BUY DOWNLOAD WIZARD                                         *         
***********************************************************************         
                                                                                
REQWIZ   LKREQ H,I#BUYWIZ,OUTWIZ,NEXTREQ=REQSTB                                 
                                                                                
MedCd    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,3,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),             +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,5,(I,B#SAVED,BRDIND),CHAR,MAXLEN=L'PKEYPRD,           +        
               TEXT=SP#PRO,OLEN=L'PKEYPRD,COL=*                                 
EstNo    LKREQ F,78,(I,B#SAVED,ESTIND),LBIN,LIST=NOD,RANGE=Y,          +        
               DEFAULT=NOT,OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                     
EstFt    LKREQ F,79,(D,B#WORKD,QPOLFLT),CHAR,LOWERCASE=Y,              +        
               TEXT=SP#ESTFI,COL=*                                              
SDate    LKREQ F,10,(D,B#WORKD,QSTRDATE),EDAT,TEXT=SP#STDT,COL=*                
EDate    LKREQ F,11,(D,B#WORKD,QENDDATE),EDAT,TEXT=SP#ENDT,COL=*                
                                                                                
         LKREQ E                                                                
                                                                                
OUTWIZ   LKOUT H                                                                
OUTREQ   LKOUT R,X'0030'                                                        
Array    LKOUT C,255,(A,ARYWIZ)                                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYWIZ   LKOUT A,(R,GETWIZ)                                                     
                                                                                
StrDt    LKOUT C,10,(D,B#SAVED,STRDATE),EDAT,ND=Y                               
EndDt    LKOUT C,11,(D,B#SAVED,ENDDATE),EDAT,ND=Y                               
Array    LKOUT C,255,(A,ARYWID)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* READ POL ESTIMATES - SET START/END DATE AND BUILD DEMO LIST         *         
***********************************************************************         
                                                                                
GETWIZ   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',WIZKEYT),('B#ESTREC',0), +        
               SAVED,0,('#FLTPOL',AFLTPOL)                                      
         BNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         ST    R2,LP_ADATA                                                      
         USING ESTHDR,R2           R2=A(ESTIMATE RECORD)                        
         OC    STRDATE,STRDATE     SET LOWEST ESTIMATE START DATE               
         JZ    *+14                                                             
         CLC   ESTART,STRDATE                                                   
         JNL   *+10                                                             
         MVC   STRDATE,ESTART                                                   
         OC    ENDDATE,ENDDATE     SET HIGHEST ESTIMATE END DATE                
         JZ    *+14                                                             
         CLC   EEND,ENDDATE                                                     
         JNH   *+10                                                             
         MVC   ENDDATE,EEND                                                     
                                                                                
         LA    R3,EDEMLST          R3=A(ESTIMATE DEMO LIST)                     
         LHI   R0,EDEMLSTN         R0=MAXIMUM N'DEMOS                           
GETWIZ02 CLI   1(R1),0             TEST END OF DEMO LIST                        
         JE    GETWIZ                                                           
         CLI   1(R1),33            IGNORE USER DEMOS                            
         JE    GETWIZ06                                                         
         CLI   1(R1),63            IGNORE WEIGHTED DEMOS                        
         JE    GETWIZ06                                                         
         GOTOR (#EDTDCD,AEDTDCD),DMCB,(R3),0,WORK                               
                                                                                
         LA    RE,DEMLST           RE=A(OUTPUT LIST)                            
         LHI   RF,DEMLSTMX         RF=MAXIMUM N'DEMLST ENTRIES                  
GETWIZ04 CLC   0(L'DEMLST,RE),WORK TEST DEMO ALREADY IN LIST                    
         JE    GETWIZ06                                                         
         CLI   0(RE),0             TEST END OF LIST                             
         JE    *+16                                                             
         AHI   RE,L'DEMLST         NO - BUMP TO NEXT DEMLST ENTRY               
         JCT   RF,GETWIZ04         DO FOR MAXIMUM N'DEMLST ENTRIES              
         J     GETWIZ                                                           
         MVC   0(L'DEMLST,RE),WORK MOVE NEW ENTRY TO DEMLST                     
                                                                                
GETWIZ06 AHI   R3,L'EDEMLIST       BUMP TO NEXT INPUT DEMO                      
         JCT   R0,GETWIZ02         DO FOR NUMBER OF ESTIMATE DEMOS              
         J     GETWIZ                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION DEMO LIST DOWNLOAD                                 *         
***********************************************************************         
                                                                                
ARYWID   LKOUT A,(D,B#SAVED,DEMLST),ROWWIDTH=L'DEMLST,EOT=EOR                   
                                                                                
DemCd    LKOUT C,14,(D,,DEMLST),CHAR                                            
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* STEWARD BUY DOWNLOAD REQUEST                                        *         
***********************************************************************         
                                                                                
REQSTB   LKREQ H,I#BUYDLD,OUTSTB,NEXTREQ=REQUESTX                               
                                                                                
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,3,(I,B#SAVED,CLTIND),(R,VALCLT),                      +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,5,(I,B#SAVED,BRDIND),(U,#VALPRD,$VALPRD),             +        
               LIST=NOD,OLEN=L'BUYKPRD,MAXLEN=L'PKEYPRD,TEXT=SP#PRO,   +        
               COL=*                                                            
EstNo    LKREQ F,78,(I,B#SAVED,ESTIND),LBIN,LIST=NOD,RANGE=Y,          +        
               DEFAULT=NOT,OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                     
EstFt    LKREQ F,79,(D,B#WORKD,QPOLFLT),CHAR,LOWERCASE=Y,              +        
               TEXT=SP#ESTFI,COL=*                                              
SDate    LKREQ F,10,(D,B#WORKD,QSTRDATE),EDAT,TEXT=SP#STDT,COL=*                
EDate    LKREQ F,11,(D,B#WORKD,QENDDATE),EDAT,TEXT=SP#ENDT,COL=*                
MktNo    LKREQ F,16,(I,B#SAVED,MKTIND),LBIN,LIST=NOD,DEFAULT=NOT,      +        
               OLEN=L'BUYKMKTN,TEXT=SP#MKT,COL=*                                
StNet    LKREQ F,19,(I,B#SAVED,STAIND),(U,#VALSTA,$VALSTA),            +        
               DEFAULT=Y,OLEN=L'BUYKSTAC,MAXLEN=STASTAL,TEXT=SP#STA,   +        
               LIST=NOD,COL=*                                                   
Demos    LKREQ F,14,(I,B#SAVED,DEMIND),(U,#VALDCD,$VALDCD),            +        
               LIST=NOD,SORT=N,OLEN=L'EDEMLIST,MAXLIST=EDEMLSTN,       +        
               MAXLEN=4,TEXT=SP#DEMO,COL=*                                      
BOnly    LKREQ F,101,(D,B#SAVED,BRDSONLY),HDRO,TEXT=SP#BRDOY,COL=*              
DlGol    LKREQ F,102,(D,B#SAVED,DLDGOALS),HDRO,TEXT=SP#DLGOL,COL=*              
DlAfd    LKREQ F,103,(D,B#SAVED,DLDAFFID),HDRO,TEXT=SP#DLAFD,COL=*              
XPaid    LKREQ F,82,(D,B#SAVED,XPAIDS),HDRO,TEXT=SP#XPAID,COL=*                 
XMiss    LKREQ F,84,(D,B#SAVED,XMISSD),HDRO,TEXT=SP#XMISS,COL=*                 
XMkgd    LKREQ F,85,(D,B#SAVED,XMGOOD),HDRO,TEXT=SP#XMKGD,COL=*                 
XZero    LKREQ F,86,(D,B#SAVED,XZERO$),HDRO,TEXT=SP#XZERO,COL=*                 
XAloc    LKREQ F,87,(D,B#SAVED,XPALOC),HDRO,TEXT=SP#XPALO,COL=*                 
XSpil    LKREQ F,88,(D,B#SAVED,XSPILL),HDRO,TEXT=SP#XSPIL,COL=*                 
                                                                                
         LKREQ E                                                                
                                                                                
OUTSTB   LKOUT H                   ** STEWARD BUY DOWNLOAD **                   
                                                                                
STBCLT   LKOUT R,X'0042'           CLIENT VALUES                                
CltCd    LKOUT C,3,(D,B#WORKD,QCLTA),CHAR                                       
CltNm    LKOUT C,77,(D,B#SAVED,CLTNAME),CHAR                                    
Spods    LKOUT C,96,(D,B#SAVED,CLTSPDS),HDRO,ND=Y                               
PolTy    LKOUT C,97,(D,B#SAVED,CLTBPOL),HDRO,ND=Y                               
         LKOUT E                                                                
                                                                                
STBBRD   LKOUT R,X'0044'           PRODUCT RECORDS                              
Array    LKOUT C,X'0044',(A,ARYBRD)                                             
         LKOUT E                                                                
                                                                                
STBPOL   LKOUT R,X'0046'           POOL ESTIMATE RECORDS                        
Array    LKOUT C,X'0046',(A,ARYPOL)                                             
         LKOUT E                                                                
                                                                                
STBDPT   LKOUT R,X'0049'           DAYPART RECORD                               
Array    LKOUT C,X'0049',(A,ARYDPT)                                             
         LKOUT E                                                                
                                                                                
STBNBY   LKOUT R,255               NETWORK MEDIA BUYS                           
Array    LKOUT C,255,(A,ARYNBY)                                                 
         LKOUT E                                                                
                                                                                
STBOBY   LKOUT R,255               SELECTIVE MEDIA BUYS                         
Array    LKOUT C,255,(A,ARYOBY)                                                 
         LKOUT E                                                                
                                                                                
STBGOL   LKOUT R,X'0060'           GOAL RECORDS                                 
Array    LKOUT C,X'0060',(A,ARYGOL)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD BRAND DOWNLOAD                         *         
***********************************************************************         
                                                                                
ARYBRD   LKOUT A,(R,NXTBRD),MULTIROW=Y,ROWNAME=PRDHDR                           
                                                                                
PrdCd    LKOUT C,5,PKEYPRD,CHAR                                                 
PrdNm    LKOUT C,7,PNAME,CHAR                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET BRAND (PRODUCT) RECORDS FOR REQUESTED CLIENT                    *         
***********************************************************************         
                                                                                
NXTBRD   GOTOR (#NXTREC,ANXTREC),DMCB,BRDKEYT,('B#PRDREC',0),SAVED,0,0          
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR POOL ESTIMATE DOWNLOAD                         *         
***********************************************************************         
                                                                                
ARYPOL   LKOUT A,(R,NXTPOL),MULTIROW=Y,ROWNAME=ESTHDR                           
                                                                                
EstNo    LKOUT C,8,EKEYEST,LBIN                                                 
EDesc    LKOUT C,9,EDESC,CHAR                                                   
SDate    LKOUT C,10,ESTART,EDAT                                                 
EDate    LKOUT C,11,EEND,EDAT                                                   
Array    LKOUT C,X'0048',(A,ARYEST)                                             
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET POOL ESTIMATE RECORDS FOR REQUESTED CLIENT                      *         
***********************************************************************         
                                                                                
NXTPOL   GOTOR (#NXTREC,ANXTREC),DMCB,POLKEYT,('B#ESTREC',0),SAVED,0,  +        
               ('#FLTPOL',AFLTPOL)                                              
         BNE   EXITY                                                            
         L     R1,IOADDR           R1=A(POOL ESTIMATE RECORD)                   
         USING ESTHDR,R1                                                        
         MVC   CORPDEMO,EDEMLIST   EXTRACT CORPORATE DEMO VALUE                 
         CLI   DPTCODE,0           TEST DAYPART CODE ESTABLISHED                
         JNE   *+10                                                             
         MVC   DPTCODE,EDAYMENU    NO - EXTRACT                                 
         CLI   ESTIND,LW_TALLQ     TEST ESTIMATE(S) GIVEN                       
         BNE   EXITY                                                            
         ICM   RE,7,AEST2          POINT TO ESTIMATE ENTRY IN POOL              
         USING LW_D,RE                                                          
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN        BUMP N'ENTRIES IN LIST                       
         AHI   RF,1                                                             
         STCM  RF,3,LW_NUMN                                                     
         LA    RF,LW_DATA2-1(RF)   ADD ESTIMATE# TO LIST OF ESTIMATES           
         MVC   0(L'EKEYEST,RF),EKEYEST                                          
         B     EXITY                                                            
         DROP  R1,RE                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BRAND (PRODUCT) ESTIMATE DOWNLOAD              *         
***********************************************************************         
                                                                                
ARYEST   LKOUT A,(R,NXTEST),MULTIROW=Y,ROWNAME=ESTHDR                           
                                                                                
PrdCd    LKOUT C,5,EKEYPRD,CHAR                                                 
DemCd    LKOUT C,14,EDEMLIST,(U,#EDTDCD,$EDTDCD),EOT=EOR                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET BRAND (PRODUCT) ESTIMATE RECORDS FOR LOCATED POOL ESTIMATE      *         
***********************************************************************         
                                                                                
NXTEST   GOTOR (#NXTREC,ANXTREC),DMCB,ESTKEYT,('B#ESTREC',SVPOLKEY),   +        
               SAVED,AFLTEST,0                                                  
         BNE   EXITY                                                            
         L     R1,IOADDR           R1=A(BRAND ESTIMATE RECORD)                  
         USING ESTHDR,R1                                                        
         OC    EDEMLIST,EDEMLIST                                                
         JNZ   *+10                                                             
         MVC   EDEMLIST,CORPDEMO                                                
         B     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD DAYPART MENU DOWNLOAD                  *         
***********************************************************************         
                                                                                
ARYDPT   LKOUT A,(R,GETDPT),NEWEL=R,EOT=EOR,ROWWIDTH=DPTTABL,          +        
               ROWNAME=DPTTABD                                                  
                                                                                
DCode    LKOUT C,33,DPTTALPH,CHAR                                               
DName    LKOUT C,75,DPTTNAME,CHAR                                               
DMast    LKOUT C,76,DPTTMAST,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD NETWORK BUY DOWNLOAD                   *         
***********************************************************************         
                                                                                
ARYNBY   LKOUT A,(R,NXTNBY),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
Array    LKOUT C,X'0050',(A,ARYSTA)                                             
Array    LKOUT C,X'0052',(A,ARYLIN)                                             
Array    LKOUT C,X'0054',(A,ARYSCD)                                             
Array    LKOUT C,X'0056',(A,ARYSPT)                                             
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION/COST/DEMO DOWNLOAD                     *         
***********************************************************************         
                                                                                
ARYSCD   LKOUT A,(R,NXTSCD),MULTIROW=Y                                          
                                                                                
Array    LKOUT C,X'0054',(A,ARYNSCD)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO BUILD NETWORK STATION/COST/DEMOS RECORDS                 *         
***********************************************************************         
                                                                                
NXTSCD   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSCD02                                                         
         MVC   SVBUYKEY,IOKEY      SAVE NETWORK BUY RECORD KEY                  
         MVC   SVIOADDR,IOADDR     AND ADDRESS OF BUY RECORD                    
         L     R1,IOADDR                                                        
         AHI   R1,BDELEM-BUYREC                                                 
         ST    R1,ANTWKEL                                                       
                                                                                
NXTSCD02 LA    R0,SCDVALS                                                       
         LHI   R1,SCDVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   PBDVALS,FF                                                       
         LA    R0,SCDVALS                                                       
         ST    R0,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
                                                                                
         L     R3,ANTWKEL          POINT TO LAST ELEMENT AND BUMP               
         USING NTWKELEM,R3                                                      
NXTSCD04 LLC   R0,NTWKLEN          LOCATE FIRST/NEXT NETWORK ELEMENT            
         AR    R3,R0                                                            
         CLI   NTWKCODE,0          TEST END OF RECORD                           
         JE    NXTSCDX                                                          
         CLI   NTWKCODE,NTWKCODQ   TEST NETWORK POINTER ELEMENT                 
         JNE   NXTSCD04                                                         
         ST    R3,ANTWKEL                                                       
                                                                                
         LA    R2,IOKEY            READ LOCAL STATION BUY                       
         USING BUYREC,R2                                                        
         MVC   BUYKEY,SVBUYKEY                                                  
         MVC   BUYKMSTA,NTWKMKST                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO6'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO6'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR           R2=A(LOCAL STATION BUY)                      
                                                                                
         GOTOR GETMSS,BUYKMSTA     GET MARKET/STATION SEQUENCE#                 
                                                                                
         GOTOR GETCST,BUYREC       GET COST                                     
         CP    SCDCOST1,PZERO                                                   
         JNE   *+10                                                             
         XC    SCDCOST1,SCDCOST1   SUPPRESS ZERO COSTS FOR NETWORK              
                                                                                
         GOTOR GETDEM,BUYREC       EXTRACT DEMO VALUES                          
         B     EXITY                                                            
                                                                                
NXTSCDX  MVC   IOKEY,SVBUYKEY      RESTORE SAVED NETWORK BUY KEY                
         MVC   IOADDR,SVIOADDR     RESTORE SAVED RECORD ADDRESS                 
         B     NOMORE                                                           
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION/COST/DEMOS LINE DOWNLOAD               *         
***********************************************************************         
                                                                                
ARYNSCD  LKOUT A,(D,B#SAVED,SCDVALS),NEWEL=Y,NROWS=1,ROWWIDTH=SCDVALL           
                                                                                
Statn    LKOUT C,19,SCDSTA,CHAR,ND=Y                                            
MktNo    LKOUT C,16,SCDMKT#,LBIN,ND=Y                                           
SCost    LKOUT C,41,SCDCOST1,SPAK,ND=Y                                          
MSRef    LKOUT C,18,SCDMKST#,LBIN,ND=Y                                          
Array    LKOUT C,255,(A,ARYDEMV)                                                
Array    LKOUT C,255,(A,ARYNSPL)                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NETWORK SPILL DEMOS                            *         
***********************************************************************         
                                                                                
ARYNSPL  LKOUT A,(R,NXTNSP),MULTIROW=Y                                          
                                                                                
Array    LKOUT C,X'0055',(A,ARYSPIL)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO BUILD NETWORK SPILL DEMOS                                *         
***********************************************************************         
                                                                                
NXTNSP   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTNSP02                                                         
         CLI   XSPILL,0            TEST SPILL REQUIRED                          
         BNE   NOMORE                                                           
         L     R1,IOADDR                                                        
         AHI   R1,BDELEM-BUYREC                                                 
         ST    R1,ANDELEM                                                       
                                                                                
NXTNSP02 LA    R0,SCDVALS                                                       
         LHI   R1,SCDVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   PBDVALS,FF                                                       
         MVI   SCDSPILL,YESQ       SET WANT SPILL DATA SENT                     
         LA    R0,SCDVALS                                                       
         ST    R0,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
                                                                                
         L     R3,ANDELEM          POINT TO LAST ELEMENT AND BUMP               
         USING NDELEM,R3                                                        
NXTNSP04 LLC   R0,NDLEN            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         CLI   NDCODE,0            TEST END OF RECORD                           
         BE    NOMORE                                                           
         CLI   NDCODE,NDCSPLQ      TEST SPILL DEMO ELEMENT                      
         JNE   NXTNSP04                                                         
         ST    R3,ANDELEM                                                       
                                                                                
         MVC   DUB(L'BUYKMKTN),NDPROG                                           
         L     RF,IOADDR                                                        
         MVC   DUB+L'BUYKMKTN(L'BUYKSTAC),BUYKSTAC-BUYKEY(RF)                   
         GOTOR GETMSS,DUB          GET MARKET/STATION SEQUENCE#                 
                                                                                
         LA    R1,NDELEM                                                        
         ICM   R1,8,HEX80                                                       
         GOTOR GETDEM              CALL GETDEM PASSING ELEMENT ADDRESS          
         B     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* GET NETWORK BUY RECORDS AND FORMAT BUY LINE FOR DOWNLOAD            *         
***********************************************************************         
                                                                                
NXTNBY   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTNBY04                                                         
         XC    LBUYVALS(LBUYVALL),LBUYVALS                                      
         CLI   ESTIND,LW_TALLQ     TEST ESTIMATE GIVEN                          
         JNE   *+10                                                             
         MVC   ESTIND(L'ESTIND+L'AEST),ESTIND2                                  
         XC    MKSTAB#,MKSTAB#     CLEAR N'ENTRIES IN MKT/STA TABLE             
         MVI   PESTNUM,0                                                        
         MVC   BUYMED,QMEDA                                                     
         CLI   BUYMED,NETMEDQ      TEST NETWORK REQUEST                         
         JE    NXTNBY02                                                         
         CLI   BUYMED,COMMEDQ      TEST COMBINED MEDIA REQUEST                  
         BNE   NOMORE                                                           
         MVI   BUYMED,NETMEDQ      SET PROCESSING NETWORK BUYS                  
                                                                                
NXTNBY02 LLC   R0,QMEDA            SAVE QMEDA                                   
         GOTOR (#VALMED,AVALMED),DMCB,BUYMED,0,BUYAGM                           
         STC   R0,QMEDA            RESTORE QMEDA                                
         BNE   NOMORE                                                           
                                                                                
NXTNBY04 GOTOR (#NXTREC,ANXTREC),DMCB,NBYKEYT,('B#BUYREC',0),SAVED,    +        
               AFLTBKY,AFLTBRD                                                  
         BNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING BUYREC,R2           R2=A(NETWORK BUY RECORD)                     
                                                                                
         CLC   PNETSTA,BUYKSTAC    TEST CHANGE OF NETWORK                       
         MVC   PNETSTA,BUYKSTAC    SET NETWORK CODE                             
         JE    NXTNBY06                                                         
         MVI   STASEND,YESQ        SET TO SEND STATION RECORD                   
         MVI   STAMED,NETMEDQ      SET MEDIA                                    
         CLI   BUYKSTAC+L'BUYKSTAC-1,CBLSUFQ                                    
         JL    *+8                                                              
         MVI   STAMED,CBLMEDQ      SET CABLE MEDIA (BASED ON STATION)           
         GOTOR GETSTA,BUYKMSTA                                                  
         MVC   STASTA,STAPQSTA     SET STATION                                  
                                                                                
NXTNBY06 GOTOR FMTLIN,BUYREC       FORMAT BUY LINE FOR DOWNLOAD                 
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD NON-NETWORK BUY DOWNLOAD               *         
***********************************************************************         
                                                                                
ARYOBY   LKOUT A,(R,NXTOBY),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
Array    LKOUT C,X'0050',(A,ARYSTA)                                             
Array    LKOUT C,X'0052',(A,ARYLIN)                                             
Array    LKOUT C,X'0054',(A,ARYORIG)                                            
Array    LKOUT C,X'0055',(A,ARYSPIL)                                            
Array    LKOUT C,X'0056',(A,ARYSPT)                                             
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET NON-NETWORK BUY RECORDS AND FORMAT BUY LINE FOR DOWNLOADING     *         
***********************************************************************         
                                                                                
                                                                                
NXTOBY   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTOBY04                                                         
                                                                                
         L     R0,AIO7             CLEAR MARKET REFERENCE TABLE                 
         LHI   R1,IO7LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   PESTNUM,0                                                        
         MVC   BUYMED,QMEDA                                                     
         CLI   BUYMED,NETMEDQ      TEST NETWORK ONLY REQUEST                    
         BE    NOMORE              YES - ALL DONE                               
         CLI   BUYMED,COMMEDQ      TEST COMBINED MEDIA REQUEST                  
         JNE   NXTOBY02                                                         
         MVI   BUYMED,TELMEDQ      YES - SET TO PROCESS TELEVISION              
                                                                                
NXTOBY02 LLC   R0,QMEDA            SAVE QMEDA                                   
         GOTOR (#VALMED,AVALMED),DMCB,BUYMED,0,BUYAGM                           
         STC   R0,QMEDA            RESTORE QMEDA                                
         BNE   NOMORE                                                           
                                                                                
NXTOBY04 LA    R0,OBYKEYT                                                       
         GOTOR (#NXTREC,ANXTREC),DMCB,(R0),('B#BUYREC',0),SAVED,       +        
               AFLTBKY,AFLTBRD                                                  
         BNE   EXITY                                                            
                                                                                
         L     R2,IOADDR           POINT TO BUY RECORD                          
         USING BUYREC,R2                                                        
                                                                                
         CLC   PMKTSTA,MKTSTA      TEST CHANGE OF MARKET/STATION                
         MVC   PMKTSTA,MKTSTA      SET MARKET/STATION                           
         JE    NXTOBY12                                                         
         XC    STAVALS(STAVALL),STAVALS                                         
         MVI   STASEND,YESQ        SET TO SEND STATION RECORD                   
         GOTOR GETSTA,MKTSTA                                                    
         MVC   STASTA,STAPQSTA     SET STATION                                  
                                                                                
         CLI   STAPQSTA+4,C'-'                                                  
         JNE   NXTOBY06                                                         
         MVC   STAPQSTA+4(1),STAPQSTA+5                                         
         J     NXTOBY08                                                         
                                                                                
NXTOBY06 CLI   STAPQSTA+4,C' '                                                  
         JE    *+12                                                             
         CLI   STAPQSTA+4,C'/'                                                  
         JNE   NXTOBY08                                                         
         MVI   STAPQSTA+4,C'T'                                                  
                                                                                
NXTOBY08 CLI   STAPMED,RADMEDQ                                                  
         JE    NXTOBY10                                                         
         CLI   STAPQSTA,C'0'       CABLE STATION?                               
         JL    NXTOBY10                                                         
         XC    WORK2,WORK2         TRANSLATE DDS 3CHAR NET TO 4CHAR             
         LA    R1,WORK2                                                         
         MVI   STAPACT-STAPACKD(R1),C'X'                                        
         MVC   STAPACOM-STAPACKD(L'STAPACOM,R1),ACOMFACS                        
         MVC   STAPQNET-STAPACKD(L'STAPQNET,R1),STAPQNET                        
         GOTOR VSTAPACK,WORK2                                                   
         CLI   STAPERR-STAPACKD(R1),0                                           
         JE    NXTOBY10                                                         
         DC    H'0'                                                             
                                                                                
NXTOBY10 MVC   STAMED,BUYMED       SET MEDIA                                    
         MVC   STAMKT#,MKTSTA      SET MARKET                                   
                                                                                
NXTOBY12 GOTOR FMTLIN,BUYREC       FORMAT BUY LINE VALUES FOR DOWNLOAD          
                                                                                
         LA    R0,SCDVALS                                                       
         LHI   R1,SCDVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   PBDVALS,FF                                                       
         MVC   SCDSPILL,SPILL                                                   
         CLI   SCDSPILL,YESQ       TEST SPILL BUY                               
         JE    NXTOBY14                                                         
         GOTOR GETCST,BUYREC       NO - SET BUY COSTS                           
                                                                                
NXTOBY14 GOTOR GETDEM,BUYREC       EXTRACT DEMO VALUES                          
         CLI   SCDSPILL,YESQ                                                    
         BNE   EXITY                                                            
         MVC   SCDMKT#,BUYKMKTN    SET SPILL MARKET                             
                                                                                
NXTOBY16 CLC   STACOD,BUYKSTAC     TEST SAME STATION CODE                       
         BE    EXITY                                                            
         GOTOR GETSTA,BUYKMSTA                                                  
         MVC   SCDSTA,STAPQSTA     SET STATION                                  
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD SPOT DOWNLOAD                          *         
***********************************************************************         
                                                                                
ARYSPT   LKOUT A,(R,GETSTS),NEWEL=B,NROWS=(B#SAVED,STSTAB#),           +        
               ROWNAME=STSTABD,ROWWIDTH=STSTABL                                 
                                                                                
MapCd    LKOUT C,255,STSMAPN,EMAP                                               
SDate    LKOUT C,47,STSDATE,CDAT,ND=Y                                           
SRef#    LKOUT C,49,STSREF#,LBIN,ND=Y                                           
P1Cod    LKOUT C,51,STSBRD1,(U,#EDTPRD,$EDTPRD),ND=Y                            
P2Cod    LKOUT C,53,STSBRD2,(U,#EDTPRD,$EDTPRD),ND=Y                            
P3Cod    LKOUT C,107,STSBRD3,(U,#EDTPRD,$EDTPRD),ND=Y                           
P4Cod    LKOUT C,109,STSBRD4,(U,#EDTPRD,$EDTPRD),ND=Y                           
P5Cod    LKOUT C,111,STSBRD5,(U,#EDTPRD,$EDTPRD),ND=Y                           
P6Cod    LKOUT C,113,STSBRD6,(U,#EDTPRD,$EDTPRD),ND=Y                           
SLen1    LKOUT C,31,STS1LEN,LBIN,ND=Y                                           
SLen2    LKOUT C,105,STS2LEN,LBIN,ND=Y                                          
AfDat    LKOUT C,55,STSAFDT,CDAT,ND=Y                                           
AfTim    LKOUT C,57,STSAFTM,LBIN,ND=Y                                           
SStat    LKOUT C,37,STSSTAT,HEXD                                                
COver    LKOUT C,43,STSCOST,SPAK,ND=Y                                           
RFact    LKOUT C,50,STSREP#,LBIN,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO BUILD STEWARD SPOT RECORDS                               *         
***********************************************************************         
                                                                                
GETSTS   L     R2,IOADDR                                                        
         USING BUYREC,R2           R2=A(BUY RECORD)                             
         XC    STSTAB#,STSTAB#     INITIALIZE N'ENTRIES IN TABLE                
         XC    SPOTDATE,SPOTDATE   INITIALIZE SPOT DATE                         
         MVC   SPOTTLEN,BDSEC      SET TOTAL SECONDS LENGTH                     
         XC    SPDVALS(SPDVALL),SPDVALS                                         
                                                                                
         LA    R2,BDELEM                                                        
         USING REGELEM,R2          R2=A(BUY ELEMENT)                            
         L     R3,AIO3                                                          
         ST    R3,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
TAB      USING STSTABD,R3          R3=A(CURRENT TABLE ENTRY)                    
         SR    R4,R4                                                            
PRV      USING STSTABD,R4          R4=A(PREVIOUS TABLE ENTRY)                   
                                                                                
GETSTS02 LLC   R0,RLEN             BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         CLI   RCODE,EOR           TEST END OF RECORD                           
         JE    GETSTS18                                                         
         CLI   RCODE,RCORGQ        TEST FOR SPOT ELEMENTS                       
         JL    GETSTS02                                                         
         CLI   RCODE,X'0D'                                                      
         JH    GETSTS02                                                         
                                                                                
         CLC   RDATE,SPOTDATE      NEW NEW DATE                                 
         JE    *+14                                                             
         MVC   SPOTDATE,RDATE      YES - SET NEW DATE                           
         MVI   SPOTREFN,0          AND INITIALIZE SPOT REFERENCE NUMBER         
                                                                                
         GOTOR FLTSPT,REGELEM      APPLY SPOT FILTERS                           
         JNE   GETSTS02                                                         
                                                                                
         XC    TAB.STSTABD(STSTABL),TAB.STSTABD                                 
                                                                                
         LA    RF,STSPTMAP         SET SPOT RECORD MAP NUMBER                   
         CLI   SPDTLEN,0           TEST FOR SPOD                                
         JE    *+8                                                              
         LA    RF,STSPDMAP         SET SPOD RECORD MAP NUMBER                   
         MVC   TAB.STSMAPN,0(RF)                                                
         MVC   TAB.STSREF#,SPOTREFN                                             
         MVI   TAB.STSREP#,1                                                    
         MVC   TAB.STSDATE,RDATE                                                
                                                                                
         OC    RPAY,RPAY           SET SPOT STATUS                              
         JZ    *+8                                                              
         OI    TAB.STSSTAT4,STSSPAID                                            
         CLI   RCODE,RCPOTOQ                                                    
         JNE   *+8                                                              
         OI    TAB.STSSTAT4,STSSPOTO                                            
         TM    RSTATUS,RSMINSDQ                                                 
         JZ    *+8                                                              
         OI    TAB.STSSTAT4,STSSMISD                                            
         TM    RSTATUS,RSMGONLQ                                                 
         JZ    *+8                                                              
         OI    TAB.STSSTAT4,STSSMKGD                                            
         TM    RSTATUS,RSNOALLQ                                                 
         JZ    *+8                                                              
         OI    TAB.STSSTAT4,STSSPRAL                                            
         CLI   RLEN,RLPOL1LQ                                                    
         JL    *+8                                                              
         OI    TAB.STSSTAT4,STSSALOC                                            
         TM    RSTATUS,RSB1PALQ                                                 
         JZ    *+8                                                              
         OI    TAB.STSSTAT3,STSSB1PA                                            
                                                                                
         TM    RSTATUS,RSRATOVQ    TEST COST OVERRIDE                           
         JZ    GETSTS04                                                         
         CLI   STAMED,NETMEDQ      NO COST OVERRIDE FOR NETWORK                 
         JE    GETSTS04                                                         
         SR    RF,RF                                                            
         ICM   RF,7,RPCOST                                                      
         TM    LINSTAT,BDSPRECQ    TEST POL NPW                                 
         JZ    *+8                                                              
         N     RF,=X'0003FFFF'                                                  
         TM    LINCIND,BDCMINSQ    TEST MINUS RATE                              
         JZ    *+6                                                              
         LNR   RF,RF                                                            
         CVD   RF,DUB                                                           
         ZAP   TAB.STSCOST,DUB     SET COST OVERRIDE                            
                                                                                
GETSTS04 CLI   SPDTLEN,0           TEST THIS IS A SPOD                          
         JNE   GETSTS06            YES                                          
         CLI   RLEN,RLPOL1LQ       NO - TEST ALLOCATED                          
         JL    *+10                                                             
         MVC   TAB.STSBRD1,RPALLOC+(RPPRD-RPALLOC)                              
         CLI   RLEN,RLPOL2LQ       TEST PIGGYBACK ALLOCATION                    
         JL    GETSTS08                                                         
         MVC   TAB.STSBRD2,RPALLOC2+(RPPRD-RPALLOC)                             
         LLC   RE,SPOTTLEN                                                      
         LLC   RF,RPALLOC2+(RPTIME-RPALLOC)                                     
         SR    RE,RF                                                            
         STC   RE,TAB.STS1LEN      SET SPOT LENGTH FOR PRIMARY BRAND            
         J     GETSTS08                                                         
                                                                                
GETSTS06 MVC   TAB.STS1LEN,RPALLOC+(RPTIME-RPALLOC)                             
         LLC   RF,SPDCOUNT                                                      
         LA    RF,TAB.STSBRDS-L'STSBRD1(RF)                                     
         MVC   0(L'TAB.STSBRD1,RF),RPALLOC+(RPPRD-RPALLOC)                      
         TM    SPDSTAT,SPDFSENT    TEST 'FIRST SPOT FOR SPOD' SENT              
         JZ    *+12                                                             
         MVI   TAB.STSREF#,0       YES - DON'T SEND REF# FOR SUBSEQUENT         
         J     GETSTS08                                                         
         OI    SPDSTAT,SPDFSENT    SET/SEND FIRST FOR SPOD                      
         OI    TAB.STSSTAT3,STSSFSPD                                            
                                                                                
GETSTS08 LA    R1,REGELEM          LOOK FOR OTHER SPOT RELATED ELEMENTS         
                                                                                
GETSTS10 LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),EOR           TEST END OF RECORD                           
         JE    GETSTS14                                                         
         CLI   0(R1),ACCODEQ       TEST POINTING TO AFFIDAVIT ELEMENT           
         JE    GETSTS12                                                         
         CLI   0(R1),RCORGQ                                                     
         JL    GETSTS10                                                         
         CLI   0(R1),X'08'                                                      
         JNH   GETSTS14                                                         
         CLI   0(R1),RCPOLOQ                                                    
         JL    GETSTS10                                                         
         CLI   0(R1),X'0D'                                                      
         JNH   GETSTS14                                                         
         J     GETSTS10                                                         
                                                                                
         USING AFFELEM,R1          PROCESS AFFIDAVIT ELEMENT                    
GETSTS12 OI    TAB.STSSTAT4,STSSAFID                                            
         CLI   DLDAFFID,0          TEST WANT AFFID DATA                         
         JE    GETSTS10                                                         
         MVC   TAB.STSAFDT,ADATE   SET AFFIDAVIT VALUES                         
         MVC   TAB.STSAFTM,ATIME                                                
         J     GETSTS10                                                         
         DROP  R1                                                               
                                                                                
GETSTS14 LTR   R4,R4               TEST FIRST TIME                              
         JZ    GETSTS16                                                         
         LLC   R1,PRV.STSREF#                                                   
         LLC   R0,PRV.STSREP#                                                   
         AR    R1,R0                                                            
         CLM   R1,1,TAB.STSREF#    TEST SPOT REF# IN SEQUENCE                   
         JNE   GETSTS16                                                         
         CLC   TAB.STSCOMP(STSCOMPL),PRV.STSCOMP                                
         JNE   GETSTS16                                                         
         LLC   R1,PRV.STSREP#      SPOT DATA IS THE SAME AS PREVIOUS...         
         AHI   R1,1                ...BUMP THE DUPLICATION FACTOR               
         STC   R1,PRV.STSREP#                                                   
         J     GETSTS02                                                         
                                                                                
GETSTS16 LA    R4,TAB.STSTABD      POINT TO CURRENT (PREVIOUS)                  
         AHI   R3,STSTABL          POINT TO NEXT (CURRENT)                      
         LH    R0,STSTAB#          BUMP SPOT TABLE ENTRY COUNT                  
         AHI   R0,1                                                             
         STH   R0,STSTAB#                                                       
         J     GETSTS02            GET NEXT ELEMENT                             
                                                                                
GETSTS18 SR    R0,R0               DO SPOT DOWNLOAD OPTIMIZATION                
         ICM   R0,3,STSTAB#                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3             POINT TO START OF TABLE                      
PRV      USING STSTABD,WORK                                                     
         XC    PRV.STSTABD(STSTABL),PRV.STSTABD                                 
                                                                                
GETSTS20 CLC   TAB.STSDATE,PRV.STSDATE                                          
         JE    *+12                                                             
         MVI   PRV.STSREF#,0       RESET REFERENCE# AND REPLICATION#            
         MVI   PRV.STSREP#,1       ON CHANGE OF SPOT DATE                       
                                                                                
         LLC   RE,PRV.STSREF#                                                   
         MVC   PRV.STSREF#,TAB.STSREF#                                          
         LLC   RF,PRV.STSREP#                                                   
         MVC   PRV.STSREP#,TAB.STSREP#                                          
         AR    RE,RF                                                            
         CLM   RE,1,TAB.STSREF#    TEST CURRENT REF# IN SEQUENCE                
         JNE   *+8                                                              
         MVI   TAB.STSREF#,0       YES - CLEAR REFERENCE#                       
         CLI   TAB.STSREP#,1       TEST REPLICATION FACTOR IS ONE               
         JNE   *+8                                                              
         MVI   TAB.STSREP#,0       YES - CLEAR REPLICATION FACTOR               
                                                                                
         CLC   PRV.STSDATE,TAB.STSDATE                                          
         MVC   PRV.STSDATE,TAB.STSDATE                                          
         JNE   *+10                                                             
         XC    TAB.STSDATE,TAB.STSDATE                                          
                                                                                
         CLC   PRV.STSBRDS,TAB.STSBRDS                                          
         MVC   PRV.STSBRDS,TAB.STSBRDS                                          
         JNE   *+10                                                             
         XC    TAB.STSBRDS,TAB.STSBRDS                                          
                                                                                
         CLC   TAB.STSMAPN,STSPDMAP                                             
         JE    GETSTS22                                                         
         SR    RE,RE               TEST 50/50 SPOT SPLIT                        
         ICM   RE,1,TAB.STS1LEN                                                 
         SLL   RE,1                                                             
         CLM   RE,1,SPOTTLEN                                                    
         JNE   GETSTS22                                                         
         MVI   TAB.STS1LEN,0                                                    
                                                                                
GETSTS22 CLC   PRV.STSAFDT,TAB.STSAFDT                                          
         MVC   PRV.STSAFDT,TAB.STSAFDT                                          
         JNE   *+10                                                             
         XC    TAB.STSAFDT,TAB.STSAFDT                                          
                                                                                
         AHI   R3,STSTABL          BUMP TO NEXT TABLE ENTRY                     
         JCT   R0,GETSTS20         DO FOR NUMBER OF SPOTS                       
         B     EXITY                                                            
         DROP  TAB,PRV,R2                                                       
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD SPILL BUY STATION/COST/DEMOS           *         
***********************************************************************         
                                                                                
ARYSPIL  LKOUT A,(D,B#SAVED,SCDVALS),NEWEL=Y,NROWS=1,                  +        
               ROWNAME=SCDVALS,ROWID=(SCDSPILL,YESQ),ROWWIDTH=SCDVALL           
                                                                                
Array    LKOUT C,X'0055',(A,ARYSCDV)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD STATION/COST/DEMOS LINE DOWNLOAD       *         
***********************************************************************         
                                                                                
ARYSCDV  LKOUT A,(D,B#SAVED,SCDVALS),NEWEL=Y,NROWS=1,ROWWIDTH=SCDVALL           
                                                                                
Statn    LKOUT C,19,SCDSTA,CHAR,ND=Y                                            
MktNo    LKOUT C,16,SCDMKT#,LBIN,ND=Y                                           
SCost    LKOUT C,41,SCDCOST1,SPAK,ND=Y                                          
MSRef    LKOUT C,18,SCDMKST#,LBIN,ND=Y                                          
Array    LKOUT C,255,(A,ARYDEMV)                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD DEMO DOWNLOAD                          *         
***********************************************************************         
                                                                                
ARYDEMV  LKOUT A,(D,B#SAVED,SCDDEMV),NROWS=(B#SAVED,SCDDEMN),          +        
               ROWWIDTH=L'SCDDEMV                                               
                                                                                
DemVl    LKOUT C,45,SCDDEM#,LBIN                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD BUYLINE DOWNLOAD                       *         
***********************************************************************         
                                                                                
ARYLIN   LKOUT A,(D,B#SAVED,LINVALS),NROWS=1,ROWWIDTH=LINVALL,NEWEL=Y           
                                                                                
EstNo    LKOUT C,8,LINEST,LBIN,ND=Y                                             
Line#    LKOUT C,21,LINLIN,LBIN,ND=Y                                            
Rottn    LKOUT C,23,LINROT,LBIN,ND=Y                                            
SDay#    LKOUT C,25,LINDAY,LBIN,ND=Y                                            
STime    LKOUT C,27,LINSTRT,LBIN                                                
ETime    LKOUT C,29,LINENDT,LBIN                                                
SptLn    LKOUT C,31,LINSEC,LBIN,ND=Y                                            
DayPt    LKOUT C,33,LINDPT,CHAR,ND=Y                                            
ProgN    LKOUT C,35,LINPGN,CHAR,ND=Y                                            
ProgC    LKOUT C,65,LINPGC,CHAR,ND=Y                                            
ConNo    LKOUT C,66,LINCON,CHAR,ND=Y                                            
AdjCd    LKOUT C,68,LINADJ,CHAR,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD ORIGINAL BUY STATION/COST/DEMOS        *         
***********************************************************************         
                                                                                
ARYORIG  LKOUT A,(D,B#SAVED,SCDVALS),NEWEL=Y,NROWS=1,ROWNAME=SCDVALS,  +        
               ROWID=(SCDSPILL,NOQ),ROWWIDTH=SCDVALL                            
                                                                                
Array    LKOUT C,X'0054',(A,ARYSCDV)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD STATION DOWNLOAD                       *         
***********************************************************************         
                                                                                
ARYSTA   LKOUT A,(D,B#SAVED,STAVALS),NROWS=1,SENDONCE=Y,NEWEL=Y,       +        
               ROWNAME=STAVALS,ROWID=(STASEND,YESQ),ROWWIDTH=STAVALL            
                                                                                
Media    LKOUT C,1,STAMED,CHAR                                                  
MktNo    LKOUT C,16,STAMKT#,LBIN,ND=Y                                           
Statn    LKOUT C,19,STASTA,CHAR                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD GOAL DOWNLOAD                          *         
***********************************************************************         
                                                                                
ARYGOL   LKOUT A,(R,NXTGOL),MULTIROW=Y,ROWNAME=GOLVALS                          
                                                                                
P1Cod    LKOUT C,51,GOLBRD1,(U,#EDTPRD,$EDTPRD),ND=Y                            
P1Sec    LKOUT C,31,GOLSEC1,LBIN,ND=Y                                           
P2Cod    LKOUT C,53,GOLBRD2,(U,#EDTPRD,$EDTPRD),ND=Y                            
P2Sec    LKOUT C,105,GOLSEC2,LBIN,ND=Y                                          
EstNo    LKOUT C,8,GOLEST,LBIN,ND=Y                                             
MktNo    LKOUT C,16,GOLMKT,LBIN,ND=Y                                            
DayPt    LKOUT C,33,GOLDPT,CHAR,ND=Y                                            
Array    LKOUT C,X'0062',(A,ARYGWK)                                             
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET GOAL RECORDS FOR (BUY/)GOAL AND MARKET LOCKIN DOWNLOADS         *         
***********************************************************************         
                                                                                
NXTGOL   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTGOL06                                                         
                                                                                
         XC    PRVVALS(PRVVALL),PRVVALS                                         
                                                                                
         CLI   DLDGOALS,0          TEST DOWNLOADING GOALS                       
         JNE   NOMORE                                                           
                                                                                
         MVC   GOALMED,QMEDA                                                    
         CLI   GOALMED,NETMEDQ     TEST NETWORK OR TELEVISION REQUEST           
         JE    *+12                                                             
         CLI   GOALMED,TELMEDQ                                                  
         JNE   NXTGOL02                                                         
         MVI   GOALMED,COMMEDQ     GOALS ARE HELD AS COMBINED MEDIA             
                                                                                
NXTGOL02 LLC   R0,QMEDA            SAVE QMEDA                                   
         GOTOR (#VALMED,AVALMED),DMCB,GOALMED,0,GOALAGM                         
         STC   R0,QMEDA            RESTORE QMEDA                                
         BNE   NOMORE                                                           
                                                                                
NXTGOL04 OC    ABRD,ABRD           TEST ANY PRODUCTS SPECIFIED                  
         JNZ   NXTGOL06                                                         
         L     RE,LP_AWMP          NO - BUILD DEFAULT ENTRY                     
         USING LW_D,RE                                                          
         STCM  RE,7,ABRD           CREAMS ABRD                                  
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVI   LW_DATA1+0,1                                                     
         MVI   LW_DATA1+1,POLPRDQ-1                                             
         AHI   RE,LQ_LN1Q+(L'GKEYPRD*2)                                         
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
NXTGOL06 GOTOR (#NXTREC,ANXTREC),DMCB,GOLKEYT,('B#GOLREC',0),SAVED,0,0          
         BNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         AHI   R2,GDELEM-GOALREC                                                
         USING GLEMENT,R2          R2=A(GOAL RECORD ELEMENT)                    
NXTGOL08 CLI   GLCODE,EOR          TEST END OF RECORD                           
         JE    NXTGOL06                                                         
         CLI   GLCODE,GLCODEQ      TEST GOAL WEEKLY ELEMENT                     
         JNE   NXTGOL10                                                         
                                                                                
         CLC   GLWEEK,STRDMONC     TEST START BEFORE REQUEST START              
         JL    NXTGOL10                                                         
         CLC   GLWEEK,ENDDATEC     TEST WEEK START AFTER REQUEST END            
         JNH   NXTGOL12                                                         
NXTGOL10 LLC   R0,GLEN             BUMP TO NEXT RECORD ELEMENT                  
         AR    R2,R0                                                            
         J     NXTGOL08                                                         
                                                                                
NXTGOL12 XC    GOLVALS(GOLVALL),GOLVALS                                         
         LA    R0,GOLVALS                                                       
         ST    R0,LP_ADATA                                                      
         L     R2,IOADDR           R2=A(GOAL RECORD)                            
         USING GOALREC,R2                                                       
         MVC   GOLBRD1,GKEYPRD     SET BRAND1 (PRODUCT) NUMBER                  
         MVC   GOLEST,GKEYEST      SET ESTIMATE NUMBER                          
         MVC   GOLMKT,GKEYMKT      SET MARKET NUMBER                            
         MVC   GOLDPT,GKEYDPT      SET DAYPART CODE                             
         MVC   GOLSEC1,GKEYSEC     SET BRAND1 SECONDS LENGTH                    
                                                                                
         CLI   GKEYPRD2,0          TEST PIGGYBACK GOAL                          
         JE    NXTGOL14                                                         
         TM    GKEYAGY,GKEY2NPQ                                                 
         JNZ   NXTGOL14                                                         
         MVC   GOLBRD2,GKEYPRD2    SET BRAND2 (PRODUCT) NUMBER                  
         LLC   RE,GKEYSLN                                                       
         SLL   RE,1                                                             
         CLM   RE,1,GKEYSEC        TEST 50/50 SPLIT                             
         JE    NXTGOL14                                                         
         LLC   RE,GKEYSEC          RE=TOTAL SECONDS LENGTH                      
         LLC   RF,GKEYSLN          RF=LENGTH ASSIGNED TO BRAND1                 
         SR    RE,RF                                                            
         STC   RE,GOLSEC2          SET BRAND2 SECONDS LENGTH                    
                                                                                
NXTGOL14 OC    GDLKGOAL,GDLKGOAL   IS THERE A GOAL LOCKIN DATE?                 
         JZ    NXTGOL16                                                         
         MVC   GOLLKIN,GDLKGOAL    YES, SEND IT OUT ON THE DOWNLOAD             
                                                                                
NXTGOL16 OC    GDIDR,GDIDR         IS THERE A PURPOSE CODE?                     
         JZ    NXTGOL18                                                         
         MVC   GOLPURP,GDIDR       YES, SEND IT OUT ON THE DOWNLOAD             
                                                                                
NXTGOL18 CLI   DLDGOALS,0          TEST DOWNLOADING GOALS                       
         JE    NXTGOL20                                                         
                                                                                
         CLC   PBRDNUM,GOLBRDS     DO GOAL RECORD OPTIMIZATION                  
         MVC   PBRDNUM,GOLBRDS                                                  
         JNE   *+10                                                             
         XC    GOLBRDS,GOLBRDS                                                  
                                                                                
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
                                                                                
NXTGOL20 LA    R2,GDELEM           TEST MARKET LOCKIN DATA PRESENT              
         USING GLKELEM,R2                                                       
NXTGOL22 CLI   GLKELEM,EOR         TEST END OF RECORD                           
         JE    NXTGOL06                                                         
         CLI   GLKELEM,GLKCOWKQ    TEST WEEK LOCKIN ELEMENT                     
         BE    EXITY                                                            
         CLI   GLKELEM,GLKCOMNQ    TEST MONTH LOCKIN ELEMENT                    
         BE    EXITY                                                            
         LLC   R0,GLKLEN                                                        
         AR    R2,R0                                                            
         J     NXTGOL22                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STEWARD GOAL WEEKLY DOWNLOAD                   *         
***********************************************************************         
                                                                                
ARYGWK   LKOUT A,(R,GETGWK),NEWEL=B,NROWS=(B#SAVED,GWKTAB#),           +        
               ROWWIDTH=GWKTABL,ROWNAME=GWKTABD                                 
                                                                                
WkStD    LKOUT C,59,GWKSTR,EDAT,ND=Y                                            
Goal$    LKOUT C,61,GWKDOL,SPAK,ND=Y                                            
GoalP    LKOUT C,63,GWKGRP,SPAK,ND=Y                                            
RFact    LKOUT C,50,GWKREP#,LBIN,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO BUILD GOAL WEEKLY RECORDS                                *         
***********************************************************************         
                                                                                
GETGWK   L     R2,IOADDR                                                        
         USING GOALREC,R2          R2=A(GOAL RECORD)                            
         XC    GWKTAB#,GWKTAB#     INITIALIZE N'ENTRIES IN GWKTAB               
         XC    GOALDATE,GOALDATE   CLEAR LAST WEEK START DATE                   
                                                                                
         LA    R2,GDELEM                                                        
         USING GLEMENT,R2          R2=A(FIRST GOAL RECORD ELEMENT)              
         L     R3,AIO3                                                          
         ST    R3,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
TAB      USING GWKTABD,R3          R3=A(CURRENT TABLE ENTRY)                    
         SR    R4,R4                                                            
PRV      USING GWKTABD,R4          R4=A(PREVIOUS TABLE ENTRY)                   
                                                                                
GETGWK02 CLI   GLCODE,EOR          TEST END OF RECORD                           
         JE    GETGWK10                                                         
                                                                                
         L     RF,LP_ADATA                                                      
         CLI   GLCODE,GLCODEQ      TEST GOAL WEEKLY ELEMENT                     
         JE    GETGWK04                                                         
         CLI   GLCODE,GLLKELQ      TEST GOAL LOCKIN ELEMENT                     
         JNE   GETGWK08                                                         
                                                                                
GETGWK04 CLC   GLWEEK,STRDMONC     TEST START BEFORE REQUEST START              
         JL    GETGWK08                                                         
         CLC   GLWEEK,ENDDATEC     TEST WEEK START AFTER REQUEST END            
         JH    GETGWK08                                                         
                                                                                
         XC    TAB.GWKTABD(GWKTABL),TAB.GWKTABD   INITIALIZE ENTRY              
                                                                                
         CLI   GLCODE,GLLKELQ      ARE WE PROCESSING LOCKIN DATA?               
         JNE   *+8                 NO, TYPE ALREADY 0                           
         MVI   TAB.GWKTYPE,7       YES, SET THE APPROPRIATE TYPE                
                                                                                
         GOTOR VDATCON,DMCB,(2,GLWEEK),TAB.GWKSTR                               
                                                                                
         ICM   R0,15,GLGRP                                                      
         CVD   R0,DUB                                                           
         ZAP   TAB.GWKGRP,DUB      SET GRPS                                     
         ICM   R0,15,GLBUDGET                                                   
         CVD   R0,DUB                                                           
         ZAP   TAB.GWKDOL,DUB      SET DOLLARS                                  
         MVI   TAB.GWKREP#,1                                                    
                                                                                
         CLC   TAB.GWKTYPE,PRV.GWKTYPE   GOAL DATA TYPE THE SAME?               
         JNE   GETGWK06                  NO, DON'T GROUP TOGETHER               
                                                                                
         LTR   R4,R4               TEST FIRST TIME                              
         JZ    GETGWK06            IT IS, DONE WITH THIS ENTRY                  
         LLC   R0,PRV.GWKREP#                                                   
         MHI   R0,7                                                             
         GOTOR VADDAY,DMCB,PRV.GWKSTR,WORK,(R0)                                 
         CLC   TAB.GWKSTR,WORK                                                  
         JNE   *+8                                                              
         OI    TAB.GWKINDS,GWKISEQ SET WEEK IN SEQUENCE                         
                                                                                
         CP    TAB.GWKGRP,PRV.GWKGRP                                            
         JNE   *+8                                                              
         OI    TAB.GWKINDS,GWKIGRP SET GRPS SAME AS PREVIOUS                    
                                                                                
         CP    TAB.GWKDOL,PRV.GWKDOL                                            
         JNE   *+8                                                              
         OI    TAB.GWKINDS,GWKIDOL SET DOLLARS SAME AS PREVIOUS                 
                                                                                
         TM    TAB.GWKINDS,GWKISEQ+GWKIGRP+GWKIDOL                              
         JNO   GETGWK06                                                         
                                                                                
         LLC   R0,PRV.GWKREP#      BUMP REPLICATION FACTOR IF WEEK IN           
         AHI   R0,1                SEQUENCE AND ALL DATA THE SAME               
         STC   R0,PRV.GWKREP#                                                   
         J     GETGWK08                                                         
                                                                                
GETGWK06 LA    R4,TAB.GWKTABD      POINT TO CURRENT (PREVIOUS)                  
         AHI   R3,GWKTABL          POINT TO NEXT (CURRENT)                      
         LH    R0,GWKTAB#          BUMP GOAL WEEK ENTRY COUNT                   
         AHI   R0,1                                                             
         STH   R0,GWKTAB#                                                       
                                                                                
GETGWK08 LLC   R0,GLEN             BUMP TO NEXT RECORD ELEMENT                  
         AR    R2,R0                                                            
         J     GETGWK02                                                         
                                                                                
GETGWK10 SR    R0,R0               DO GOAL WEEK DOWNLOAD OPTIMIZATION           
         ICM   R0,3,GWKTAB#                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3             POINT TO START OF TABLE                      
                                                                                
GETGWK12 CLI   TAB.GWKREP#,1       TEST REPLICATION FACTOR IS ONE               
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
         JCT   R0,GETGWK12         DO FOR NUMBER OF ENTRIES                     
         B     EXITY                                                            
         DROP  TAB,PRV,R2                                                       
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE CLIENT CODE FOR SPOT STEWARD                    *         
***********************************************************************         
                                                                                
VALCLT   GOTOR (#VALCLT,AVALCLT),LP_AINP                                        
         BNE   EXIT                                                             
         L     R1,ACLTREC                                                       
         CLI   CPROF-CLTRECD(R1),C'0'                                           
         BE    EXITY                                                            
         B     EXITN                                                            
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
         CLC   PESTNUM,BUYKEST     TEST CHANGE OF ESTIMATE NUMBER               
         MVC   PESTNUM,BUYKEST                                                  
         JE    *+10                NO - DON'T BOTHER SENDING IT                 
         MVC   LINEST,BUYKEST                                                   
                                                                                
         MVC   LINLIN,BUYKBUY                                                   
         CLI   BDDAY,B'01111100'   TEST MONDAY-FRIDAY ROTATION                  
         JE    *+10                YES - DON'T BOTHER SENDING IT                
         MVC   LINROT,BDDAY                                                     
         LLC   R1,BDSEDAY                                                       
         SRL   R1,4                                                             
         CHI   R1,1                TEST MONDAY START DAY                        
         JE    *+8                 YES - DON'T BOTHER SENDING IT                
         STC   R1,LINDAY                                                        
         MVC   LINSTRT,BDTIMST                                                  
         MVC   LINENDT,BDTIMEND                                                 
         CLI   BDSEC,SEC30Q        TEST DEFAULT SECOND SPOT LENGTH              
         JE    *+10                YES - DON'T BOTHER SENDING IT                
         MVC   LINSEC,BDSEC                                                     
         MVC   LINDPT,BDDAYPT                                                   
         MVC   LINSTAT,BDSTAT                                                   
         MVC   LINCIND,BDCIND                                                   
                                                                                
         L     RF,ACLTREC          FORMAT PROGRAM ADJACENCY CODE                
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
                                                                                
FMTLIN04 CLI   BUYMED,NETMEDQ      TEST THIS IS A NETWORK BUY                   
         JNE   FMTLIN08                                                         
         LA    RF,BDPROGRM+3       YES - DEAL WITH SHOW CODES                   
         LHI   R0,3                                                             
         LHI   R1,2                                                             
         BASR  RE,0                                                             
         CLI   0(RF),C'-'                                                       
         JE    FMTLIN06                                                         
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCTR  R0,RE                                                            
         J     FMTLIN08                                                         
                                                                                
FMTLIN06 BASR  RE,0                                                             
         MVC   LINPGC(0),BDPROGRM  SET PROGRAM CODE                             
         EX    R1,0(RE)                                                         
         LHI   RF,L'BDPROGRM-3                                                  
         SR    RF,R1                                                            
         LA    R1,BDPROGRM+2(R1)                                                
         BASR  RE,0                                                             
         MVC   LINPGN(0),0(R1)     SET PROGRAM NAME                             
         EX    RF,0(RE)                                                         
         J     FMTLIN10                                                         
                                                                                
FMTLIN08 MVC   LINPGN,BDPROGRM     SET PROGRAM NAME (NO CODE)                   
                                                                                
FMTLIN10 LA    R1,BDELEM           LOCATE CONTRACT NUMBER ELEMENT               
         USING IDELEM,R1                                                        
         SR    R0,R0                                                            
FMTLIN12 CLI   IDELEM,EOR          TEST END OF RECORD                           
         JE    FMTLINX                                                          
         CLI   IDELEM,IDELEMQ      TEST CONTRACT NUMBER ELEMENT                 
         JE    FMTLIN14                                                         
         IC    R0,IDELLEN                                                       
         AR    R1,R0                                                            
         J     FMTLIN12                                                         
FMTLIN14 MVC   LINCON,IDCONNO      SET CONTRACT NUMBER                          
                                                                                
FMTLINX  B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ESTABLISH MARKET/STATION SEQUENCE NUMBER                 *         
***********************************************************************         
                                                                                
GETMSS   NTR1  LABEL=NO                                                         
         LH    R0,MKSTAB#          LOOK-UP MARKET/STATION IN TABLE              
         LR    R2,R1                                                            
         LR    RF,R1                                                            
         GOTOR VBINSRCH,DMCB,(1,(R2)),AMKSTAB,(R0),MKSTABL,            +        
               L'BUYKMSTA,MKSMAXN                                               
         MVC   MKSTAB#,10(R1)      SET N'TABLE ENTRIES                          
         ICM   RF,7,1(R1)          POINT TO FOUND/ADDED ENTRY                   
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),0             SET REFERENCE# IF ADDED                      
         JE    *+10                                                             
         MVC   MKSTREF#-MKSTABD(,RF),MKSTAB#                                    
         MVC   SCDMKST#,MKSTREF#-MKSTABD(RF)                                    
         CLI   0(R1),0             SEND MARKET/STATION CODE IF ADDED            
         JE    GETMSSX                                                          
         LR    R1,R2                                                            
         MVC   SCDMKT#,0(R1)       ELSE SEND MARKET                             
         GOTOR GETSTA,(R1)                                                      
         MVC   SCDSTA,STAPQSTA     AND STATION TOO                              
                                                                                
GETMSSX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT BUY DEMO VALUES INTO SCDDEMV                     *         
***********************************************************************         
                                                                                
GETDEM   NTR1  LABEL=NO                                                         
                                                                                
         LA    R2,0(R1)                                                         
         USING BUYREC,R2                                                        
         STCM  R1,8,CALLMODE       SAVE CALLING MODE                            
                                                                                
         MVI   WORK,NDCORGQ        SET ELEMENT CODE FOR SEARCH                  
                                                                                
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
         TM    CALLMODE,X'40'      TEST SPILL-OUT DEMOS                         
         JZ    GETDEM08                                                         
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
                                                                                
GETDEM10 TM    CALLMODE,X'40'      TEST SPILL-OUT DEMOS                         
         JNZ   GETDEM14            YES - DON'T DO DEMO SEARCH                   
                                                                                
         L     RE,DUB                                                           
         LA    RF,NDEMNO                                                        
         USING NDEMNO,RF                                                        
GETDEM12 CLC   NDEMNO,0(R1)        MATCH REQUEST DEMO TO BUY DEMO               
         JE    GETDEM14                                                         
         AHI   RF,NDEMLNQ                                                       
         JCT   RE,GETDEM12                                                      
         J     GETDEM16                                                         
                                                                                
GETDEM14 MVC   SCDDEMO,NDEMNO      SET DEMO CODE                                
         MVC   FULL,NDEMRAW        SET DEMO VALUE                               
         NI    FULL,FF-NDEMMANQ                                                 
         GOTOR ADJPRC              ADJUST DEMO PRECISION                        
         MVC   SCDDEM#,FULL                                                     
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
         JNZ   GETDEM24                                                         
         DC    H'0'                                                             
                                                                                
GETDEM24 TM    0(R1),NDEMMANQ      TEST OVERRIDE DEMO                           
         JZ    GETDEM26                                                         
         MVC   PBDDEMO,NDEMNO      YES - BUILD OVERRIDE ENTRY                   
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),0(R1)     3 BYTE DEMO VALUE                            
         NI    FULL+1,FF-NDEMMANQ  TAKE OFF OVERRIDE IF ANY                     
                                                                                
         TM    FULL+1,NDEM2DEC     2 DEC BIT IN 3-BYTE DEMO VALUE?              
         JZ    *+12                                                             
         NI    FULL+1,FF-NDEM2DEC  MOVE THE 2 DEC BIT TO HOB                    
         OI    FULL,NDEM2DEC                                                    
                                                                                
         ST    RE,SAVERE           SAVE RE                                      
         GOTOR ADJPRC                                                           
         L     RE,SAVERE                                                        
         MVC   PBDDEM#,FULL+1                                                   
         AHI   RE,L'PBDDEMV        BUMP TO NEXT OUTPUT ENTRY                    
                                                                                
GETDEM26 AHI   RF,NDEMLNQ          BUMP TO NEXT DEMO                            
         AHI   R1,L'PDEMO          BUMP TO NEXT DEMO VALUE                      
         JCT   R0,GETDEM24         DO FOR NUMBER OF DEMOS                       
         MVI   PBDVALS,FF                                                       
                                                                                
GETDEMX  B     EXITY                                                            
         DROP  R1,R2,RE,RF                                                      
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
         CLI   SVS002DP,C'Y'       DOES AGENCY USE 2 DECIMAL PLACES             
         JNE   ADJPRCX             NO - LEAVE IT ALONE                          
         L     R0,FULL             ADJUST 1 DECIMAL PRECISION TO 2              
         MHI   R0,10                                                            
         ST    R0,FULL                                                          
         J     ADJPRCX                                                          
                                                                                
ADJPRC02 NI    FULL,FF-NDEM2DEC    TURN OFF 2 DECIMAL PLACE FLAG                
         CLI   SVS002DP,C'Y'       TEST AGENCY USES 2 DECIMAL PLACES            
         JE    ADJPRCX             YES - LEAVE IT ALONE                         
         L     R0,FULL             ADJUST 2 DECIMAL PRECISION TO 1              
         SRDA  R0,31               R1=2*FULL                                    
         LHI   RF,10                                                            
         DR    R0,RF                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,FULL                                                          
                                                                                
ADJPRCX  LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         DROP  R2                                                               
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
         CLI   BUYMED,NETMEDQ      FOR NETWORK - DEFAULT IS DOLLARS             
         JNE   GETCST02            UNLESS THE INDICATOR SAYS NOT                
         OC    BUYKMKTN,BUYKMKTN   EXCEPT FOR LOCAL MARKET EXPLODED BUY         
         JNZ   GETCST06            WHICH ARE ALWAYS IN PENNIES                  
         TM    BDCIND2,BDCRATPQ                                                 
         JNZ   GETCST06                                                         
         J     GETCST04                                                         
                                                                                
GETCST02 TM    BDCIND2,BDCNBRDQ    FOR OTHER MEDIA DEFAULT IS PENNIES           
         JZ    GETCST06            UNLESS THE INDICATOR SAYS NOT                
                                                                                
GETCST04 MHI   R0,100              CONVERT TO PENNIES                           
                                                                                
GETCST06 CVD   R0,DUB                                                           
         ZAP   SCDCOST1,DUB        SET COST VALUE                               
         ZAP   SCDCOST2,DUB        SET COST2 VALUE = COST                       
                                                                                
         LA    RF,BDELEM           LOCATE COST2 ELEMENTS ON BUY                 
         USING COS2ELEM,RF                                                      
         SR    R0,R0                                                            
GETCST08 CLI   COS2ELEM,EOR                                                     
         JE    GETCSTX                                                          
         CLI   COS2ELEM,COS2ELQ                                                 
         JE    GETCST12                                                         
         CLI   COS2ELEM,C2FCELQ                                                 
         JE    GETCST14                                                         
                                                                                
GETCST10 LLC   R0,COS2ELEM+1                                                    
         AR    RF,R0                                                            
         J     GETCST08                                                         
                                                                                
GETCST12 ICM   R0,15,2(RF)         SET COST 2 VALUE                             
         CVD   R0,DUB                                                           
         ZAP   SCDCOST2,DUB                                                     
         J     GETCST10                                                         
                                                                                
GETCST14 MVC   SCDC2FAC,2(RF)      SET COST 2 FACTOR                            
         J     GETCST10                                                         
                                                                                
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
         JE    GETSTA02                                                         
         CLC   STAPQNET,SPACES     TEST CABLE CHANNEL SET                       
         JE    *+8                                                              
         MVI   STAPQSTA+L'STAPQSTA-1,C'/'                                       
         J     GETSTAX                                                          
                                                                                
GETSTA02 LLC   R0,STAPQSTA+L'STAPQSTA-1                                         
         LA    RE,STAPQSTA+L'STAPQSTA-2                                         
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
         STC   R0,2(RE)                                                         
         MVI   3(RE),C' '                                                       
                                                                                
GETSTAX  B     EXITY                                                            
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
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
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
                                                                                
REQUESTX LKREQ X                                                                
         EJECT                                                                  
         LKARY T                                                                
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
         JNZ   *+6                                                              
         DC    H'0'                                                             
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
* BUILD MARKET/STATION KEY DRIVER ENTRIES FOR DESKTOP                 *         
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
* RESOLVE CLIENT DETAILS FOR BUY DOWNLOAD                             *         
***********************************************************************         
                                                                                
RESCLT   NTR1  LABEL=NO                                                         
         XC    CLTVALS(CLTVALL),CLTVALS                                         
         GOTOR (#GETCLT,AGETCLT)                                                
         BNE   EXIT                                                             
         GOTOR (#EDTCLT,AEDTCLT),DMCB,QCLTX,,QCLTA                              
         L     RF,ACLTREC                                                       
         USING CLTRECD,RF                                                       
         MVC   CLTNAME,CNAME                                                    
         TM    COPT3,COP3SPOD                                                   
         JZ    *+8                                                              
         MVI   CLTSPDS,YESQ        SET CLIENT MAY HAVE SPODS                    
                                                                                
         CLI   CPROF,C'0'                                                       
         JE    *+8                                                              
         MVI   CLTBPOL,YESQ        SET CLIENT IS BRAND/POOL                     
                                                                                
         OC    ABRD,ABRD           TEST BRAND (PRODUCT) DEFINED                 
         JNZ   RESCLTX                                                          
         ICM   RE,7,ABRD1                                                       
         JNZ   *+8                                                              
         L     RE,LP_AWMP          NO - BUILD KEY DRIVER ENTRY                  
         USING LW_D,RE                                                          
         STCM  RE,7,ABRD1                                                       
         MVI   BRDIND1,LW_TSINQ                                                 
         MVI   LW_TYPE,LW_TSINQ                                                 
         MVI   LW_DATA1,POLPRDQ                                                 
                                                                                
         CLI   CLTBPOL,YESQ        TEST BRAND POOL CLIENT                       
         JNE   RESCLT02                                                         
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVI   LW_DATA1,1                                                       
         MVI   LW_DATA1+1,POLPRDQ-1                                             
                                                                                
RESCLT02 AHI   RE,LW_LN1Q+2                                                     
         STCM  RE,7,ABRD2                                                       
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVC   LW_DATA1(L'BRCRNG),BRCRNG                                        
         AHI   RE,LW_LN1Q+L'BRCRNG                                              
         CLM   RE,15,LP_AWMP                                                    
         JL    RESCLTX                                                          
         ST    RE,LP_AWMP                                                       
                                                                                
RESCLTX  B     EXITY                                                            
         DROP  RE,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO BUY KEY (DROP PASSIVE KEYS/SET SPILL)   *         
***********************************************************************         
                                                                                
FLTBKY   MVC   MKTSTA,IOKEY+(BUYKMKTN-BUYKEY)                                   
         MVI   SPILL,NOQ                                                        
         CLI   IOKEY+(BUYKBUY-BUYKEY),FF                                        
         BER   RE                                                               
         TM    IOKEY+(BUYKBUY-BUYKEY),X'80'                                     
         JZ    FLTBKY02                                                         
         CLI   BUYMED,NETMEDQ      TEST NETWORK MEDIA                           
         JE    FLTBKY02            YES - ALWAYS EXCLUDE SPILL                   
         MVI   SPILL,YESQ          SET THIS ONE IS SPILL                        
         CLI   XSPILL,0            TEST EXCLUDING SPILL                         
         BR    RE                                                               
                                                                                
FLTBKY02 CLI   IOKEY+(BUYKBUY-BUYKEY),0                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO BUY RECORD - TEST ANY SPOT QUALIFIES    *         
***********************************************************************         
                                                                                
         USING BUYREC,R1                                                        
FLTBRD   NTR1  LABEL=NO                                                         
         CLC   BDSTART,ENDDATEB    TEST BUY OVERLAPS PERIOD                     
         BH    EXITN                                                            
         CLC   BDEND,STRDATEB                                                   
         BL    EXITN                                                            
         CLI   XZERO$,0            TEST EXCLUDING ZERO DOLLAR BUYS              
         JE    *+14                                                             
         OC    BDCOST,BDCOST       YES - APPLY COST TEST                        
         BZ    EXITN                                                            
                                                                                
         MVC   SPOTTLEN,BDSEC      SET TOTAL SECONDS LENGTH FOR FLTSPT          
         XC    SPDVALS(SPDVALL),SPDVALS                                         
         MVI   SPOTREFN,0                                                       
         LA    R1,BDELEM                                                        
         USING REGELEM,R1                                                       
FLTBRD02 LLC   R0,RLEN             BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   RCODE,EOR           TEST END OF RECORD                           
         BE    EXITN                                                            
         GOTOR FLTSPT              APPLY SPOT FILTERS                           
         JNE   FLTBRD02                                                         
         B     EXITY               AT LEAST ONE SPOT QUALIFIES                  
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO SPOT ELEMENT AND UPDATE SPOD COUNTS     *         
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
                                                                                
         CLI   CLTSPDS,0           TEST CLIENT MAY HAVE SPODS                   
         JE    FLTSPT04                                                         
         CLI   RLEN,RLPOL1LQ       IS THIS A SPOD ELEMENT                       
         JNE   FLTSPT02                                                         
         CLI   RPTIME,0                                                         
         JE    FLTSPT02                                                         
         CLC   RPTIME,SPOTTLEN                                                  
         JE    FLTSPT02                                                         
                                                                                
         CLI   RPPRD,0             YES IT IS                                    
         JNE   *+6                                                              
         DC    H'0'                SPOD MUST HAVE A BRAND ALLOCATION            
         CLC   SPDTLEN,SPOTTLEN    TEST PREVIOUS SPOD EXHAUSTED                 
         JNE   *+10                                                             
         XC    SPDVALS(SPDVALL),SPDVALS                                         
         LLC   R0,RPTIME                                                        
         LLC   RF,SPDTLEN          UPDATE TOTAL SPOD LENGTH                     
         AR    RF,R0                                                            
         CLM   RF,1,SPOTTLEN                                                    
         JNH   *+6                                                              
         DC    H'0'                SPOD TOTAL EXCEEDS BUY SECONDS               
         STC   RF,SPDTLEN                                                       
         LLC   RF,SPDCOUNT         UPDATE SPOD COUNT                            
         AHI   RF,1                                                             
         CHI   RF,6                                                             
         JNH   *+6                                                              
         DC    H'0'                TOO MANY SPODS                               
         STC   RF,SPDCOUNT                                                      
         CHI   RF,1                                                             
         JE    FLTSPT04                                                         
         J     FLTSPT06                                                         
                                                                                
FLTSPT02 XC    SPDVALS(SPDVALL),SPDVALS                                         
                                                                                
FLTSPT04 LLC   R0,SPOTREFN         BUMP SPOT REFERENCE NUMBER                   
         AHI   R0,1                                                             
         STC   R0,SPOTREFN                                                      
                                                                                
FLTSPT06 CLI   XPAIDS,0            TEST EXCLUDE PAID FILTER SET                 
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
* ROUTINE TO APPLY FILTERS TO BRAND ESTIMATE KEY (DROP POL ESTIMATES) *         
***********************************************************************         
                                                                                
FLTEST   CLC   IOKEY+(EKEYPRD-EKEY)(L'EKEYPRD),POLPRD                           
         B     SETCCC                                                           
         EJECT                                                                  
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
SETCCC   JZ    *+8                 SET CONVERSE CONDITION CODE                  
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
                                                                                
STSPTMAP DC    X'0056'             STEWARD SPOT RECORD MAP NUMBER               
STSPDMAP DC    X'0057'             STEWARD SPOD RECORD MAP NUMBER               
                                                                                
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
STBUYD#  DC    AL2(I#BUYDLD)       STEWARD BUY/GOAL DOWNLOAD                    
         DC    AL1(MAPIBUYD)                                                    
         DC    AL1(MAPIAMCE)                                                    
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
MODLCAN  DC    C'E',C'R',C'I',AL1(0)                                            
                                                                                
PZERO    DC    P'0'                                                             
EZEROS   DC    C'00000000'                                                      
DEFAGY   EQU   EZEROS              DEFAULT AGENCY CODE FOR DAYPARTS             
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
         EJECT                                                                  
WIZKEYT  LKKEY H,EKEY,SAVED        ** WIZARD ESTIMATE DRIVER **                 
         LKKEY LIT,EKEYTYPE,EKEYTYPQ                                            
         LKKEY WMP,EKEYAM,AMED                                                  
         LKKEY WMP,EKEYCLT,ACLT                                                 
         LKKEY SIN,EKEYPRD,POLPRD                                               
         LKKEY WMP,EKEYEST,AEST                                                 
         LKKEY LIT,EKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
BRDKEYT  LKKEY H,PKEY              ** BRAND KEY DRIVER TABLE **                 
         LKKEY LIT,PKEYTYPE,PKEYTYPQ                                            
         LKKEY WMP,PKEYAM,AMED                                                  
         LKKEY WMP,PKEYCLT,ACLT                                                 
         LKKEY RNG,PKEYPRD,BRCRNG                                               
         LKKEY LIT,PKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
POLKEYT  LKKEY H,EKEY              ** POL ESTIMATE KEY DRIVER TABLE **          
         LKKEY LIT,EKEYTYPE,EKEYTYPQ                                            
         LKKEY WMP,EKEYAM,AMED                                                  
         LKKEY WMP,EKEYCLT,ACLT                                                 
         LKKEY SIN,EKEYPRD,POLPRD                                               
         LKKEY WMP,EKEYEST,AEST                                                 
         LKKEY LIT,EKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
ESTKEYT  LKKEY H,EKEY              ** BRAND EST. KEY DRIVER TABLE **            
         LKKEY SIN,EKEY,SVPOLKEY,EKEYPRD-EKEY                                   
         LKKEY RNG,EKEYPRD,BRCRNG                                               
         LKKEY SIN,EKEYEST,SVPOLKEY+(EKEYEST-EKEY)                              
         LKKEY LIT,EKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
NBYKEYT  LKKEY H,BUYKEY            ** NETWORK BUY KEY DRIVER TABLE **           
         LKKEY SIN,BUYKAM,BUYAGM                                                
         LKKEY WMP,BUYKCLT,ACLT                                                 
         LKKEY WMP,BUYKPRD,ABRD1                                                
         LKKEY LIT,BUYKMKTN,0                                                   
         LKKEY WMP,BUYKSTAC,ASTA                                                
         LKKEY WMP,BUYKEST,AEST                                                 
         LKKEY ALL,BUYKBUY                                                      
         LKKEY E                                                                
                                                                                
OBYKEYT  LKKEY H,BUYKEY            ** OTHER BUY KEY DRIVER TABLE **             
         LKKEY SIN,BUYKAM,BUYAGM                                                
         LKKEY WMP,BUYKCLT,ACLT                                                 
         LKKEY WMP,BUYKPRD,ABRD1                                                
         LKKEY WMP,BUYKMKTN,AMKT                                                
         LKKEY WMP,BUYKSTAC,ASTA                                                
         LKKEY WMP,BUYKEST,AEST                                                 
         LKKEY ALL,BUYKBUY                                                      
         LKKEY E                                                                
                                                                                
GOLKEYT  LKKEY H,GKEY              ** GOAL KEY DRIVER TABLE **                  
         LKKEY LIT,GKEYTYPE,GKEYTYPQ                                            
         LKKEY SIN,GKEYAM,GOALAGM                                               
         LKKEY WMP,GKEYCLT,ACLT                                                 
         LKKEY WMP,GKEYPRD,ABRD                                                 
         LKKEY WMP,GKEYMKT,AMKT                                                 
         LKKEY WMP,GKEYEST,AEST                                                 
         LKKEY ALL,GKEYDPT,,L'GKEYDPT+L'GKEYSLN+L'GKEYSEC                       
         LKKEY ALL,GKEYAGY,,L'GKEYAGY                                           
         LKKEY RNG,GKEYPRD2,BR#RNG                                              
         LKKEY E                                                                
                                                                                
MKTKEYT  LKKEY H,MKTKEY            ** MARKET DRIVER **                          
         LKKEY LIT,MKTKTYPE,MKTKTYPQ                                            
         LKKEY SIN,MKTKMED,MEDIA                                                
         LKKEY RNG,MKTKMKT,MKTRNGE                                              
         LKKEY SIN,MKTKAGY,AGENCY                                               
         LKKEY LIT,MKTKFILL,C'0'                                                
         LKKEY E                                                                
         EJECT                                                                  
LVALUES  DS    0F                  ** LITERALS MOVED TO SAVED **                
         DC    A(FLTEST)                                                        
         DC    A(FLTBKY)                                                        
         DC    A(FLTBRD)                                                        
                                                                                
         DC    C'POL'                                                           
         DC    X'414040',C'999'                                                 
         DC    X'00',X'FE'                                                      
         DC    C'00009999'                                                      
         DC    C'T'                                                             
         EJECT                                                                  
B#AGYREC EQU   3                   IO1 - AGENCY RECORD                          
B#STAREC EQU   4                   IO2 - STATION RECORD                         
B#CLTREC EQU   4                         CLIENT RECORD                          
B#MKTREC EQU   4                         MARKET RECORD                          
B#PRDREC EQU   5                   IO3 - PRODUCT RECORD                         
B#ESTREC EQU   6                   IO4 - ESTIMATE RECORD                        
B#GOLREC EQU   6                         GOAL RECORD                            
B#BUYREC EQU   7                   IO5 - BUY RECORD                             
                                                                                
COMMEDQ  EQU   C'C'                CANADIAN COMBINED MEDIA (REQUEST)            
NETMEDQ  EQU   C'N'                CANADIAN NETWORK MEDIA                       
TELMEDQ  EQU   C'T'                TELEVISION MEDIA                             
CBLMEDQ  EQU   C'C'                CABLE MEDIA                                  
RADMEDQ  EQU   C'R'                RADIO MEDIA                                  
                                                                                
SEC30Q   EQU   30                  DEFAULT SECONDS LENGTH                       
EOR      EQU   0                   END OF RECORD ELEMENT                        
CBLSUFQ  EQU   X'B0'               SUFFIX FOR CABLE STATIONS                    
COS2ELQ  EQU   X'71'               COST2 ELEMENT                                
C2FCELQ  EQU   X'73'               COST2 FACTOR ELEMENT                         
PDELEMQ  EQU   X'22'               POST BUY DEMO ORIGINAL ELEMENT               
SDELEMQ  EQU   X'23'               POST BUY DEMO SPILL ELEMENT                  
DLOVELQ  EQU   X'24'               DEMO LOOK-UP OVERRIDE ELEMENT                
IDELEMQ  EQU   X'70'               ID ELEMENT                                   
CMELEMQ  EQU   X'66'               COMMENT ELEMENT                              
DPTKTYPQ EQU   X'08'               DAYPART RECORD CODE                          
POLPRDQ  EQU   X'FF'               POOL PRODUCT NUMBER                          
         EJECT                                                                  
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
                                                                                
WVALUES  DS    0X                  ** LITERAL VALUES **                         
                                                                                
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AFLTEST  DS    A                   A(BRAND ESTIMATE FILTER ROUTINE)             
AFLTBKY  DS    A                   A(BUY KEY FILTER ROUTINE)                    
AFLTBRD  DS    A                   A(BUY RECORD FILTER ROUTINE)                 
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
                                                                                
POLPRD   DS    CL3                 POL BRAND CODE                               
BRCRNG   DS    CL6                 BRAND CODE KEY RANGE                         
BR#RNG   DS    XL2                 BRAND NUMBER KEY RANGE                       
MKTRNGE  DS    CL8                 MARKET NUMBER RANGE                          
MEDIA    DS    C                   MEDIA CODE                                   
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
                                                                                
SVIOADDR DS    XL(L'IOADDR)        SAVED RECORD ADDRESS                         
SVBUYKEY DS    XL(L'IOKEY)         SAVED BUY KEY                                
SVPOLKEY DS    XL(L'IOKEY)         SAVED POOL ESTIMATE KEY                      
                                                                                
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
                                                                                
MAPI1    DS    X                   ** MAP INDICATOR BYTE 1 **                   
MAPIBUYD EQU   X'80'               BUY DOWNLOAD                                 
                                                                                
MAPI2    DS    X                   ** MAP INDICATOR BYTE 2 **                   
MAPIAMCE EQU   X'80'               RESOLVE AGENCY/MEDIA/CLIENT/EST              
                                                                                
BUYAGM   DS    X                   AGENCY/MEDIA CODE FOR BUY RECORDS            
BUYMED   DS    C                   MEDIA CODE FOR BUY RECORDS                   
                                                                                
SPILL    DS    C                   YESQ IF SPILL BUY                            
SPOTDATE DS    XL(L'RDATE)         SPOT DATE                                    
SPOTTLEN DS    XL(L'BDSEC)         SPOT LENGTH TOTAL                            
SPOTREFN DS    X                   SPOT REFERENCE NUMBER                        
CALLMODE DS    X                   SUB-ROUTINE CALLING MODE                     
MDDFOUND DS    C                   MAKEGOOD DEMO RECORD FOUND                   
                                                                                
GOALDATE DS    XL(L'GLWEEK)        GOAL WEEK START DATE                         
GOALAGM  DS    X                   AGENCY/MEDIA CODE FOR GOAL RECORDS           
GOALMED  DS    C                   MEDIA CODE FOR GOAL RECORDS                  
                                                                                
ANTWKEL  DS    A                   A(CURRENT NETWORK ELEMENT)                   
ANDELEM  DS    A                   A(CURRENT DEMO SPILL ELEMENT)                
ADEMOEL  DS    A                   A(CURRENT DEMO ELEMENT)                      
AMKSTAB  DS    A                   A(MARKET/STATION TABLE)                      
MKSMAXN  DS    A                   MAXIMUM N'MKSTAB ENTRIES                     
                                                                                
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
                                                                                
BRDIND   DS    X                   BRAND (NUMBER)                               
ABRD     DS    AL3                                                              
                                                                                
BRDIND1  DS    X                   BRAND (CODE)                                 
ABRD1    DS    AL3                                                              
                                                                                
BRDIND2  DS    X                   BRAND (CODE)                                 
ABRD2    DS    AL3                                                              
                                                                                
ESTIND   DS    X                   ESTIMATE                                     
AEST     DS    AL3                                                              
                                                                                
ESTIND2  DS    X                   ESTIMATE (BUILT FROM POOL ESTIMATES)         
AEST2    DS    AL3                                                              
                                                                                
DEMIND   DS    X                   DEMOS                                        
ADEM     DS    AL3                                                              
                                                                                
AGENCY   DS    CL(L'AGYKAGY)       AGENCY CODE                                  
                                                                                
STRDATEB DS    XL3                 REQUEST START DATE (BINARY)                  
ENDDATEB DS    XL3                 REQUEST END DATE (BINARY)                    
                                                                                
STRDATEC DS    XL2                 REQUEST START DATE (COMPRESSED)              
ENDDATEC DS    XL2                 REQUEST END DATE (COMPRESSED)                
STRDMONC DS    XL2                 REQUEST START MONDAY (COMPRESSED)            
                                                                                
BRDSONLY DS    X                   BRANDS ONLY (ALLOCATION)?                    
DLDGOALS DS    X                   DOWNLOAD GOALS?                              
DLDAFFID DS    X                   DOWNLOAD AFFIDAVIT DATA?                     
                                                                                
XPAIDS   DS    X                   EXCLUDE PAID SPOTS?                          
XMISSD   DS    X                   EXCLUDE MISSED SPOTS?                        
XMGOOD   DS    X                   EXCLUDE MAKEGOOD SPOTS?                      
XZERO$   DS    X                   EXCLUDE ZERO DOLLAR SPOTS?                   
XPALOC   DS    X                   EXCLUDE PRE-ALLOCATED SPOTS?                 
XSPILL   DS    X                   EXCLUDE SPILL?                               
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
                                                                                
CLTVALS  DS    0X                  ** CLIENT VALUES **                          
CLTNAME  DS    CL(L'CNAME)         CLIENT NAME                                  
CLTSPDS  DS    X                   CLIENT ALLOWS SPODS                          
CLTBPOL  DS    X                   CLIENT IS BRAND/POOL                         
CLTVALL  EQU   *-CLTVALS                                                        
                                                                                
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
                                                                                
STRDATE  DS    CL6                 START DATE                                   
ENDDATE  DS    CL6                 END DATE                                     
                                                                                
DEMLSTMX EQU   255                 MAXIMUM N'DEMLST ENTRIES                     
DEMLST   DS    (DEMLSTMX)CL4,X     DEMO LIST                                    
                                                                                
         ORG   OUTVALS                                                          
GOLVALS  DS    0X                  ** GOAL HEADER VALUES **                     
GOLBRDS  DS    0XL(L'GOLBRD1+L'GOLBRD2)                                         
GOLBRD1  DS    CL(L'GKEYPRD)       BRAND1 CODE                                  
GOLBRD2  DS    CL(L'GKEYPRD2)      BRAND2 CODE                                  
GOLSECS  DS    0XL(L'GOLSEC1+L'GOLSEC2)                                         
GOLSEC1  DS    XL(L'GKEYSEC)       BRAND1 SECONDS LENGTH                        
GOLSEC2  DS    XL(L'GKEYSEC)       BRAND2 SECONDS LENGTH                        
GOLEST   DS    XL(L'GKEYEST)       ESTIMATE NUMBER                              
GOLMKT   DS    XL(L'GKEYMKT)       MARKET NUMBER                                
GOLDPT   DS    CL(L'GKEYDPT)       DAYPART CODE                                 
GOLLKIN  DS    XL(L'GDLKGOAL)      GOAL LOCKIN DATE                             
GOLPURP  DS    XL(L'GDIDR)         GOAL PURPOSE CODE                            
GOLVALL  EQU   *-GOLVALS                                                        
                                                                                
         ORG   OUTVALS                                                          
CORPDEMO DS    XL(L'EDEMLIST)      CORPORATE DEMO FROM POOL ESTIMATE            
                                                                                
DPTCODE  DS    CL(L'EDAYMENU)      MASTER DAYPART CODE                          
                                                                                
DPTTAB   DS    (DPTTMAXN)CL(DPTTABL),X                                          
                                                                                
STASTAL  EQU   L'STAPQSTA+L'STAPQNET                                            
                                                                                
STAVALS  DS    0X                  ** STATION VALUES **                         
STASEND  DS    C                   YESQ TO SEND STATION VALUES                  
STAMED   DS    CL(L'QMEDA)         MEDIA CODE                                   
STASTA   DS    CL(STASTAL)         STATION(/CABLE NETWORK)                      
STAMKT#  DS    XL(L'BUYKMKTN)      MARKET NUMBER                                
STAVALL  EQU   *-STAVALS                                                        
                                                                                
LINVALS  DS    0X                  ** BUYLINE VALUES **                         
LINEST   DS    XL(L'BUYKEST)       ESTIMATE NUMBER                              
LINLIN   DS    X                   BUY LINE NUMBER                              
LINROT   DS    XL(L'BDDAY)         ROTATION                                     
LINDAY   DS    X                   START DAY NUMBER                             
LINSTRT  DS    XL(L'BDTIMST)       START TIME                                   
LINENDT  DS    XL(L'BDTIMEND)      END TIME                                     
LINSEC   DS    XL(L'BDSEC)         SECONDS LENGTH                               
LINDPT   DS    CL(L'BDDAYPT)       DAYPART CODE                                 
LINPGN   DS    CL(L'BDPROGRM)      PROGRAM NAME                                 
LINPGC   DS    CL5                 SHOW CODE (XXXX, =XXX OR =XXXX)              
LINCON   DS    CL12                CONTRACT NUMBER                              
LINADJ   DS    CL2                 PROGRAM ADJACENCY CODE                       
LINSTAT  DS    XL(L'BDSTAT)        STATUS                                       
LINCIND  DS    XL(L'BDCIND)        COST INDICATORS                              
LINVALL  EQU   *-LINVALS                                                        
                                                                                
SCDVALS  DS    0X                  ** STATION/COST/DEMO VALUES **               
SCDSPILL DS    C                   YESQ IF A SPILL BUY                          
SCDSTA   DS    CL(STASTAL)         STATION/NETWORK                              
SCDMKT#  DS    XL(L'BUYKMKTN)      MARKET NUMBER                                
SCDMKTN  DS    XL(L'MKTNAME)              NAME                                  
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
SCDVALL  EQU   *-SCDVALS                                                        
                                                                                
PBDVALS  DS    0X                  ** BUY POST DEMOS **                         
PBDDEMV  DS    0XL(L'PBDDEMO+L'PBDDEM#)                                         
PBDDEMO  DS    XL3                 DEMO CODE                                    
PBDDEM#  DS    XL3                 DEMO VALUE                                   
         DS    (EDEMLSTN)XL(L'PBDDEMV)                                          
                                                                                
SPDVALS  DS    0X                  ** SPOD VALUES **                            
SPDTLEN  DS    X                   TOTAL SPOD LENGTH                            
SPDCOUNT DS    X                   SPOD COUNT                                   
SPDSTAT  DS    X                   ** SPOD STATUS **                            
SPDFSENT EQU   X'80'               FIRST SPOT FOR SPOD SENT                     
SPDVALL  EQU   *-SPDVALS                                                        
         ORG                                                                    
                                                                                
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
                                                                                
PRVVALS  DS    0X                  ** PREVIOUS VALUES **                        
PESTNUM  DS    XL(L'BUYKEST)                                                    
PMKTSTA  DS    0XL(L'PMKTNUM+L'PNETSTA)                                         
PMKTNUM  DS    XL(L'BUYKMKTN)                                                   
PNETSTA  DS    XL(L'BUYKSTAC)                                                   
PBRDNUM  DS    XL(L'GKEYPRD+L'GKEYPRD2)                                         
PSECLEN  DS    XL(L'GKEYSEC*2)                                                  
PDPTCOD  DS    CL(L'GKEYDPT)                                                    
PCPPEST  DS    XL(L'GDCPPES)                                                    
PCPPEST2 DS    XL(L'GDCPPES2)                                                   
PRVVALL  EQU   *-PRVVALS                                                        
                                                                                
MKTSTA   DS    0XL(L'MKTNUM+L'STACOD)                                           
MKTNUM   DS    XL(L'BUYKMKTN)                                                   
STACOD   DS    XL(L'BUYKSTAC)                                                   
                                                                                
STSTAB#  DS    H                   N'ENTRIES IN STSTAB                          
GWKTAB#  DS    H                   N'ENTRIES IN GWKTAB                          
MKSTAB#  DS    H                   N'ENTRIES IN MKSTAB                          
MKSTAB   DS    (16*ONEK)X          THIS AREA IS ONLY USED OFFLINE               
MKSTMAX2 EQU   (*-MKSTAB)/MKSTABL  MAXIMUM N'TABLE ENTRIES (OFFLINE)            
MKSTMAX1 EQU   (IO7LQ+IO8LQ)/MKSTABL   MAX N'TABLE ENTRIES (ONLINE)             
                                                                                
MKSTABD  DSECT                     ** MARKET/STATION TABLE **                   
MKSTMKST DS    XL(L'BUYKMSTA)      MARKET/STATION CODE                          
MKSTREF# DS    XL2                 MARKET/STATION REFERENCE#                    
MKSTABL  EQU   *-MKSTABD                                                        
                                                                                
DPTTABD  DSECT                     ** DAYPART TABLE **                          
DPTTALPH DS    C                   DAYPART ALPHA CODE                           
DPTTNAME DS    CL3                 DAYPART NAME                                 
DPTTMAST DS    CL3                 MASTER DAYPART NAME                          
DPTTMSCD DS    X                   MASTER/SLAVE CODE                            
DPTTABL  EQU   *-DPTTABD                                                        
DPTTMAXN EQU   36                  MAXIMUM N'TABLE ENTRIES                      
                                                                                
STSTABD  DSECT                     ** STEWARD SPOT VALUES **                    
STSMAPN  DS    XL(L'LD_CODE)       MAP NUMBER                                   
STSREF#  DS    XL(L'SPOTREFN)      SPOT REFERENCE NUMBER                        
STSREP#  DS    X                   SPOT REPLICATION FACTOR                      
                                                                                
STSCOMP  DS    0X                  ** DATA FOR DUPLICATION COMPARE **           
STSDATE  DS    XL(L'RDATE)         SPOT DATE                                    
STSBRDS  DS    0XL6                ** BRAND ALLOCATIONS **                      
STSBRD1  DS    XL(L'RPPRD)         ALLOCATED SPOD1/PRIMARY BRAND                
STSBRD2  DS    XL(L'RPPRD)         ALLOCATED SPOD2/PIGGYBACK BRAND              
STSBRD3  DS    XL(L'RPPRD)         ALLOCATED SPOD3 BRAND                        
STSBRD4  DS    XL(L'RPPRD)         ALLOCATED SPOD4 BRAND                        
STSBRD5  DS    XL(L'RPPRD)         ALLOCATED SPOD5 BRAND                        
STSBRD6  DS    XL(L'RPPRD)         ALLOCATED SPOD6 BRAND                        
STS1LEN  DS    XL(L'RPTIME)        LENGTH FOR SPOD1/PRIMARY BRAND               
STS2LEN  DS    XL(L'RPTIME)        LENGTH FOR SPOD2/PIGGYBACK BRAND             
STSAFDT  DS    XL(L'ADATE)         AFFIDAVIT DATE                               
STSAFTM  DS    XL(L'ATIME)         AFFIDAVIT TIME                               
STSSTAT  DS    0XL4                ** SPOT STATUS **                            
STSSTAT1 DS    X                   ** STATUS BYTE 1 **                          
STSSTAT2 DS    X                   ** STATUS BYTE 2 **                          
STSSTAT3 DS    X                   ** STATUS BYTE 3 **                          
STSSB1PA EQU   X'02'               BRAND 1 PAYS ALL (PIGGYBACKS)                
STSSFSPD EQU   X'01'               FIRST SPOT FOR NEW SPOD                      
STSSTAT4 DS    X                   ** STATUS BYTE 4 **                          
STSSAFID EQU   X'80'               AFFIDAVIT DATA AVAILABLE                     
STSSALOC EQU   X'40'               ALLOCATED                                    
STSSPRAL EQU   X'20'               PRE-ALLOCATED                                
STSSMKGD EQU   X'10'               MAKEGOOD                                     
STSSMISD EQU   X'08'               MISSED                                       
STSSPOTO EQU   X'04'               +OTO                                         
STSSMOTO EQU   X'02'               -OTO                                         
STSSPAID EQU   X'01'               PAID                                         
STSCOST  DS    PL6                 COST OVERRIDE                                
STSCOMPL EQU   *-STSCOMP                                                        
                                                                                
STSTABL  EQU   *-STSTABD                                                        
                                                                                
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
GWKTABL  EQU   *-GWKTABD                                                        
                                                                                
DEMTABD  DSECT                     ** DEMO TABLE **                             
DEMTCOD  DS    0CL(L'DEMTMOD+L'DEMTNUM)                                         
DEMTMOD  DS    CL1                 DEMO MODIFIER                                
DEMTNUM  DS    CL3                 DEMO NUMBER                                  
DEMTNAM  DS    0CL(L'DEMTNMOD+L'DEMTNNAM)                                       
DEMTNMOD DS    CL1                 DEMO MODIFIER                                
DEMTNNAM DS    CL6                 DEMO NAME                                    
DEMTABL  EQU   *-DEMTABD                                                        
         EJECT                                                                  
                                                                                
* SPLNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
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
                                                                                
SAVERE   DS    A                   SAVE RE                                      
SAVERF   DS    A                   SAVE RF                                      
SAVER0   DS    A                   SAVE R0                                      
SAVER1   DS    A                   SAVE R1                                      
SAVER2   DS    A                   SAVE R2                                      
SAVER3   DS    A                   SAVE R3                                      
SAVER4   DS    A                   SAVE R4                                      
                                                                                
SVIOVALS DS    XL(IOVALL)          SAVED I/O VALUES                             
                                                                                
LBUYVALS DS    0X                  ** LAST BUY VALUES **                        
LBUYGBKY DS    XL(GBSTA+L'GBSTA-GBAGYMD)                                        
LBUYVALL EQU   *-LBUYVALS                                                       
                                                                                
* OTHER INCLUDED DSECTS                                                         
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENDAYPT                                                     
       ++INCLUDE SPGENESTD                                                      
       ++INCLUDE SPGENEQU                                                       
       ++INCLUDE SPGETBUBLD                                                     
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPTRCMML                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENSTEQ                                                      
       ++INCLUDE SPGENSDEF                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPLNK11   09/12/17'                                      
         END                                                                    
