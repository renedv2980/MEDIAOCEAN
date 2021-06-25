*          DATA SET RELNK23    AT LEVEL 012 AS OF 05/02/14                      
*PHASE T82B23A                                                                  
RELNK23  TITLE '- RepPak Station Xpress Upload - Offer update'                  
         PRINT NOGEN                                                            
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,WORKERKEY=RORD,SEGMENT=Y,LINKIO=Y,               +        
               APPEND=Y,REQUEST=*,CODE=CODE,IDF=Y,                     +        
               SYSPHASE=SYSPHASE,SYSTEM=REPSYSQ,FILES=FILES,           +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#CON,RCONRECD,     +        
               B#BUY,RBUYRECD),RLEN=2500,LOADFACSOFF=Y                          
                                                                                
B#CON    EQU   3                   RCONRECD                                     
ARCONREC EQU   LP_BLKS+((B#CON-1)*L'LP_BLKS),,C'A'                              
B#BUY    EQU   4                   RBUYRECD                                     
ARBUYREC EQU   LP_BLKS+((B#BUY-1)*L'LP_BLKS),,C'A'                              
         EJECT                                                                  
                                                                                
CODE     NMOD1 0,**RL23**                                                       
                                                                                
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
         ST    RD,WKBASERD                                                      
         USING OFFICED,OFCBLK                                                   
                                                                                
INIT04   MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   AMQIO,CMQIO-COMFACSD(RF)                                         
*                                                                               
         MVI   FATALERR,C'N'                                                    
         DROP  R6,R7,RB                                                         
                                                                                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
                                                                                
* * * *  BRAS  RE,SETGLOBB         Set Globber values (deactived)               
                                                                                
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   LP_CMODE,RRUNSTRQ   Test first for run                           
         JNE   INIREQ                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY               No                                           
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)                                                     
         MVC   LP_AUIR1,AROUT1     Set A(index routines 1)                      
         GOTOR (RF),DMCB,('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)                                                     
         MVC   LP_AUIR2,AROUT2     Set A(index routines 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Initialize download/upload variables                                *         
***********************************************************************         
                                                                                
INIREQ   CLI   LP_CMODE,RINIREQQ   Test 'initialize request'                    
         JE    *+12                                                             
         CLI   LP_CMODE,RPRCWRKQ   Test 'process work' mode                     
         JNE   RUNREQ                                                           
         MVC   LP_BLKS+((B#CON-01)*L'LP_BLKS)(L'AIOS),AIO3                      
         MVC   LP_BLKS+((B#BUY-01)*L'LP_BLKS)(L'AIOS),AIO4                      
         MVC   AGY,LP_AGY          Set agency code                              
*                                                                               
         LA    R0,QVALUES          Clear request values                         
         LHI   R1,QVALUEL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    MSSAGEID,MSSAGEID   Init message ID                              
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RREPKEY,RE          Look up master Rep code                      
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGY                                                     
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   IOKEY(L'RREPKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                No Rep record                                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO1'                           
         L     R3,AIO1                                                          
         LA    R3,RREPELEM-RREPREC(R3)                                          
         CLI   0(R3),X'01'                                                      
         JE    *+6                                                              
         DC    H'0'                No Rep element                               
*                                                                               
         USING RREPELEM,R3                                                      
         MVC   MASTRCOD,AGY        Init master Rep code                         
         CLC   RREPMAST,SPACES     Have master control?                         
         JNH   *+20                                                             
         CLC   RREPMAST,=X'FFFF'   Master control?                              
         JE    *+10                                                             
         MVC   MASTRCOD,RREPMAST   Set master Rep code                          
         DROP  R3                                                               
*                                                                               
         MVI   RUNINDS,0                                                        
         MVI   RPYIND1,0                                                        
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   LP_CMODE,RRUNREQQ   Test 'run request' mode                      
         JNE   RUNEND                                                           
                                                                                
         CLI   FATALERR,C'Y'       Fatal error occurred?                        
         JE    EXITY                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNREQ02                                                         
                                                                                
         L     RF,AMASTC           Set trace option                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,REPDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,REPFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENFIL,(4,0),0                               
                                                                                
RUNREQ02 L     RF,LP_ALPXD         Extract server id                            
         MVC   MQSVR,LP_MQSVR-LP_XD(RF)                                         
                                                                                
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JNZ   EXITN                                                            
         J     EXITY               Exit back to DDLINK                          
         EJECT                                                                  
***********************************************************************         
* Last for upload                                                     *         
***********************************************************************         
                                                                                
RUNEND   CLI   LP_CMODE,RRUNENDQ   Test last time request to server             
         JNE   EXITY                                                            
                                                                                
         GOTOR VDATAMGR,DMCB,DMCOMMIT,0                                         
                                                                                
         L     R3,LP_ALPXD         Point to LP_XD                               
         USING LP_XD,R3                                                         
         OC    LP_LKEY1,LP_LKEY1   Test any key to unlock                       
         JZ    EXITY                                                            
         GOTOR VLOCKET,DMCB,('LKUNGLQ',LP_LKEY1)                                
         CLI   4(R1),0                                                          
         JE    EXITY                                                            
         DC    H'0'                Die for now                                  
         DROP  R3                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Offer update                                                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0H                                                               
OFFERUP  LKREQ H,Q#UPOFR,NEXTREQ=REQEND,ROUTINE=OFRUPD,NEWREC=Y                 
                                                                                
OfficeCd LKREQ F,001,(D,B#SAVED,QOFFCODE),CHAR,TEXT=(*,OFFCDLIT),COL=*          
StaCaLet LKREQ F,002,(D,B#SAVED,QSTACODE),CHAR,TEXT=(*,STACLLIT),COL=*          
OrderNum LKREQ F,003,(D,B#SAVED,QORDNUMB),SPAK,TEXT=(*,ORDNOLIT),COL=*          
OffGrpCd LKREQ F,004,(D,B#SAVED,QOFFGRPC),CHAR,TEXT=(*,OFRGCLIT),COL=*          
ActnCode LKREQ F,005,(D,B#SAVED,QACTCODE),CHAR,TEXT=(*,ACTCDLIT),COL=*          
GrpComms LKREQ F,006,(I,B#SAVED,QGRPCIND),CHAR,OLEN=MAXGCMLQ,          +        
               LIST=F,SORT=N,TEXT=(*,GRCOMLIT),COL=*,DELIM=X'00'                
StaUsrNm LKREQ F,010,(D,B#SAVED,QSTAUSRN),CHAR,TEXT=(*,STAUNLIT),COL=*          
StaUsrEm LKREQ F,011,(D,B#SAVED,QSTAUEML),CHAR,TEXT=(*,STAUELIT),COL=*          
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
MisLine# LKREQ F,020,(I,B#SAVED,QMISIND),UBIN,TEXT=(*,MSLN#LIT),       +        
               OLEN=L'MISLINE#,ARRAY=S                                          
MisStrDt LKREQ F,021,,BDAT,TEXT=(*,MSSTDLIT),OLEN=MAXDTCHQ                      
MisEndDt LKREQ F,022,,BDAT,TEXT=(*,MSENDLIT),OLEN=MAXDTCHQ                      
MisSptWk LKREQ F,023,,UBIN,TEXT=(*,MSSPWLIT),OLEN=L'MISSPTWK,ARRAY=E            
                                                                                
MIS_D    DSECT ,                   ** Missed buys array **                      
MISLINE# DS    XL(L'RMKGMGLI)      Missed line number                           
MISSTRDT DS    XL(L'RMKGMGD1)      Missed start date                            
         DS    XL(MAXDTCHQ-L'MISSTRDT)                                          
MISENDDT DS    XL(L'RMKGMGD2)      Missed end date                              
         DS    XL(MAXDTCHQ-L'MISENDDT)                                          
MISSPTWK DS    XL(L'RMKGMGSP)      Missed spots per week                        
MISLNQ   EQU   *-MIS_D             Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
OfLLine# LKREQ F,040,(I,B#SAVED,QOFLIND),UBIN,TEXT=(*,OLLN#LIT),       +        
               OLEN=L'OFLLINE#,ARRAY=S                                          
OfLType_ LKREQ F,041,,CHAR,TEXT=(*,OLOTYLIT),OLEN=L'OFLTYPE_                    
OfLRate_ LKREQ F,042,,CBIN,TEXT=(*,OLRATLIT),OLEN=L'OFLRATE_                    
OfLLeng_ LKREQ F,043,,UBIN,TEXT=(*,OLLENLIT),OLEN=L'OFLLENG_                    
OfLLenIn LKREQ F,044,,CHAR,TEXT=(*,OLLNILIT),OLEN=L'OFLLENIN                    
OfLProgm LKREQ F,045,,CHAR,TEXT=(*,OLPRGLIT),OLEN=L'OFLPROGM,          +        
               DELIM=X'00'                                                      
OfLLnCm1 LKREQ F,046,,CHAR,TEXT=(*,OLLCMLIT),OLEN=L'OFLLNCM1,          +        
               DELIM=X'00'                                                      
OfLLnCm2 LKREQ F,047,,CHAR,TEXT=(*,OLLCMLIT),OLEN=L'OFLLNCM2,          +        
               DELIM=X'00'                                                      
OfLPrgRa LKREQ F,048,,SPAK,TEXT=(*,OLPGRLIT),OLEN=L'OFLPRGRA,ARRAY=E            
                                                                                
OFL_D    DSECT ,                   ** Offer line array **                       
OFLLINE# DS    XL(L'RMKGKRTY)      Offer line number                            
OFLTYPE_ DS    CL1                 Offer type                                   
OFLRATE_ DS    XL(L'RMKGCOS)       Offered rate                                 
OFLLENG_ DS    XL(L'RMKGDUR)       Offered length                               
OFLLENIN DS    CL1                 Offered length indicator (Min/Sec)           
OFLPROGM DS    CL34                Offered program                              
OFLLNCM1 DS    CL(MAXCOMLQ)        Offered line comment 1                       
OFLLNCM2 DS    CL(MAXCOMLQ)        Offered line comment 2                       
OFLPRGRA DS    PL5                 Offered program rating                       
OFLLNQ   EQU   *-OFL_D             Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
DTmLine# LKREQ F,070,(I,B#SAVED,QDTMIND),UBIN,TEXT=(*,OLX02LIT),       +        
               OLEN=L'DTMLINE#,ARRAY=S                                          
DTmStrDy LKREQ F,071,,UBIN,TEXT=(*,OLSTDLIT),OLEN=L'DTMSTRDY                    
DTmEndDy LKREQ F,072,,UBIN,TEXT=(*,OLENDLIT),OLEN=L'DTMENDDY                    
DTmDays_ LKREQ F,073,,CHAR,TEXT=(*,OLDAYLIT),OLEN=L'DTMDAYS_                    
DTmStrTm LKREQ F,074,,UBIN,TEXT=(*,OLSTTLIT),OLEN=L'DTMSTRTM                    
DTmEndTm LKREQ F,075,,UBIN,TEXT=(*,OLENTLIT),OLEN=L'DTMENDTM                    
DTmEtCC? LKREQ F,076,,CHAR,TEXT=(*,OLECCLIT),OLEN=L'DTMETMCC,ARRAY=E            
                                                                                
DTM_D    DSECT ,                   ** Day/Time element array **                 
DTMLINE# DS    XL(L'RMKGKRTY)      Offer line number                            
DTMSTRDY DS    XL(L'RMKGDYIN)      Start day                                    
DTMENDDY DS    XL(L'RMKGDYIN)      End day                                      
DTMDAYS_ DS    CL7                 Days                                         
DTMSTRTM DS    XL(L'RMKGDYT1)      Start time                                   
DTMENDTM DS    XL(L'RMKGDYT2)      End time                                     
DTMETMCC DS    CL1                 End time is CC?  Y/N switch                  
DTMLNQ   EQU   *-DTM_D             Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
EfDLine# LKREQ F,100,(I,B#SAVED,QEFDIND),UBIN,TEXT=(*,OLX03LIT),       +        
               OLEN=L'EFDLINE#,ARRAY=S                                          
EfDStrDt LKREQ F,101,,BDAT,TEXT=(*,OLESDLIT),OLEN=MAXDTCHQ                      
EfDEndDt LKREQ F,102,,BDAT,TEXT=(*,OLEEDLIT),OLEN=MAXDTCHQ                      
EfDDays_ LKREQ F,103,,CHAR,TEXT=(*,OLWKALIT),OLEN=L'EFDWAWIN                    
EfDStrTm LKREQ F,104,,UBIN,TEXT=(*,OLOSPLIT),OLEN=L'EFDSPTWK,ARRAY=E            
                                                                                
EFD_D    DSECT ,                   ** Day/Time element array **                 
EFDLINE# DS    XL(L'RMKGKRTY)      Offer line number                            
EFDSTRDT DS    XL(L'RMKGDTST)      Start date                                   
         DS    XL(MAXDTCHQ-L'EFDSTRDT)                                          
EFDENDDT DS    XL(L'RMKGDTED)      End date                                     
         DS    XL(MAXDTCHQ-L'EFDENDDT)                                          
EFDWAWIN DS    CL1                 Week/Alternate Week Indicator                
EFDSPTWK DS    XL(L'RMKGDTNW)      Spots per week                               
EFDLNQ   EQU   *-EFD_D             Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
         LKREQ E                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
OFRUPD   CLI   FATALERR,C'Y'       Fatal error occurred?                        
         JE    EXITY                                                            
         BRAS  RE,CHKGLOBB         Check Globber control object                 
         JE    EXITY                                                            
         BRAS  RE,CHKACTCD         Check action code                            
         JNE   EXITY                                                            
         BRAS  RE,CHKUPDMD         Check update mode                            
         JNE   EXITY                                                            
         BRAS  RE,PRCOFRHD         Process offer header                         
         JNE   EXITY                                                            
         BRAS  RE,PRCOFRLN         Process offer lines                          
         JNE   EXITY                                                            
         BRAS  RE,DELOFRLN         Delete offer lines                           
         JNE   EXITY                                                            
         BRAS  RE,PRCBUYLN         Process buy lines                            
         JNE   EXITY                                                            
         BRAS  RE,FNLOFRHD         Finalize offer header                        
         JNE   EXITY                                                            
         BRAS  RE,UPDCONRC         Update contract record                       
         JNE   EXITY                                                            
         BRAS  RE,SNDREPLY         Send reply record after completion           
         BRAS  RE,PRCMQMSG         Process MQ message                           
         BRAS  RE,GLOBBCON         Call CON program                             
         J     EXITY                                                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
REQEND   LKREQ X                                                                
         EJECT                                                                  
                                                                                
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
                                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETGLOBB NTR1  BASE=*,LABEL=*      Set Globber object                           
*                                                                               
         LA    R2,ELEM2                                                         
         USING GLRLINCD,R2                                                      
         GOTOR VGLOBBER,DMCB,=C'GETD',ELEM2,GLRLINLQ,GLRKLINC                   
         TM    DMCB+8,X'10'                                                     
         JNZ   SETGL30                                                          
         CLC   =C'B=70',GLRLINRS   Offer Update request?                        
         JE    EXIT                                                             
         BRAS  RE,DELGLLOB         Delete Globber Link object                   
*                                                                               
SETGL30  L     RE,ATWA             Save Link request command                    
         USING TWAD,RE                                                          
         CLC   =C'B=70',LNKINP+4   Offer Update request?                        
         JNE   EXIT                                                             
         XC    ELEM2,ELEM2         Build Link command object (save it)          
         MVI   GLRLINLN,8          Save upload request and office code          
         MVC   GLRLINFD(8),LNKINP+4                                             
         GOTOR VGLOBBER,DMCB,=C'PUTD',ELEM2,GLRLINLQ,GLRKLINC                   
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2,RE                                                            
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* Link Globber object is taken out for Offer Update request                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DELGLLOB NTR1  BASE=*,LABEL=*      Delete Globber Link object                   
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'DELE',ELEM2,GLRLINLQ,GLRKLINC                   
         J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKGLOBB NTR1  BASE=*,LABEL=*      Check Globber control object                 
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'GETD',ELEM2,GLCONLNQ,GLRKACT                    
         TM    DMCB+8,X'10'                                                     
         JNZ   EXITN                                                            
         GOTOR VGLOBBER,DMCB,=C'DELE',,,GLRKACT                                 
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,ELEM2                                                         
         USING GLCONNUM,R2                                                      
         OC    GLCONERR,GLCONERR   Came back with error?                        
         JZ    CHKGL20                                                          
         XC    ERRORMSG,ERRORMSG                                                
         SR    RF,RF                                                            
         ICM   RF,3,GLCONERR                                                    
         BRAS  RE,GET_ETXT                                                      
         BRAS  RE,REPLYERR         Reply error message                          
         J     CHKGL30                                                          
*                                                                               
CHKGL20  BRAS  RE,REPLYCMG         Reply completion message                     
*                                                                               
CHKGL30  BRAS  RE,DELGLLOB         Delete Globber Link object                   
         J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
GET_ETXT LR    R0,RE                                                            
         GOTOR VGETTXT,DMCB+12,(RF),(60,ERRMSGST),(C'E',DMCB),0,0,0             
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBBCON NTR1  BASE=*,LABEL=*      Call CON program                             
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JNE   EXITY                                                            
*                                                                               
         XC    ELEM2,ELEM2         Build Globber control object                 
         LA    R1,ELEM2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    From Rep/Link                                
         MVC   GLVXFRPR,=C'LIN'                                                 
         MVC   GLVXTOSY,=C'REP'    To Rep/Con                                   
         MVC   GLVXTOPR,=C'CON'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
         GOTOR VGLOBBER,DMCB,=C'PUTD',ELEM2,24,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GLOBC30  XC    ELEM2,ELEM2         Build Contract action object                 
         LA    R3,ELEM2                                                         
         USING GLCONNUM,R3                                                      
         GOTOR (RFCONNUM,AREPFACS),DMCB,(2,SVCON9CM),(5,GLCONNUM)               
GLOBC34  MVC   GLCONCA(3),=C'MGS'                                               
         OI    GLCONFLG,GLCONRPQ   Request reply                                
         GOTOR VGLOBBER,DMCB,=C'PUTD',ELEM2,GLCONLNQ,GLRKACT                    
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         DROP  R3                                                               
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKACTCD NTR1  BASE=*,LABEL=*      Check action code                            
*                                                                               
         MVI   SVMGOFLG,0          Init makegood offer add/del'd flag           
*                                                                               
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    CKACC10                                                          
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    CKACC10                                                          
         CLI   QACTCODE,QAC_DELQ   Delete (MGX)?                                
         JE    CKACC10                                                          
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JNE   CKACC05X                                                         
         OC    QORDNUMB,QORDNUMB   Have order number?                           
         JZ    CKACC42X                                                         
         J     CKACC52                                                          
*                                                                               
CKACC05X XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEINVAL),TXEINVAL                                    
         MVC   ERRORMSG+L'TXEINVAL(L'ACTCDLIT),ACTCDLIT                         
         J     RPLYERRX                                                         
*                                                                               
CKACC10  CLC   QOFFGRPC,SPACES     Have offer group code?                       
         JH    CKACC20                                                          
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    CKACC40                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'OFRGCLIT),OFRGCLIT                         
         J     RPLYERRX                                                         
*                                                                               
CKACC20  CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JNE   CKACC40                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEINVAL),TXEINVAL                                    
         MVC   ERRORMSG+L'TXEINVAL(L'OFRGCLIT),OFRGCLIT                         
         J     RPLYERRX                                                         
*                                                                               
CKACC40  CLC   QOFFCODE,SPACES     Have office code?                            
         JH    CKACC42F                                                         
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'OFFCDLIT),OFFCDLIT                         
         J     RPLYERRX                                                         
CKACC42F CLC   QSTACODE,SPACES     Have station call letter?                    
         JH    CKACC42M                                                         
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'STACLLIT),STACLLIT                         
         J     RPLYERRX                                                         
CKACC42M OC    QORDNUMB,QORDNUMB   Have order number?                           
         JNZ   CKACC50                                                          
CKACC42X XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'ORDNOLIT),ORDNOLIT                         
         J     RPLYERRX                                                         
*                                                                               
CKACC50  BRAS  RE,CKMSBARY         Check for missed buy data array              
         JE    *+20                                                             
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMSBYI),TXEMSBYI                                    
         J     RPLYERRX                                                         
         BRAS  RE,CKOFLARY         Check for offer line data array              
         JE    *+20                                                             
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEOFRLI),TXEOFRLI                                    
         J     RPLYERRX                                                         
         BRAS  RE,CKDTFARY         Check for day/time eff date arrays           
         JE    *+20                                                             
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEDTEFF),TXEDTEFF                                    
         J     RPLYERRX                                                         
*                                                                               
         CLI   SVOFRTYP,C'M'       Makegood offer type?                         
         JNE   CKACC52                                                          
         OC    NUM_MIS_,NUM_MIS_   Have missed buy data?                        
         JNE   CKACC52                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'MSDATLIT),MSDATLIT                         
         J     RPLYERRX                                                         
*                                                                               
CKACC52  ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),QORDNUMB                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   SVCON9CM,WORK                                                    
         GOTOR (RFCONNUM,AREPFACS),DMCB,(2,SVCON9CM),(3,SVCON9RV)               
*                                                                               
         XC    SVOFFGRC,SVOFFGRC   Init offer group code for ADD                
*                                                                               
         BRAS  RE,BLDCKEY          Build contract record key                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO3'                            
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         JE    CKACC54                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEINVAL),TXEINVAL                                    
         MVC   ERRORMSG+L'TXEINVAL(L'ORDNOLIT),ORDNOLIT                         
         J     RPLYERRX                                                         
*                                                                               
CKACC54  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,GTCONVAL         Get contract values                          
*                                                                               
         J     EXITY                                                            
*                                                                               
RPLYERRX BRAS  RE,REPLYERR         Reply error message                          
         BRAS  RE,DELGLLOB         Delete Globber Link object                   
         J     EXITN                                                            
*                                                                               
GTCONVAL ST    RE,SAVE_RE                                                       
         LA    R0,SVCONVAL         Init contract values to be saved             
         LHI   R1,SVCONVLQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO3             Point to contract record                     
         USING RCONREC,R2                                                       
         MVC   SVCONREP,RCONKREP   Contract rep code                            
         MVC   SVCONSTA,RCONKSTA   Contract station call letters                
         MVC   SVCONCON,RCONKCON   Contract #                                   
         GOTOR (RFCONNUM,AREPFACS),DMCB,(1,RCONKCON),(5,SVCON#CH)               
         CLI   RCONCODE,X'01'      Have contract description elem?              
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCONMOD,RCONMOD    Contract mod number                          
         DROP  R2                                                               
*                                                                               
         BRAS  RE,CKSIDE                                                        
*                                                                               
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'        Look for Send Info elem                      
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   GTCVAL30                                                         
         USING RCONSEND,R2                                                      
         MVC   SVCONCRV,RCONSRV    Current rep version                          
         MVC   SVCONCSV,RCONSSV    Current station version                      
         TM    RCONSENF,X'80'      Last send by rep?                            
         JZ    *+16                                                             
         MVC   SVLASSDT,RCONSRDT                                                
         MVC   SVSNDTIM,RCONSRTI                                                
         TM    RCONSENF,X'40'      Last send by station?                        
         JZ    *+16                                                             
         MVC   SVLASSDT,RCONSSDT                                                
         MVC   SVSNDTIM,RCONSSTI                                                
         DROP  R2                                                               
*                                                                               
GTCVAL30 L     R2,AIO3             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1D'        Look for DARE agency order elem              
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   GTCVAL40                                                         
         USING RCONDREL,R2                                                      
         MVC   SVCONRLK,RCONDRLK   DARE agency order number                     
         DROP  R2                                                               
*                                                                               
GTCVAL40 L     R2,AIO3             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'12'        Look for expanded SAR element                
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   GTCVAL50                                                         
         USING RSARXEL,R2                                                       
         MVC   SVPDEMCT,RSARXDEM   Save primary demo category                   
         DROP  R2                                                               
*                                                                               
GTCVAL50 MVI   WIPSFLAG,NOQ        Set WIP=false                                
         L     R2,AIO3             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1F'        Extended description elem                    
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   GTCVAL52                                                         
         USING RCONXEL,R2                                                       
         MVC   SVCONFSW,RCONCONF   Save confirmation switch                     
         MVC   SVTRORD#,RCONTRF    Save traffic order number                    
         TM    RCONCONF,X'40'      Confirmed now?                               
         JO    GTCVAL60            Yes - not WIP                                
         DROP  R2                                                               
*                                                                               
GTCVAL52 L     R2,AIO3             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'        Send info elem                               
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   GTCVAL60                                                         
         USING RCONSEND,R2                                                      
         TM    RCONSENF,X'30'      REP/STA version not advanced?                
         JO    GTCVAL60                                                         
         MVI   WIPSFLAG,YESQ       Set WIP=true                                 
         DROP  R2                                                               
*                                                                               
GTCVAL60 MVI   SVOFRPND,NOQ        Set makegood offer pending to NO             
         L     R2,AIO3             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1E'        Random flag element                          
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   GTCVAL64                                                         
         USING RCONRFEL,R2                                                      
         CLI   RCONR#MO,0          Any makegood offers?                         
         JNH   *+8                                                              
         MVI   SVOFRPND,YESQ       Set makegood offer pending to YES            
         DROP  R2                                                               
*                                                                               
GTCVAL64 MVI   SVTAKOVR,NOQ        Set takeover contract switch to NO           
         L     R2,AIO3             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1C'        Takeover DARE element                        
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   GTCVAL70                                                         
         USING RCONTKEL,R2                                                      
         MVI   SVTAKOVR,YESQ       Set takeover contract switch to YES          
         DROP  R2                                                               
*                                                                               
GTCVAL70 L     RE,SAVE_RE                                                       
         BR    RE                                                               
*                                                                               
REPLYHDR LR    R0,RE                                                            
         TM    RPYIND1,RPYHDRTQ    Already replied header reply rec?            
         BNZR  RE                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',R#UPOFR)                
         OI    RPYIND1,RPYHDRTQ                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
REPLYCMG ST    RE,SAVE_RE                                                       
         BRAS  RE,REPLYHDR                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',007),          +        
               ('LD_CHARQ',COMPLMSG),(L'COMPLMSG,0)                             
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
*                                                                               
REPLYERR ST    RE,SAVE_RE                                                       
         BRAS  RE,REPLYHDR                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',008),          +        
               ('LD_CHARQ',ERRORMSG),(L'ERRORMSG,0)                             
         MVI   ERRNUM,X'FF'        Error is logged                              
         MVI   FATALERR,C'Y'       Error occurred                               
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* Check if contract is on Rep/Station side                                      
* Returns R/S in CONWSIDE                                                       
* Returns Y/N in CONSNDSW to indicate there's a SEND element or not             
* Returns version number in SVVERSN#                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSIDE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   CONWSIDE,C'R'       Deafult to Rep side                          
         MVI   CONSNDSW,NOQ        Init to no SEND element                      
         MVI   SVVERSN#,0          Init version number                          
         L     R2,AIO3             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'        Look for Send info elem                      
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   CKSIDEX             Not found, on Rep side                       
*                                                                               
         USING RCONSEND,R2                                                      
         MVI   CONSNDSW,YESQ       Yes, SEND elem found                         
         MVC   SVVERSN#,RCONSRV    Set to use Rep's version number              
         CLC   RCONSRV,RCONSSV                                                  
         JNL   CKSIDEX                                                          
         MVC   SVVERSN#,RCONSSV    Set to use Sta's version number              
         MVI   CONWSIDE,C'S'       Station side                                 
*                                                                               
CKSIDEX  J     EXIT                                                             
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMSBARY NTR1  BASE=*,LABEL=*      Check for missed buy data array              
*                                                                               
         CLI   QACTCODE,QAC_DELQ   Delete (MGX)?                                
         JE    EXITY                                                            
*                                                                               
         XC    NUM_MIS_,NUM_MIS_   Init # of missed buys to process             
         SR    R3,R3                                                            
         ICM   R3,7,QAMIS          Have missed buy data?                        
         JZ    EXITY                                                            
         MVC   NUM_MIS_,LW_NUMN-LW_D(R3)                                        
         USING MIS_D,R3                                                         
         AHI   R3,LW_LN2Q          Point to 1st set of missed buy data          
         SR    R4,R4                                                            
         ICM   R4,3,NUM_MIS_       # of missed buys to process                  
*                                                                               
CKMSB10  OC    MISLINE#,MISLINE#   Have missed buy line?                        
         JZ    EXITN                                                            
         OC    MISSTRDT,MISSTRDT   Have missed start date?                      
         JZ    EXITN                                                            
         OC    MISSPTWK,MISSPTWK   Have missed spot/week?                       
         JZ    EXITN                                                            
         LA    R3,MISLNQ(R3)       Bump to next entry in array                  
         JCT   R4,CKMSB10                                                       
*                                                                               
         J      EXITY                                                           
         DROP   RB,R3                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKOFLARY NTR1  BASE=*,LABEL=*      Check for offer line data array              
*                                                                               
         MVI   SVOFRTYP,0          Init offer type                              
*                                                                               
         CLI   QACTCODE,QAC_DELQ   Delete (MGX)?                                
         JE    EXITY                                                            
*                                                                               
         XC    NUM_OFL_,NUM_OFL_   Init # of offer lines to process             
         SR    R3,R3                                                            
         ICM   R3,7,QAOFL          Have offer line data?                        
         JZ    EXITN                                                            
         MVC   NUM_OFL_,LW_NUMN-LW_D(R3)                                        
         USING OFL_D,R3                                                         
         AHI   R3,LW_LN2Q          Point to 1st set of offer line data          
         SR    R4,R4                                                            
         ICM   R4,3,NUM_OFL_       # of offer lines to process                  
*                                                                               
CKOFL10  OC    OFLLINE#,OFLLINE#   Have offer line number?                      
         JZ    EXITN                                                            
         OC    OFLTYPE_,OFLTYPE_   Have offer type?                             
         JZ    EXITN                                                            
*                                                                               
         MVC   SVOFRTYP,OFLTYPE_   Save offer type to process X05 elem          
*                                                                               
         LA    R3,OFLLNQ(R3)       Bump to next entry in array                  
         JCT   R4,CKOFL10                                                       
*                                                                               
         J      EXITY                                                           
         DROP   RB,R3                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKDTFARY NTR1  BASE=*,LABEL=*      Check for day/time eff date arrays           
*                                                                               
         CLI   QACTCODE,QAC_DELQ   Delete (MGX)?                                
         JE    EXITY                                                            
*                                                                               
         CLI   SVOFRTYP,C'M'       Makegood offer type?                         
         JE    CKDTF10                                                          
         CLI   SVOFRTYP,C'B'       Bonus offer type?                            
         JE    CKDTF10                                                          
         J     EXITY                                                            
*                                                                               
CKDTF10  XC    NUM_DTM_,NUM_DTM_   Init # of Day/Time elem entries              
         SR    R3,R3                                                            
         ICM   R3,7,QADTM          Have Day/Time data?                          
         JZ    EXITN                                                            
         MVC   NUM_DTM_,LW_NUMN-LW_D(R3)                                        
         USING DTM_D,R3                                                         
         AHI   R3,LW_LN2Q          Point to 1st set of offer line data          
         SR    R4,R4                                                            
         ICM   R4,3,NUM_DTM_       # of Day/Time arrarys to process             
*                                                                               
CKDTF20  OC    DTMLINE#,DTMLINE#   Have offer line number?                      
         JZ    EXITN                                                            
*                                                                               
         LA    R3,DTMLNQ(R3)       Bump to next entry in array                  
         JCT   R4,CKDTF20                                                       
         DROP  R3                                                               
*                                                                               
         XC    NUM_EFD_,NUM_EFD_   Init # of Eff Date elem entries              
         SR    R3,R3                                                            
         ICM   R3,7,QAEFD          Have Effective Date data?                    
         JZ    EXITN                                                            
         MVC   NUM_EFD_,LW_NUMN-LW_D(R3)                                        
         USING EFD_D,R3                                                         
         AHI   R3,LW_LN2Q          Point to 1st set of offer line data          
         SR    R4,R4                                                            
         ICM   R4,3,NUM_EFD_       # of Effective Date arrays to proc           
*                                                                               
CKDTF40  OC    EFDLINE#,EFDLINE#   Have offer line number?                      
         JZ    EXITN                                                            
*                                                                               
         LA    R3,EFDLNQ(R3)       Bump to next entry in array                  
         JCT   R4,CKDTF40                                                       
         DROP  R3                                                               
*                                                                               
         J      EXITY                                                           
         DROP   RB                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKUPDMD NTR1  BASE=*,LABEL=*      Check update mode                            
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JE    EXITY                                                            
*                                                                               
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    CKUPM20                                                          
         BRAS  RE,BLDHKEY          Build offer header key                       
         MVC   SVOFFGRC,QOFFGRPC                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOREPDIR+IO1'                           
         CLC   IOKEY(L'RMKGKEY),IOKEYSAV                                        
         JE    CKUPM30                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXERRCHG),TXERRCHG                                    
         J     RPLYERRX                                                         
*                                                                               
CKUPM20  BRAS  RE,BLDHKEY          Build offer header key                       
         LA    RE,IOKEY                                                         
         USING RMKGKEY,RE                                                       
         XC    RMKGKGRP,RMKGKGRP   Prepare to calculate group code              
         XC    HALF1,HALF1         To store new group code                      
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOREPDIR+IO1'                           
CKUPM22  CLC   IOKEY(RMKGKGRP-RMKGKEY),IOKEYSAV                                 
         JNE   CKUPM26                                                          
         MVC   HALF1,IOKEY+RMKGKGRP-RMKGKEY                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOSQD+IOREPDIR+IO1'                           
         J     CKUPM22                                                          
*                                                                               
CKUPM26  OC    HALF1,HALF1         Have highest group code?                     
         JNZ   CKUPM26D                                                         
         MVC   SVOFFGRC,=C'AA'                                                  
         CLI   SVTAKOVR,YESQ       Takeover contract?                           
         JNE   CKUPM30                                                          
*                                                                               
* If takeover, avoid conflict with previous Rep's offer codes by                
* starting offer code at DA instead of AA                                       
*                                                                               
         MVC   SVOFFGRC,=C'DA'                                                  
         J     CKUPM30                                                          
*                                                                               
CKUPM26D CLI   SVTAKOVR,YESQ       Takeover contract?                           
         JNE   CKUPM26F                                                         
*                                                                               
* If takeover, and offer not already started at DA, force it to start           
* at group code DA                                                              
*                                                                               
         CLC   HALF1,=C'DA'                                                     
         JNL   CKUPM26F                                                         
         MVC   HALF1,=C'DA'                                                     
         J     CKUPM28                                                          
*                                                                               
CKUPM26F CLI   HALF1+1,C'I'                                                     
         JNE   *+12                                                             
         MVI   HALF1+1,C'J'                                                     
         J     CKUPM28                                                          
*                                                                               
         CLI   HALF1+1,C'R'                                                     
         JNE   *+12                                                             
         MVI   HALF1+1,C'S'                                                     
         J     CKUPM28                                                          
*                                                                               
         CLI   HALF1+1,C'Z'                                                     
         JNE   CKUPM26H                                                         
         MVI   HALF1+1,C'A'                                                     
         SR    RE,RE                                                            
         IC    RE,HALF1            Advance first letter                         
         AHI   RE,1                Good until letter J                          
         STC   RE,HALF1                                                         
         J     CKUPM28                                                          
*                                                                               
CKUPM26H SR    RE,RE                                                            
         IC    RE,HALF1+1          Advance second letter                        
         AHI   RE,1                                                             
         STC   RE,HALF1+1                                                       
*                                                                               
CKUPM28  MVC   SVOFFGRC,HALF1      Get new group code                           
*                                                                               
CKUPM30  DS    0H                                                               
*                                                                               
         J     EXITY                                                            
*                                                                               
BLDCKEY  XC    IOKEY,IOKEY         Build contract record key                    
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         USING RCONREC,R1                                                       
         MVI   RCONPTYP,X'8C'      Passive Pointer 1 for Contract rec           
         MVC   RCONPREP,LP_AGY     Rep code                                     
         MVC   RCONPCON,SVCON9CM   Contract number in 9's complement            
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
BLDHKEY  XC    IOKEY,IOKEY         Build offer header record key                
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         USING RMKGKEY,R1                                                       
         MVI   RMKGKTYP,X'11'      Offer record type                            
         MVC   RMKGKREP,LP_AGY     Rep code                                     
         MVC   RMKGKOFF,QOFFCODE   Office code                                  
         MVC   RMKGKSTA,QSTACODE   Station call letter                          
         MVC   RMKGKCON,SVCON9RV   Contract# in 9's complement reversed         
         MVC   RMKGKGRP,QOFFGRPC   Offer group code                             
         OC    SVOFFGRC,SVOFFGRC   Have offer group code from ADD?              
         JZ    *+10                                                             
         MVC   RMKGKGRP,SVOFFGRC   Set of offer group code from ADD             
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
BLDBKEY  LR    R0,RE                                                            
         XC    IOKEY,IOKEY         Prepare to build buy key                     
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RF,IOKEY                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
CKYESNO  CLI   0(RF),YESQ                                                       
         BER   RE                                                               
         CLI   0(RF),NOQ                                                        
         BER   RE                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC is equal - found element                  
         CLI   0(R2),0                                                          
         JNE   NXTEL                                                            
         LTR   R2,R2               CC is not equal - element not found          
NXTELX   BR    RE                                                               
                                                                                
R_GET    LR    R0,RE                                                            
         MVI   SVERRIND,0                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOREPDIR+IO1'                         
         JE    R_GET02                                                          
         MVI   SVERRIND,1                                                       
         J     R_GETX                                                           
R_GET02  GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
R_GETX   CLI   SVERRIND,0          Error?  To set condition code                
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
R_PUT    LR    R0,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCOFRHD NTR1  BASE=*,LABEL=*      Process offer header                         
*                                                                               
         MVC   SVMQMFRM,=C'LIN'    Init MQ messaage from LIN program            
         MVI   WKHPASSW,0          Offer header record passives switch          
         XC    FIRSTAIR,FIRSTAIR   Init first air date                          
         XC    SVMKGSAL,SVMKGSAL   Init salesperson code                        
         XC    SVMKGTEM,SVMKGTEM   Init sales team code                         
         XC    SVMKGADV,SVMKGADV   Init advertiser code                         
         XC    SVMKGDSP,SVMKGDSP   Init developemental salesperson code         
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JE    EXITY                                                            
*                                                                               
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    P_HDR20                                                          
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    P_HDR40                                                          
         CLI   QACTCODE,QAC_DELQ   Delete (MGX)?                                
         JE    P_HDR60                                                          
         DC    H'0'                Unsupported action                           
*                                                                               
P_HDR20  BRAS  RE,BLDHKEY          Build header key                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOREPDIR+IO1'                         
         CLC   IOKEY(L'RCFCKEY),IOKEYSAV                                        
         JNE   P_HDR22                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXERRADD),TXERRADD                                    
         J     RPLYERRX                                                         
*                                                                               
P_HDR22  L     R0,AIO1             Init offer header record                     
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R4,AIO1             Build new offer header record                
         USING RMKGREC,R4                                                       
         MVI   RMKGKTYP,RMKGKIDQ                                                
         MVC   RMKGKREP,LP_AGY                                                  
         MVC   RMKGKOFF,QOFFCODE                                                
         MVC   RMKGKSTA,QSTACODE                                                
         MVC   RMKGKCON,SVCON9RV                                                
         MVC   RMKGKGRP,SVOFFGRC                                                
         MVI   RMKGLEN+1,36        Default length - empty record                
         MVC   SVOFHKEY,RMKGREC    Offer header key - add                       
*                                                                               
         XC    ELEM,ELEM           Building Group status elem                   
         LA    R3,ELEM                                                          
         USING RMKGSDEM,R3                                                      
         MVI   RMKGSECD,X'01'      Group status elem code                       
         MVI   RMKGSELN,RMKGSELQ                                                
*                                                                               
         MVC   RMKGSCVR,SVCONCSV   Current station version number               
         MVC   RMKGSMOD,SVCONMOD   Current contract mod number                  
         MVC   RMKGDARN,SVCONRLK   Agency DARE order number                     
*                                                                               
         MVC   RMKGSCRD,TODAYC     Creation date - compressed                   
         OI    RMKGSFG2,RMGF2STQ   Station is changing this offer               
         GOTOR GETTIME,DMCB,RMKGSCRT                                            
*                                                                               
         MVC   SVMKGSTA,RMKGSCST   Current offer group status                   
         MVC   SVMKGFG1,RMKGSFG1   Current offer group DARE flag                
         MVC   SVMKGFG2,RMKGSFG2   Current offer group change flag              
*                                                                               
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(R4),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,SETX0AEL                                                      
         CLI   ELEM,0              Have Salesperson/team elem to add?           
         JE    P_HDR26                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(R4),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
P_HDR26  SR    R2,R2                                                            
         ICM   R2,7,QAGRC          Have group comment to process?               
         JZ    P_HDR30                                                          
         MVC   NUM_GRC_,LW_NUMN-LW_D(R2)                                        
         AHI   R2,LW_LN2Q          Point to first comment                       
         SR    R3,R3                                                            
         ICM   R3,3,NUM_GRC_       # of comment to process                      
*                                                                               
P_HDR28  XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING RMKGGCEM,R1                                                      
         MVI   RMKGGCCD,X'10'      Group comment description elem               
*                                                                               
         LA    RE,0(R2)                                                         
         AHI   RE,(MAXGCMLQ-1)     Point to last character in comment           
         LA    RF,MAXGCMLQ         Default to max group comment length          
         CLI   0(RE),C' '                                                       
         JH    *+12                                                             
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         J     *-12                                                             
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RMKGGCCM(0),0(R2)   Save group comment                           
         AHI   RF,1+2              Adjustment for elem length                   
         STC   RF,RMKGGCLN                                                      
         DROP  R1                                                               
*                                                                               
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(R4),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,MAXGCMLQ(R2)     Point to next comment in array               
         JCT   R3,P_HDR28                                                       
*                                                                               
P_HDR30  GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOREPFIL+IO1'                           
*                                                                               
         MVC   SVOFHKEY+28(4),IODA DA of offer header record just added         
         MVI   WKHPASSW,HPASADDQ   Offer header record is added                 
*                                                                               
         J     EXITY               End of offer header ADD                      
         DROP  R4                                                               
*                                                                               
P_HDR40  BRAS  RE,BLDHKEY          Build header key                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOREPDIR+IO1'                         
         CLC   IOKEY(L'RCFCKEY),IOKEYSAV                                        
         JE    P_HDR42                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXERRCHG),TXERRCHG                                    
         J     RPLYERRX                                                         
*                                                                               
P_HDR42  GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1             Point to offer header record                 
         USING RMKGREC,R4                                                       
         CLI   RMKGSECD,X'01'      Group status elem code?                      
         JE    *+6                                                              
         DC    H'0'                Bad offer header record                      
         CLI   RMKGSELN,RMKGSELQ   Group header main elem length?               
         JE    *+6                                                              
         DC    H'0'                Bad offer header record                      
*                                                                               
         MVC   SVOFHKEY,RMKGREC    Offer header key - change                    
*                                                                               
         MVC   RMKGSCVR,SVCONCSV   Current station version number               
         MVC   RMKGSMOD,SVCONMOD   Current contract mod number                  
         MVC   RMKGDARN,SVCONRLK   Agency DARE order number                     
*                                                                               
         MVC   RMKGSLAD,TODAYC     Last activity date - compressed              
         MVI   RMKGSCST,RMKGSRVQ   Offer is revised                             
         MVI   RMKGSFG2,RMGF2STQ   Station is changing this offer               
         GOTOR GETTIME,DMCB,RMKGSLAT                                            
*                                                                               
         MVC   SVMKGSTA,RMKGSCST   Current offer group status                   
         MVC   SVMKGFG1,RMKGSFG1   Current offer group DARE flag                
         MVC   SVMKGFG2,RMKGSFG2   Current offer group change flag              
*                                                                               
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'10',(R4)),0,0                  
*                                                                               
P_HDR46  SR    R2,R2                                                            
         ICM   R2,7,QAGRC          Have group comment to process?               
         JZ    P_HDR50                                                          
         MVC   NUM_GRC_,LW_NUMN-LW_D(R2)                                        
         AHI   R2,LW_LN2Q          Point to first comment                       
         SR    R3,R3                                                            
         ICM   R3,3,NUM_GRC_       # of comment to process                      
*                                                                               
*                                                                               
P_HDR48  XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING RMKGGCEM,R1                                                      
         MVI   RMKGGCCD,X'10'      Group comment description elem               
*                                                                               
         LA    RE,0(R2)                                                         
         AHI   RE,(MAXGCMLQ-1)     Point to last character in comment           
         LA    RF,MAXGCMLQ         Default to max group comment length          
         CLI   0(RE),C' '                                                       
         JH    *+12                                                             
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         J     *-12                                                             
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RMKGGCCM(0),0(R2)   Save group comment                           
         AHI   RF,1+2              Adjustment for elem length                   
         STC   RF,RMKGGCLN                                                      
         DROP  R1                                                               
*                                                                               
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(R4),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,MAXGCMLQ(R2)     Point to next comment in array               
         JCT   R3,P_HDR48                                                       
*                                                                               
P_HDR50  MVI   RMKGCNTL,0          Reset control status in record               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
*                                                                               
         LA    R4,IOKEY                                                         
         MVI   RMKGCNTL-2,0        Reset control status in directory            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR+IO1'                         
         DROP  R4                                                               
*                                                                               
         MVC   SVOFHKEY+28(4),IODA DA of offer header record just chg'd         
         MVI   WKHPASSW,HPASCHGQ   Offer header record is changed               
*                                                                               
         L     R2,AIO1             Point to offer header record                 
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,RMKGXELQ     Look for salesperson/team info elem          
         BRAS  RE,NXTEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGXEL,R2                                                       
         MVC   SVMKGSAL,RMKGXSAL   Saleperson code - saved                      
         MVC   SVMKGTEM,RMKGXTEM   Team code - saved                            
         MVC   SVMKGADV,RMKGXADV   Advertiser code - saved                      
         MVC   SVMKGDSP,RMKGXDSP   Developemental salesperon code - sav         
         DROP  R2                                                               
*                                                                               
         J     EXITY               End of offer header CHANGE                   
*                                                                               
P_HDR60  BRAS  RE,BLDHKEY          Build header key                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOREPDIR+IO1'                         
         CLC   IOKEY(L'RCFCKEY),IOKEYSAV                                        
         JE    P_HDR62                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXERRDEL),TXERRDEL                                    
         J     RPLYERRX                                                         
*                                                                               
P_HDR62  GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
*                                                                               
         L     R2,AIO1             Point to offer header record                 
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,RMKGXELQ     Look for salesperson/team info elem          
         BRAS  RE,NXTEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGXEL,R2                                                       
         MVC   SVMKGSAL,RMKGXSAL   Saleperson code - saved                      
         MVC   SVMKGTEM,RMKGXTEM   Team code - saved                            
         MVC   SVMKGADV,RMKGXADV   Advertiser code - saved                      
         MVC   SVMKGDSP,RMKGXDSP   Developemental salesperon code - sav         
         DROP  R2                                                               
*                                                                               
         L     R4,AIO1             Point to offer header record                 
         USING RMKGREC,R4                                                       
         MVC   SVOFHKEY,RMKGREC    Offer header key - delete                    
         OI    RMKGCNTL,X'80'      Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
*                                                                               
         MVC   SVOFHKEY+28(4),IODA DA of offer header rec to be deleted         
*                                                                               
         LA    R4,IOKEY                                                         
         OI    RMKGCNTL-2,X'80'    Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR+IO1'                         
*                                                                               
         MVI   WKHPASSW,HPASDELQ   Offer header record is deleted               
         MVI   SVMGOFLG,C'X'       Indicate makegood offer is deleted           
*                                                                               
         J     EXITY               End of offer header DELETE                   
         DROP  R4                                                               
*                                                                               
SETX0AEL ST    RE,SAVE_RE          Set X'0A' elem - Saleperson/team             
         XC    ELEM,ELEM                                                        
         L     R2,AIO3             Point to contract record                     
         USING RCONREC,R2                                                       
         CLI   RCONCODE,X'01'      Have contract description elem?              
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,ELEM                                                          
         USING RMKGXEL,R3                                                       
         MVI   RMKGXEL,RMKGXELQ    Saleperson/team info elem code               
         MVI   RMKGXLEN,RMKGXLNQ   Element length                               
         MVC   RMKGXSAL,RCONSAL    Saleperson code                              
         MVC   SVMKGSAL,RCONSAL    Saleperson code - saved                      
         MVC   RMKGXOFF,RCONKOFF   Office code                                  
         MVC   RMKGXTEM,RCONTEM    Team code                                    
         MVC   SVMKGTEM,RCONTEM    Team code - saved                            
         MVC   RMKGXSTA,RCONKSTA   Station code                                 
         MVC   RMKGXADV,RCONKADV   Advertiser code                              
         MVC   SVMKGADV,RCONKADV   Advertiser code - saved                      
         MVC   RMKGXAGY,RCONKAGY   Agency code                                  
         MVC   RMKGXAOF,RCONKAOF   Agency office code                           
         MVC   RMKGXGRP,RCONKGRP   Group/Subgroup                               
         MVC   RMKGXDSP,RCONDVSP   Developemental salesperon code               
         MVC   SVMKGDSP,RCONDVSP   Developemental salesperon code - sav         
         MVC   RMKGXDCT,RCONDVCT   Developemental contract type                 
         MVC   RMKGXFLT,RCONDATE   Flight dates                                 
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
         DROP  R2,R3                                                            
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCHPASV NTR1  BASE=*,LABEL=*      Process header record passives               
*                                                                               
         CLI   WKHPASSW,HPASADDQ   Offer header record is added?                
         JE    P_HPAS06                                                         
         CLI   WKHPASSW,HPASCHGQ   Offer header record is changed?              
         JE    P_HPAS40                                                         
         CLI   WKHPASSW,HPASDELQ   Offer header record is deleted?              
         JE    P_HPAS10                                                         
         DC    H'0'                Mode not supported                           
*                                                                               
P_HPAS06 OC    SVOFHKEY+28(4),SVOFHKEY+28    Have disk address?                 
         JZ    P_HPAS_X                                                         
*                                                                               
P_HPAS10 XC    IOKEY,IOKEY         Init A011 passive key                        
         MVI   IOKEY+00,X'A0'      MG first passive key                         
         MVI   IOKEY+01,X'11'                                                   
         MVC   IOKEY+09(02),SVOFHKEY+06      Rep code                           
         MVC   IOKEY+11(03),SVMKGSAL         Salesperson code                   
         MVC   IOKEY+14(02),SVMKGTEM         Sales team code                    
         MVC   IOKEY+16(11),SVOFHKEY+10      Sta/Con#/Group                     
*                                                                               
P_HPAS20 GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOREPDIR+IO3'                           
         CLC   IOKEY(27),IOKEYSAV                                               
         JNE   P_HPAS24                                                         
         NI    IOKEY+27,X'7F'      Turn off delete bit                          
         CLI   WKHPASSW,HPASDELQ   Offer header record is deleted?              
         JNE   *+8                                                              
         OI    IOKEY+27,X'80'      Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR'                             
         J     P_HPAS30                                                         
*                                                                               
P_HPAS24 CLI   WKHPASSW,HPASDELQ   Offer header record is deleted?              
         JE    P_HPAS30                                                         
         MVC   IOKEY,IOKEYSAV                Restore key                        
         MVC   IOKEY+28(04),SVOFHKEY+28      Disk address                       
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOREPDIR'                               
*                                                                               
P_HPAS30 CLI   IOKEY,X'A1'         Second passive key (A111) processed?         
         JE    P_HPAS40                                                         
         MVI   IOKEY,X'A1'         Set second passive key type                  
         MVC   WORK(3),IOKEY+11    Save saleperson part of key                  
         MVC   IOKEY+11(2),IOKEY+14          Slide team up in key               
         MVC   IOKEY+13(3),WORK    Insert salesperson back to key               
         J     P_HPAS20            Process second passive                       
*                                                                               
P_HPAS40 XC    IOKEY,IOKEY         Init A001 passive key                        
         LA    R2,IOKEY                                                         
         USING RMKGREC,R2                                                       
         MVI   RMGSPTYP+0,X'A0'                                                 
         MVI   RMGSPTYP+1,X'01'                                                 
         MVC   RMGSPREP,SVOFHKEY+06          Rep code                           
         MVC   RMGSPSAL,SVMKGSAL             Salesperson code                   
         MVC   RMGSPSTA,SVOFHKEY+10          Station                            
         MVC   RMGSPCON,SVCON9CM             Contract# (9's comp)               
         MVC   RMGSPADV,SVMKGADV             Advertiser code                    
         MVC   RMGSPGRP,SVOFHKEY+19          Group code                         
*                                                                               
         GOTOR VDATCON,DMCB,(3,FIRSTAIR),(2,RMGSPDAT)                           
         MVC   WORK2(L'IOKEY),IOKEY                                             
         MVC   WORK2+(RMGSPWIP-RMGSPTYP)(L'RMGSPWIP),SVMKGFG2                   
         MVC   WORK2+(RMGSPSTT-RMGSPTYP)(L'RMGSPSTT),SVMKGSTA                   
         MVC   WORK2+(RMGSPDST-RMGSPTYP)(L'RMGSPDST),SVMKGFG1                   
*                                                                               
         XC    RMGSPDAT,RMGSPDAT                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOREPDIR+IO3'                           
*                                                                               
P_HPAS44 CLC   IOKEY(RMGSPDAT-RMGSPTYP),IOKEYSAV                                
         JNE   P_HPAS48                                                         
         MVC   SVOFHKEY+28(4),IOKEY+28                                          
         TM    IOKEY+27,X'80'      Already deleted?                             
         JO    P_HPAS46                                                         
         OI    IOKEY+27,X'80'      Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR'                             
P_HPAS46 GOTOR (#IOEXEC,AIOEXEC),'IOSQD+IOREPDIR+IO3'                           
         J     P_HPAS44                                                         
*                                                                               
P_HPAS48 CLI   QACTCODE,QAC_DELQ   Delete?                                      
         JE    P_HPAS50                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(27),WORK2     Passive key to be added                      
         GOTOR (#IOEXEC,AIOEXEC),'IORDD+IOREPDIR+IO3'                           
         CLC   IOKEY(27),WORK2                                                  
         JNE   P_HPAS4B                                                         
         TM    IOKEY+27,X'80'      Already deleted?                             
         JZ    P_HPAS50                                                         
         NI    IOKEY+27,X'FF'-X'80'                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR'                             
         J     P_HPAS50                                                         
*                                                                               
P_HPAS4B XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(27),WORK2     Passive key to be added                      
         MVC   IOKEY+28(04),SVOFHKEY+28      Disk address                       
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOREPDIR'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
P_HPAS50 OC    SVMKGDSP,SVMKGDSP   Have dev salesperson code?                   
         JZ    P_HPAS60                                                         
         XC    IOKEY,IOKEY         Init A002 passive key                        
         LA    R2,IOKEY                                                         
         USING RMKGREC,R2                                                       
         MVI   RMGDSTYP+0,X'A0'                                                 
         MVI   RMGDSTYP+1,X'02'                                                 
         MVC   RMGDSREP,SVOFHKEY+06          Rep code                           
         MVC   RMGDSDSL,SVMKGDSP             Dev salesperson code               
         MVC   RMGDSSTA,SVOFHKEY+10          Station                            
         MVC   RMGDSCON,SVCON9CM             Contract# (9's comp)               
         MVC   RMGDSADV,SVMKGADV             Advertiser code                    
         MVC   RMGDSGRP,SVOFHKEY+19          Group code                         
         GOTOR VDATCON,DMCB,(3,FIRSTAIR),(2,RMGDSDAT)                           
         MVC   WORK2(L'IOKEY),IOKEY                                             
         MVC   WORK2+(RMGDSWIP-RMGDSTYP)(L'RMGDSWIP),SVMKGFG2                   
         MVC   WORK2+(RMGDSSTT-RMGDSTYP)(L'RMGDSSTT),SVMKGSTA                   
         MVC   WORK2+(RMGDSDST-RMGDSTYP)(L'RMGDSDST),SVMKGFG1                   
*                                                                               
         XC    RMGDSDAT,RMGDSDAT                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOREPDIR+IO3'                           
*                                                                               
P_HPAS54 CLC   IOKEY(RMGDSDAT-RMGDSTYP),IOKEYSAV                                
         JNE   P_HPAS58                                                         
         MVC   SVOFHKEY+28(4),IOKEY+28                                          
         TM    IOKEY+27,X'80'      Already deleted?                             
         JO    P_HPAS56                                                         
         OI    IOKEY+27,X'80'      Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR'                             
P_HPAS56 GOTOR (#IOEXEC,AIOEXEC),'IOSQD+IOREPDIR+IO3'                           
         J     P_HPAS54                                                         
*                                                                               
P_HPAS58 CLI   QACTCODE,QAC_DELQ   Delete?                                      
         JE    P_HPAS60                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(27),WORK2     Passive key to be added                      
         GOTOR (#IOEXEC,AIOEXEC),'IORDD+IOREPDIR+IO3'                           
         CLC   IOKEY(27),WORK2                                                  
         JNE   P_HPAS5B                                                         
         TM    IOKEY+27,X'80'      Already deleted?                             
         JZ    P_HPAS60                                                         
         NI    IOKEY+27,X'FF'-X'80'                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR'                             
         J     P_HPAS60                                                         
*                                                                               
P_HPAS5B XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(27),WORK2     Passive key to be added                      
         MVC   IOKEY+28(04),SVOFHKEY+28      Disk address                       
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOREPDIR'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
P_HPAS60 DS    0H                  Future passive keys                          
*                                                                               
P_HPAS_X J     EXITY               End of offer header passives                 
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNLOFRHD NTR1  BASE=*,LABEL=*      Finalize offer header                        
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JE    EXITY                                                            
         CLI   QACTCODE,QAC_DELQ   Delete?                                      
         JE    F_HDR70                                                          
*                                                                               
         OC    FIRSTAIR,FIRSTAIR   Have first air date?                         
         JZ    F_HDR70                                                          
*                                                                               
         BRAS  RE,BLDHKEY          Build header key                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOREPDIR+IO1'                         
         CLC   IOKEY(L'RCFCKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1             Point to offer header record                 
         USING RMKGREC,R4                                                       
         CLI   RMKGSECD,X'01'      Group status elem code?                      
         JE    *+6                                                              
         DC    H'0'                Bad offer header record                      
         CLI   RMKGSELN,RMKGSELQ   Group header main elem length?               
         JE    *+6                                                              
         DC    H'0'                Bad offer header record                      
*                                                                               
         GOTOR VDATCON,DMCB,(3,FIRSTAIR),(2,RMKGFOFD)                           
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
*                                                                               
F_HDR70  BRAS  RE,PRCHPASV         Process header record passives               
*                                                                               
         J     EXITY               End of finalizing offer header               
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCOFRLN NTR1  BASE=*,LABEL=*      Process offer lines                          
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JE    EXITY                                                            
*                                                                               
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    P_LNS20                                                          
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    P_LNS20                                                          
         CLI   QACTCODE,QAC_DELQ   Delete (MGX)?                                
         JE    P_LNS60                                                          
         DC    H'0'                Unsupported action                           
*                                                                               
P_LNS20  SR    R2,R2                                                            
         ICM   R2,7,QAOFL          Point to offer line array                    
         USING OFL_D,R2                                                         
         AHI   R2,LW_LN2Q          Point to 1st set of offer line data          
         SR    R3,R3                                                            
         ICM   R3,3,NUM_OFL_       # of offer lines to process                  
*                                                                               
P_LNS20Q LA    R0,SVOFLVAL         Init offer line values to be saved           
         LHI   R1,SVOFLVLQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    IOKEY,IOKEY         Build offer line record key                  
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         BRAS  RE,BLDOFLKR         Build offer line key/record                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOREPDIR+IO1'                         
         CLC   IOKEY(L'RMKGKEY),IOKEYSAV                                        
         JE    P_LNS40                                                          
*                                                                               
         XC    IOKEY,IOKEY         Rebuild key to add offer line rec            
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         BRAS  RE,BLDOFLKR         Build offer line key/record                  
*                                                                               
         L     R0,AIO1             Init offer line record                       
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R1,AIO1             Build new offer line record                  
         USING RMKGREC,R1                                                       
         BRAS  RE,BLDOFLKR         Build offer line key/record                  
         MVI   RMKGLEN+1,36        Default length - empty record                
         DROP  R1                                                               
*                                                                               
         BRAS  RE,PRCX01EL         Process offer line main element              
*                                                                               
         MVC   SVOFRLN#,OFLLINE#   Save offer line# for X02 X03 elems           
         BRAS  RE,PRCX02EL         Process multiple X'02' element               
         BRAS  RE,PRCX03EL         Process multiple X'03' element               
         BRAS  RE,PRCX05EL         Process multiple X'05' element               
*                                                                               
         BRAS  RE,PRCREMEL         Process remaining elements                   
*                                                                               
         BRAS  RE,FNLX01EL         Finalize main X'01' element                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOREPFIL+IO1'                           
         JE    *+6                                                              
         DC    H'0'                Must be able to add offer line rec           
*                                                                               
         MVI   SVMGOFLG,C'A'       Indicate makegood offer is added             
*                                                                               
P_LNS38X LA    R2,OFLLNQ(R2)       Bump to next offer line in array             
         JCT   R3,P_LNS20Q                                                      
         J     EXITY               End of offer ADD/CHANGE                      
*                                                                               
P_LNS40  GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
*                                                                               
         BRAS  RE,PRCX01EL         Process offer line main element              
*                                                                               
         MVC   SVOFRLN#,OFLLINE#   Save offer line# for X02 X03 elems           
         BRAS  RE,PRCX02EL         Process multiple X'02' element               
         BRAS  RE,PRCX03EL         Process multiple X'03' element               
         BRAS  RE,PRCX05EL         Process multiple X'05' element               
*                                                                               
         BRAS  RE,PRCREMEL         Process remaining elements                   
*                                                                               
         BRAS  RE,FNLX01EL         Finalize main X'01' element                  
*                                                                               
         L     R4,AIO1                                                          
         USING RMKGREC,R4                                                       
         MVI   RMKGCNTL,0          Reset control status in record               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
         LA    R4,IOKEY                                                         
         MVI   RMKGCNTL-2,0        Reset control status in directory            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR+IO1'                         
*                                                                               
         J     P_LNS38X            Bump to next entry and loop                  
         DROP  R4                                                               
*                                                                               
P_LNS60  XC    SVBYLTAB,SVBYLTAB   Init table of buy lines for X'66' el         
         XC    IOKEY,IOKEY         Build offer line record key                  
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         USING RMKGKEY,R1          R1 points to key or record area              
         MVI   RMKGKTYP,X'11'      Offer record type                            
         MVC   RMKGKREP,LP_AGY     Rep code                                     
         MVC   RMKGKOFF,QOFFCODE   Office code                                  
         MVC   RMKGKSTA,QSTACODE   Station call letter                          
         MVC   RMKGKCON,SVCON9RV   Contract# in 9's complement reversed         
         MVC   RMKGKGRP,SVOFFGRC   Offer group code                             
         MVC   RMKGKPLN,=X'FFFFFF'                                              
         MVI   RMKGKLIN,1          Always 1, hard coded                         
         MVI   RMKGKRTY,1          Start with first offer line                  
         DROP  R1                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
P_LNS62  CLC   IOKEY(RMKGKRTY-RMKGKEY),IOKEYSAV                                 
         JE    P_LNS64G                                                         
         J     P_LNS69X                                                         
*                                                                               
P_LNS64S GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPFIL+IO1'                            
         CLC   IOKEY(RMKGKRTY-RMKGKEY),IOKEYSAV                                 
         JE    P_LNS64G                                                         
         J     P_LNS69X                                                         
*                                                                               
P_LNS64G GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1                                                          
         LA    R4,FRSTELEM(R4)     Point to first element                       
         USING RMKGMGEL,R4                                                      
P_LNS66  CLI   RMKGMGCD,0          End of record?                               
         JE    P_LNS68                                                          
         CLI   RMKGMGCD,X'05'      Buy makegood reference elem?                 
         JNE   P_LNS66M                                                         
         LA    RE,SVBYLTAB         Point table of buylines for X'66'            
P_LNS66F CLI   0(RE),0             End of table?                                
         JNE   *+14                                                             
         MVC   0(1,RE),RMKGMGLI    Save buy line # to table                     
         J     P_LNS66M                                                         
         CLC   RMKGMGLI,0(RE)      Already in table?                            
         JE    P_LNS66M                                                         
         LA    RE,1(RE)                                                         
         J     P_LNS66F                                                         
P_LNS66M LLC   R1,RMKGMGLN                                                      
         AR    R4,R1               Bump to next element                         
         J     P_LNS66                                                          
         DROP  R4                                                               
*                                                                               
P_LNS68  L     R4,AIO1             Point to offer record                        
         USING RMKGREC,R4                                                       
         OI    RMKGCNTL,X'80'      Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
         LA    R4,IOKEY                                                         
         OI    RMKGCNTL-2,X'80'    Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR+IO1'                         
*                                                                               
         MVI   SVMGOFLG,C'X'       Indicate makegood offer is deleted           
*                                                                               
         J     P_LNS64S                                                         
*                                                                               
P_LNS69X J     EXITY               End of offer DELETE                          
         DROP  R4                                                               
*                                                                               
         USING RMKGKEY,R1          R1 points to key or record area              
BLDOFLKR MVI   RMKGKTYP,X'11'      Offer record type                            
         MVC   RMKGKREP,LP_AGY     Rep code                                     
         MVC   RMKGKOFF,QOFFCODE   Office code                                  
         MVC   RMKGKSTA,QSTACODE   Station call letter                          
         MVC   RMKGKCON,SVCON9RV   Contract# in 9's complement reversed         
         MVC   RMKGKGRP,SVOFFGRC   Offer group code                             
         MVC   RMKGKPLN,=X'FFFFFF'                                              
         MVI   RMKGKLIN,1          Always 1, hard coded                         
         MVC   RMKGKRTY,OFLLINE#   Offer line# from request array               
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
FNLX01EL ST    RE,SAVE_RE          Finalize main X'01' element                  
         L     R4,AIO1                                                          
         LA    R4,FRSTELEM(R4)                                                  
         USING RMKGELEM,R4                                                      
         CLI   RMKGCODE,X'01'      Main element?                                
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   RMKGELLN,X'2B'      Correct offer line main elem length?         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   RMKGNW,SVOFLNPW     Number per week                              
         MVC   RMKGTSPT,SVOFLTSP   Total spot                                   
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,RMKGTSPT       Total spot                                   
         ICM   R1,15,RMKGCOS       Cost                                         
         MR    RE,R1               Total cost = total spot * cost               
         STCM  RF,15,RMKGTCOS      Total cost                                   
         MVC   RMKGTWKS,SVOFLTWK   Total weeks                                  
*                                                                               
         LLC   RE,SVOFLSDY         Start day                                    
         SLL   RE,4                Put start day in high nibble                 
         LLC   RF,SVOFLEDY         End day                                      
         OR    RE,RF                                                            
         STC   RE,RMKGSTED         Start-end day for all X'02' elements         
*                                                                               
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
         DROP  R4                                                               
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DELOFRLN NTR1  BASE=*,LABEL=*      Delete offer lines                           
*                                                                               
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JNE   EXITY                                                            
         OC    NUM_OFL_,NUM_OFL_   Have offer lines?                            
         JZ    EXITY                                                            
*                                                                               
         XC    SVBYLTB2,SVBYLTB2   Init table of buy lines for X'66' el         
*                                                                               
         XC    IOKEY,IOKEY         Build offer line record key                  
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         USING RMKGKEY,R1          R1 points to key or record area              
         MVI   RMKGKTYP,X'11'      Offer record type                            
         MVC   RMKGKREP,LP_AGY     Rep code                                     
         MVC   RMKGKOFF,QOFFCODE   Office code                                  
         MVC   RMKGKSTA,QSTACODE   Station call letter                          
         MVC   RMKGKCON,SVCON9RV   Contract# in 9's complement reversed         
         MVC   RMKGKGRP,SVOFFGRC   Offer group code                             
         MVC   RMKGKPLN,=X'FFFFFF'                                              
         MVI   RMKGKLIN,1          Always 1, hard coded                         
         SR    RE,RE                                                            
         ICM   RE,3,NUM_OFL_       Number of offer lines in request             
         AHI   RE,1                                                             
         STC   RE,RMKGKRTY         First offer line outside of array            
         DROP  R1                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
D_LNS62  CLC   IOKEY(RMKGKRTY-RMKGKEY),IOKEYSAV                                 
         JE    D_LNS64G                                                         
         J     D_LNS70                                                          
*                                                                               
D_LNS64S GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPFIL+IO1'                            
         CLC   IOKEY(RMKGKRTY-RMKGKEY),IOKEYSAV                                 
         JE    D_LNS64G                                                         
         J     D_LNS70                                                          
*                                                                               
D_LNS64G GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1                                                          
         LA    R4,FRSTELEM(R4)     Point to first element                       
         USING RMKGMGEL,R4                                                      
D_LNS66  CLI   RMKGMGCD,0          End of record?                               
         JE    D_LNS68                                                          
         CLI   RMKGMGCD,X'05'      Buy makegood reference elem?                 
         JNE   D_LNS66M                                                         
         LA    RE,SVBYLTB2         Point table of buylines for X'66'            
D_LNS66F CLI   0(RE),0             End of table?                                
         JNE   *+14                                                             
         MVC   0(1,RE),RMKGMGLI    Save buy line # to table                     
         J     D_LNS66M                                                         
         CLC   RMKGMGLI,0(RE)      Already in table?                            
         JE    D_LNS66M                                                         
         LA    RE,1(RE)                                                         
         J     D_LNS66F                                                         
D_LNS66M LLC   R1,RMKGMGLN                                                      
         AR    R4,R1               Bump to next element                         
         J     D_LNS66                                                          
         DROP  R4                                                               
*                                                                               
D_LNS68  L     R4,AIO1             Point to offer record                        
         USING RMKGREC,R4                                                       
         OI    RMKGCNTL,X'80'      Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
         LA    R4,IOKEY                                                         
         OI    RMKGCNTL-2,X'80'    Mark for deletion                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR+IO1'                         
         J     D_LNS64S                                                         
*                                                                               
D_LNS70  OC    SVBYLTB2,SVBYLTB2   Have buy lines to process X'66' elm?         
         JZ    D_LNS_X                                                          
         LA    R2,SVBYLTB2                                                      
         USING MIS_D,R2            Note: only MISLINE# is used                  
D_LNS74  CLI   MISLINE#,0          End of table?                                
         JE    D_LNS_X                                                          
         BRAS  RE,GTBUYLN          Get buy line                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),QOFFGRPC    Offer group code to be removed               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'66',(RF)),(2,ELEM),0           
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
         LA    R2,1(R2)            Bump to next entry in table                  
         J     D_LNS74                                                          
         DROP  R2                                                               
*                                                                               
D_LNS_X  J     EXITY               End of offer line DELETE                     
         DROP  RB                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETTIME  NTR1  BASE=*,LABEL=*      REtrieve time, return as HH:MM:SS:TT         
*                                                                               
         L     R4,0(R1)            Point to output area                         
*                                                                               
         TIME  DEC                                                              
*                                                                               
         STCM  R0,4,1(R4)          Minutes                                      
         STCM  R0,2,2(R4)          Seconds                                      
         SRL   R0,24               Shift hours to lowe-order                    
         STC   R0,WORK                                                          
         GOTO1 VHEXOUT,DMCB,WORK,WORK+1,1,=C'TOG'                               
         PACK  DUB,WORK+1(2)                                                    
         CVB   R2,DUB                                                           
         LA    R2,DDSTMADJ(R2)     Add adjustment for DDS time                  
         EDIT  (R2),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 VHEXIN,DMCB,WORK+17,(R4),2,0                                     
*                                                                               
         J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* R2       => Points to current processing offer line data in array             
* AIO1     => Contains record to be modified                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCX01EL NTR1  BASE=*,LABEL=*      Process offer line main element              
*                                                                               
         USING OFL_D,R2                                                         
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    PX01_20                                                          
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    PX01_30                                                          
         DC    H'0'                Unsupported action                           
*                                                                               
PX01_20  L     R4,AIO1                                                          
         LA    R4,FRSTELEM(R4)                                                  
         USING RMKGELEM,R4                                                      
         CLI   RMKGCODE,X'01'      Have offer line main element?                
         JNE   PX01_30                                                          
         CLI   RMKGELLN,X'2B'      Have correct offer line main elem?           
         JNE   PX01_30                                                          
*                                                                               
         MVC   RMKGCOS,OFLRATE_    Offer rate from request array                
         MVC   RMKGCHGD,TODAYB     Creation date - binary                       
         MVC   RMKGKMOD,SVCONMOD   Contract modification number                 
         MVC   RMKGDUR,OFLLENG_    Length in second                             
         CLI   OFLLENIN,C'M'       Length indicator is minutes?                 
         JNE   *+8                                                              
         OI    RMKGDUR,X'80'       Set length in minutes                        
         MVC   RMKGVER,SVCONCRV    Current rep version                          
*                                                                               
         MVI   RMKGRTS,0           Set offer type to Makegood (default)         
         CLI   OFLTYPE_,C'B'       Bonus?                                       
         JNE   *+8                                                              
         OI    RMKGRTS,X'20'       Set to Bonus                                 
         CLI   OFLTYPE_,C'P'       Preempt/Credit?                              
         JNE   *+8                                                              
         OI    RMKGRTS,X'10'       Set to Preempt/Credit                        
         CLI   OFLTYPE_,C'L'       Late Run?                                    
         JNE   *+8                                                              
         OI    RMKGRTS,X'08'       Set to Late Run                              
*                                                                               
         J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
PX01_30  XC    ELEM,ELEM           Building Group status elem                   
         LA    R4,ELEM                                                          
         USING RMKGELEM,R4                                                      
         MVI   RMKGCODE,X'01'      Offer line description elem code             
         MVI   RMKGELLN,X'2B'      Element length                               
*                                                                               
         MVC   RMKGCOS,OFLRATE_    Offer rate from request array                
         MVC   RMKGCREA,TODAYB     Creation date - binary                       
         MVC   RMKGKMOD,SVCONMOD   Contract modification number                 
         MVC   RMKGCHGI,=C'A '     Change indicator - A=Added                   
         MVC   RMKGDUR,OFLLENG_    Length in second                             
         CLI   OFLLENIN,C'M'       Length indicator is minutes?                 
         JNE   *+8                                                              
         OI    RMKGDUR,X'80'       Set length in minutes                        
         MVC   RMKGVER,SVCONCRV    Current rep version                          
*                                                                               
         MVI   RMKGRTS,0           Set offer type to Makegood (default)         
         CLI   OFLTYPE_,C'B'       Bonus?                                       
         JNE   *+8                                                              
         OI    RMKGRTS,X'20'       Set to Bonus                                 
         CLI   OFLTYPE_,C'P'       Preempt/Credit?                              
         JNE   *+8                                                              
         OI    RMKGRTS,X'10'       Set to Preempt/Credit                        
         CLI   OFLTYPE_,C'L'       Late Run?                                    
         JNE   *+8                                                              
         OI    RMKGRTS,X'08'       Set to Late Run                              
         DROP  R4                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SVOFRLN# => Contains offer line number to be processed                        
* AIO1     => Contains record to be modified                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCX02EL NTR1  BASE=*,LABEL=*      Process multiple X'02' element               
*                                                                               
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    PX02_20                                                          
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    PX02_30                                                          
         DC    H'0'                Unsupported action                           
*                                                                               
PX02_20  L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'02',(RF)),0,0                  
*                                                                               
PX02_30  SR    R2,R2                                                            
         ICM   R2,7,QADTM          Point to day/time data array                 
         JZ    EXIT                                                             
         MVC   NUM_DTM_,LW_NUMN-LW_D(R2)                                        
         USING DTM_D,R2                                                         
         AHI   R2,LW_LN2Q          Point to 1st set of day/time data            
         SR    R3,R3                                                            
         ICM   R3,3,NUM_DTM_       # of day/time elems to process               
*                                                                               
PX02_36  CLC   SVOFRLN#,DTMLINE#   Day/time for current offer line?             
         JNE   PX02_42                                                          
         XC    ELEM,ELEM           Building Group status elem                   
         LA    R1,ELEM                                                          
         USING RMKGDYEL,R1                                                      
         MVI   RMKGDYCD,X'02'      Day/time element code                        
         MVI   RMKGDYLN,X'09'      Element length                               
*                                                                               
         LLC   RE,DTMSTRDY         Start day                                    
         SLL   RE,4                Put start day in high nibble                 
         LLC   RF,DTMENDDY         End day                                      
         OR    RE,RF                                                            
         STC   RE,RMKGDYIN         Start-end day indicator                      
*                                                                               
         CLI   SVOFLSDY,0          Have absolute start day?                     
         JH    *+14                                                             
         MVC   SVOFLSDY,DTMSTRDY                                                
         J     PX02_36F                                                         
         LLC   RE,SVOFLSDY         Start day - absolute                         
         LLC   RF,DTMSTRDY         Start day - current value in array           
         CR    RE,RF                                                            
         JL    *+8                                                              
         STC   RF,SVOFLSDY         Save new absolute start day                  
*                                                                               
PX02_36F CLI   SVOFLEDY,0          Have absolute end day?                       
         JH    *+14                                                             
         MVC   SVOFLEDY,DTMENDDY                                                
         J     PX02_36M                                                         
         LLC   RE,SVOFLEDY         End day - absolute                           
         LLC   RF,DTMSTRDY         End day - current value in array             
         CR    RE,RF                                                            
         JH    *+8                                                              
         STC   RF,SVOFLEDY         Save new absolute end day                    
*                                                                               
PX02_36M CLI   DTMDAYS_+0,C'Y'     Monday?                                      
         JNE   *+8                                                              
         OI    RMKGDAYS,X'40'                                                   
         CLI   DTMDAYS_+1,C'Y'     Tuesday?                                     
         JNE   *+8                                                              
         OI    RMKGDAYS,X'20'                                                   
         CLI   DTMDAYS_+2,C'Y'     Wednesday?                                   
         JNE   *+8                                                              
         OI    RMKGDAYS,X'10'                                                   
         CLI   DTMDAYS_+3,C'Y'     Thursday?                                    
         JNE   *+8                                                              
         OI    RMKGDAYS,X'08'                                                   
         CLI   DTMDAYS_+4,C'Y'     Friday?                                      
         JNE   *+8                                                              
         OI    RMKGDAYS,X'04'                                                   
         CLI   DTMDAYS_+5,C'Y'     Saturday?                                    
         JNE   *+8                                                              
         OI    RMKGDAYS,X'02'                                                   
         CLI   DTMDAYS_+6,C'Y'     Sunday?                                      
         JNE   *+8                                                              
         OI    RMKGDAYS,X'01'                                                   
*                                                                               
         MVC   RMKGDYT1,DTMSTRTM   Start time                                   
         MVC   RMKGDYT2,DTMENDTM   End time                                     
         CLI   DTMETMCC,YESQ       End time is CC?                              
         JNE   *+10                                                             
         MVC   RMKGDYT2,=C'CC'     To indicate end time is CC                   
         DROP  R1                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PX02_42  LA    R2,DTMLNQ(R2)       Point to next day/time in array              
         JCT   R3,PX02_36                                                       
*                                                                               
         J     EXIT                                                             
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SVOFRLN# => Contains offer line number to be processed                        
* AIO1     => Contains record to be modified                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCX03EL NTR1  BASE=*,LABEL=*      Process multiple X'03' element               
*                                                                               
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    PX03_20                                                          
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    PX03_30                                                          
         DC    H'0'                Unsupported action                           
*                                                                               
PX03_20  L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'03',(RF)),0,0                  
*                                                                               
PX03_30  SR    R2,R2                                                            
         ICM   R2,7,QAEFD          Point to effective date data array           
         JZ    EXIT                                                             
         MVC   NUM_EFD_,LW_NUMN-LW_D(R2)                                        
         USING EFD_D,R2                                                         
         AHI   R2,LW_LN2Q          Point to 1st set of eff date data            
         SR    R3,R3                                                            
         ICM   R3,3,NUM_EFD_       # of effective date data to process          
*                                                                               
PX03_36  CLC   SVOFRLN#,EFDLINE#   Eff date for current offer line?             
         JNE   PX03_42                                                          
         XC    ELEM,ELEM           Building effective date elem                 
         LA    R4,ELEM                                                          
         USING RMKGDTEL,R4                                                      
         MVI   RMKGDTCD,X'03'      Effective date element code                  
         MVI   RMKGDTLN,X'0B'      Element length                               
         MVC   RMKGDTST,EFDSTRDT   Start date                                   
         MVC   RMKGDTED,EFDENDDT   End date                                     
         OC    RMKGDTED,RMKGDTED   Have end date?                               
         JNZ   *+10                                                             
         MVC   RMKGDTED,RMKGDTST   Set to start end if no end date              
*                                                                               
         OC    FIRSTAIR,FIRSTAIR   Have first air date?                         
         JNZ   *+10                                                             
         MVC   FIRSTAIR,RMKGDTST   Set start date to first air date             
         CLC   FIRSTAIR,RMKGDTST                                                
         JH    *-12                                                             
*                                                                               
         CLI   EFDWAWIN,C'E'       Every week?                                  
         JNE   *+8                                                              
         OI    RMKGDTIN,X'80'                                                   
         CLI   EFDWAWIN,C'A'       Alternating week?                            
         JNE   *+8                                                              
         OI    RMKGDTIN,X'40'                                                   
         CLI   RMKGDTIN,0                                                       
         JNE   *+8                                                              
         OI    RMKGDTIN,X'80'      Set to every week (default)                  
*                                                                               
         MVC   RMKGDTNW,EFDSPTWK   Spot per week                                
         MVC   SVOFLNPW,RMKGDTNW                                                
*                                                                               
         MVI   RMKGDTWK,0          Init # of week                               
         XC    WORK,WORK                                                        
         GOTOR VDATCON,DMCB,(3,RMKGDTST),(0,WORK+12)                            
         GOTOR VDATCON,DMCB,(3,RMKGDTED),(0,WORK+18)                            
         MVC   WORK(6),WORK+12     Start date in YYMMDD                         
         MVI   BYTE1,07            Set to 1 week                                
         TM    RMKGDTIN,X'40'      Alternating week?                            
         JZ    *+8                                                              
         MVI   BYTE1,14            Set to 2 weeks                               
*                                                                               
PX03_38  LLC   RE,RMKGDTWK                                                      
         AHI   RE,1                Bump up a week                               
         STC   RE,RMKGDTWK                                                      
         LLC   RF,BYTE1            Number of days to add                        
         GOTOR VADDAY,DMCB,WORK,WORK+6,(RF)                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),WORK+18     Past end date?                               
         JNH   PX03_38                                                          
*                                                                               
         LLC   RE,RMKGDTWK         Number of weeks                              
         LLC   RF,SVOFLTWK                                                      
         AR    RF,RE                                                            
         STC   RF,SVOFLTWK         Sum of total # of weeks                      
*                                                                               
         LLC   RE,RMKGDTNW         Spots per week                               
         STH   RE,HALF1                                                         
         LLC   RE,RMKGDTWK         Number of weeks                              
         MH    RE,HALF1            Total spots                                  
         SR    RF,RF                                                            
         ICM   RF,3,SVOFLTSP                                                    
         AR    RF,RE                                                            
         STCM  RF,3,SVOFLTSP       Sum of total spots for all X'03' elm         
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
PX03_42  LA    R2,EFDLNQ(R2)       Point to next eff date in array              
         JCT   R3,PX03_36                                                       
*                                                                               
         J     EXIT                                                             
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SVOFRLN# => Contains offer line number to be processed                        
* AIO1     => Contains record to be modified                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCX05EL NTR1  BASE=*,LABEL=*      Process multiple X'05' element               
*                                                                               
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    PX05_20                                                          
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    PX05_30                                                          
         DC    H'0'                Unsupported action                           
*                                                                               
PX05_20  L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'05',(RF)),0,0                  
*                                                                               
PX05_30  SR    R2,R2                                                            
         ICM   R2,7,QAMIS          Point to missed buy data array               
         JZ    EXIT                                                             
         MVC   NUM_MIS_,LW_NUMN-LW_D(R2)                                        
         USING MIS_D,R2                                                         
         AHI   R2,LW_LN2Q          Point to 1st set of missed buy data          
         SR    R3,R3                                                            
         ICM   R3,3,NUM_MIS_       # of missed buy data to process              
*                                                                               
         CLI   SVOFRTYP,C'P'       Preempt/credit?                              
         JNE   PX05_36                                                          
         CLC   NUM_MIS_,NUM_OFL_   # of missed buys = # of offer lines?         
         JE    *+6                                                              
         DC    H'0'                Uneven entries for Preempt type              
         LHI   R3,1                Set to 1 missed buy entry to process         
         CLI   SVOFRLN#,1          First offer line?                            
         JE    PX05_36                                                          
         LLC   RE,SVOFRLN#         Get offer line number                        
         BCTR  RE,0                Adjustment for looping                       
         LA    R2,MISLNQ(R2)       Point to next missed buy in array            
         JCT   RE,*-4                                                           
*                                                                               
PX05_36  XC    ELEM,ELEM           Build Buy make-good reference elem           
         LA    R1,ELEM                                                          
         USING RMKGMGEL,R1                                                      
         MVI   RMKGMGCD,X'05'      Buy make-good reference elem code            
         MVI   RMKGMGLN,X'0A'      Element length                               
         MVC   RMKGMGLI,MISLINE#   Missed buy line number                       
         MVC   RMKGMGD1,MISSTRDT   Missed start date                            
         MVC   RMKGMGD2,MISENDDT   Missed end date                              
         MVI   RMKGMGSP,0          Init missed spots per week                   
         CLI   SVOFRTYP,C'M'       Makegood offer type?                         
         JNE   PX05_36M                                                         
         CLI   SVOFRLN#,1          Offer line 1?                                
         JNE   *+10                                                             
PX05_36M MVC   RMKGMGSP,MISSPTWK   Missed spots per week                        
         DROP  R1                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PX05_42  LA    R2,MISLNQ(R2)       Point to next missed buy in array            
         JCT   R3,PX05_36                                                       
*                                                                               
         J     EXIT                                                             
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* R2       => Points to current processing offer line data in array             
* AIO1     => Contains record to be modified                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCREMEL NTR1  BASE=*,LABEL=*      Process remaining elements                   
*                                                                               
         USING OFL_D,R2                                                         
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    PREM_20                                                          
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    PREM_30                                                          
         DC    H'0'                Unsupported action                           
*                                                                               
PREM_20  L     R4,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'11',(R4)),0,0                  
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'21',(R4)),0,0                  
*                                                                               
PREM_30  L     R1,AIO1                                                          
         LA    R1,FRSTELEM(R1)                                                  
PREM_30M CLI   0(R1),0             End of record?                               
         JE    PREM_32                                                          
         CLI   0(R1),RMKGDMCQ      Rep demo value element?                      
         JE    PREM_30P                                                         
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         J     PREM_30M                                                         
         USING RMKGDMEL,R1                                                      
PREM_30P MVC   RMKGDMCT,SVPDEMCT   Primary demo category                        
         MVC   RMKGDM2M,RMKGDMDM   Save previous value                          
         MVC   RMKGDMDM,=X'FFFFFFFF'     Set to no value                        
         OC    OFLPRGRA,OFLPRGRA   Have program rating?                         
         JZ    PREM_40                                                          
         MVO   WORK(5),OFLPRGRA    Offered program rating                       
         MVC   RMKGDMDM,WORK       Demo value (PWOS)                            
         J     PREM_40                                                          
         DROP  R1                                                               
*                                                                               
PREM_32  XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING RMKGDMEL,R1                                                      
         MVI   RMKGDMCD,RMKGDMCQ   Rep demo value element code                  
         MVI   RMKGDMLN,X'0D'      Element length                               
         MVC   RMKGDMCT,SVPDEMCT   Primary demo category                        
         MVC   RMKGDMDM,=X'FFFFFFFF'     Set to no value                        
         OC    OFLPRGRA,OFLPRGRA   Have program rating?                         
         JZ    PREM_32M                                                         
         MVO   WORK(5),OFLPRGRA    Offered program rating                       
         MVC   RMKGDMDM,WORK       Demo value (PWOS)                            
PREM_32M MVC   RMKGDM2M,=X'FFFFFFFF'     Set to no previous value               
         DROP  R1                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREM_40  CLC   OFLLNCM1,SPACES     Have offered line comment 1?                 
         JNH   PREM_44                                                          
         XC    ELEM,ELEM           Build Rep demo value element                 
         LA    R4,ELEM                                                          
         USING RMKGDCEL,R4                                                      
         MVI   RMKGDCCD,X'11'      Detail comment element code                  
*                                                                               
         LA    RE,OFLLNCM1                                                      
         AHI   RE,(MAXCOMLQ-1)     Point to last character in comment           
         LA    RF,MAXCOMLQ         Default to max comment length                
         CLI   0(RE),C' '                                                       
         JH    *+12                                                             
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         J     *-12                                                             
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RMKGDCCM(0),OFLLNCM1                                             
         AHI   RF,1+2              Adjustment for elem length                   
         STC   RF,RMKGDCLN                                                      
         DROP  R4                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREM_44  CLC   OFLLNCM2,SPACES     Have offered line comment 2?                 
         JNH   PREM_46                                                          
         XC    ELEM,ELEM           Build Rep demo value element                 
         LA    R4,ELEM                                                          
         USING RMKGDCEL,R4                                                      
         MVI   RMKGDCCD,X'11'      Detail comment element code                  
*                                                                               
         LA    RE,OFLLNCM2                                                      
         AHI   RE,(MAXCOMLQ-1)     Point to last character in comment           
         LA    RF,MAXCOMLQ         Default to max comment length                
         CLI   0(RE),C' '                                                       
         JH    *+12                                                             
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         J     *-12                                                             
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RMKGDCCM(0),OFLLNCM2                                             
         AHI   RF,1+2              Adjustment for elem length                   
         STC   RF,RMKGDCLN                                                      
         DROP  R4                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREM_46  CLC   OFLPROGM,SPACES     Have offered program?                        
         JNH   PREM_48                                                          
         XC    ELEM,ELEM           Build Rep demo value element                 
         LA    R4,ELEM                                                          
         USING RMKGPGEL,R4                                                      
         MVI   RMKGPGCD,RMKGPGEQ   Program name element code                    
*                                                                               
         LA    RE,OFLPROGM                                                      
         AHI   RE,(L'OFLPROGM-1)   Point to last character in program           
         LA    RF,L'OFLPROGM       Default to max program text length           
         CLI   0(RE),C' '                                                       
         JH    *+12                                                             
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         J     *-12                                                             
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RMKGPGM(0),OFLPROGM                                              
         AHI   RF,1+2              Adjustment for elem length                   
         STC   RF,RMKGPGLN                                                      
         DROP  R4                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREM_48  L     R1,AIO1                                                          
         LA    R1,FRSTELEM(R1)                                                  
PREM_48M CLI   0(R1),0             End of record?                               
         JE    PREM_48P                                                         
         CLI   0(R1),X'20'         Have status control elem?                    
         JE    PREM_50                                                          
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         J     PREM_48M                                                         
*                                                                               
PREM_48P XC    ELEM,ELEM           Build default status control elem            
         LA    R4,ELEM                                                          
         USING RMKGSTEL,R4                                                      
         MVI   RMKGSTCD,X'20'      Status control elem code                     
         MVI   RMKGSTLN,10         Status control elem length                   
         DROP  R4                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREM_50  DS    0H                                                               
*                                                                               
         J     EXIT                                                             
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCBUYLN NTR1  BASE=*,LABEL=*      Process buy lines                            
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JE    EXITY                                                            
*                                                                               
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    PBUYL30                                                          
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    PBUYL30                                                          
         CLI   QACTCODE,QAC_DELQ   Delete (MGX)?                                
         JE    PBUYL20                                                          
         DC    H'0'                Unsupported action                           
*                                                                               
PBUYL20  OC    SVBYLTAB,SVBYLTAB   Have buy lines to process X'66' elm?         
         JZ    EXITY                                                            
         LA    R2,SVBYLTAB                                                      
         USING MIS_D,R2            Note: only MISLINE# is used                  
PBUYL24  CLI   MISLINE#,0          End of table?                                
         JE    EXITY                                                            
         BRAS  RE,GTBUYLN          Get buy line                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),QOFFGRPC    Offer group code to be removed               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'66',(RF)),(2,ELEM),0           
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
         LA    R2,1(R2)            Bump to next entry in table                  
         J     PBUYL24                                                          
         DROP  R2                                                               
*                                                                               
PBUYL30  SR    R2,R2                                                            
         ICM   R2,7,QAMIS          Point to missed buy data array               
         JZ    EXITY                                                            
         MVC   NUM_MIS_,LW_NUMN-LW_D(R2)                                        
         USING MIS_D,R2                                                         
         AHI   R2,LW_LN2Q          Point to 1st set of missed buy data          
         SR    R3,R3                                                            
         ICM   R3,3,NUM_MIS_       # of missed buy data to process              
         SR    RE,RE                                                            
         AHI   RE,1                                                             
         STH   RE,HALF1            Set to first missed buy data                 
         XC    SVBYLTAB,SVBYLTAB   Init table of buy lines to process           
*                                                                               
PBUYL40  BRAS  RE,GTBUYLN          Get buy line                                 
*                                                                               
         LA    RE,SVBYLTAB         Point to table of buy lines to proc          
PBUYL42  CLI   0(RE),0             End of table?                                
         JE    PBUYL44             Not in table, need to process it             
         CLC   MISLINE#,0(RE)      Buy line match entry in table?               
         JE    PBUYL50             Already removed X'66' elems once             
         LA    RE,1(RE)            Bump to next buy line in table               
         J     PBUYL42                                                          
*                                                                               
PBUYL44  MVC   0(1,RE),MISLINE#    Save processed buy line                      
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),SVOFFGRC    Offer group code to be removed               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'66',(RF)),(2,ELEM),0           
*                                                                               
PBUYL50  XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING RBMGMSEL,RE                                                      
         MVI   RBMGMSCD,X'66'      Makegood missed element code                 
         MVI   RBMGMSLN,X'0C'      Element length                               
         MVC   RBMGMSGD,SVOFFGRC   Offer group code                             
         MVC   RBMGMSR#+0,MISLINE# Buy line #                                   
         MVI   RBMGMSR#+1,1        Hard coded 1 (RMKGKLIN)                      
         MVI   RBMGMMUL,1          Default multi-line to 1                      
         CLI   SVOFRTYP,C'P'       Preempt/credit?                              
         JNE   *+12                                                             
         LH    R1,HALF1                                                         
         STC   R1,RBMGMMUL         Set to current line # in array               
         MVC   RBMGMSDT,MISSTRDT   Start date                                   
         MVI   RBMGMSLI,1          Hard coded 1 (RMKGKLIN)                      
         MVC   RBMGMSSP,MISSPTWK   Missed # of spots per week                   
         DROP  RE                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PBUYL60  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
*                                                                               
         LH    R1,HALF1                                                         
         AHI   R1,1                Bump up missed buy set counter               
         STH   R1,HALF1                                                         
         LA    R2,MISLNQ(R2)       Point to next missed buy in array            
         JCT   R3,PBUYL40          Process next set of missed buy               
*                                                                               
         J     EXITY               Done updating X'66' elems in buy             
*                                                                               
GTBUYLN  ST    RE,SAVE_RE          Get buy line                                 
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R4,IOKEY                                                         
         USING RBUYKEY,R4                                                       
         MVI   RBUYKTYP,RBUYKIDQ   Buy record                                   
         MVC   RBUYKREP,LP_AGY     Rep code                                     
         MVC   RBUYKCON,SVCON9RV   Contract # in 9's comp reversed              
         MVC   RBUYKLIN,MISLINE#   Buy line #                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOREPDIR+IO1'                          
         J     GTBYLN40                                                         
GTBYLN20 GOTOR (#IOEXEC,AIOEXEC),'IOSQUP+IOREPDIR+IO1'                          
GTBYLN40 CLC   IOKEY(RBUYKPLN-RBUYKEY),IOKEYSAV                                 
         JE    *+6                                                              
         DC    H'0'                Buy line not found                           
*                                                                               
         CLC   RBUYKPLN,=X'FFFFFF' Default plan code?                           
         JNE   GTBYLN20                                                         
         CLC   RBUYKLIN,MISLINE#   Buy line found?                              
         JNE   GTBYLN20                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
*                                                                               
         DROP  RB,R2,R4                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UPDCONRC NTR1  BASE=*,LABEL=*      Update contract record                       
*                                                                               
         BRAS  RE,BLDCKEY          Build contract record key                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOREPDIR+IO1'                          
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JE    U_CONR40                                                         
*                                                                               
         CLI   SVMGOFLG,C'A'       Makegood offer added?                        
         JE    *+12                                                             
         CLI   SVMGOFLG,C'X'       Makegood offer deleted?                      
         JNE   U_CONR30                                                         
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1E'        Look for random flag element                 
         CLC   ELCODE,0(R2)                                                     
         JE    *+14                                                             
         BRAS  RE,NXTEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RCONRFEL,R2                                                      
         LLC   RE,RCONR#MO         Get number of makegood offers                
         CLI   SVMGOFLG,C'A'       Makegood offer added?                        
         JNE   *+12                                                             
         AHI   RE,1                Bump up count                                
         J     U_CONR22                                                         
         CLI   SVMGOFLG,C'X'       Makegood offer deleted?                      
         JNE   *+12                                                             
         SHI   RE,1                Bump down count                              
         J     U_CONR22                                                         
         DC    H'0'                Unsupported makegood flag                    
*                                                                               
U_CONR22 STC   RE,RCONR#MO         Save adjusted makegood offer counter         
         DROP  R2                                                               
*                                                                               
U_CONR30 L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'21'        Look for MG offers send element              
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   U_CONR34                                                         
         USING RCONMGEL,R2                                                      
         CLI   QACTCODE,QAC_CHGQ   Change?                                      
         JE    *+8                                                              
         OI    RCONMGFG,X'80'      Set makegood offer status to WIP             
         J     U_CONR40                                                         
         DROP  R2                                                               
*                                                                               
U_CONR34 XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING RCONMGEL,R2                                                      
         MVI   RCONMGCD,X'21'      Makegood offers send element code            
         MVI   RCONMGLN,RCONMGLQ   Element length                               
         OI    RCONMGFG,X'80'      Set makegood offer status to WIP             
         DROP  R2                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM,=C'ADD=CODE'            
*                                                                               
U_CONR40 XC    ELEM2,ELEM2         Prepare to build station user name           
         LA    R1,ELEM2                                                         
         USING RCONSUEL,R1                                                      
         MVI   RCONSUCD,RCONSUEQ   Element code                                 
         MVI   RCONSULN,RCONSUOQ+1 Default element length                       
         MVC   RCONSUDT,TODAYB     Set last changed date to today               
         CLC   QSTAUSRN,SPACES     Have station user name?                      
         JNH   U_CONR42                                                         
*                                                                               
         LA    R4,QSTAUSRN                                                      
         MVI   WKMAXLEN,L'QSTAUSRN                                              
         BRAS  RE,GETFLDLN         Get station user name length                 
         CHI   RF,0                Have input?                                  
         JNH   U_CONR44                                                         
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RCONSUNM(0),0(R4)   Station user name                            
         AHI   RF,1+RCONSUOQ       Adjustment for elem length                   
         STC   RF,RCONSULN                                                      
         DROP  R1                                                               
*                                                                               
U_CONR42 L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
U_CONR44 XC    ELEM2,ELEM2         Prepare to build station user name           
         LA    R1,ELEM2                                                         
         USING RCONSEEL,R1                                                      
         MVI   RCONSECD,RCONSEEQ   Element code                                 
         MVI   RCONSELN,RCONSEOQ+1 Default element length                       
         MVC   RCONSEDT,TODAYB     Set last changed date to today               
         CLC   QSTAUEML,SPACES     Have station user e-mail address?            
         JNH   U_CONR48                                                         
*                                                                               
         LA    R4,QSTAUEML                                                      
         MVI   WKMAXLEN,L'QSTAUEML                                              
         BRAS  RE,GETFLDLN         Get station user e-mail length               
         CHI   RF,0                Have input?                                  
         JNH   U_CONR54                                                         
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RCONSEML(0),0(R4)   Station user e-mail address                  
         AHI   RF,1+RCONSEOQ       Adjustment for elem length                   
         STC   RF,RCONSELN                                                      
         DROP  R1                                                               
*                                                                               
U_CONR48 L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
U_CONR54 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
*                                                                               
         J     EXITY                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* R4           points to input field                                            
* WKMAXLEN     contains max length of input field                               
* RF           will return actual length of input field                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETFLDLN LR    R0,RE               Get actual input field length                
         LA    RE,0(R4)                                                         
         LLC   RF,WKMAXLEN                                                      
         BCTR  RF,0                                                             
         AR    RE,RF               Point to last character in comment           
         LLC   RF,WKMAXLEN         Default to max input length                  
GETFLN12 CLI   0(RE),C' '                                                       
         JH    GETFLN_X                                                         
         CHI   RF,0                No input?                                    
         JNH   GETFLN_X                                                         
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         J     GETFLN12                                                         
GETFLN_X LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SNDREPLY NTR1  BASE=*,LABEL=*      Send reply record after completion           
*                                                                               
         CLI   QACTCODE,QAC_SNDQ   Send?                                        
         JE    EXIT                                                             
*                                                                               
         BRAS  RE,REPLYHDR                                                      
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',001),          +        
               ('LD_CHARQ',QOFFCODE),(L'QOFFCODE,0)                             
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',002),          +        
               ('LD_CHARQ',QSTACODE),(L'QSTACODE,0)                             
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',003),          +        
               ('LD_SPAKQ',QORDNUMB),(L'QORDNUMB,0)                             
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',004),          +        
               ('LD_CHARQ',SVOFFGRC),(L'SVOFFGRC,0)                             
*                                                                               
S_RPY50  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCMQMSG NTR1  BASE=*,LABEL=*,WORK=(R4,MQMSG_DL)                                
*                                                                               
         USING MQMSG_D,R4                                                       
*                                                                               
         CLI   QACTCODE,QAC_ADDQ   Add?                                         
         JE    PRCMQ10                                                          
         CLI   QACTCODE,QAC_DELQ   Delete (MGX)?                                
         JE    PRCMQ10                                                          
         J     EXIT                                                             
*                                                                               
PRCMQ10  BRAS  RE,BLDCKEY          Build contract record key                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO3'                            
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,GTCONVAL         Get contract values                          
         BRAS  RE,INISTATS         Initialize status for MQ message             
*                                                                               
         LA    R0,MQMSG                                                         
         LHI   R1,MQMSGLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear MQ message                             
*                                                                               
         MVC   MSSAGEID+00(2),SVCONREP                                          
         MVC   MSSAGEID+02(8),SVCON#CH                                          
         MVC   MSSAGEID+10(06),=C'REPLIN'                                       
*                                                                               
         LA    R1,MQMSG                                                         
         MVC   0(L'QULABEL,R1),QULABEL                                          
         LA    R1,L'QULABEL(R1)                                                 
         MVC   0(L'CRLF,R1),CRLF   Add CRLF                                     
         LA    R1,L'CRLF(R1)       Bump past CRLF                               
*                                                                               
         LA    R2,L'MGOUPDTE                                                    
         LA    R3,MGOUPDTE                                                      
         BRAS  RE,STARTTAG                                                      
*                                                                               
         LA    R2,L'MQMSGFRM                                                    
         LA    R3,MQMSGFRM                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVMQMFRM                                                    
         LA    RF,SVMQMFRM                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'MASTREPC                                                    
         LA    R3,MASTREPC                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'MASTRCOD                                                    
         LA    RF,MASTRCOD                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'REPCODE_                                                    
         LA    R3,REPCODE_                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'AGY                                                         
         LA    RF,AGY                                                           
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'STATION_                                                    
         LA    R3,STATION_                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVCONSTA                                                    
         LA    RF,SVCONSTA                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'REPORDR#                                                    
         LA    R3,REPORDR#                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVCON#CH                                                    
         LA    RF,SVCON#CH                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'TRAFORD#                                                    
         LA    R3,TRAFORD#                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVTRORD#,SVTRORD#                                                
         JZ    *+16                                                             
         LA    R0,L'SVTRORD#                                                    
         LA    RF,SVTRORD#                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'VERSION_                                                    
         LA    R3,VERSION_                                                      
         BRAS  RE,STARTTAG                                                      
         EDIT  (B1,SVVERSN#),(3,0(R1)),ALIGN=LEFT,ZERO=NOBLANK                  
         AHI   R1,3                                                             
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'MOD_____                                                    
         LA    R3,MOD_____                                                      
         BRAS  RE,STARTTAG                                                      
         CLI   SVCONMOD,X'FF'                                                   
         JE    PRCMQ31X                                                         
         EDIT  (B1,SVCONMOD),(3,0(R1)),ALIGN=LEFT,ZERO=NOBLANK                  
         AHI   R1,3                                                             
PRCMQ31X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'WIP_____                                                    
         LA    R3,WIP_____                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'WIPSFLAG                                                    
         LA    RF,WIPSFLAG                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'STATUS__                                                    
         LA    R3,STATUS__                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'MQMSGSTA                                                    
         LA    RF,MQMSGSTA                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'LASSNDON                                                    
         LA    R3,LASSNDON                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVLASSDT,SVLASSDT                                                
         JZ    PRCMQ34X                                                         
         LR    R0,R1                                                            
         LR    RF,R1                                                            
         GOTOR VDATCON,DMCB,(2,SVLASSDT),(20,0(RF))                             
         LR    R1,R0                                                            
         AHI   R1,10                                                            
PRCMQ34X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'LASSNDTM                                                    
         LA    R3,LASSNDTM                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVSNDTIM,SVSNDTIM                                                
         JZ    *+14                                                             
         MVC   0(L'SVSNDTIM,R1),SVSNDTIM                                        
         AHI   R1,L'SVSNDTIM                                                    
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'OFPENREP                                                    
         LA    R3,OFPENREP                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVOFRPND                                                    
         LA    RF,SVOFRPND                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,L'MGOUPDTE                                                    
         LA    R3,MGOUPDTE                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R2,MQMSG                                                         
         LR    R3,R1                                                            
         SR    R3,R2               Length of MQ message                         
         CHI   R3,MQMSG_DL                                                      
         JNH   *+6                                                              
         DC    H'0'                Big message                                  
*                                                                               
         GOTOR AMQIO,DMCB,=CL8'PUT',(R2),(R3),0,0,DUB,C'UNIM',MSSAGEID          
*                                                                               
         J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
STARTTAG MVI   0(R1),OPENBRKQ      Build start tag <....>                       
         AHI   R1,1                                                             
         BCTR  R2,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R1),0(R3)                                                    
         EX    R2,0(RF)                                                         
         AHI   R2,1                                                             
         AR    R1,R2                                                            
         MVI   0(R1),CLOSBRKQ                                                   
         AHI   R1,1                                                             
         BR    RE                                                               
*                                                                               
END__TAG BCTR  R1,0                Check trailing spaces/nulls                  
         CLI   0(R1),0             Null?                                        
         JE    END__TAG                                                         
         CLI   0(R1),C' '          Space?                                       
         JE    END__TAG                                                         
         AHI   R1,1                Bump to build end tag                        
         MVI   0(R1),OPENBRKQ      Build end tag </...>                         
         MVI   1(R1),BACKSLAQ                                                   
         AHI   R1,1+1                                                           
         BCTR  R2,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R1),0(R3)                                                    
         EX    R2,0(RF)                                                         
         AHI   R2,1                                                             
         AR    R1,R2                                                            
         MVI   0(R1),CLOSBRKQ                                                   
         AHI   R1,1                                                             
         BR    RE                                                               
         DROP  RB                                                               
*                                                                               
CKSPCHAR CLI   0(RF),AMP____Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XML_AMPQ,R1),XML_AMPQ                                        
         AHI   R1,L'XML_AMPQ                                                    
         J     CKSPCH60                                                         
         CLI   0(RF),LT_____Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XML_LT_Q,R1),XML_LT_Q                                        
         AHI   R1,L'XML_LT_Q                                                    
         J     CKSPCH60                                                         
         CLI   0(RF),GT_____Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XML_GT_Q,R1),XML_GT_Q                                        
         AHI   R1,L'XML_GT_Q                                                    
         J     CKSPCH60                                                         
         CLI   0(RF),QUOT___Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XMLQUOTQ,R1),XMLQUOTQ                                        
         AHI   R1,L'XMLQUOTQ                                                    
         J     CKSPCH60                                                         
         CLI   0(RF),APOS___Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XMLAPOSQ,R1),XMLAPOSQ                                        
         AHI   R1,L'XMLAPOSQ                                                    
         J     CKSPCH60                                                         
         MVC   0(1,R1),0(RF)       Save input field one char at a time          
         AHI   R1,1                Bump to next char in MQ message              
CKSPCH60 AHI   RF,1                Bump to next char in input field             
         JCT   R0,CKSPCHAR         Loop for all chars in input field            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INISTATS NTR1  BASE=*,LABEL=*      Initialize status for MQ message             
*                                                                               
         XC    MQMSGSTA,MQMSGSTA                                                
*                                                                               
         CLI   SVVERSN#,1          Version at 1?                                
         JNE   INISTA20                                                         
         TM    SVCONFSW,X'40'      Confirmed?                                   
         JNZ   INISTA22                                                         
         MVC   MQMSGSTA(L'NEW___TX),NEW___TX                                    
         J     INISTA_X                                                         
*                                                                               
INISTA20 TM    SVCONFSW,X'40'      Confirmed?                                   
         JZ    INISTA30                                                         
INISTA22 XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING RCFCKEY,R5                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,SVCONREP                                                
         MVC   RCFCKCON,SVCONCON                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   RCFCKEY,IOKEYSAV    Have partial confirmation comment?           
         JNE   INISTA26                                                         
         TM    RCFCCNTL-2,X'80'    Deleted?                                     
         JNZ   INISTA26                                                         
         MVC   MQMSGSTA(L'PARTCFTX),PARTCFTX                                    
         J     INISTA_X                                                         
INISTA26 MVC   MQMSGSTA(L'CONFIRTX),CONFIRTX                                    
         J     INISTA_X                                                         
         DROP  R5                                                               
*                                                                               
INISTA30 CLI   CONWSIDE,C'R'       Rep side?                                    
         JNE   INISTA40                                                         
         MVC   MQMSGSTA(L'REVISDTX),REVISDTX                                    
         J     INISTA_X                                                         
INISTA40 CLI   CONWSIDE,C'S'       Station side?                                
         JNE   INISTA50                                                         
         MVC   MQMSGSTA(L'RETURNTX),RETURNTX                                    
         J     INISTA_X                                                         
*                                                                               
INISTA50 DS    0H                                                               
*                                                                               
INISTA_X J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_BPID LR    R0,RE                                                            
         XC    HALF1,HALF1         Binary PID to be returned                    
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,(2,0),0,0                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       Have secret code?                            
         JZ    *+10                                                             
         MVC   HALF1,FAPASSWD      Return password ID number                    
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP  RB                                                               
                                                                                
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG ,                                                                
                                                                                
* Fields for Offer Group Header record                                          
OFFCDLIT DC    C'Office Code'                                                   
STACLLIT DC    C'Station Call Letters'                                          
ORDNOLIT DC    C'Order Number'                                                  
OFRGCLIT DC    C'Offer Group Code'                                              
ACTCDLIT DC    C'Action Code'                                                   
GRCOMLIT DC    C'Group Comment'                                                 
STAUNLIT DC    C'Station User Name'                                             
STAUELIT DC    C'Station User E-mail'                                           
                                                                                
* Fields for Offer Group - Missed Buys                                          
MSDATLIT DC    C'Missed Buy data'                                               
MSLN#LIT DC    C'Missed Line Number'                                            
MSSTDLIT DC    C'Missed Start Date'                                             
MSENDLIT DC    C'Missed End Date'                                               
MSSPWLIT DC    C'Missed Spots/Week'                                             
                                                                                
* Fields for Offer Line record                                                  
OLLN#LIT DC    C'Offer Line Number'                                             
OLOTYLIT DC    C'Offer Type'                                                    
OLRATLIT DC    C'Offer Rate'                                                    
OLLENLIT DC    C'Offer Length'                                                  
OLLNILIT DC    C'Offer Length Indicator'                                        
OLPRGLIT DC    C'Offer Program'                                                 
OLLCMLIT DC    C'Offer Line Comment'                                            
OLPGRLIT DC    C'Offer Program Rating'                                          
                                                                                
* Fields for Offer Line - Day/Time elements                                     
OLX02LIT DC    C'Offer Line Number - X02'                                       
OLSTDLIT DC    C'Start Day'                                                     
OLENDLIT DC    C'End Day'                                                       
OLDAYLIT DC    C'Days'                                                          
OLSTTLIT DC    C'Start Time'                                                    
OLENTLIT DC    C'End Time'                                                      
OLECCLIT DC    C'End Time is CC?'                                               
                                                                                
* Fields for Offer Line - Effective Date elements                               
OLX03LIT DC    C'Offer Line Number - X03'                                       
OLESDLIT DC    C'Start Date'                                                    
OLEEDLIT DC    C'End Date'                                                      
OLWKALIT DC    C'Week/Alt Wk Indicator'                                         
OLOSPLIT DC    C'Offered Spots/Week'                                            
                                                                                
TXERRADD DC    C'Cannot add, record already exist'                              
TXERRCHG DC    C'Cannot change, record not found'                               
TXERRDEL DC    C'Cannot delete, record not found'                               
TXEINVAC DC    C'Invalid update action - '                                      
TXEINVAL DC    C'Invalid input field - '                                        
TXEMISSG DC    C'Missing input field - '                                        
TXEMSBYI DC    C'Missed Buy data incomplete'                                    
TXEOFRLI DC    C'Offer line data incomplete'                                    
TXEDTEFF DC    C'Day/time and effecitve date data incomplete'                   
                                                                                
COMPLMSG DC    C'Makegood offer send'                                           
                                                                                
OPENBRKQ EQU   C'<'                                                             
CLOSBRKQ EQU   C'>'                                                             
BACKSLAQ EQU   C'/'                                                             
*                                                                               
AMP____Q EQU   C'&&'               Ampersand                                    
LT_____Q EQU   C'<'                Less than                                    
GT_____Q EQU   C'>'                Greater than                                 
QUOT___Q EQU   C'"'                Quotation mark                               
APOS___Q EQU   C''''               Apostrophe                                   
*                                                                               
XML_AMPQ DC    C'&&amp;'           XML equivalent of &                          
XML_LT_Q DC    C'&&lt;'            XML equivalent of <                          
XML_GT_Q DC    C'&&gt;'            XML equivalent of >                          
XMLQUOTQ DC    C'&&quot;'          XML equivalent of "                          
XMLAPOSQ DC    C'&&apos;'          XML equivalent of '                          
*                                                                               
CRLF     DC    X'0D25'             CRLF                                         
QULABEL  DC    CL16'REPCON2STX******'                                           
*                                                                               
PARTCFTX DC    C'PARTIALLY CONFIRMED'                                           
CONFIRTX DC    C'CONFIRMED'                                                     
RETURNTX DC    C'RETURNED'                                                      
REVISDTX DC    C'REVISED'                                                       
NEW___TX DC    C'NEW'                                                           
*                                                                               
MGOUPDTE DC    C'offer-update'                                                  
*                                                                               
MQMSGFRM DC    C'MQ-message-from'                                               
MASTREPC DC    C'masterrepcode'                                                 
REPCODE_ DC    C'repcode'                                                       
STATION_ DC    C'station'                                                       
REPORDR# DC    C'rep-order-number'                                              
TRAFORD# DC    C'traffic-order-number'                                          
VERSION_ DC    C'version'                                                       
MOD_____ DC    C'mod'                                                           
WIP_____ DC    C'wip'                                                           
STATUS__ DC    C'status'                                                        
LASSNDON DC    C'last-sent-on'                                                  
LASSNDTM DC    C'last-sent-time'                                                
OFPENREP DC    C'offers-pending-rep'                                            
*                                                                               
                                                                                
PZERO    DC    P'0'                                                             
DMKEY    DC    C'DMKEY   '                                                      
DMCOMMIT DC    C'COMMIT  '                                                      
                                                                                
FILES    DS    0X                  ** System/file list **                       
         DC    C'REP    '          System for open calls                        
         DC    C'N'                                                             
REPDIR   DC    C'REPDIR '                                                       
         DC    C'N'                                                             
REPFIL   DC    C'REPFIL '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
                                                                                
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
SYSREPQ  EQU   C'R'                Rep system letter                            
EOR      EQU   0                   End of record element code                   
MAXGCMLQ EQU   70                  Max length of group comment lines            
MAXCOMLQ EQU   60                  Max length of normal comment lines           
MAXDTCHQ EQU   10                  Max length of date character format          
DDSTMADJ EQU   6                   DDS time adjustment                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SAVED    DSECT ,                   ** Saved storage **                          
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
LINKIO   DS    A                   A(LINKIO)                                    
ALIOB    DS    A                   A(LINKIO CONTROL BLOCK)                      
AMQIO    DS    A                   A(MQIO)                                      
                                                                                
ALASTELM DS    A                   A(last element)                              
                                                                                
WKBASERD DS    F                                                                
SAVE_RE  DS    F                                                                
WKRECLEN DS    H                   Record length                                
                                                                                
AGY      DS    CL2                 Agency code                                  
MASTRCOD DS    CL2                 Master Rep Code                              
                                                                                
WKHPASSW DS    CL1                 Offer header record passives switch          
HPASADDQ EQU   C'A'                Offer header is added                        
HPASCHGQ EQU   C'C'                Offer header is changed                      
HPASDELQ EQU   C'D'                Offer header is deleted                      
                                                                                
SVOFHKEY DS    XL32                Saved offer header key                       
                                                                                
SVMQMFRM DS    CL3                 MQ message from LIN program                  
MQMSGSTA DS    CL19                MQ message status                            
WIPSFLAG DS    CL1                                                              
MSSAGEID DS    XL24                Message ID                                   
                                                                                
SVCON9CM DS    XL(L'RCONPCON)      Contract # in 9's complement                 
SVCON9RV DS    XL(L'RMKGKCON)      Con# in 9's complement/Reversed              
                                                                                
CONWSIDE DS    CL1                 Contract side - Rep/Station/Both             
CONSNDSW DS    CL1                 Contract has SEND elem switch - Y/N          
SVVERSN# DS    XL1                 Current version number                       
SVMKGSTA DS    XL(L'RMKGSCST)      Current offer group status                   
SVMKGFG1 DS    XL(L'RMKGSFG1)      Current offer group DARE flag                
SVMKGFG2 DS    XL(L'RMKGSFG2)      Current offer group change flag              
                                                                                
SVMKGSAL DS    CL(L'RMKGXSAL)      Salesperson code                             
SVMKGTEM DS    CL(L'RMKGXTEM)      Team code                                    
SVMKGADV DS    CL(L'RMKGXADV)      Advertiser code                              
SVMKGDSP DS    CL(L'RMKGXDSP)      Developemental salesperson code              
                                                                                
RUNINDS  DS    X                   Local indicators                             
RUNINIT  EQU   X'01'               1st time building disk address list          
                                                                                
RPYIND1  DS    X                                                                
RPYHDRTQ EQU   X'80'               Header reply record is replied               
                                                                                
MQSVR    DS    XL(L'LP_MQSVR)      External service call                        
                                                                                
SVERRIND DS    XL1                 Error indicator                              
SVERRFLD DS    AL2                 Map code of error field                      
ERRMSGST DS    XL8                 Error msg start (dummy fld header)           
ERRORMSG DS    CL(MAXCOMLQ)        Error message                                
                                                                                
ERRNUM   DS    XL2                 Error number                                 
FIRSTAIR DS    XL3                 First air date of offers                     
FATALERR DS    C                                                                
WKMAXLEN DS    X                   Max input length                             
SVMGOFLG DS    CL1                 Saved makegood offers add/del'd flag         
SVOFRLN# DS    XL(L'RMKGKRTY)      Saved current offer line# in array           
SVOFRTYP DS    CL(L'OFLTYPE_)      Saved current offer type in array            
SVBYLTAB DS    XL256               Table of 255 buy lines and EOT               
SVBYLTB2 DS    XL256               Table of 255 buy lines and EOT (2)           
                                                                                
OFCBLK   DS    XL(OFCLENQ)         OFFICER block                                
                                                                                
SVCONVAL DS    0X                  Start of saved contract values               
SVCONREP DS    XL(L'RCONKREP)      Contract rep code                            
SVCONSTA DS    XL(L'RCONKSTA)      Contract station call letters                
SVCONCON DS    XL(L'RCONKCON)      Contract #                                   
SVCON#CH DS    CL8                 Contract number in character format          
SVCONCRV DS    XL(L'RCONSRV)       Current rep version                          
SVCONCSV DS    XL(L'RCONSSV)       Current station version                      
SVCONMOD DS    XL(L'RCONMOD)       Contract mod number                          
SVLASSDT DS    XL(L'RCONSRDT)      Save last send date                          
SVSNDTIM DS    CL(L'RCONSRTI)      Save last send time                          
SVCONRLK DS    XL(L'RCONDRLK)      DARE agency order number                     
SVPDEMCT DS    XL(L'RMKGDMCT)      Primary demo category                        
SVTRORD# DS    XL(L'RCONTRF)       Traffic order number                         
SVCONFSW DS    XL(L'RCONCONF)      Save confirmation switch                     
SVOFRPND DS    CL1                 Save makegood offer pending flag             
SVTAKOVR DS    CL1                 Takeover contract switch                     
SVCONVLQ EQU   *-SVCONVAL          Length of saved contract values              
                                                                                
SVOFLVAL DS    0X                  Start of saved offer line values             
SVOFLNPW DS    XL(L'RMKGNW)        Number per week  => XL1                      
SVOFLTSP DS    XL(L'RMKGTSPT)      Total spot       => XL2                      
SVOFLTWK DS    XL(L'RMKGTWKS)      Total weeks      => XL1                      
SVOFLSDY DS    XL(L'DTMSTRDY)      Start days       => XL1                      
SVOFLEDY DS    XL(L'DTMENDDY)      End days         => XL1                      
*                                                                               
SVOFLVLQ EQU   *-SVOFLVAL          Length of saved offer line values            
                                                                                
QVALUES  DS    0X                  Request values                               
*                                                                               
QOFFCODE DS    CL(L'RMKGKOFF)      Office code                                  
QSTACODE DS    CL(L'RMKGKSTA)      Station call letter                          
QORDNUMB DS    CL(L'RMKGKCON+1)    Order number                                 
QOFFGRPC DS    CL(L'RMKGKGRP)      Offer group code                             
SVOFFGRC DS    CL(L'RMKGKGRP)      Save offer group code (from ADD)             
*                                                                               
QACTCODE DS    CL1                 Action code                                  
QAC_ADDQ EQU   C'A'                Add                                          
QAC_CHGQ EQU   C'C'                Change                                       
QAC_SNDQ EQU   C'S'                Send                                         
QAC_DELQ EQU   C'X'                Delete (MGX)                                 
*                                                                               
QSTAUSRN DS    CL30                Station user name                            
QSTAUEML DS    CL60                Station user e-mail address                  
*                                                                               
QGRPCIND DS    X                   Array of Group comments                      
QAGRC    DS    AL3                                                              
NUM_GRC_ DS    XL(L'LW_NUMN)       Number of Group comments                     
*                                                                               
QMISIND  DS    X                   Array of Missed Buys                         
QAMIS    DS    AL3                                                              
NUM_MIS_ DS    XL(L'LW_NUMN)       Number of Missed buys entries                
*                                                                               
QOFLIND  DS    X                   Array of Offer Lines                         
QAOFL    DS    AL3                                                              
NUM_OFL_ DS    XL(L'LW_NUMN)       Number of Offer Lines entries                
*                                                                               
QDTMIND  DS    X                   Array of Day/Time elements                   
QADTM    DS    AL3                                                              
NUM_DTM_ DS    XL(L'LW_NUMN)       Number of Day/Time elem entries              
*                                                                               
QEFDIND  DS    X                   Array of Effective Date elements             
QAEFD    DS    AL3                                                              
NUM_EFD_ DS    XL(L'LW_NUMN)       Number of Eff Date elem entries              
*                                                                               
QVALUEL  EQU   *-QVALUES                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MQMSG_D  DSECT                                                                  
*                                                                               
         DS    0D                                                               
MQMSG    DS    4000C               MQ message build area                        
MQMSGX   DS    0X                                                               
MQMSGLNQ EQU   MQMSGX-MQMSG                                                     
MQMSG_DX EQU   *                                                                
MQMSG_DL EQU   MQMSG_DX-MQMSG_D                                                 
         EJECT                                                                  
                                                                                
* Other included books follow                                                   
         PRINT OFF                                                              
       ++INCLUDE RELNKWRK                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE REGLCON                                                        
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012RELNK23   05/02/14'                                      
         END                                                                    
