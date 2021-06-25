*          DATA SET RELNK22    AT LEVEL 012 AS OF 09/12/19                      
*PHASE T82B22A                                                                  
RELNK22  TITLE '- RepPak Station Xpress Upload'                                 
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
                                                                                
CODE     NMOD1 0,**RL22**                                                       
                                                                                
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
                                                                                
         BRAS  RE,SETGLOBB         Set Globber values                           
                                                                                
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
         MVI   SVREQTYP,0          Init request type                            
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
                                                                                
***********************************************************************         
* Rep Order upload                                                    *         
***********************************************************************         
                                                                                
         DS    0H                                                               
ORDERUP  LKREQ H,Q#UPORD,NEXTREQ=BUY_UPL,ROUTINE=ORDUPD,NEWREC=Y                
                                                                                
ActnCode LKREQ F,001,(D,B#SAVED,QACTCODE),CHAR,TEXT=(*,ACTCDLIT),COL=*          
SaleOrd# LKREQ F,002,(D,B#SAVED,QCONNUMB),SPAK,TEXT=(*,ORDNOLIT),COL=*          
Version# LKREQ F,003,(D,B#SAVED,QVERNUMB),UBIN,TEXT=(*,VERNOLIT),COL=*          
TrafOrd# LKREQ F,004,(D,B#SAVED,QTRAFORD),CHAR,TEXT=(*,TRAO#LIT),COL=*          
HdrStaOC LKREQ F,005,(I,B#SAVED,QSTAOCOM),CHAR,OLEN=MAXCOMLQ,          +        
               LIST=F,SORT=N,TEXT=(*,HSOCMLIT),COL=*,DELIM=X'00'                
MkGd2Flw LKREQ F,006,(D,B#SAVED,QMKGD2FW),CHAR,TEXT=(*,MG2FWLIT),COL=*          
Buyline# LKREQ F,007,(I,B#SAVED,QBUYLIN#),UBIN,OLEN=1,                 +        
               LIST=F,SORT=N,TEXT=(*,BYLN#LIT),COL=*                            
BuyOrdCm LKREQ F,008,(I,B#SAVED,QBUYOCOM),CHAR,OLEN=MAXCOMLQ*2,        +        
               LIST=F,SORT=N,TEXT=(*,BYOCMLIT),COL=*,DELIM=X'00'                
ParCmfCm LKREQ F,009,(I,B#SAVED,QPCFMCOM),CHAR,OLEN=MAXCOMLQ,          +        
               LIST=F,SORT=N,TEXT=(*,PCFMCLIT),COL=*,DELIM=X'00'                
StaUsrNm LKREQ F,010,(D,B#SAVED,QSTAUSRN),CHAR,TEXT=(*,STAUNLIT),COL=*          
StaUsrEm LKREQ F,011,(D,B#SAVED,QSTAUEML),CHAR,TEXT=(*,STAUELIT),COL=*          
                                                                                
         LKREQ E                                                                
                                                                                
ORDUPD   MVI   SVREQTYP,LKORDUPQ   Set request type to Order Update             
         CLI   FATALERR,C'Y'       Fatal error occurred?                        
         JE    EXITY                                                            
         GOTOR CHKGLOBB            Check Globber control object                 
         JE    EXITY                                                            
         BRAS  RE,PRCQMAPS         Process request maps                         
         JNE   EXITY                                                            
         BRAS  RE,PRCMQMSG         Process MQ message                           
         BRAS  RE,CHKUPMOD         Check update mode                            
         JE    EXITY                                                            
         GOTOR GLOBBCON            Call CON program                             
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Rep Buy upload                                                      *         
***********************************************************************         
                                                                                
         DS    0H                                                               
BUY_UPL  LKREQ H,Q#UPBUY,NEXTREQ=REQEND,ROUTINE=BUYUPD,NEWREC=Y                 
                                                                                
ActnCode LKREQ F,001,(D,B#SAVED,QACTCODE),CHAR,TEXT=(*,ACTCDLIT),COL=*          
SaleOrd# LKREQ F,002,(D,B#SAVED,QCONNUMB),SPAK,TEXT=(*,ORDNOLIT),COL=*          
Buyline# LKREQ F,007,(I,B#SAVED,QBUYLIN#),UBIN,OLEN=1,                 +        
               LIST=F,SORT=N,TEXT=(*,BYLN#LIT),COL=*                            
BuyOrdCm LKREQ F,008,(I,B#SAVED,QBUYOCOM),CHAR,OLEN=MAXCOMLQ*2,        +        
               LIST=F,SORT=N,TEXT=(*,BYOCMLIT),COL=*,DELIM=X'00'                
                                                                                
         LKREQ E                                                                
                                                                                
BUYUPD   MVI   SVREQTYP,LKBUYUPQ   Set request type to Buy Update               
         CLI   FATALERR,C'Y'       Fatal error occurred?                        
         JE    EXITY                                                            
         BRAS  RE,PRCQBUYS         Process request buy maps                     
         JNE   EXITY                                                            
         XC    REPLYMSG,REPLYMSG                                                
         MVC   REPLYMSG(L'TX_DONE_),TX_DONE_                                    
         BRAS  RE,RPLY_BUY         Reply buy updated message                    
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
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
PRCQMAPS NTR1  BASE=*,LABEL=*      Process request maps for Order Updt          
*                                                                               
         XC    NUMBUYL#,NUMBUYL#   Init # of buylines                           
         XC    NUMBUYOC,NUMBUYOC   Init # of buyline comments                   
         MVI   PRCMQMSW,NOQ        Init MQ msg processing switch                
         MVC   SVMQMFRM,=C'LIN'    Init MQ messaage from LIN program            
*                                                                               
         BRAS  RE,BLDCKEY          Build contract key                           
         BRAS  RE,R_GET                                                         
         JE    VQMAP10                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXECONNF),TXECONNF                                    
         EDIT  QCONNUMB,(8,ERRORMSG+L'TXECONNF),FILL=0                          
VQMAP08X BRAS  RE,REPLYERR                                                      
         J     DELGLB_X                                                         
*                                                                               
VQMAP10  BRAS  RE,EXTRSTAO         Extract options in station record            
         BRAS  RE,PSETCONR         Pre-set contract record                      
         BRAS  RE,CKWIPS           Check for WIP status                         
*                                                                               
         BRAS  RE,CKVERSN#         Check version number                         
         JNE   VQMAP08X                                                         
*                                                                               
         CLI   QACTCODE,QASENDRQ   Send?                                        
         JE    VQMAP30                                                          
         CLI   QACTCODE,QACONFMQ   Confirm?                                     
         JE    VQMAP40                                                          
         CLI   QACTCODE,QAPARTCQ   Partial Confirm?                             
         JE    VQMAP50                                                          
         CLI   QACTCODE,QAEC2TRQ   EC to Traffic?                               
         JE    VQMAP40                                                          
         CLI   QACTCODE,QATRFOUQ   Traffic Order# Update?                       
         JE    VQMAP70                                                          
*                                                                               
VQMAP20  XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEINVAC),TXEINVAC                                    
         MVC   ERRORMSG+L'TXEINVAC(L'QACTCODE),QACTCODE                         
         J     VQMAP08X                                                         
*                                                                               
VQMAP30  SR    R3,R3                                                            
         ICM   R3,7,ASTAOCOM       Have list of station order comments?         
         JZ    VQMAP38L                                                         
         MVC   NUMSTAOC,LW_NUMN-LW_D(R3)                                        
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,ABUYLIN#       Have list of buyline numbers?                
         JZ    *+14                                                             
         MVC   NUMBUYL#,LW_NUMN-LW_D(R3)                                        
         AHI   R3,LW_LN2Q                                                       
         SR    R4,R4                                                            
         ICM   R4,7,ABUYOCOM       Have list of buyline order comments?         
         JZ    *+14                                                             
         MVC   NUMBUYOC,LW_NUMN-LW_D(R4)                                        
         AHI   R4,LW_LN2Q                                                       
         SR    RE,RE                                                            
         ICM   RE,3,NUMBUYOC                                                    
         CHI   RE,0                Have buy order comment?                      
         JE    VQMAP30M            Done via Buy Update request                  
         CLC   NUMBUYL#,NUMBUYOC                                                
         JNE   VQMAP38F                                                         
*                                                                               
* * * *  CHI   RE,10               More than 10 comments in single upd?         
* * * *  JH    VQMAP38G                                                         
*                                                                               
VQMAP30M CLI   WIPSFLAG,YESQ       WIP?                                         
         JE    VQMAP38M                                                         
*                                                                               
         BRAS  RE,UPDCONR          Update contract record                       
         JNE   DELGLB_X                                                         
         BRAS  RE,UPDTRFO          Update traffic order number                  
         JNE   VQMAP76E                                                         
         BRAS  RE,R_PUT            Write back contract record                   
*                                                                               
         BRAS  RE,CKSIDE           Get bumped version values                    
*                                                                               
VQMAP32  SR    R0,R0                                                            
         ICM   R0,3,NUMBUYL#       R0=remaining Buyline count                   
         JZ    EXITY               Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMBUYL#                                                    
         MVC   SVBUYLIN,0(R3)                                                   
*                                                                               
         BRAS  RE,BLDBKEY          Build buy key                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         J     VQMAP32M                                                         
VQMAP32K GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO1'                            
VQMAP32M CLC   IOKEY(RBUYKPLN-RBUYKEY),IOKEYSAV                                 
         JNE   VQMAP38E                                                         
         LA    RE,IOKEY                                                         
         USING RBUYKEY,RE                                                       
         CLC   RBUYKPLN,=X'FFFFFF' Default plan code?                           
         JNE   VQMAP32K                                                         
         CLC   RBUYKLIN,SVBUYLIN   Buyline found?                               
         JNE   VQMAP32K                                                         
         BRAS  RE,R_GET                                                         
         JNE   VQMAP38E                                                         
         DROP  RE                                                               
*                                                                               
         L     RE,AIO1             Point to buy record                          
         USING RBUYREC,RE                                                       
         SR    RF,RF                                                            
         ICM   RF,3,RBUYLEN                                                     
         CHI   RF,MAXBUYLQ         Buy rec length near supported max?           
         JH    VQMAP38P                                                         
         DROP  RE                                                               
         BRAS  RE,UPDBUYLN         Update Buyline with order comments           
         BRAS  RE,R_PUT            Write back buy record                        
         AHI   R3,001              Point to next buyline number                 
         AHI   R4,120              Point to next buyline order comment          
         J     VQMAP32                                                          
*                                                                               
VQMAP38E XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEBLNNF),TXEBLNNF                                    
         EDIT  (1,SVBUYLIN),(3,ERRORMSG+L'TXEBLNNF),ALIGN=LEFT                  
         DC    H'0'                Need to unwind contract version              
         J     VQMAP08X                                                         
VQMAP38F XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMSB#C),TXEMSB#C                                    
         J     VQMAP08X                                                         
VQMAP38G XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXTOOMBY),TXTOOMBY                                    
         J     VQMAP08X                                                         
VQMAP38L XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'HSOCMLIT),HSOCMLIT                         
         J     VQMAP08X                                                         
VQMAP38M XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXERWIPS),TXERWIPS                                    
         J     VQMAP08X                                                         
VQMAP38P XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEBLRMX),TXEBLRMX                                    
         DC    H'0'                Need to unwind contract version              
         J     VQMAP08X                                                         
*                                                                               
VQMAP40  CLI   QACTCODE,QAEC2TRQ   EC to Traffic?                               
         JNE   *+12                                                             
         TM    SVSTAXOP,X'80'      EC is allowed?                               
         JZ    VQMAP77E                                                         
*                                                                               
         BRAS  RE,UPDCONR          Update contract record                       
         JNE   DELGLB_X                                                         
         BRAS  RE,UPDTRFO          Update traffic order number                  
         JNE   VQMAP76E                                                         
         BRAS  RE,R_PUT            Write back contract record                   
         J     EXITY                                                            
*                                                                               
VQMAP50  SR    RE,RE                                                            
         ICM   RE,7,APCFMCOM       Have partial confirm comments?               
         JZ    VQMAP58E                                                         
         MVC   NUMPCFMC,LW_NUMN-LW_D(RE)                                        
*                                                                               
         CLI   QMKGD2FW,C' '       Have makegood to follow flag?                
         JNH   VQMAP58F                                                         
         LA    RF,QMKGD2FW                                                      
         BRAS  RE,CKYESNO          Valid Y/N input?                             
         JNE   VQMAP58G                                                         
*                                                                               
         BRAS  RE,UPDCONR          Update contract record                       
         JNE   DELGLB_X                                                         
         BRAS  RE,UPDTRFO          Update traffic order number                  
         JNE   VQMAP76E                                                         
         BRAS  RE,R_PUT            Write back contract record                   
*                                                                               
         BRAS  RE,PRCCFCOM         Process confirmation comments                
*                                                                               
         J     EXITY                                                            
*                                                                               
VQMAP58E XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'PCFMCLIT),PCFMCLIT                         
         J     VQMAP08X                                                         
*                                                                               
VQMAP58F XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'MG2FWLIT),MG2FWLIT                         
         J     VQMAP08X                                                         
*                                                                               
VQMAP58G XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEINVAL),TXEINVAL                                    
         MVC   ERRORMSG+L'TXEINVAL(L'MG2FWLIT),MG2FWLIT                         
         J     VQMAP08X                                                         
*                                                                               
VQMAP70  DS    0H                  Validate traffic order# here                 
*                                                                               
VQMAP74  BRAS  RE,UPDTRFO          Update traffic order number                  
         JNE   VQMAP76E                                                         
         BRAS  RE,R_PUT            Write back contract record                   
         J     EXITY                                                            
*                                                                               
**MAP76E XC    ERRORMSG,ERRORMSG                                                
**NO-OP  MVC   ERRORMSG(L'TXENOX1F),TXENOX1F                                    
VQMAP76E J     VQMAP08X                                                         
*                                                                               
VQMAP77E XC    ERRORMSG,ERRORMSG                                                
         LA    RF,396              Station not EC user                          
         BRAS  RE,GET_ETXT                                                      
         J     VQMAP08X                                                         
*                                                                               
DELGLB_X GOTOR VGLOBBER,DMCB,=C'DELE',ELEM2,GLRLINLQ,GLRKLINC                   
         J     EXITN                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKWIPS   ST    RE,SAVE_RE                                                       
         XC    SVTRORD#,SVTRORD#   Init traffic order number                    
         MVI   WIPSFLAG,NOQ        Set WIP=false                                
         L     R2,AIO1             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1F'        Look for Extended description elem           
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   CKWIP10                                                          
         USING RCONXEL,R2                                                       
         MVC   SVCONFSW,RCONCONF   Save confirmation switch                     
         MVC   SVTRORD#,RCONTRF    Save traffic order number                    
         TM    RCONCONF,X'40'      Confirmed now?                               
         JO    CKWIPX              Yes - not WIP                                
         DROP  R2                                                               
*                                                                               
CKWIP10  L     R2,AIO1             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'        Look for Send info elem                      
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   CKWIPX                                                           
         USING RCONSEND,R2                                                      
         TM    RCONSENF,X'30'      REP/STA version not advanced?                
         JO    CKWIPX                                                           
         MVI   WIPSFLAG,YESQ       Set WIP=true                                 
         DROP  R2                                                               
*                                                                               
CKWIPX   L     RE,SAVE_RE                                                       
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REPLYHDR LR    R0,RE                                                            
         TM    RPYIND1,RPYHDRTQ    Already replied header reply rec?            
         BNZR  RE                                                               
         SR    RF,RF                                                            
         CLI   SVREQTYP,LKORDUPQ   Order Update request?                        
         JNE   *+8                                                              
         LHI   RF,R#UPORD                                                       
         CLI   SVREQTYP,LKBUYUPQ   Buy Update request?                          
         JNE   *+8                                                              
         LHI   RF,R#UPBUY                                                       
         LTR   RF,RF                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',(RF))                   
         OI    RPYIND1,RPYHDRTQ                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
REPLYERR ST    RE,SAVE_RE                                                       
         BRAS  RE,REPLYHDR                                                      
         SR    RF,RF                                                            
         CLI   SVREQTYP,LKORDUPQ   Order Update request?                        
         JNE   *+8                                                              
         LHI   RF,008                                                           
         CLI   SVREQTYP,LKBUYUPQ   Buy Update request?                          
         JNE   *+8                                                              
         LHI   RF,001                                                           
         LTR   RF,RF                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',(RF)),         +        
               ('LD_CHARQ',ERRORMSG),(L'ERRORMSG,0)                             
         MVI   ERRNUM,X'FF'        Error is logged                              
         MVI   FATALERR,C'Y'       Error occurred                               
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDCKEY  XC    IOKEY,IOKEY         Prepare to build contract key                
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         USING RCONREC,R1                                                       
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),QCONNUMB                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   SVCON9CM,WORK                                                    
         MVI   RCONPTYP,X'8C'      Passive Pointer 1 for Contract rec           
         MVC   RCONPREP,AGY        Rep code                                     
         MVC   RCONPCON,WORK       Contract number in 9's complement            
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
BLDBKEY  LR    R0,RE                                                            
         XC    IOKEY,IOKEY         Prepare to build buy key                     
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RF,IOKEY                                                         
         USING RBUYREC,RF                                                       
         MVI   RBUYKTYP,X'0B'      Buy record type                              
         MVC   RBUYKREP,SVCONREP   Rep code                                     
         MVC   RBUYKPLN,=X'FFFFFF' Plan (package) code, default=3X'FF'          
         MVI   RBUYKMLN,0          Master line number                           
         MVC   RBUYKLIN,SVBUYLIN   Line number                                  
         GOTOR (RFCONNUM,AREPFACS),DMCB,(2,SVCON9CM),(3,RBUYKCON)               
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
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
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Check for version number                                           *         
*  AIO1 contains contract record to be updated                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
CKVERSN# NTR1  BASE=*,LABEL=*      Check version number                         
*                                                                               
         BRAS  RE,CKSIDE                                                        
*                                                                               
         CLI   QACTCODE,QASENDRQ   Send?                                        
         JE    CKVER#10                                                         
         CLI   QACTCODE,QACONFMQ   Confirm?                                     
         JE    CKVER#10                                                         
         CLI   QACTCODE,QAPARTCQ   Partial Confirm?                             
         JE    CKVER#10                                                         
*                                                                               
         J     CKVER#X                                                          
*                                                                               
CKVER#10 CLI   QVERNUMB,0          Have version number?                         
         JH    CKVER#16                                                         
         CLI   QACTCODE,QASENDRQ   Send? (Not required for SEND)                
         JE    CKVER#30                                                         
         J     CKVER#91                                                         
*                                                                               
CKVER#16 CLI   CONSNDSW,YESQ       Have SEND element?                           
         JNE   CKVER#93                                                         
         CLI   CONWSIDE,C'R'       On Rep's side?                               
         JE    CKVER#92                                                         
         CLC   QVERNUMB,SVVERSN#   Version number is not higher?                
         JH    CKVER#93                                                         
*                                                                               
CKVER#30 J     CKVER#X             Version number is validated                  
*                                                                               
CKVER#91 XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'VERNOLIT),VERNOLIT                         
         J     CKVER#E                                                          
*                                                                               
CKVER#92 XC    ERRORMSG,ERRORMSG                                                
         LA    RF,168              Latest Rep version not yet sent              
         BRAS  RE,GET_ETXT                                                      
         J     CKVER#E                                                          
*                                                                               
CKVER#93 XC    ERRORMSG,ERRORMSG                                                
         LA    RF,184              Latest version not yet sent                  
         BRAS  RE,GET_ETXT                                                      
         J     CKVER#E                                                          
*                                                                               
CKVER#X  J     EXITY                                                            
CKVER#E  J     EXITN                                                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Check if contract is on Rep/Station/Both side                       *         
* Returns R/S/B in CONWSIDE                                           *         
* Returns Y/N in CONSNDSW to indicate there's a SEND element or not   *         
* Returns R/S in CONWSIDE                                             *         
* Returns version number in SVVERSN#                                  *         
* Returns lastest Rep version in SVCONCRV                             *         
* Returns lastest Sta version in SVCONCSV                             *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
CKSIDE   ST    RE,SAVE_RE                                                       
         MVI   CONWSIDE,C'R'       Deafult to Rep side                          
         MVI   CONSNDSW,NOQ        Init to no SEND element                      
         MVI   SVVERSN#,0          Init version number                          
         L     R2,AIO1             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'        Look for Send info elem                      
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   CKSIDEX             Not found, on Rep side                       
*                                                                               
         USING RCONSEND,R2                                                      
         MVI   CONSNDSW,YESQ       Yes, SEND elem found                         
         MVC   SVCONCRV,RCONSRV    Save Rep's version number                    
         MVC   SVCONCSV,RCONSSV    Save Sta's version number                    
         MVC   SVVERSN#,RCONSRV    Set to use Rep's version number              
         CLC   RCONSRV,RCONSSV                                                  
         JNL   *+10                                                             
         MVC   SVVERSN#,RCONSSV    Set to use Sta's version number              
         TM    RCONSENF,X'80'      Last sent by Rep?                            
         JZ    CKSIDE20                                                         
         MVI   CONWSIDE,C'S'       Station side                                 
         TM    RCONSENF,X'10'      Station version not advanced?                
         JZ    CKSIDEX                                                          
         MVI   CONWSIDE,C'B'       Rep could touch it again                     
*                                                                               
CKSIDE20 TM    RCONSENF,X'20'      Rep version not advanced?                    
         JZ    CKSIDEX                                                          
         MVI   CONWSIDE,C'B'       Station could touch it again                 
CKSIDEX  L     RE,SAVE_RE                                                       
         BR    RE                                                               
         DROP  R2                                                               
*                                                                               
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Get station record and extract options                             *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
EXTRSTAO NTR1  BASE=*,LABEL=*      Pre-set contract record                      
*                                                                               
         LA    R0,SVSTAVAL         Init station values to be saved              
         LHI   R1,SVSTAVLQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   QACTCODE,QAEC2TRQ   EC to Traffic                                
         JNE   EXSTA_X                                                          
*                                                                               
         L     R2,AIO1             Point to contract record                     
         USING RCONREC,R2                                                       
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING RSTAKEY,R3                                                       
         MVI   RSTAKTYP,RSTAKIDQ   Station record type                          
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         DROP  R2                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   RSTAKEY,IOKEYSAV    Found station record?                        
         JNE   EXSTA90                                                          
         DROP  R3                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO1'                           
         L     R2,AIO1             Point to station record                      
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'08'        Look for extra description elem              
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   EXSTA42                                                          
         USING RSTAXXEL,R2                                                      
         MVC   SVSTAXOP,RSTAXOPT   Save expanded station option                 
         DROP  R2                                                               
*                                                                               
EXSTA42  DS    0H                                                               
*                                                                               
EXSTA90  BRAS  RE,BLDCKEY          Build contract key                           
         BRAS  RE,R_GET                                                         
         JE    *+6                                                              
         DC    H'0'                Must restore contract record                 
*                                                                               
EXSTA_X  J     EXIT                                                             
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Pre-set Contract record before various upload actions              *         
*  AIO1 contains contract record to be updated                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
PSETCONR NTR1  BASE=*,LABEL=*      Pre-set contract record                      
*                                                                               
         CLI   SVREQTYP,LKBUYUPQ   Buy Update request?                          
         JE    PSETC30                                                          
         CLI   QACTCODE,QATRFOUQ   Traffic Order# Update?                       
         JE    PSETC30                                                          
         CLI   QACTCODE,QAEC2TRQ   EC to Traffic                                
         JE    PSETC30                                                          
*                                                                               
         L     R2,AIO1             Remove all Rep & Sta order comments          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'82',(R2)),0,0                  
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'92',(R2)),0,0                  
*                                                                               
* Remove all Station user name and e-mail elements                              
*                                                                               
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),('RCONSUEQ',(R2)),0,0             
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),('RCONSEEQ',(R2)),0,0             
*                                                                               
PSETC30  LA    R0,SVCONVAL         Init contract values to be saved             
         LHI   R1,SVCONVLQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO1             Point to contract record                     
         USING RCONREC,R2                                                       
         MVC   SVCONSTA,RCONKSTA   Station call letters                         
         MVC   SVCONREP,RCONKREP   Rep code                                     
         MVC   SVCONCON,RCONKCON   Contract number                              
         GOTOR (RFCONNUM,AREPFACS),DMCB,(1,RCONKCON),(5,SVCON#CH)               
         CLI   RCONCODE,X'01'      Have contract description elem?              
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCONMOD,RCONMOD    Contract mod number                          
         DROP  R2                                                               
*                                                                               
         L     R2,AIO1             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'        Look for Send Info elem                      
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   PSETC40                                                          
         USING RCONSEND,R2                                                      
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
PSETC40  MVI   SVOFRPND,NOQ        Set makegood offer pending to NO             
         L     R2,AIO1             Point to contract record                     
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1E'        Random flag element                          
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   PSETC42                                                          
         USING RCONRFEL,R2                                                      
         CLI   RCONR#MO,0          Any makegood offers?                         
         JNH   *+8                                                              
         MVI   SVOFRPND,YESQ       Set makegood offer pending to YES            
         DROP  R2                                                               
*                                                                               
PSETC42  DS    0H                                                               
*                                                                               
PSETC_X  J     EXIT                                                             
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Update contract record                                             *         
*  AIO1 contains contract record to be updated                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
UPDCONR  NTR1  BASE=*,LABEL=*      Update contract record                       
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,7,ASTAOCOM                                                    
         JZ    UPDCON40                                                         
         AHI   R4,LW_LN2Q          Point to first comment                       
         SR    R3,R3                                                            
         ICM   R3,3,NUMSTAOC       # of comment to process                      
*                                                                               
UPDCON22 XC    ELEM2,ELEM2         Prepare to build station order comm          
         LA    R1,ELEM2                                                         
         USING RCONSOEL,R1                                                      
         MVI   RCONSOCO,X'92'      Station order comment elem code              
*                                                                               
         MVI   WKMAXLEN,MAXCOMLQ                                                
         BRAS  RE,GETFLDLN         Get order comment length                     
         CHI   RF,0                Have input?                                  
         JNH   UPDCON26                                                         
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RCONSOCM(0),0(R4)   Save station order comment                   
         AHI   RF,1+2              Adjustment for elem length                   
         STC   RF,RCONSOCO+1                                                    
         DROP  R1                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDCON26 LA    R4,MAXCOMLQ(R4)     Point to next comment in array               
         JCT   R3,UPDCON22                                                      
*                                                                               
UPDCON40 XC    ELEM2,ELEM2         Prepare to build station user name           
         LA    R1,ELEM2                                                         
         USING RCONSUEL,R1                                                      
         MVI   RCONSUCD,RCONSUEQ   Element code                                 
         MVI   RCONSULN,RCONSUOQ+1 Default element length                       
         MVC   RCONSUDT,TODAYB     Set last changed date to today               
         CLC   QSTAUSRN,SPACES     Have station user name?                      
         JNH   UPDCON42                                                         
*                                                                               
         LA    R4,QSTAUSRN                                                      
         MVI   WKMAXLEN,L'QSTAUSRN                                              
         BRAS  RE,GETFLDLN         Get station user name length                 
         CHI   RF,0                Have input?                                  
         JNH   UPDCON46                                                         
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RCONSUNM(0),0(R4)   Station user name                            
         AHI   RF,1+RCONSUOQ       Adjustment for elem length                   
         STC   RF,RCONSULN                                                      
         DROP  R1                                                               
*                                                                               
UPDCON42 L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDCON46 XC    ELEM2,ELEM2         Prepare to build station user name           
         LA    R1,ELEM2                                                         
         USING RCONSEEL,R1                                                      
         MVI   RCONSECD,RCONSEEQ   Element code                                 
         MVI   RCONSELN,RCONSEOQ+1 Default element length                       
         MVC   RCONSEDT,TODAYB     Set last changed date to today               
         CLC   QSTAUEML,SPACES     Have station user e-mail address?            
         JNH   UPDCON48                                                         
*                                                                               
         LA    R4,QSTAUEML                                                      
         MVI   WKMAXLEN,L'QSTAUEML                                              
         BRAS  RE,GETFLDLN         Get station user e-mail length               
         CHI   RF,0                Have input?                                  
         JNH   UPDCON54                                                         
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RCONSEML(0),0(R4)   Station user e-mail address                  
         AHI   RF,1+RCONSEOQ       Adjustment for elem length                   
         STC   RF,RCONSELN                                                      
         DROP  R1                                                               
*                                                                               
UPDCON48 L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDCON54 CLI   QACTCODE,QASENDRQ   Send?                                        
         JNE   EXITY                                                            
         BRAS  RE,GOUCVER          Advance contract version                     
         JNE   EXITN                                                            
*                                                                               
         J     EXITY                                                            
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Update traffic order number                                        *         
*  AIO1 contains contract record to be updated                        *         
*  Process MQ message if traffic order number is changed              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
UPDTRFO  NTR1  BASE=*,LABEL=*      Update traffic order number                  
*                                                                               
         MVC   WORK+20(4),IOKEY+28 save disk address                            
         L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1F'        Online contract (extended desc elem)         
         CLC   ELCODE,0(R2)                                                     
         JE    UPDTRF03                                                         
         BRAS  RE,NXTEL                                                         
         JE    UPDTRF03                                                         
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXENOX1F),TXENOX1F  No online contract found          
         J     EXITN               Error                                        
*                                                                               
         USING RCONXEL,R2                                                       
UPDTRF03 MVC   WORK(L'RCONTRF),RCONTRF                                          
                                                                                
         LHI   R1,10                                                            
         LA    RE,QTRAFORD                                                      
         AHI   RE,L'QTRAFORD-1     Start at the end                             
UPDTRF05 CLI   0(RE),C' '          Get rid of trailing spaces                   
         BH    UPDTRF10                                                         
         MVI   0(RE),0                                                          
         AHI   RE,-1               Go backwards                                 
         BCT   R1,UPDTRF05                                                      
                                                                                
UPDTRF10 MVC   WORK+L'RCONTRF(L'RCONTRF),QTRAFORD                               
**NO-OP  OC    WORK,SPACES                                                      
         XC    RCONTRF,RCONTRF                                                  
         CLC   QTRAFORD,SPACES     Have traffic order#?                         
         JNH   UPDTRF20                                                         
         MVC   RCONTRF,QTRAFORD    Update traffic order#                        
*                                                                               
* CHECK IF TRAFFIC NUMBER ALREADY ASSIGNED TO A ANOTHER CONTRACT                
*                                                                               
         XC    IOKEY,IOKEY         Prepare to build passive X'A203'             
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         USING RCONREC,R1                                                       
         MVC   RCONTNTP(2),=X'A203' KEY TYPE                                    
         MVC   RCONTNRP,SVCONREP    REP CODE                                    
         MVC   RCONTNT#,RCONTRF     TRAFFIC NUMBER                              
         DROP  R1                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   IOKEY(RCONTNCN-RCONKEY),IOKEYSAV                                 
         JNE   UPDTRF20                                                         
         LA    R1,IOKEY                                                         
         USING RCONREC,R1                                                       
         CLC   RCONTNCN,SVCONCON    OK IF REASSINGING SAME CONTRACT#            
         BE    UPDTRF20                                                         
         DROP  R1                                                               
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXETRDUP),TXETRDUP  TRAFFIC# ALREADY ASSIGNED         
         LA    R3,ERRORMSG+L'TXETRDUP                                           
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),IOKEY+23(4)                                             
         EDIT  (P5,WORK2),(8,0(R3)),ALIGN=LEFT                                  
         J     EXITN                          ERROR                             
*                                                                               
UPDTRF20 BRAS  RE,TRAFKEYS         MAINTAIN TRAFFIC PASSIVES                    
*                                                                               
UPDTRF30 CLC   WORK(L'RCONTRF),WORK+L'RCONTRF                                   
         JE    *+14                                                             
         MVC   SVTRORD#,RCONTRF    Updated traffic order# for MQ msg            
         MVI   PRCMQMSW,YESQ       Set switch to process MQ msg later           
*                                                                               
         J     EXITY                                                            
         DROP  RB,R2                                                            
         EJECT                                                                  
                                                                                
*                                                                               
*   DELETE OLD TRAFFIC NUMBER PASSIVE, ADD NEW TRAFFIC NUMBER                   
*        PASSIVE, AS APPROPRIATE.                                               
*                                                                               
         USING RCONXEL,R2                                                       
TRAFKEYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    WORK(L'RCONTRF),WORK    OLD KEY ENTERED?                         
         BZ    TRFK20                                                           
*                                                                               
         XC    IOKEY,IOKEY         Prepare to build passive X'A203'             
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         USING RCONREC,R1                                                       
         MVC   RCONTNTP(2),=X'A203'   KEY TYPE                                  
         MVC   RCONTNRP,SVCONREP   REP CODE                                     
         MVC   RCONTNT#,WORK       ORIGINAL TRAFFIC NUMBER                      
         MVC   RCONTNCN,SVCONCON   CONTRACT NUMBER                              
         DROP  R1                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOREPDIR+IO1'                          
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         BNE   TRFK20              NO  - NOTHING TO TURN OFF                    
         OI    IOKEY+27,X'80'      YES - SET KEY TO 'DELETED'                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR+IO1'                         
*                                                                               
TRFK20   OC    WORK+10(L'RCONTRF),WORK+10   NEW TRAFFIC NUMBER?                 
         JZ    EXITY                                                            
*                                                                               
         XC    IOKEY,IOKEY         Prepare to build passive X'A203'             
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R1,IOKEY                                                         
         USING RCONREC,R1                                                       
         MVC   RCONTNTP(2),=X'A203' KEY TYPE                                    
         MVC   RCONTNRP,SVCONREP    REP CODE                                    
         MVC   RCONTNT#,RCONTRF     TRAFFIC NUMBER                              
         MVC   RCONTNCN,SVCONCON    CONTRACT NUMBER                             
         DROP  R1                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOREPDIR+IO1'                         
         CLC   IOKEY(L'RCONKEY),IOKEYSAV  KEY FOUND ON FILE?                    
         BNE   TRFK40              NO  - KEY MUST BE ADDED                      
         NI    IOKEY+27,X'FF'-X'80'  YES - TURN OFF DELETE BIT                  
         MVC   IOKEY+28(4),WORK+20   INSERT NEW RECORD DISK ADDRESS             
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR+IO1'                         
         J     EXITY                                                            
*                                                                               
TRFK40   MVC   IOKEY(27),IOKEYSAV    RESTORE KEY SOUGHT                         
         MVI   IOKEY+27,0            CLEAR CONTROL BYTE                         
         MVC   IOKEY+28(4),WORK+20   INSERT NEW RECORD DISK ADDRESS             
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOREPDIR+IO1'  ADD NEW KEY              
*                                                                               
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Update confirmation comments record                                *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
PRCCFCOM NTR1  BASE=*,LABEL=*      Process confirmation comments                
*                                                                               
         MVI   BYTE2,0             Temporary switch for ADDREC                  
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING RCFCREC,R4                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,SVCONREP                                                
         MVC   RCFCKCON,SVCONCON                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOREPDIR+IO1'                         
         CLC   IOKEY(L'RCFCKEY),IOKEYSAV                                        
         JNE   PRCCFM20                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOREPFIL+IO1'                        
         J     PRCCFM40                                                         
         DROP  R4                                                               
                                                                                
PRCCFM20 L     R4,AIO1             Add new record                               
         USING RCFCREC,R4                                                       
         XC    RCFCKEY(36),RCFCKEY                                              
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,SVCONREP                                                
         MVC   RCFCKCON,SVCONCON                                                
         MVI   RCFCLEN+1,36        Default length - empty record                
*                                                                               
         XC    ELEM2,ELEM2         Building info element                        
         LA    R3,ELEM2                                                         
         USING RCFCIEL,R3                                                       
         MVI   RCFCICD,X'01'       Info elem code                               
         MVI   RCFCILEN,RCFCILNQ                                                
         MVC   RCFCIVER,SVVERSN#                                                
         CLI   QMKGD2FW,YESQ       Makegood to follow is Y?                     
         JNE   *+8                                                              
         OI    RCFCIFLG,X'80'                                                   
         CLI   QMKGD2FW,NOQ        Makegood to follow is N?                     
         JNE   *+8                                                              
         OI    RCFCIFLG,X'40'                                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),(R4),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE2,1             Indicate ADDREC is needed                    
         DROP  R3,R4                                                            
*                                                                               
PRCCFM40 L     R4,AIO1             Remove existing comment elements             
         USING RCFCREC,R4                                                       
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'02',RCFCREC),0,0               
         DROP  R4                                                               
*                                                                               
         L     R2,AIO1             Update makegood to follow flag               
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'01'        Info element code                            
         CLC   ELCODE,0(R2)                                                     
         JE    *+14                                                             
         BRAS  RE,NXTEL                                                         
         JE    *+6                                                              
         DC    H'0'                Info element not found                       
         USING RCFCIEL,R2                                                       
         CLI   QMKGD2FW,YESQ       Makegood to follow is Y?                     
         JNE   *+12                                                             
         NI    RCFCIFLG,X'FF'-X'40'                                             
         OI    RCFCIFLG,X'80'                                                   
         CLI   QMKGD2FW,NOQ        Makegood to follow is N?                     
         JNE   *+12                                                             
         NI    RCFCIFLG,X'FF'-X'80'                                             
         OI    RCFCIFLG,X'40'                                                   
         DROP  R2                                                               
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,7,APCFMCOM                                                    
         AHI   R4,LW_LN2Q          Point to first comment                       
         SR    R3,R3                                                            
         ICM   R3,3,NUMPCFMC       # of comment to process                      
*                                                                               
PRCCFM50 XC    ELEM2,ELEM2                                                      
         LA    R1,ELEM2                                                         
         USING RCFCTEL,R1                                                       
         MVI   RCFCTCD,X'02'       Comment element code                         
*                                                                               
         CLI   QACTCODE,QAPARTCQ   Partial Confirm?                             
         JNE   PRCCFM52                                                         
         MVI   RCFCTLEN,MAXCOMLQ+2                                              
         MVC   RCFCTEXT(MAXCOMLQ),0(R4)                                         
         J     PRCCFM58                                                         
*                                                                               
PRCCFM52 MVI   WKMAXLEN,MAXCOMLQ                                                
         BRAS  RE,GETFLDLN         Get order comment length                     
         CHI   RF,0                Have input?                                  
         JNH   PRCCFM59                                                         
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RCFCTEXT(0),0(R4)   Save order comment                           
         AHI   RF,1+2              Adjustment for elem length                   
         STC   RF,RCFCTLEN                                                      
         DROP  R1                                                               
*                                                                               
PRCCFM58 L     RF,AIO1                                                          
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
PRCCFM59 LA    R4,MAXCOMLQ(R4)     Point to next comment in array               
         JCT   R3,PRCCFM50                                                      
*                                                                               
         CLI   BYTE2,1             ADDREC?                                      
         JNE   PRCCFM60                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOREPFIL+IO1'                           
         J     EXITY                                                            
*                                                                               
PRCCFM60 L     R4,AIO1                                                          
         USING RCFCREC,R4                                                       
         MVI   RCFCCNTL,0          Reset control status in record               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOREPFIL+IO1'                        
                                                                                
         LA    R4,IOKEY                                                         
         MVI   RCFCCNTL-2,0        Reset control status in directory            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOREPDIR+IO1'                         
         DROP  R4                                                               
*                                                                               
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Update buy record - buyline order comments                         *         
*  R4 points to order comments in request array                       *         
*  AIO1 contains buy record to be updated                             *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
UPDBUYLN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVREQTYP,LKBUYUPQ   Buy Update request?                          
         JE    UPDBUY10                                                         
         CLI   QACTCODE,QASENDRQ   Send?                                        
         JNE   UPDBY90                                                          
         OC    NUMBUYOC,NUMBUYOC   Have buy comment to process?                 
         JZ    UPDBY52             Comments are done via Buy Update             
*                                                                               
UPDBUY10 L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'D',=C'REPFILE'),(X'84',(RF)),0,0                  
*                                                                               
UPDBY20  CLC   0(MAXCOMLQ,R4),SPACES                                            
         JNH   *+8                                                              
         MVI   BYTE2,1             Init order comment loop counter              
         CLC   MAXCOMLQ(MAXCOMLQ,R4),SPACES                                     
         JNH   *+8                                                              
         MVI   BYTE2,2             There are two lines of comments              
*                                                                               
         CLI   BYTE2,0             Have order comment to process?               
         JH    *+6                                                              
         DC    H'0'                Bad comments in request array                
*                                                                               
UPDBY30  SR    RE,RE                                                            
         ICM   RE,1,BYTE2          Have order comment to process?               
         JZ    UPDBY50             Exit if all done                             
         BCTR  RE,0                                                             
         STCM  RE,1,BYTE2                                                       
                                                                                
         XC    ELEM2,ELEM2                                                      
         LA    R1,ELEM2                                                         
         USING RBUYOCEL,R1                                                      
         MVI   RBUYOCCD,X'84'      Order comment elem code                      
         MVI   RBUYOCID,0          Set to station input comment                 
*                                                                               
         MVI   WKMAXLEN,MAXCOMLQ                                                
         BRAS  RE,GETFLDLN         Get order comment length                     
         CHI   RF,0                Have input?                                  
         JNH   UPDBY36                                                          
*                                                                               
         BCTR  RF,0                For EX instruction                           
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RBUYOCNT(0),0(R4)   Save order comment                           
         AHI   RF,1+(RBUYOCNT-RBUYOCEL)                                         
         STC   RF,RBUYOCLN                                                      
         DROP  R1                                                               
*                                                                               
         L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDBY36  LA    R4,MAXCOMLQ(R4)                                                  
         J     UPDBY30                                                          
*                                                                               
UPDBY50  CLI   SVREQTYP,LKBUYUPQ   Buy Update request?                          
         JE    UPDBY70                                                          
*                                                                               
UPDBY52  L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         USING RBUYELEM,R2                                                      
         CLI   RBUYCODE,X'01'      Buy description element present?             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   RBUYCHGD,TODAYB     Set last changed date to today               
         MVC   RBUYCHGI,=C'O '     Set order comments changed                   
         MVC   HALF2,RBUYCHGI      Save change indicators                       
         MVC   RBUYVER,SVCONCSV    Set buy version to latest Sta ver            
         DROP  R2                                                               
*                                                                               
UPDBY60  XC    ELEM2,ELEM2         Prepare to build MOD code trap elem          
         LA    R2,ELEM2                                                         
         USING RBUYMCEL,R2                                                      
         MVI   RBUYMCCD,X'D0'      MOD code trap element code                   
         MVI   RBUYMCLN,5          MOD code trap element length                 
         MVC   RBUYMCVR,SVCONCSV   Buy version number                           
         MVC   RBUYMCMO,HALF2      Buy MOD codes                                
         DROP  R2                                                               
*                                                                               
         J     UPDBY70             Bypass X'D0' element for now                 
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'D0'        Looking for MOD code trap element            
         USING RBUYMCEL,R2                                                      
         CLC   ELCODE,RBUYMCCD                                                  
         JE    *+12                                                             
UPDBY64  BRAS  RE,NXTEL                                                         
         JNE   UPDBY66                                                          
         CLC   ELEM2(5),RBUYMCEL   Already have element?                        
         JE    UPDBY70                                                          
         J     UPDBY64                                                          
         DROP  R2                                                               
*                                                                               
UPDBY66  L     RF,AIO1                                                          
         GOTOR VHELLO,DMCB,(C'P',=C'REPFILE'),(RF),ELEM2,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDBY70  DS    0H                                                               
*                                                                               
UPDBY90  J     EXIT                                                             
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
MAXCOMLQ EQU   60                                                               
*                                                                               
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
CHKUPMOD NTR1  BASE=*,LABEL=*      Check update mode                            
*                                                                               
* Traffic Order# update request does not Globber to CON program                 
*                                                                               
         CLI   QACTCODE,QATRFOUQ   Traffic Order# Update?                       
         JNE   CUPMOD90                                                         
         BRAS  RE,REPLYHDR                                                      
         BRAS  RE,REPLYFLD                                                      
         GOTOR VGLOBBER,DMCB,=C'DELE',ELEM2,GLRLINLQ,GLRKLINC                   
         J     EXITY                                                            
*                                                                               
CUPMOD90 J     EXITN                                                            
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REPLYFLD NTR1  BASE=*,LABEL=*      Reply fields (Order Update Response)         
*                                                                               
         BRAS  RE,BLDCKEY          Build contract key                           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         JE    R_FLD20                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXECONNF),TXECONNF                                    
         EDIT  QCONNUMB,(8,ERRORMSG+L'TXECONNF),FILL=0                          
         BRAS  RE,REPLYERR                                                      
         J     EXITN                                                            
R_FLD20  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         USING RCONREC,R2                                                       
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',001),          +        
               ('LD_CHARQ',RCONKSTA),(L'RCONKSTA,0)                             
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',002),          +        
               ('LD_SPAKQ',QCONNUMB),(L'QCONNUMB,0)                             
*                                                                               
         BRAS  RE,CKSIDE                                                        
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',003),          +        
               ('LD_UBINQ',SVVERSN#),(L'SVVERSN#,0)                             
*                                                                               
         CLI   QACTCODE,QACONFMQ   Confirm?                                     
         JE    *+12                                                             
         CLI   QACTCODE,QAPARTCQ   Partial Confirm?                             
         JNE   R_FLD30                                                          
         L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'01'        Contract description element                 
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   R_FLD30                                                          
         USING RCONELEM,R2                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',004),          +        
               ('LD_UBINQ',RCONMOD),(L'RCONMOD,0)                               
*                                                                               
R_FLD30  CLI   QACTCODE,QAEC2TRQ   EC to Traffic?                               
         JNE   R_FLD40                                                          
         L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'15'        EC Control element                           
         CLC   ELCODE,0(R2)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   R_FLD40                                                          
         USING RCONECEL,R2                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',005),          +        
               ('LD_CDATQ',RCONECDT),(L'RCONECDT,0)                             
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',006),          +        
               ('LD_HEXDQ',RCONECTM),(L'RCONECTM,0)                             
*                                                                               
R_FLD40  CLI   QACTCODE,QATRFOUQ   Traffic Order# Update?                       
         JNE   R_FLD50                                                          
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',007),          +        
               ('LD_CHARQ',QTRAFORD),(L'QTRAFORD,0)                             
*                                                                               
R_FLD50  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GOUCVER  NTR1  BASE=*,LABEL=*      Advance contract version                     
         L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'1F'        Extended description element                 
         CLC   ELCODE,0(R2)                                                     
         JE    *+14                                                             
         BRAS  RE,NXTEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R2                                                       
         TM    RCONCONF,X'40'      Confirmed?                                   
         JZ    GOUCV040                                                         
         NI    RCONCONF,X'FF'-X'40'  Unconfirm                                  
         OI    RCONCONF,X'20'+X'80'  Conf previously + Not confirmed            
         DROP  R2                                                               
*                                                                               
GOUCV040 L     R2,AIO1                                                          
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'        Send element                                 
         CLC   ELCODE,0(R2)                                                     
         JE    *+14                                                             
         BRAS  RE,NXTEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R2                                                      
         TM    RCONSENF,X'10'      Station version not advanced?                
         JZ    GOUCV100                                                         
         DROP  R2                                                               
*                                                                               
* Advance station version and update version date                               
*                                                                               
         MVC   DUB+0(4),VHELLO                                                  
         MVC   DUB+4(4),VDATCON                                                 
         GOTOR (RFGENVER,AREPFACS),DMCB,(C'S',AIO1),(X'80',DUB)                 
         JZ    GOUCV050                                                         
         XC    ERRORMSG,ERRORMSG                                                
         LR    RF,R3               Error returned by GENVER                     
         BRAS  RE,GET_ETXT                                                      
         BRAS  RE,REPLYERR                                                      
         J     EXITN                                                            
*                                                                               
GOUCV050 DS    0H                                                               
*                                                                               
GOUCV100 DS    0H                                                               
         J     EXITY                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
SETGLOBB NTR1  BASE=*,LABEL=*      Set Globber object                           
*                                                                               
         LA    R2,ELEM2                                                         
         USING GLRLINCD,R2                                                      
         GOTOR VGLOBBER,DMCB,=C'GETD',ELEM2,GLRLINLQ,GLRKLINC                   
         TM    DMCB+8,X'10'                                                     
         JNZ   SETGL30                                                          
         CLC   =C'B=50',GLRLINRS   Order Update request?                        
         JE    EXIT                                                             
         GOTOR VGLOBBER,DMCB,=C'DELE',ELEM2,GLRLINLQ,GLRKLINC                   
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
SETGL30  L     R3,ATWA             Save Link request command                    
         USING TWAD,R3                                                          
         CLC   =C'B=50',LNKINP+4   Order Update request?                        
         JNE   EXIT                                                             
         XC    ELEM2,ELEM2         Build Link command object                    
         MVC   BYTE1,LNKINP+4+4+3                                               
         NI    BYTE1,X'0F'         Length of "order #" data in binary           
         LLC   RE,BYTE1                                                         
         AHI   RE,4+4+3+2          Add overhead                                 
         STC   RE,GLRLINLN         Length of FALINK command to remember         
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         MVC   GLRLINFD(0),LNKINP  Save FALINK command as GLOB. object          
         EX    RE,0(RF)                                                         
         GOTOR VGLOBBER,DMCB,=C'PUTD',ELEM2,GLRLINLQ,GLRKLINC                   
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2,R3                                                            
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
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
         BRAS  RE,REPLYHDR                                                      
         BRAS  RE,REPLYFLD                                                      
*                                                                               
         LA    R2,ELEM2                                                         
         USING GLCONNUM,R2                                                      
         OC    GLCONERR,GLCONERR   Came back with error?                        
         JZ    CHKGL30                                                          
         XC    ERRORMSG,ERRORMSG                                                
         SR    RF,RF                                                            
         ICM   RF,3,GLCONERR                                                    
         BRAS  RE,GET_ETXT                                                      
         BRAS  RE,REPLYERR                                                      
CHKGL30  GOTOR VGLOBBER,DMCB,=C'DELE',ELEM2,GLRLINLQ,GLRKLINC                   
         J     EXITY                                                            
                                                                                
GET_ETXT LR    R0,RE                                                            
         GOTOR VGETTXT,DMCB+12,(RF),(60,ERRMSGST),(C'E',DMCB),0,0,0             
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
         DROP  RB,R2                                                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
GLOBBCON NTR1  BASE=*,LABEL=*      Call CON program                             
*                                                                               
         CLI   QACTCODE,QATRFOUQ   Traffic Order# Update?                       
         JE    EXITY                                                            
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
         L     R2,AIO1             Point to contract record                     
         USING RCONREC,R2                                                       
         CLI   RCONKTYP,X'0C'      Have contract record?                        
         JE    GLOBC10                                                          
         BRAS  RE,BLDCKEY          Build contract key                           
         BRAS  RE,R_GET                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
GLOBC10  CLI   QACTCODE,QASENDRQ   Send?                                        
         JE    GLOBC30                                                          
         CLI   QACTCODE,QACONFMQ   Confirm?                                     
         JE    GLOBC40                                                          
         CLI   QACTCODE,QAPARTCQ   Partial Confirm?                             
         JE    GLOBC40                                                          
         CLI   QACTCODE,QAEC2TRQ   EC to Traffic?                               
         JE    GLOBC50                                                          
*                                                                               
         DC    H'0'                Invalid request action                       
*                                                                               
GLOBC30  L     R2,AIO1             Point to contract record                     
         USING RCONREC,R2                                                       
         XC    ELEM2,ELEM2         Build Contract action object                 
         LA    R3,ELEM2                                                         
         USING GLCONNUM,R3                                                      
         GOTOR (RFCONNUM,AREPFACS),DMCB,(1,RCONKCON),(5,GLCONNUM)               
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'                                                     
         CLC   ELCODE,0(R2)                                                     
         JE    GLOBC34                                                          
         BRAS  RE,NXTEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R2                                                      
GLOBC34  MVC   GLCONCA(5),=C'SEND='                                             
         EDIT  RCONSRV,(3,GLCONCA+5),ALIGN=LEFT                                 
         OI    GLCONFLG,GLCONRPQ   Request reply                                
         GOTOR VGLOBBER,DMCB,=C'PUTD',ELEM2,GLCONLNQ,GLRKACT                    
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         DROP  R3,R2                                                            
*                                                                               
GLOBC40  L     R2,AIO1             Point to contract record                     
         USING RCONREC,R2                                                       
         XC    ELEM2,ELEM2         Build Contract action object                 
         LA    R3,ELEM2                                                         
         USING GLCONNUM,R3                                                      
         GOTOR (RFCONNUM,AREPFACS),DMCB,(1,RCONKCON),(5,GLCONNUM)               
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'20'                                                     
         CLC   ELCODE,0(R2)                                                     
         JE    GLOBC44                                                          
         BRAS  RE,NXTEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R2                                                      
GLOBC44  MVC   GLCONCA(3),=C'CF='                                               
         EDIT  RCONSRV,(3,GLCONCA+3),ALIGN=LEFT                                 
         OI    GLCONFLG,GLCONRPQ   Request reply                                
         GOTOR VGLOBBER,DMCB,=C'PUTD',ELEM2,GLCONLNQ,GLRKACT                    
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         DROP  R3,R2                                                            
*                                                                               
GLOBC50  L     R2,AIO1             Point to contract record                     
         USING RCONREC,R2                                                       
         XC    ELEM2,ELEM2         Build Contract action object                 
         LA    R3,ELEM2                                                         
         USING GLCONNUM,R3                                                      
         GOTOR (RFCONNUM,AREPFACS),DMCB,(1,RCONKCON),(5,GLCONNUM)               
         MVC   GLCONCA(5),=CL5'EC   '                                           
         LA    R2,FRSTELEM(R2)                                                  
         MVI   ELCODE,X'15'                                                     
         CLC   ELCODE,0(R2)                                                     
         JE    GLOBC54                                                          
         BRAS  RE,NXTEL                                                         
         JNE   *+10                                                             
GLOBC54  MVC   GLCONCA(5),=CL5'E2   '                                           
         OI    GLCONFLG,GLCONRPQ  Request reply                                 
         GOTOR VGLOBBER,DMCB,=C'PUTD',ELEM2,GLCONLNQ,GLRKACT                    
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         DROP  R3,R2                                                            
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCMQMSG NTR1  BASE=*,LABEL=*,WORK=(R4,MQMSG_DL)                                
*                                                                               
         USING MQMSG_D,R4                                                       
*                                                                               
         CLI   PRCMQMSW,YESQ       Need to process MQ message?                  
         JNE   EXIT                                                             
*                                                                               
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
         LA    R2,L'STATRF#U                                                    
         LA    R3,STATRF#U                                                      
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
         LA    R2,L'STATRF#U                                                    
         LA    R3,STATRF#U                                                      
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
         LA    R2,IOKEY                                                         
         USING RCFCKEY,R2                                                       
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
         DROP  R2                                                               
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
PRCQBUYS NTR1  BASE=*,LABEL=*      Process request maps for Buy Update          
*                                                                               
         XC    NUMBUYL#,NUMBUYL#   Init # of buylines                           
         XC    NUMBUYOC,NUMBUYOC   Init # of buyline comments                   
*                                                                               
         BRAS  RE,BLDCKEY          Build contract key                           
         BRAS  RE,R_GET                                                         
         JE    VQBUY10                                                          
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXECONNF),TXECONNF                                    
         EDIT  QCONNUMB,(8,ERRORMSG+L'TXECONNF),FILL=0                          
VQBUY08X BRAS  RE,REPLYERR                                                      
         J     DELGLB_X                                                         
*                                                                               
VQBUY10  BRAS  RE,PSETCONR         Pre-set contract record                      
*                                                                               
         CLI   QACTCODE,QABYCHGQ   Change?                                      
         CLI   QACTCODE,QABYCHGQ   Change?                                      
         JE    VQBUY30                                                          
*                                                                               
         XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEINVAC),TXEINVAC                                    
         MVC   ERRORMSG+L'TXEINVAC(L'QACTCODE),QACTCODE                         
         J     VQBUY08X                                                         
*                                                                               
VQBUY30  SR    R3,R3                                                            
         ICM   R3,7,ABUYLIN#       Have list of buyline numbers?                
         JZ    *+14                                                             
         MVC   NUMBUYL#,LW_NUMN-LW_D(R3)                                        
         AHI   R3,LW_LN2Q                                                       
         SR    R4,R4                                                            
         ICM   R4,7,ABUYOCOM       Have list of buyline order comments?         
         JZ    *+14                                                             
         MVC   NUMBUYOC,LW_NUMN-LW_D(R4)                                        
         AHI   R4,LW_LN2Q                                                       
         CLC   NUMBUYL#,NUMBUYOC                                                
         JNE   VQBUY38F                                                         
*                                                                               
VQBUY32  SR    R0,R0                                                            
         ICM   R0,3,NUMBUYL#       R0=remaining Buyline count                   
         JZ    EXITY               Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMBUYL#                                                    
         MVC   SVBUYLIN,0(R3)                                                   
*                                                                               
         BRAS  RE,BLDBKEY          Build buy key                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         J     VQBUY32M                                                         
VQBUY32K GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO1'                            
VQBUY32M CLC   IOKEY(RBUYKPLN-RBUYKEY),IOKEYSAV                                 
         JNE   VQBUY38E                                                         
         LA    RE,IOKEY                                                         
         USING RBUYKEY,RE                                                       
         CLC   RBUYKPLN,=X'FFFFFF' Default plan code?                           
         JNE   VQBUY32K                                                         
         CLC   RBUYKLIN,SVBUYLIN   Buyline found?                               
         JNE   VQBUY32K                                                         
         BRAS  RE,R_GET                                                         
         JNE   VQBUY38E                                                         
         DROP  RE                                                               
*                                                                               
         L     RE,AIO1             Point to buy record                          
         USING RBUYREC,RE                                                       
         SR    RF,RF                                                            
         ICM   RF,3,RBUYLEN                                                     
         CHI   RF,MAXBUYLQ         Buy rec length near supported max?           
         JH    VQBUY38P                                                         
         DROP  RE                                                               
         BRAS  RE,UPDBUYLN         Update Buyline with order comments           
         BRAS  RE,R_PUT            Write back buy record                        
         AHI   R3,001              Point to next buyline number                 
         AHI   R4,120              Point to next buyline order comment          
         J     VQBUY32                                                          
*                                                                               
VQBUY38E XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEBLNNF),TXEBLNNF                                    
         EDIT  (1,SVBUYLIN),(3,ERRORMSG+L'TXEBLNNF),ALIGN=LEFT                  
         J     VQBUY08X                                                         
VQBUY38F XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMSB#C),TXEMSB#C                                    
         J     VQBUY08X                                                         
VQBUY38G XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXTOOMBY),TXTOOMBY                                    
         J     VQBUY08X                                                         
VQBUY38L XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEMISSG),TXEMISSG                                    
         MVC   ERRORMSG+L'TXEMISSG(L'HSOCMLIT),HSOCMLIT                         
         J     VQBUY08X                                                         
VQBUY38P XC    ERRORMSG,ERRORMSG                                                
         MVC   ERRORMSG(L'TXEBLRMX),TXEBLRMX                                    
         J     VQBUY08X                                                         
*                                                                               
RPLY_BUY ST    RE,SAVE_RE          Reply buy updated message                    
         BRAS  RE,REPLYHDR                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',001),          +        
               ('LD_CHARQ',REPLYMSG),(L'REPLYMSG,0)                             
         L     RE,SAVE_RE                                                       
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG ,                                                                
                                                                                
ACTCDLIT DC    C'Action Code'                                                   
ORDNOLIT DC    C'Order Number'                                                  
VERNOLIT DC    C'Version Number'                                                
TRAO#LIT DC    C'Traffic Order Number'                                          
HSOCMLIT DC    C'Header Station Ord Comm'                                       
MG2FWLIT DC    C'Makegood to follow'                                            
UPDBLLIT DC    C'Update Buyline#'                                               
BLOCMLIT DC    C'Buyline Ord Comm'                                              
BYLN#LIT DC    C'Buyline #'                                                     
BYOCMLIT DC    C'Buyline Order Comment'                                         
PCFMCLIT DC    C'Partial Confirm comment'                                       
STAUNLIT DC    C'Station User Name'                                             
STAUELIT DC    C'Station User E-mail'                                           
                                                                                
TXERRADD DC    C'Cannot add, record already exist'                              
TXERRCHG DC    C'Cannot change, record not found'                               
TXEINVAC DC    C'Invalid update action - '                                      
TXEINVAL DC    C'Invalid input field - '                                        
TXEMISSG DC    C'Missing input field - '                                        
TXENOX1F DC    C'No online contract found, cannot update '                      
TXECONNF DC    C'Contract not found - '                                         
TXEMSB#C DC    C'Missing Buyline numbers or order comments'                     
TXTOOMBY DC    C'Too many buys for this order update'                           
TXEBLNNF DC    C'Buyline not found - '                                          
TXERWIPS DC    C'Cannot update, Rep is working on contract'                     
TXEBLRMX DC    C'Buyline record exceeded maximum allowable length'              
TXETRDUP DC    C'Traffic# already assigned to contract# '                       
TX_DONE_ DC    C'Done'                                                          
*                                                                               
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
STATRF#U DC    C'station-traffic-order-update'                                  
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
SYSPRTQ  EQU   C'P'                Print system letter                          
EOR      EQU   0                   End of record element code                   
MAXBUYLQ EQU   1000-(2*60)         Need room for two buy order comments         
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
                                                                                
ACLTSTAT DS    A                   A(PCLTSTAT)                                  
                                                                                
WKBASERD DS    F                                                                
SAVE_RE  DS    F                                                                
WKRECLEN DS    H                   Record length                                
                                                                                
SVLINCLN DS    XL(L'GLRLINLN)      Save Link request command length             
SVLINCFD DS    CL(L'GLRLINFD)      Save Link request command field              
                                                                                
WKMAXLEN DS    X                   Max input length                             
                                                                                
AGY      DS    CL2                 Agency code                                  
MASTRCOD DS    CL2                 Master Rep Code                              
                                                                                
SVREQTYP DS    XL1                 Save request type                            
LKORDUPQ EQU   1                   Rep Link Order Update                        
LKBUYUPQ EQU   2                   Rep Link Buy Update                          
                                                                                
SVMQMFRM DS    CL3                 MQ message from LIN program                  
PRCMQMSW DS    CL1                 Process MQ message switch                    
MQMSGSTA DS    CL19                MQ message status                            
WIPSFLAG DS    CL1                                                              
MSSAGEID DS    XL24                Message ID                                   
                                                                                
SVSTAVAL DS    0X                  Start of saved station values                
SVSTAXOP DS    XL(L'RSTAXOPT)      Expanded station option                      
SVSTAVLQ EQU   *-SVSTAVAL          Length of saved station values               
                                                                                
SVCONVAL DS    0X                  Start of saved contract values               
SVCONREP DS    CL(L'RCONKREP)      Rep code                                     
SVCONSTA DS    CL(L'RCONKSTA)      Station call letters                         
SVCONCON DS    XL(L'RCONKCON)      Contract number                              
SVCON#CH DS    CL8                 Contract number in character format          
SVCONMOD DS    XL(L'RCONMOD)       Contract mod number                          
SVLASSDT DS    XL(L'RCONSRDT)      Save last send date                          
SVSNDTIM DS    CL(L'RCONSRTI)      Save last send time                          
SVCONFSW DS    XL(L'RCONCONF)      Save confirmation switch                     
SVOFRPND DS    CL1                 Save makegood offer pending flag             
SVCONVLQ EQU   *-SVCONVAL          Length of saved contract values              
                                                                                
SVBUYLIN DS    XL(L'RBUYKLIN)      Buy line number                              
SVCON9CM DS    XL(L'RCONPCON)      Contract # in 9's complement                 
SVVERSN# DS    XL(L'RCFCIVER)                                                   
SVCONCRV DS    XL(L'RCONSRV)       Save Rep's latest version number             
SVCONCSV DS    XL(L'RCONSSV)       Save Sta's latest version number             
SVTRORD# DS    XL(L'RCONTRF)       Traffic order number                         
CONWSIDE DS    CL1                 Contract side - Rep/Station/Both             
CONSNDSW DS    CL1                 Contract has SEND elem switch - Y/N          
                                                                                
RUNINDS  DS    X                   Local indicators                             
RUNINIT  EQU   X'01'               1st time building disk address list          
                                                                                
RPYIND1  DS    X                                                                
RPYHDRTQ EQU   X'80'               Header reply record is replied               
                                                                                
MQSVR    DS    XL(L'LP_MQSVR)      External service call                        
                                                                                
SVERRIND DS    XL1                 Error indicator                              
SVERRFLD DS    AL2                 Map code of error field                      
ERRMSGST DS    XL8                 Error msg start (dummy fld header)           
ERRORMSG DS    CL(MAXCOMLQ)        Error message                                
REPLYMSG DS    CL(MAXCOMLQ)        Reply message                                
                                                                                
ERRNUM         MVC   ERRORMSG(L'TXEBLRMX),TXEBLRMX                              
FATALERR DS    C                                                                
                                                                                
OFCBLK   DS    XL(OFCLENQ)         OFFICER block                                
                                                                                
QVALUES  DS    0X                  Request values                               
*                                                                               
QACTCODE DS    CL1                 Action code                                  
*                                                                               
*                                  For Order Update request                     
QASENDRQ EQU   C'S'                S = Send (Return)                            
QACONFMQ EQU   C'C'                C = Confirm                                  
QAPARTCQ EQU   C'P'                P = Partial Confirm                          
QAEC2TRQ EQU   C'E'                E = EC to Traffic                            
QATRFOUQ EQU   C'T'                T = Traffic Order# Update                    
*                                                                               
*                                  For Buy Update request                       
QABYCHGQ EQU   C'C'                C = change                                   
*                                                                               
QCONNUMB DS    PL5                 Sale order number (Contract #)               
QVERNUMB DS    XL1                 Version number                               
QTRAFORD DS    CL(L'RCONTRF)       Traffic order number                         
QMKGD2FW DS    CL1                 Makegood to follow                           
QSTAUSRN DS    CL30                Station user name                            
QSTAUEML DS    CL60                Station user e-mail address                  
*                                                                               
QSTAOCOM DS    X                                                                
ASTAOCOM DS    AL3                 Array of station order comments              
NUMSTAOC DS    XL(L'LW_NUMN)       N'Station order comments to process          
*                                                                               
QBUYLIN# DS    X                                                                
ABUYLIN# DS    AL3                 Array of buyline numbers                     
NUMBUYL# DS    XL(L'LW_NUMN)       N'Buyline numbers to process                 
*                                                                               
QBUYOCOM DS    X                                                                
ABUYOCOM DS    AL3                 Array of buyline order comments              
NUMBUYOC DS    XL(L'LW_NUMN)       N'Buyline order comments to process          
*                                                                               
QPCFMCOM DS    X                                                                
APCFMCOM DS    AL3                 Array of partial confirm comments            
NUMPCFMC DS    XL(L'LW_NUMN)       N'Partial confirm comments to proc           
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
*                                                                               
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
**PAN#1  DC    CL21'012RELNK22   09/12/19'                                      
         END                                                                    
