*          DATA SET PPLNK11    AT LEVEL 072 AS OF 10/14/20                      
*PHASE T41411A                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE PPBYOUT                                                                
*INCLUDE PPFMTINO                                                               
PPLNK11  TITLE '- PRINT SYSTEM INSERTION, INVOICE && CFM DOWNLOADS'             
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 070 30AUG19 <SPEC-37433> Display extended allocations for ZZZ  *         
***********************************************************************         
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,CODE=CODE,REQUEST=*,FILES=FILES,ABENDLIST=ABENDS,*        
               SLOWLIST=SLOWS,SERVERTYPE=TSTADBY,SYSTEM=PRTSYSQ,IDF=Y, *        
               WORKERKEY=PPID,APPEND=Y,LOADFACSOFF=Y,SEGMENT=Y,        *        
               SYSPHASE=SYSPHASE,BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,  *        
               B#BUY,PBUYKEY,B#INVVAL,INVVALSD,B#INVCOM,IVCOMNTD,      *        
               B#INSARY,INSVARY,B#INSHIS,ICAD),                        *        
               AUTOCLEAR=Y                                                      
         EJECT                                                                  
                                                                                
CODE     NMOD1 0,**PL11*,RR=RE                                                  
         LR    R0,RE                                                            
         USING LP_D,R1                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
                                                                                
         LARL  R6,GLOBALS                                                       
         USING GLOBALS,R6          R6=A(GLOBAL LITERALS)                        
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         USING RUNFACSD,RE         RE=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST RUNNING ONLINE                          
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK 1              
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK 2              
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,=AL2(WORKL)                                                 
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
         USING SAVED,R8,RA         R8/RA=A(SAVE W/S)                            
                                                                                
INIT04   MVC   ATWA,LP_ATWA                                                     
         MVC   VMASTC,RMASTC                                                    
         MVC   VSYSFACS,RSYSFACS                                                
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   VBUFFRIN,RBUFFRIN                                                
         DROP  RE                                                               
                                                                                
         USING PBUD,PBUREC         PUBLICATION RECORD                           
         USING RBUD,RBUREC         PUBLISHER/REP RECORD                         
         USING IBUD,IBUREC         INVOICE RECORD                               
         USING TSARD,TSARBLK       TSAR CONTROL BLOCK                           
         USING MINBLKD,MINBLOCK    MINIO CONTROL BLOCK                          
         USING CFMIOD,CFMIOC       CFMIO CONTROL BLOCK                          
         USING PPBYOUTD,WORKAREA   PPBYOUT CONTROL BLOCK                        
                                                                                
         LA    RA,SAVED            SET UP SECOND SAVED REGISTER                 
         AHI   RA,4096                                                          
         ST    R1,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    R0,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
         TM    GIND1,GIONLINE      TEST RUNNING OFFLINE                         
         BNZ   RUNSTR02                                                         
                                                                                
         L     R2,ALP                                                           
         USING LP_D,R2                                                          
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)                                                     
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
                                                                                
         GOTOR (RF),DMCB,('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)                                                     
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
         L     RF,VSYSFACS         COPY FACILITIES ADCONS                       
         MVC   SYSADDR(SYSADDRL),0(RF)                                          
         DROP  R2                                                               
                                                                                
RUNSTR02 MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         LA    R1,OADCONS          RELOCATE ADCONS                              
         LA    R0,OADCONSN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         A     RF,SRVRRELO                                                      
         ST    RF,0(R1)                                                         
         AHI   R1,L'OADCONS                                                     
         BCTR  R0,RE                                                            
                                                                                
         LHI   R0,QGETINS          LOAD GETINS                                  
         ICM   R0,B'1110',T00A                                                  
         GOTOR VCALLOV,DMCB,0,(R0),0                                            
         MVC   VGETINS,0(R1)                                                    
                                                                                
         LHI   R0,QPSTVAL          LOAD PSTVAL                                  
         ICM   R0,B'1110',T00A                                                  
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   VPSTVAL,0(R1)                                                    
                                                                                
         LHI   R0,QPUBVAL          LOAD PUBVAL                                  
         ICM   R0,B'1110',T00A                                                  
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   VPUBVAL,0(R1)                                                    
                                                                                
         LHI   R0,QPUBEDIT         LOAD PUBEDIT                                 
         ICM   R0,B'1110',T00A                                                  
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   VPUBEDIT,0(R1)                                                   
                                                                                
         LHI   R0,QPGETADR         LOAD GETADR                                  
         ICM   R0,B'1110',T00A                                                  
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   VPPGETAD,0(R1)                                                   
                                                                                
         L     RF,ACOMFACS         SET A(MINIO)                                 
         MVC   VMINIO,CMINIO-COMFACSD(RF)                                       
                                                                                
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         LA    R0,SAVED                                                         
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
B#CLT    EQU   3                   CLIENT RECORD                                
B#PRD    EQU   3                   PRODUCT RECORD                               
B#BUY    EQU   3                   BUY RECORD                                   
B#PUB    EQU   3                   PUBLICATION RECORD                           
         MVC   LP_BLKS+((B#BUY-1)*L'LP_BLKS),AIO2                               
B#ACH    EQU   4                   ADDITIONAL CHARGES TABLE                     
         MVC   LP_BLKS+((B#ACH-1)*L'LP_BLKS),AIO6                               
B#INVVAL EQU   5                   INVOICE VALUES                               
B#PROFVL EQU   5                   PROFILE VALUES                               
B#AGYBLK EQU   5                   BLOCK USED FOR AGENCY RECORD                 
B#INSHIS EQU   5                   INSERTION HISTORY                            
         MVC   LP_BLKS+((B#INVVAL-1)*L'LP_BLKS),AIO4                            
         MVC   AINVVAL,AIO4                                                     
B#INVCOM EQU   6                   INVOICE COMMENTS                             
B#INSARY EQU   6                   ARRAYS FOR BUY/INVOICE DATA                  
B#BCCBLK EQU   6                   BLOCK USED FOR BUY CUSTOM COLUMN             
B#PIVBLK EQU   6                   Block used for Vendor Inv# from Pay          
B#ESRRPY EQU   6                   EIO SETUP RECORD REPLY                       
         MVC   LP_BLKS+((B#INVCOM-1)*L'LP_BLKS),AIO5                            
                                                                                
         TM    GIND1,GIONLINE      TEST RUNNING ONLINE                          
         JZ    EXITY                                                            
         L     RF,ACOMFACS         YES - NEED DICTIONARY WORDS TOO              
         L     RF,CDICTATE-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,C'LL  ',ACTVDICT,ACTVWHAT                              
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNPMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    REQVALS(REQVALL),REQVALS                                         
         XC    LASTS(LASTL),LASTS                                               
         LA    R0,PBUREC           CLEAR PUBLICATION RECORD                     
         LHI   R1,PBUL                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    RBUREC,RBUREC       CLEAR PUBLISHER/REP RECORD                   
         L     R0,AIO1             CLEAR THE I/O AREAS                          
         LHI   R1,IOAREAL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,IBUKEYL                                                   
         MVI   TSPAGN,8                                                         
         MVI   TSINDS,TSINODSK+TSIXTTWA                                         
         LHI   R0,IBUL                                                          
         STCM  R0,3,TSRECL                                                      
         LA    R0,IBUREC                                                        
         STCM  R0,15,TSAREC                                                     
         MVI   TSACTN,TSAINI       INITIALIZE TSAR BUFFER                       
         GOTOR BUFFER                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSAADD       SET TSAR ACTION TO 'ADD'                     
                                                                                
         TM    GIND1,GIONLINE      TEST RUNNING OFFLINE                         
         BNZ   PRCWRKX             NO                                           
                                                                                
         GOTOR VBUFFRIN,DMCB,('BUFFAINI',PUBBUFF),PBUD,ACOMFACS                 
         GOTOR VBUFFRIN,DMCB,('BUFFAINI',REPBUFF),RBUD,ACOMFACS                 
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,PRTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PRTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PUBDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PUBFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENDIR,(4,0),0                               
                                                                                
PRCWRKX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         OC    ENDDATE,ENDDATE     SET END DATE IF NOT GIVEN                    
         BNZ   *+10                                                             
         MVC   ENDDATE,EFFS                                                     
                                                                                
         MVC   AGY,LP_AGY          SET AGENCY CODE                              
         L     RF,ATWA                                                          
         MVC   TWAAGY-TWAD(,RF),AGY                                             
         ICM   RF,15,VMASTC        SET TRACE OPTION IF OFFLINE                  
         JZ    *+10                                                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         CLC   LP_QMAPN,M@DLREF    TEST REFRESH INSERTION DOWNLOAD              
         BE    RUNREQ02                                                         
         CLC   LP_QMAPN,M@DLHST    TEST INSERTION HISTORY DOWNLOAD              
         BE    RUNREQ04                                                         
         CLC   LP_QMAPN,M@CFMCLT   TEST CFM CLIENT DOWNLOAD                     
         BE    RUNREQ06                                                         
         CLC   LP_QMAPN,M@CFMPRD   TEST CFM PRODUCT DOWNLOAD                    
         BE    RUNREQ08                                                         
         CLC   LP_QMAPN,M@CFMPUB   TEST CFM VENDOR DOWNLOAD                     
         BE    RUNREQ10                                                         
         CLC   LP_QMAPN,M@STATSD   TEST STATS DOWNLOAD                          
         BE    RUNREQ12                                                         
                                                                                
         GOTOR BLDPUB              BUILD PUBLICATION LIST                       
                                                                                
RUNREQ02 GOTOR BLDACC              BUILD ADDITIONAL CHARGE CODES                
         B     RUNREQGO                                                         
                                                                                
RUNREQ04 GOTOR BLDHST              BUILD INSERTION HISTORY DOWNLOAD             
         B     RUNREQGO                                                         
                                                                                
RUNREQ06 GOTOR BLDMED              BUILD MEDIA LIST                             
         B     RUNREQGO                                                         
                                                                                
RUNREQ08 GOTOR BLDMRC              BUILD MEDIA/RECORD/CLIENT TABLE              
         JNE   EXITY                                                            
         B     RUNREQGO                                                         
                                                                                
RUNREQ10 GOTOR BLDPUM              BUILD PUBLICATION MEDIA LIST                 
         JNE   EXITY                                                            
         B     RUNREQGO                                                         
                                                                                
RUNREQ12 GOTOR INICFM              INITIALIZE FOR CFMIO READING                 
         JNE   EXITY               EXIT IF ERROR OR NOTHING TO DO               
                                                                                
RUNREQGO L     R1,ALP                                                           
         GOTOR LP_APUTO            CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT MEDIA CODE FOR STATS                                        *         
***********************************************************************         
                                                                                
CNVMED   LM    R2,R3,0(R1)         R2=A(INPUT),R3=A(OUTPUT)                     
         MVC   0(L'PCLTKMED,R3),0(R2)                                           
         J     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE CLIENT CODE FOR STATS                                      *         
***********************************************************************         
                                                                                
VALCFC   LM    R2,R3,0(R1)         R2=A(MEDIA),R3=A(CLIENT)                     
         L     RF,ATWA                                                          
         MVC   QMED-TWAD(,RF),0(R2)                                             
         GOTOR (#VALCLT,AVALCLT),DMCB,(R3),L'PCLTKCLT,1(R2)                     
         J     EXIT                                                             
                                                                                
***********************************************************************         
* VALIDATE PUBLICATION CODE FOR STATS                                 *         
***********************************************************************         
                                                                                
VALCFV   LM    R2,R3,0(R1)         R2=A(INPUT),R3=A(OUTPUT)                     
         LA    R2,0(R2)            CLEAR HOB OF R2                              
         LA    RF,17(R2)                                                        
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         BRCT  RF,*-8                                                           
         SR    RF,R2                                                            
         AHI   RF,1                RF=L'INPUT CODE                              
         GOTOR VPUBVAL,DMCB,((RF),(R2)),(0,(R3))                                
         CLI   0(R1),FF                                                         
         JE    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* CALL CFMIO TO GET NEXT OUTPUT RECORD FOR STATS DOWNLOAD             *         
***********************************************************************         
                                                                                
NXTSTAT  LA    R0,PPVALUES                                                      
         ST    R0,LP_ADATA         SET A(OUTPUT VALUES)                         
         GOTOR VCFMIO,CFMIOD                                                    
         JNE   NOMORE                                                           
         J     EXITY                                                            
                                                                                
***********************************************************************         
* GET SPACE DESCRIPTION FOR CFMIO                                     *         
***********************************************************************         
                                                                                
GETSPC   LM    R2,R3,0(R1)         R2=A(INSERTION),R3=A(OUTPUT)                 
         ST    R2,PBYOINPT                                                      
         LA    R0,WORK                                                          
         ST    R0,PBYOVALS                                                      
         MVC   PBYODTCN,VDATCON                                                 
         GOTOR VPPBYOUT,DMCB,PPBYOUTD                                           
         MVC   0(L'PBYOSPC1,R3),PBYOSPC1                                        
         J     EXITY                                                            
                                                                                
***********************************************************************         
* EDIT PUBLICATION CODE FOR STATS DOWNLOAD                            *         
***********************************************************************         
                                                                                
EDTPUB   L     R2,LP_AINP                                                       
         L     R4,LP_AOUT                                                       
         OC    0(L'PPPUBCOD,R2),0(R2)                                           
         JNZ   *+14                                                             
         XC    LP_OLEN,LP_OLEN                                                  
         J     EXITY                                                            
         MVC   0(20,R4),SPACES                                                  
         GOTOR VPUBEDIT,DMCB,(X'08',(R2)),(C'S',(R4))                           
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET CLIENT RECORDS FOR CFM CLIENT DOWNLOAD                          *         
***********************************************************************         
                                                                                
NXTCLT   J     *+12                                                             
         DC    C'*NXTCLT*'                                                      
         LR    RB,RF                                                            
         USING NXTCLT,RB                                                        
                                                                                
NXTCLT02 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',CLTTAB),                 *        
               ('B#CLT',0),SAVED,('#LIMACC',ALIMACC)                            
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING PCLTRECD,R2         R2=A(CLIENT RECORD)                          
         XC    IOADDR,IOADDR                                                    
         MVC   SVCLTKEY,IOKEY                                                   
         USING PPRDRECD,IOKEY                                                   
         MVI   PPRDKRCD,X'06'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR'                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PPRDKEY(PPRDKPRD-PPRDKEY),IOKEYSAV                               
         MVC   IOKEY,SVCLTKEY      TEST ANY PRODUCTS FOR THIS CLIENT            
         BNE   NXTCLT02            NO - IGNORE                                  
         CLC   LASTMED,PCLTKMED                                                 
         MVC   LASTMED,PCLTKMED                                                 
         BNE   *+8                                                              
         MVI   PCLTKMED,0          ONLY SEND MEDIA ONCE                         
         J     EXITY                                                            
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET PRODUCT RECORD FOR CFM PRODUCT DOWNLOAD                         *         
***********************************************************************         
                                                                                
NXTPRD   J     *+12                                                             
         DC    C'*NXTPRD*'                                                      
         LR    RB,RF                                                            
         USING NXTPRD,RB                                                        
                                                                                
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',PRDTAB),                 *        
               ('B#PRD',0),SAVED,0                                              
         L     R2,IOADDR                                                        
         USING PPRDRECD,R2         R2=A(PRODUCT RECORD)                         
         CLC   LASTMED,PPRDKMED                                                 
         MVC   LASTMED,PPRDKMED                                                 
         BNE   *+8                                                              
         MVI   PPRDKMED,0          ONLY SEND MEDIA ONCE                         
         CLC   LASTCLT,PPRDKCLT                                                 
         MVC   LASTCLT,PPRDKCLT                                                 
         BNE   *+10                                                             
         XC    PPRDKCLT,PPRDKCLT   ONLY SEND CLIENT ONCE                        
         J     EXITY                                                            
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET PUBLICATION RECORD FOR CFM VENDOR DOWNLOAD                      *         
***********************************************************************         
                                                                                
NXTVEN   J     *+12                                                             
         DC    C'*NXTVEN*'                                                      
         LR    RB,RF                                                            
         USING NXTVEN,RB                                                        
                                                                                
         L     R2,AIO2                                                          
         USING PUBRECD,R2          R2=A(PUBLICATION RECORD)                     
         CLI   PUBKMED,FF                                                       
         BE    NXTVEN04                                                         
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',PUBTAB),                 *        
               ('X'80'+B#PUB',0),SAVED,0                                        
         BNE   NXTVEN02                                                         
         XC    BUYPUB,BUYPUB                                                    
         GOTOR VPUBEDIT,DMCB,(X'08',PUBKPUB),(C'S',BUYPUB)                      
         J     EXITY                                                            
                                                                                
NXTVEN02 L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RNEXT                                                
         MVI   PUBKMED,FF                                                       
         J     EXITY                                                            
                                                                                
NXTVEN04 L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RQUIT                                                
         J     EXITY                                                            
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET PUBLICATION REP RECORDS FOR CFM VENDOR DOWNLOAD                 *         
***********************************************************************         
                                                                                
NXTPUR   J     *+12                                                             
         DC    C'*NXTPUR*'                                                      
         LR    RB,RF                                                            
         USING NXTPUR,RB                                                        
         MVC   LP_ADATA,AIO4                                                    
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTPUR02                                                         
         MVC   SVPUBKEY,IOKEY      SAVE PUBLICATION KEY                         
         L     R2,AIO2                                                          
         USING PUBRECD,R2          R2=A(PUBLICATION RECORD)                     
         CLI   LASTMED,0           TEST FIRST TIME FOR FIRST MEDIA              
         BE    NXTPUR04                                                         
         CLC   PUBKMED,LASTMED     TEST CHANGE OF MEDIA                         
         BE    NXTPUR06                                                         
         XC    LASTREP,LASTREP                                                  
                                                                                
NXTPUR02 LH    RE,LASTREP                                                       
         AHI   RE,1                                                             
         STH   RE,LASTREP                                                       
         CHI   RE,9999                                                          
         BH    NXTPUR04                                                         
         BCTR  RE,0                                                             
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         IC    RF,BITLIST(RF)                                                   
         LR    R1,RE                                                            
         A     R1,AIO3                                                          
         BASR  RE,0                                                             
         EX    RF,8(RE)            TEST REP REFERENCE BIT ON                    
         JZ    NXTPUR02                                                         
         TM    0(R1),0                                                          
                                                                                
         LA    R2,IOKEY                                                         
         USING PREPRECD,R2         R2=A(REP KEY)                                
         XC    PREPKEY,PREPKEY                                                  
         MVC   PREPKAGY,AGY                                                     
         MVC   PREPKMED,LASTMED                                                 
         MVI   PREPKRCD,PREPKRCQ                                                
         LH    R0,LASTREP                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PREPKREP,DUB                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
NXTPUR04 L     R0,AIO3             CLEAR I/O AREA 3 FOR REP ARRAY               
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
NXTPUR06 L     R2,AIO2                                                          
         USING PUBRECD,R2          R2=A(PUBLICATION RECORD)                     
         CLI   PUBKMED,FF                                                       
         BE    NXTPUR10                                                         
         MVC   LASTMED,PUBKMED     SAVE THIS MEDIA                              
         OC    PUBPLSH,PUBPLSH                                                  
         BZ    NXTPUR08                                                         
         CLC   PUBPLSH,SPACES                                                   
         BE    NXTPUR08                                                         
         PACK  DUB,PUBPLSH                                                      
         CVB   RE,DUB                                                           
         LTR   RE,RE                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RF,BITLIST(RF)                                                   
         A     RE,AIO3                                                          
         OC    0(1,RE),0(RF)       TURN ON REP REFERENCE BIT                    
                                                                                
NXTPUR08 MVC   IOKEY,SVPUBKEY                                                   
         J     NOMORE                                                           
                                                                                
NXTPUR10 L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RQUIT                                                
         J     EXITY                                                            
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET BUY RECORDS, APPLY FILTERS AND FORMAT FIELDS FOR DOWNLOADING    *         
***********************************************************************         
                                                                                
NXTBUY   J     *+12                                                             
         DC    C'*NXTBUY*'                                                      
         LR    RB,RF                                                            
         USING NXTBUY,RB                                                        
                                                                                
         MVI   READDELS,YESQ       SET TO INCLUDE DELETED BUYS                  
         TM    FILTSTAT,FILTSLDE+FILTSLUD+FILTSTDE+FILTSTUD                     
         BNM   NXTBY02                                                          
         MVI   READDELS,0          SET NOT TO GET DELETED BUYS                  
         TM    FILTSTAT,FILTSLDE+FILTSTDE                                       
         BZ    NXTBY02                                                          
         MVI   READDELS,YESQ       SET TO INCLUDE DELETED BUYS                  
         TM    FILTSTAT,FILTSLUD+FILTSTUD                                       
         BNZ   NXTBY02                                                          
         MVI   READDELS,ONLYQ      SET TO ONLY GET DELETED RECORDS              
                                                                                
NXTBY02  CLI   FLTDLDTT,0          DOWNLOAD BY DATE?                            
         BE    NXTBY12                                                          
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',BUYPPTB),('B#BUY',0),    +        
               (READDELS,SAVED),0                                               
         JNE   EXITY                                                            
         B     NXTBY16                                                          
NXTBY12  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',BUYTAB),('B#BUY',0),     *        
               (READDELS,SAVED),0                                               
         JNE   EXITY                                                            
NXTBY16  L     R2,IOADDR                                                        
         USING PBUYRECD,R2         R2=A(BUY KEY)                                
                                                                                
         CLI   FLTDLDTT,0          DOWNLOAD BY DATE?                            
         BE    NXTBY18                                                          
         SR    RE,RE                                                            
         ICM   RE,7,APUB           NEED TO PROCESS PUB FLITERING?               
         BZ    NXTBY18                                                          
         USING LW_D,RE                                                          
         TM    PUBIND,LQ_TSINQ     SINGLE ENTRY PUB?                            
         BZ    *+18                                                             
         CLC   PBUYKPUB(LPUBKPZE),LW_DATA1                                      
         BNE   NXTBY02                                                          
         B     NXTBY18                                                          
                                                                                
         TM    PUBIND,LQ_TRNGQ     RANGE OF PUB VALUES?                         
         BZ    *+28                                                             
         CLC   PBUYKPUB(LPUBKPZE),LW_DATA1                                      
         BL    NXTBY02                                                          
         CLC   PBUYKPUB(LPUBKPZE),LW_DATA1+LPUBKPZE                             
         BH    NXTBY02                                                          
         B     NXTBY18                                                          
                                                                                
         TM    PUBIND,LQ_TLSTQ     LIST OF PUB VALUES?                          
         BZ    NXTBY02                                                          
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN                                                     
         LA    R1,LW_DATA2                                                      
         CLC   PBUYKPUB(LPUBKPZE),0(R1)                                         
         BE    NXTBY18                                                          
         LA    R1,LPUBKPZE(R1)                                                  
         BCT   RF,*-14                                                          
         B     NXTBY02                                                          
         DROP  RE                                                               
                                                                                
NXTBY18  CLC   PBUYKMED,LASTMED    TEST CHANGE OF MEDIA/CLIENT                  
         BNE   *+14                                                             
         CLC   PBUYKCLT,LASTCLT                                                 
         BE    NXTBY30                                                          
         MVC   LASTMED,PBUYKMED    SAVE NEW MEDIA/CLIENT                        
         MVC   LASTCLT,PBUYKCLT                                                 
                                                                                
         GOTOR (#GETCLT,AGETCLT),DMCB,PBUYKAGY,PBUYKMED,PBUYKCLT                
         BE    NXTBY20                                                          
         CLI   FLTDLDTT,0          DOWNLOAD BY DATE?                            
         BE    NXTBY19                                                          
         ICM   R0,7,IOKEY+(PBYPCLT-PBYPKEY)                                     
         AHI   R0,1                                                             
         STCM  R0,7,IOKEY+(PBYPCLT-PBYPKEY)                                     
         LA    RE,IOKEY+(PBYPPRD-PBYPKEY)                                       
         XC    0(PBYPCNTL-PBYPPRD,RE),0(RE)                                     
         B     NXTBY02                                                          
                                                                                
NXTBY19  LA    R2,IOKEY                                                         
         ICM   R0,7,PBUYKCLT       BUMP TO NEXT CLIENT RECORD                   
         AHI   R0,1                                                             
         STCM  R0,7,PBUYKCLT                                                    
         XC    PBUYKPRD(PBUYLEN-PBUYKPRD),PBUYKPRD                              
         B     NXTBY02                                                          
                                                                                
NXTBY20  GOTOR GETCPR              GET CLIENT PROFILES                          
                                                                                
NXTBY30  GOTOR FBUY,1              FILTER AND FORMAT BUY RECORD                 
         BNE   NXTBY02             DIDN'T PASS THE FILTER TESTS                 
                                                                                
         ICM   RE,15,T_INSCNT                                                   
         AHI   RE,1                                                             
         STCM  RE,15,T_INSCNT      TOTAL # OF INSERTIONS COUNTER                
         CHI   RE,1                                                             
         BNL   *+6                                                              
         DC    H'0'                                                             
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT BUY RECORD FOR INSERTION REFRESH DOWNLOAD                  *         
***********************************************************************         
                                                                                
NXTSER   J     *+12                                                             
         DC    C'*NXTSER*'                                                      
         LR    RB,RF                                                            
         USING NXTSER,RB                                                        
         USING LP_D,R1                                                          
         LA    R0,BUYVALS                                                       
         ST    R0,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTSER02                                                         
         SR    RE,RE                                                            
         ICM   RE,7,ASER           POINT TO WORK MAP POOL ENTRY                 
         JZ    NOMORE                                                           
         USING LW_D,RE                                                          
         LHI   R0,1                                                             
         LA    R1,LW_DATA1         SET N'ENTRIES TO 1                           
         CLI   LW_TYPE,LQ_TSINQ    TEST SINGLE ENTRY                            
         BE    *+12                                                             
         ICM   R0,3,LW_NUMN        NO - PICK UP N'ENTRIES FROM POOL             
         LA    R1,LW_DATA2                                                      
         DROP  RE                                                               
         STH   R0,NSER             SET N'ENTRIES TO BE PROCESSED                
         ST    R1,ANXTSER          SET A(NEXT ONE TO BE PROCESSED)              
         B     NXTSER04                                                         
                                                                                
NXTSER02 LH    R0,NSER             BUMP TO NEXT ENTRY IN POOL                   
         SHI   R0,1                                                             
         JZ    NOMORE                                                           
         STH   R0,NSER                                                          
         L     R1,ANXTSER                                                       
         AHI   R1,BUYSERSL                                                      
         ST    R1,ANXTSER                                                       
                                                                                
NXTSER04 GOTOR GETBUY,(R1)         READ THE BUY RECORD                          
         BE    NXTSER06                                                         
                                                                                
         GOTOR XC_BLCK#,B#INSARY   CLEAR BUY ARRAY DATA                         
         GOTOR XCBUYVAL                                                         
                                                                                
         ZAP   BUYCDPCT,PZERO                                                   
         L     R1,AIO2             CLEAR BUY RECORD VALUES                      
         XC    0(256,R1),0(R1)                                                  
         L     R1,ANXTSER          SET SERIAL NUMBER                            
         MVC   BUYSER(BUYSERSL),0(R1)                                           
         MVI   BUYBSTAT,BUYBSNFD   SET STATUS TO 'NOT FOUND'                    
         J     EXITY                                                            
                                                                                
NXTSER06 L     R2,IOADDR           R2=A(BUY RECORD)                             
                                                                                
         CLC   PBUYKMED,LASTMED    TEST CHANGE OF MEDIA/CLIENT                  
         BNE   *+14                                                             
         CLC   PBUYKCLT,LASTCLT                                                 
         BE    NXTSER08                                                         
         MVC   LASTMED,PBUYKMED    SAVE NEW MEDIA/CLIENT                        
         MVC   LASTCLT,PBUYKCLT                                                 
                                                                                
         GOTOR (#GETCLT,AGETCLT),DMCB,PBUYKAGY,PBUYKMED,PBUYKCLT                
         BNE   NXTSER02                                                         
                                                                                
         GOTOR GETCPR              GET CLIENT PROFILES                          
                                                                                
NXTSER08 GOTOR FBUY,0              FORMAT BUY RECORD FOR DOWNLOAD               
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
         DROP  RB,R1                                                            
         EJECT                                                                  
***********************************************************************         
* GET INVOICE RECORDS FROM VENDOR INVOICE NUMBERS                     *         
***********************************************************************         
                                                                                
NXTIV#   J     *+12                                                             
         DC    C'*NXTIV#*'                                                      
         LR    RB,RF                                                            
         USING NXTIV#,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         USING INVVALSD,R5                                                      
                                                                                
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         JNE   *+20                                                             
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   *+12                                                             
         GOTOR XC_IVALS,5          CLEAR SPECIAL REP DATA AND TABLE             
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTIV#70                                                         
                                                                                
         BRAS  RE,MODFYFIL         MODIFY FILTERS                               
                                                                                
         ICM   R1,7,AMED                                                        
         MVC   WRKMEDCD,LW_DATA1-LW_D(R1)                                       
         GOTOR (#VALMED,AVALMED),DMCB,WRKMEDCD,L'PAGYKMED,WRKMEDCD              
         JNE   NXTIV#90                                                         
                                                                                
         ICM   R1,7,ACLT                                                        
         MVC   WRKCLTCD,LW_DATA1-LW_D(R1)                                       
         OC    WRKCLTCD,SPACES                                                  
         CLC   WRKCLTCD,SPACES     Have client code?                            
         JNH   NXTIV#10                                                         
         CLC   WRKCLTCD,=C'***'                                                 
         JE    NXTIV#10                                                         
         GOTOR (#VALCLT,AVALCLT),DMCB,WRKCLTCD,L'PCLTKCLT,WRKCLTCD              
         JNE   NXTIV#90                                                         
                                                                                
NXTIV#10 CLI   INVDLSW,0           DOWNLOADING INVOICES SW NOT YET SET?         
         JNE   *+8                                                              
         MVI   INVDLSW,YESQ                                                     
         MVI   INVDLMOD,0          INIT INVOICE DOWNLOAD MODE                   
                                                                                
         XC    IOKEY,IOKEY         SET UP INVOICE PASSIVE KEY                   
I        USING PNV3KEY,IOKEY                                                    
         MVC   I.PNV3AGY,AGY                                                    
         MVC   I.PNV3MED,WRKMEDCD                                               
         MVI   I.PNV3RCD,PNV3RCDQ                                               
         MVC   I.PNV3CLT,WRKCLTCD                                               
                                                                                
         XC    WRKPUBCD,WRKPUBCD   Init pub (PWOS)                              
         OC    BUYPUB,SPACES                                                    
         CLC   BUYPUB,SPACES       Have pub?                                    
         JNH   NXTIV#14                                                         
         LHI   RF,L'BUYPUB                                                      
         LA    RE,BUYPUB+L'BUYPUB-1                                             
         CLI   0(RE),C' '                                                       
         JH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         GOTOR VPUBVAL,DMCB,((RF),BUYPUB),WRKPUBCD                              
         CLI   0(R1),FF                                                         
         JE    NXTIV#92            INVALID PUB, NO NEED TO CONTINUE             
                                                                                
NXTIV#14 XC    TEMP2,TEMP2         FOR SETTING PUB BUFFER (DRR)                 
                                                                                
         OC    INVVNDR#,INVVNDR#                                                
         JZ    NXTIV#24            NO VENDOR INVOICE #, TRY DATES               
         MVI   INVDLMOD,IVDLMD_#   DOWNLOAD BY INOICE NUMBER MODE               
         MVC   I.PNV3PBCD,WRKPUBCD                                              
         MVC   I.PNV3INV#,INVVNDR#                                              
         OC    I.PNV3INV#,SPACES                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDEL+IOHI+IODIR+IO1'                        
                                                                                
         CLC   I.PNV3KEY,IOKEYSAV  RECORD FOUND?                                
         JNE   NXTIV#92            RECORD NOT FOUND, NOTHING TO RETURN          
                                                                                
         MVC   TEMP2(L'PNV3MED),I.PNV3MED                                       
         MVC   TEMP2+L'PNV3MED(L'PNV3PBCD),I.PNV3PBCD                           
                                                                                
         TM    I.PNV3CNTL,PNVCDELQ RECORD IS DELETED?                           
         JZ    NXTIV#54                                                         
         GOTOR XC_IVALS,0          CLEAR ALL INVOICE REPLY VALUES               
         MVC   INVKEY(07),=C'DELETED'                                           
         MVI   INVDLSW,0           RESET INVOICE DOWNLOAD SWITCH                
         J     EXITY                                                            
                                                                                
NXTIV#24 OC    STRDATE,STRDATE                                                  
         JZ    NXTIV#40            No start date, try creation date             
         MVI   INVDLMOD,IVDLMD_D   DOWNLOAD BY START AND END DATES MODE         
                                                                                
I        USING PNV2KEY,IOKEY                                                    
         MVI   I.PNV2RCD,PNV2RCDQ                                               
         MVC   I.PNV2PUB,WRKPUBCD                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    NXTIV#26                                                         
         DC    H'0'                                                             
NXTIV#26 CLI   INVDLMOD,IVDLMD_C   Download by invoice creation date?           
         JE    NXTIV#46                                                         
         CLC   I.PNV2KEY(PNV2SDTE-PNV2KEY),IOKEYSAV                             
         JNE   NXTIV#92                                                         
         CLC   I.PNV2SDTE,ENDDATE                                               
         JH    NXTIV#82                                                         
         CLC   I.PNV2EDTE,STRDATE                                               
         JL    NXTIV#82                                                         
                                                                                
         MVC   TEMP2(L'PNV2MED),I.PNV2MED                                       
         MVC   TEMP2+L'PNV2MED(L'PNV2PUB),I.PNV2PUB                             
         J     NXTIV#52                                                         
                                                                                
NXTIV#40 OC    STRDAT2,STRDAT2     Have creation start date?                    
         JZ    NXTIV#92            No creation date, nothing to look up         
         MVI   INVDLMOD,IVDLMD_C   Download by creation date mode               
                                                                                
I        USING PNV6KEY,IOKEY                                                    
         MVI   I.PNV6RCD,PNV6RCDQ                                               
         MVC   I.PNV6CDTE,STRDAT2                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    NXTIV#46                                                         
         DC    H'0'                                                             
NXTIV#46 CLC   I.PNV6KEY(PNV6CDTE-PNV6KEY),IOKEYSAV                             
         JNE   NXTIV#92                                                         
         CLC   I.PNV6CDTE,ENDDAT2                                               
         JH    NXTIV#92                                                         
                                                                                
         CLC   WRKCLTCD,SPACES     Have client code?                            
         JNH   *+14                                                             
         CLC   I.PNV6CLT,WRKCLTCD  Match client code?                           
         JNE   NXTIV#82            No, read next passive key                    
                                                                                
         OC    WRKPUBCD,WRKPUBCD   Have pub code?                               
         JZ    *+14                                                             
         CLC   I.PNV6PUB,WRKPUBCD  Match pub code?                              
         JNE   NXTIV#82            No, read next passive key                    
                                                                                
         CLC   INVSURCE,SPACES                                                  
         JNH   NXTIV#50            No invoice source to filter on               
         LHI   R0,L'INVSURCE                                                    
         LA    RF,INVSURCE         Point to filtering entries                   
         CLC   I.PNV6SRCE,0(RF)    Invoice source match?                        
         JE    NXTIV#50                                                         
         LA    RF,1(RF)            Point to next filter entry                   
         JCT   R0,*-14                                                          
         J     NXTIV#82            Failed filter, read next passive key         
                                                                                
NXTIV#50 MVC   TEMP2(L'PNV6MED),I.PNV6MED                                       
         MVC   TEMP2+L'PNV6MED(L'PNV6PUB),I.PNV6PUB                             
                                                                                
NXTIV#52 MVC   INVWKKEY,IOKEY      SAVE IT FOR NEXT ROUND                       
                                                                                
NXTIV#54 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         L     RE,AIO1                                                          
         MVC   MINMAST,0(RE)       SAVE MINIO MASTER KEY                        
         MVC   MINMKEY(L'PNVKEY),MINMAST                                        
         GOTOR XC_IVALS,4          PREPARE TO REPLY INVOICE HDR VALUES          
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTIV#56            ONLY NEED TO INIT ONCE                       
         GOTOR MININIT             INIT MINIO                                   
         MVC   MINMKEY(L'PNVKEY),MINMAST                                        
         GOTOR VMINIO,MINPARMS,('MINOPN',MINBLKD)                               
         CLI   MINERR,0                                                         
         JE    NXTIV#56                                                         
         DC    H'0'                                                             
                                                                                
NXTIV#56 LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVHDRD,RE                                                       
         MVI   PNVHKCDE,PNVHKIDQ   INVOICE HEADER ELEM ID                       
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         JNZ   *+6                                                              
         DC    H'0'                THERE MUST BE AN INVOICE HDR ELEM!           
                                                                                
         TM    PNVHSTUS,PNVHDLQ    DELETED?                                     
         JNZ   NXTIV#80            DELETED, PROC NEXT ONE                       
                                                                                
         OC    FILTSREP,FILTSREP                                                
         JZ    *+14                                                             
         CLC   FILTSREP,PNVHSREP   SPECIAL REP MATCHED FILTER?                  
         JNE   NXTIV#80            NO, PROC NEXT ONE                            
                                                                                
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY DATES MODE?                      
         JNE   NXTIV#60                                                         
         CLC   INVMATST,SPACES                                                  
         JNH   NXTIV#58            No invoice status to filter on               
         LHI   R0,L'INVMATST                                                    
         LA    RF,INVMATST         Point to filtering entries                   
         CLC   PNVHSTAT,0(RF)      Invoice source match?                        
         JE    NXTIV#58                                                         
         LA    RF,1(RF)            Point to next filter entry                   
         JCT   R0,*-14                                                          
         J     NXTIV#80            FAILED FILTER, PROC NEXT ONE                 
                                                                                
NXTIV#58 CLC   INVSURCE,SPACES                                                  
         JNH   NXTIV#60            No invoice source to filter on               
         LHI   R0,L'INVSURCE                                                    
         LA    RF,INVSURCE         Point to filtering entries                   
         CLI   PNVHIVSR,0                                                       
         JNE   *+8                                                              
         MVI   PNVHIVSR,INVADB_Q   Set tp reserved AdBuyer equate               
         CLC   PNVHIVSR,0(RF)      Invoice source match?                        
         JE    NXTIV#60                                                         
         LA    RF,1(RF)            Point to next filter entry                   
         JCT   R0,*-14                                                          
                                                                                
         J     NXTIV#80            FAILED FILTER, PROC NEXT ONE                 
                                                                                
NXTIV#60 GOTOR FIVH                FORMAT INVOICE HEADER VALUES                 
                                                                                
         MVI   INVCMSW,0           INIT INV COMMENT SW FOR LATER                
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         JNE   EXITY                                                            
         MVC   WORK,TEMP2                                                       
         GOTOR SETPBUFF            PUB REPLY RECORD DRIVEN BY INVOICES          
         J     EXITY                                                            
                                                                                
NXTIV#70 CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         JNE   NXTIV#96                                                         
                                                                                
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY DATES MODE?                      
         JE    NXTIV#80                                                         
         CLI   INVDLMOD,IVDLMD_C   Download by invoice creation date?           
         JNE   NXTIV#86                                                         
NXTIV#80 MVC   IOKEY(L'INVWKKEY),INVWKKEY                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    NXTIV#82                                                         
         DC    H'0'                                                             
                                                                                
NXTIV#82 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JE    NXTIV#26                                                         
         DC    H'0'                                                             
                                                                                
NXTIV#86 CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENTS DONE?                        
         JNE   NXTIV#96                                                         
         MVI   INVCMSW,0           SET TO DO NEXT HEADER COMMENT                
         J     NXTIV#96                                                         
                                                                                
NXTIV#90 MVI   DL_TYPE,DL_MSGQ     SET TO REPLY MESSAGE                         
         MVC   SVMSGMAP,=AL2(D#CLTCOD)                                          
         MVC   SVMSGNUM,=AL2(207)                                               
                                                                                
NXTIV#92 GOTOR XC_IVALS,8          CLEAR ALL, EXCEPT SPECIAL REP TAB            
                                                                                
NXTIV#96 J     NOMORE                                                           
                                                                                
         DROP  RB,R3,R5,I,RE                                                    
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET INVOICE RECORDS FROM INVOICE KEY(S)                             *         
***********************************************************************         
                                                                                
NXTIVK   J     *+12                                                             
         DC    C'*NXTIVK*'                                                      
         LR    RB,RF                                                            
         USING NXTIVK,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         GOTOR XC_IVALS,4          PREPARE TO REPLY INVOICE HDR VALUES          
         USING INVVALSD,R5                                                      
                                                                                
NXTIVK02 CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTIVK04                                                         
         CLI   INVDLSW,0           DOWNLOADING INVOICES SW NOT YET SET?         
         BNE   *+8                                                              
         MVI   INVDLSW,YESQ                                                     
         MVI   INVCMSW,0           INIT INV COMMENT SW FOR LATER                
         MVI   INVDLMOD,IVDLMD_K   DOWNLOAD IS DRIVEN BY INVOICE KEY(S)         
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,ASER           POINT TO WORK MAP POOL ENTRY                 
         BZ    NXTIVK14                                                         
         USING LW_D,RE                                                          
         LHI   R0,1                                                             
         LA    R1,LW_DATA1         SET N'ENTRIES TO 1                           
         CLI   LW_TYPE,LQ_TSINQ    TEST SINGLE ENTRY                            
         BE    *+12                                                             
         ICM   R0,3,LW_NUMN        NO - PICK UP N'ENTRIES FROM POOL             
         LA    R1,LW_DATA2                                                      
         STH   R0,NSER             SET N'ENTRIES TO BE PROCESSED                
         ST    R1,ANXTSER          SET A(NEXT ONE TO BE PROCESSED)              
         B     NXTIVK06                                                         
                                                                                
NXTIVK04 LH    R0,NSER             BUMP TO NEXT ENTRY IN POOL                   
         SHI   R0,1                                                             
         BZ    NXTIVK16            NO MORE TO PROC, DONE                        
         STH   R0,NSER                                                          
         L     R1,ANXTSER                                                       
         AHI   R1,INVKEYSL                                                      
         ST    R1,ANXTSER                                                       
                                                                                
NXTIVK06 GOTOR GETINV,(R1)         READ INVOICE RECORD                          
         CLI   IOERR,0             ANY ERROR DETECTED?                          
         BE    NXTIVK08            TREAT ALL ERROR AS DELETED                   
         MVC   INVKEY(INVKEYSL),0(R1)                                           
         MVI   INVHGSTA,GST_DELQ                                                
         MVI   INVDLSW,0           RESET INVOICE DOWNLOAD SWITCH                
         J     EXITY               TRY NEXT INVOICE KEY                         
                                                                                
NXTIVK08 L     R2,IOADDR           R2=A(INVOICE RECORD)                         
         MVC   MINMAST,0(R2)       SAVE MINIO MASTER KEY                        
         MVC   MINMKEY(L'PNVKEY),MINMAST                                        
         GOTOR MININIT             INIT MINIO                                   
         MVC   MINMKEY(L'PNVKEY),MINMAST                                        
         GOTOR VMINIO,MINPARMS,('MINOPN',MINBLKD)                               
         CLI   MINERR,0                                                         
         BE    NXTIVK10                                                         
         DC    H'0'                                                             
                                                                                
NXTIVK10 LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVHDRD,RE                                                       
         MVI   PNVHKCDE,PNVHKIDQ   INVOICE HEADER ELEM ID                       
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         BNZ   *+6                                                              
         DC    H'0'                THERE MUST BE AN INVOICE HDR ELEM!           
                                                                                
         GOTOR FIVH                FORMAT INVOICE HEADER VALUES                 
         MVI   INVCMSW,0           INIT INV COMMENT SW FOR LATER                
         J     EXITY                                                            
                                                                                
NXTIVK12 CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENTS DONE?                        
         BNE   NXTIVK16                                                         
         MVI   INVCMSW,0           SET TO DO NEXT HEADER COMMENT                
         B     NXTIVK16                                                         
                                                                                
NXTIVK14 GOTOR XC_IVALS,0          CLEAR ALL INVOICE REPLY VALUES               
         MVI   INSDLSW,0           RESET INSERTION DOWNLOAD SWITCH              
         MVI   INVDLSW,0           RESET INVOICE DOWNLOAD SWITCH                
                                                                                
NXTIVK16 J     NOMORE                                                           
         DROP  RB,R3,R5,RE                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET INVOICE ITEMS FROM INSERTIONS (DRR)                             *         
***********************************************************************         
                                                                                
NXTIVI   J     *+12                                                             
         DC    C'*NXTIVI*'                                                      
         LR    RB,RF                                                            
         USING NXTIVI,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         GOTOR XC_IVALS,0          CLEAR ALL REPLY VALUES                       
         USING INVVALSD,R5                                                      
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTIVI10                                                         
         OI    DL_FLAG1,IVIRPLYQ   SET E#INSDET REPLY RECORD (DRR) BIT          
         MVI   INSDLSW,NOQ         NO NEED TO DOWNLOAD INSERTION DATA           
         MVI   INVDLMOD,0          NO NEED TO DOWNLOAD INVOICE DATA             
         XC    IBUREC(IBUL),IBUREC                                              
         MVI   TSACTN,TSARDH       SET TO READ HIGH                             
                                                                                
NXTIVI10 GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BNZ   NXTIVI90                                                         
         MVI   TSACTN,TSANXT       SET TO GET NEXT RECORD                       
         OC    IBUMEDCD,IBUMEDCD                                                
         BZ    NXTIVI10                                                         
         OC    IBUINVS#,IBUINVS#                                                
         BZ    NXTIVI10                                                         
         OC    IBUIVDS#,IBUIVDS#                                                
         BZ    NXTIVI10                                                         
         CLC   IBUIVDS#,EFFS                                                    
         BE    NXTIVI10                                                         
         OC    IBUINSKY,IBUINSKY                                                
         BZ    *+6                                                              
         DC    H'0'                BAD BUFFER KEY                               
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTIVI20                                                         
         GOTOR MININIT             INIT MINIO                                   
                                                                                
NXTIVI20 LA    RE,MINMKEY                                                       
         USING PNVKEY,RE                                                        
         XC    PNVKEY,PNVKEY                                                    
         MVC   PNVKAGY,AGY                                                      
         MVC   PNVKMED,IBUMEDCD    MEDIA                                        
         MVI   PNVKRCD,PNVKRCDQ                                                 
         MVC   PNVKSER#,IBUINVS#                                                
         GOTOR VMINIO,MINPARMS,('MINOPN',MINBLKD)                               
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING PNVHDRD,RE                                                       
         MVI   PNVHKCDE,PNVHKIDQ   INVOICE HEADER ELEM ID                       
         MVI   PNVHKLEN,PNVHDRLQ   INVOICE HEADER ELEM LENGTH                   
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE IT FOR DRR DOWNLOAD                
         GOTOR FIVH                FORMAT INVOICE HEADER VALUES                 
                                                                                
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING PNVDTLD,RE                                                       
         MVI   PNVDKCDE,PNVDKIDQ   INVOICE DETAIL ELEM ID                       
         MVI   PNVDKLEN,PNVDTLLQ   INVOICE DETAIL ELEM LENGTH                   
         MVC   PNVDKSQN,IBUIVDS#   INVOICE DETAIL SEQUENCE#                     
         MVI   PNVDKTYP,PNVDKDSQ   INVOICE DETAIL TYPE                          
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE IT FOR DRR DOWNLOAD                
                                                                                
         CLC   PNVDKSQN,IBUIVDS#   INVOICE DETAIL SEQUENCE# MATCH?              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR FIVD                FORMAT INVOICE DETAIL VALUES                 
                                                                                
         J     EXITY                                                            
                                                                                
NXTIVI90 NI    DL_FLAG1,X'FF'-IVIRPLYQ                                          
         J     NOMORE                                                           
                                                                                
         DROP  RB,R3,R5,RE                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET INVOICE RECORDS FOR INSERTIONS TO BE DOWNLOADED                 *         
***********************************************************************         
                                                                                
NXTINV   J     *+12                                                             
         DC    C'*NXTINV*'                                                      
         LR    RB,RF                                                            
         USING NXTINV,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         USING INVVALSD,R5                                                      
                                                                                
         GOTOR XC_IVALS,4          PREPARE TO REPLY INVOICE HDR VALUES          
                                                                                
         CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   NXTINV12                                                         
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTINV08                                                         
         MVI   INVCMSW,0           INIT INV COMMENT SW FOR LATER                
         MVI   INVDLMOD,IVDLMD_B   DL MODE IS DRIVEN BY BUY (INSERTION)         
         XC    IBUREC(IBUL),IBUREC                                              
         MVI   TSACTN,TSARDH       SET TO READ HIGH                             
                                                                                
NXTINV02 GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BNZ   NXTINV10                                                         
         MVI   TSACTN,TSANXT       SET TO GET NEXT RECORD                       
                                                                                
NXTINV04 OC    IBUREC(L'IBUMEDCD),IBUREC                                        
         BZ    NXTINV02                                                         
         OC    IBUREC+L'IBUMEDCD(L'IBUINVS#),IBUREC+L'IBUMEDCD                  
         BZ    NXTINV02                                                         
         GOTOR MININIT             INIT MINIO                                   
         LA    RE,MINMKEY                                                       
         USING PNVKEY,RE                                                        
         MVC   PNVKAGY,AGY                                                      
         MVC   PNVKMED,IBUMEDCD    MEDIA                                        
         MVI   PNVKRCD,PNVKRCDQ                                                 
         MVC   PNVKSER#,IBUINVS#                                                
         GOTOR VMINIO,MINPARMS,('MINOPN',MINBLKD)                               
         CLI   MINERR,0                                                         
         BE    NXTINV06                                                         
         DC    H'0'                                                             
                                                                                
NXTINV06 LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVHDRD,RE                                                       
         MVI   PNVHKCDE,PNVHKIDQ   INVOICE HEADER ELEM ID                       
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         BNZ   *+6                                                              
         DC    H'0'                THERE MUST BE AN INVOICE HDR ELEM!           
                                                                                
         CLI   DL_TYPE,DL_PRBQ                                                  
         BE    NXTINV14            DON'T REPLY FOR PROBE DOWNLOAD               
         GOTOR FIVH                FORMAT INVOICE HEADER VALUES                 
         B     NXTINV14                                                         
                                                                                
NXTINV08 GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BZ    NXTINV04            NOT YET, DO NEXT INV KEY IN BUFFER           
                                                                                
NXTINV10 MVI   INVDLSW,0           DONE WITH INVOICE DOWNLOAD                   
                                                                                
NXTINV12 MVI   LP_RMODE,LP_RLAST   NO MORE TO PROCESS                           
NXTINV14 CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENTS DONE?                        
         BNE   *+8                                                              
         MVI   INVCMSW,0           SET TO DO NEXT HEADER COMMENT                
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
         DROP  RB,R3,R5,RE                                                      
         EJECT                                                                  
***********************************************************************         
* GET INVOICE COMMENTS                                                *         
***********************************************************************         
                                                                                
GETIVC   J     *+12                                                             
         DC    C'*GETIVC*'                                                      
         LR    RB,RF                                                            
         USING GETIVC,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         USING INVVALSD,R5                                                      
         L     R4,LP_BLKS+((B#INVCOM-1)*L'LP_BLKS)                              
                                                                                
         GOTOR XC_IVALS,2          PREPARE TO REPLY COMMENT(S)                  
                                                                                
         TM    DL_FLAG1,IVIRPLYQ   E#INSDET REPLY RECORD (DRR)?                 
         JNZ   NOMORE                                                           
                                                                                
         CLI   DL_TYPE,DL_PRBQ                                                  
         JE    NOMORE              DON'T REPLY FOR PROBE DOWNLOAD               
                                                                                
         CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   GETIVC22                                                         
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY DATES MODE?                      
         JE    *+12                                                             
         CLI   INVDLMOD,IVDLMD_C   Download by Creation date mode?              
         JNE   GETIVC02                                                         
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BNE   *+12                                                             
         MVI   INSDLSW,YESQ        SET TO REPLY INSERTION DATA                  
         B     GETIVC02                                                         
         GOTOR XC_IVALS,9          CLEAR ALL, EXCEPT HEADER                     
         B     GETIVC22                                                         
                                                                                
GETIVC02 CLI   MINMKEY+L'PNVKAGY+L'PNVKMED,PNVKRCDQ                             
         BE    *+6                                                              
         DC    H'0'                MINIO MASTER KEY IS SET AT HDR LV            
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GETIVC07                                                         
                                                                                
GETIVC04 ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(08,RE),0(RE)                                                   
         BZ    GETIVC22            NOT FOUND, NO NEED TO REPLY                  
                                                                                
         GOTOR MNXTEL,ELEM                                                      
                                                                                
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BNE   GETIVC07                                                         
         MVI   BYTE3,PNVCKHDQ      DO # OF INV HEADER COMMENT FILTER            
         CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENT?                              
         BNE   *+8                                                              
         MVI   BYTE3,PNVDKIDQ      DO # OF INV DETAIL COMMENT FILTER            
         GOTOR IVCM#FLT                                                         
                                                                                
GETIVC07 ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVCOMLQ,RE),0(RE)                                             
         BZ    GETIVC22            NOT FOUND, NO NEED TO REPLY                  
                                                                                
         USING PNVCOMD,RE                                                       
                                                                                
         CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENT?                              
         BNE   GETIVC08                                                         
         CLI   PNVCKCDE,PNVCKDTQ   INVOICE DETAIL COMMENT CODE?                 
         BE    GETIVC10                                                         
         B     GETIVC22            DETAIL COMMENT ELEM NOT FOUND                
                                                                                
GETIVC08 CLI   PNVCKCDE,PNVCKHDQ   INVOICE HEADER COMMENT CODE?                 
         BE    GETIVC10                                                         
         B     GETIVC22            HEADER COMMENT ELEM NOT FOUND                
                                                                                
GETIVC10 CLI   PNVCKTYP,PNVCKCMQ   COMMENT IS GENERAL COMMENTS TYPE?            
         BE    GETIVC12                                                         
         CLI   PNVCKTYP,PNVCKPBQ   COMMENT IS BUYER/PAYER DIALOGUE?             
         BE    GETIVC12                                                         
                                                                                
         B     GETIVC04            TRY NEXT COMMENT ELEMENT                     
                                                                                
GETIVC12 CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENT?                              
         BNE   GETIVC14                                                         
         CLC   PNVCKDSQ,INVELMKY+2 DETAIL SEQ NUMBER MATCH?                     
         BNE   GETIVC22            NO, DONE WITH THIS COMMENT                   
                                                                                
GETIVC14 TM    PNVCSTAT,PNVCDLQ    DELETED?                                     
         BZ    GETIVC16                                                         
         B     GETIVC04                                                         
                                                                                
* * * *  CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY?                     
* * * *  BNE   GETIVC04                                                         
* * * *  MVI   INVCGSTA,GST_DELQ   COMMENT IS DELETED                           
* * * *  GOTOR MNXTEL,ELEM         POINT TO NEXT POSSIBLE COMMENT ELEM          
* * * *  B     GETIVC24            REPLY DELETED STATUS                         
                                                                                
GETIVC16 ST    RE,FULL1                                                         
         OC    PNVCPID,PNVCPID                                                  
         BNZ   *+14                                                             
         MVC   INVPIDNM(11),=C'** BLANK **'                                     
         B     GETIVC17                                                         
                                                                                
         GOTOR GETWHO,DMCB,PNVCPID,(L'INVPIDNM,INVPIDNM)                        
                                                                                
GETIVC17 L     RE,FULL1                                                         
         MVC   INVCOMDT,PNVCDATE                                                
                                                                                
*ETIVC18 MVC   FULL2(L'PNVCKCGP),PNVCKCGP                                       
GETIVC18 MVC   FULL2(L'PNVCKTYP+L'PNVCKCGP),PNVCKTYP                            
         SR    RF,RF                                                            
         IC    RF,PNVCKLEN                                                      
         SHI   RF,PNVCOMLQ         MINUS COMMENT ELEM OVERHEAD                  
         CHI   RF,0                ANY COMMENT TO BE REPLIED?                   
         BL    GETIVC20            NO, TRY NEXT COMMENT ELEM                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PNVCCOM                                                  
         LA    R4,L'IVCOMMNT(R4)   POINT TO NEXT ENTRY IN ARRAY                 
                                                                                
GETIVC20 GOTOR MNXTEL,ELEM                                                      
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVCOMLQ,RE),0(RE)                                             
         BZ    GETIVC24            NOT FOUND, REPLY CURRENT COMMENT             
                                                                                
         CLI   PNVCKCDE,PNVCKHDQ   INVOICE HEADER COMMENT CODE?                 
         BE    *+12                                                             
         CLI   PNVCKCDE,PNVCKDTQ   INVOICE DETAIL COMMENT CODE?                 
         BNE   GETIVC24            DO NEXT COMMENT                              
                                                                                
         CLI   PNVCKTYP,PNVCKCMQ   COMMENT IS GENERAL COMMENTS TYPE?            
         BE    *+12                                                             
         CLI   PNVCKTYP,PNVCKPBQ   COMMENT IS BUYER/PAYER DIALOGUE?             
         BNE   GETIVC24            DO NEXT COMMENT                              
                                                                                
* * * *  CLC   PNVCKCGP,FULL2      STILL IN SAME GROUP?                         
* * * *  BE    GETIVC18            YES, CONTINUE COMMENT ARRAY                  
         CLC   PNVCKTYP(2),FULL2   STILL IN SAME TYPE AND GROUP?                
         BE    GETIVC18            YES, CONTINUE COMMENT ARRAY                  
         B     GETIVC24            DO NEXT COMMENT                              
                                                                                
GETIVC22 J     NOMORE                                                           
                                                                                
GETIVC24 J     EXITY               EXIT TO SEND INVOICE COMMENT                 
         DROP  RB,R3,R5,RE                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET INVOICE DETAIL VALUES                                           *         
***********************************************************************         
                                                                                
GETIDV   J     *+12                                                             
         DC    C'*GETIDV*'                                                      
         LR    RB,RF                                                            
         USING GETIDV,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         USING INVVALSD,R5                                                      
                                                                                
         GOTOR XC_IVALS,1          PREPARE TO REPLY DETAIL VALUES               
                                                                                
         CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   GETIDV14                                                         
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY DATES MODE?                      
         JE    *+12                                                             
         CLI   INVDLMOD,IVDLMD_C   Download by Creation date mode?              
         JNE   GETIDV02                                                         
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BNE   *+12                                                             
         MVI   INSDLSW,YESQ        SET TO REPLY INSERTION DATA                  
         B     GETIDV02                                                         
         GOTOR XC_IVALS,9          CLEAR ALL, EXCEPT HEADER                     
         B     GETIDV14                                                         
                                                                                
GETIDV02 CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GETIDV12                                                         
         MVI   INVCMSW,PNVCKDTQ    SW SET TO DO DETAIL COMMENT LATER            
         CLI   MINMKEY+L'PNVKAGY+L'PNVKMED,PNVKRCDQ                             
         BE    *+6                                                              
         DC    H'0'                MINIO MASTER KEY IS SET AT HDR LV            
                                                                                
         LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVDTLD,RE                                                       
         MVI   PNVDKCDE,PNVDKIDQ   INVOICE DETAIL ELEM ID                       
         MVI   1(RE),1             MATCH ONLY FIRST BYTE (FOR MGETEL)           
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    GETIDV14            NOT FOUND, NO NEED TO REPLY                  
                                                                                
                                                                                
GETIDV04 MVC   INVELMKY,0(RE)      SAVE ELEM KEY FOR NEXT ROUND                 
         CLI   PNVDKTYP,PNVDKDSQ   DETAIL DESCRIPTION ELEMENT?                  
         BNE   GETIDV06            NO, TRY NEXT ELEM                            
         B     GETIDV10            REPLY INVOICE DETAIL                         
                                                                                
* * * *  TM    PNVDSTAT,PNVDDLQ    DETAIL ELEM IS DELETED?                      
* * * *  BZ    GETIDV10            NO, DO FURTHER CHECKINGS                     
* * * *  CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY?                     
* * * *  BE    GETIDV10            NEED TO REPLY DELETED STATUS                 
                                                                                
GETIDV06 GOTOR MNXTEL,ELEM                                                      
                                                                                
GETIDV08 ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    GETIDV14            NOT FOUND, NO NEED TO REPLY                  
         B     GETIDV04                                                         
                                                                                
GETIDV10 GOTOR FIVD                FORMAT INVOICE DETAIL VALUES                 
         GOTOR PRCPIDDT            PROCESS PID AND DATE IN DETAIL ELEMS         
         GOTOR PRCIVSMY            Process invoice summary values               
         J     EXITY                                                            
                                                                                
GETIDV12 CLI   MINMKEY+L'PNVKAGY+L'PNVKMED,PNVKRCDQ                             
         BE    *+6                                                              
         DC    H'0'                MUST HAVE CORRECT MASTER MINIO KEY           
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(L'INVELMKY),INVELMKY                                        
         SR    RF,RF                                                            
         ICM   RF,3,INVELMKY+2                                                  
         AHI   RF,1                BUMP UP DETAIL SEQ NUMBER                    
         STCM  RF,3,ELEM+2                                                      
         MVI   ELEM+1,L'INVELMKY                                                
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    GETIDV14            NOT FOUND, NO NEED TO REPLY                  
         B     GETIDV08                                                         
                                                                                
GETIDV14 J     NOMORE                                                           
                                                                                
         DROP  RB,R3,R5,RE                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Set invoice summary reply record                                    *         
***********************************************************************         
*                                                                               
NXISMY   J     *+12                                                             
         DC    C'*NXISMY*'                                                      
         LR    RB,RF                                                            
         USING NXISMY,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         USING INVVALSD,R5                                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST   First time?                                  
         JNE   NXISM12                                                          
*                                                                               
         J     EXITY                                                            
*                                                                               
NXISM12  J     NOMORE                                                           
*                                                                               
         DROP  RB,R3,R5                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET INSERTION DATA FROM INVOICE CRITERIAS                           *         
***********************************************************************         
                                                                                
NXTIVB   J     *+12                                                             
         DC    C'*NXTIVB*'                                                      
         LR    RB,RF                                                            
         USING NXTIVB,RB                                                        
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         USING INVVALSD,R5                                                      
                                                                                
         CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   NXTIVB80                                                         
         CLI   INVDLMOD,IVDLMD_#   DOWNLOAD BY INVOICE NUMBER?                  
         BE    NXTIVB05                                                         
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY(S)?                  
         BE    NXTIVB05                                                         
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BNE   NXTIVB90                                                         
                                                                                
NXTIVB05 CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTIVB10                                                         
         XC    IBUREC(IBUL),IBUREC                                              
         MVI   TSACTN,TSARDH       SET TO READ HIGH                             
                                                                                
NXTIVB10 GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BNZ   NXTIVB90                                                         
         MVI   TSACTN,TSANXT       SET TO GET NEXT RECORD                       
         OC    IBUINSKY,IBUINSKY   ANYTHING IN INSERTION KEY?                   
         BZ    NXTIVB10                                                         
                                                                                
         GOTOR GETBUY,IBUINSKY     READ BUY RECORD USING SERIAL NUMBER          
         BNE   NXTIVB70            TRY NEXT INSERTION IN BUFFER                 
         L     R2,IOADDR           R2=A(BUY RECORD)                             
         GOTOR (#GETCLT,AGETCLT),DMCB,PBUYKAGY,PBUYKMED,PBUYKCLT                
         BNE   NXTIVB10            FAILED CLT SECURITY, GET NEXT ONE            
         GOTOR GETCPR              GET CLIENT PROFILES                          
         GOTOR FBUY,0              FORMAT BUY RECORD FOR DOWNLOAD               
                                                                                
         L     R4,LP_BLKS+((B#INSARY-1)*L'LP_BLKS)                              
         USING INSVARY,R4                                                       
         GOTOR XC_BARYD,BINVARYQ   INIT BUY INVOICE DATA ARRAY                  
                                                                                
         MVC   TEMP2(IBUKEYL),IBUKEY                                            
         LA    R1,PBUYFRST         POINT TO FIRST BUY ELEMENT                   
         USING PBNVELMD,R1                                                      
         SR    R0,R0                                                            
         SR    RE,RE               RE=NUMBER OF INVOICE ELEMENTS                
         LA    RF,BINVTAB                                                       
         USING BINVTAB,RF                                                       
NXTIVB20 CLI   PBNVELM,0           TEST END OF RECORD                           
         BE    NXTIVB60                                                         
         CLI   PBNVELM,PBNVELQ     TEST INVOICE ELEMENT                         
         BNE   NXTIVB40                                                         
                                                                                
         ST    RE,DUB2                                                          
         ST    RF,DUB2+4                                                        
         MVC   WORK2(L'BUYSMED),BUYSMED                                         
         MVC   WORK2+L'BUYSMED(L'PBNVSER#),PBNVSER#                             
         BRAS  RE,TSTINVS#         INVOICE SERIAL # IS GOOD?                    
         L     RE,DUB2                                                          
         L     RF,DUB2+4                                                        
         BNE   NXTIVB40                                                         
                                                                                
         AHI   RE,1                                                             
         CHI   RE,BINVTABM                                                      
         BNH   *+6                                                              
         DC    H'0'                TOO MANY INVOICE ELEMENTS                    
         MVC   BINVKEY(L'BUYSMED),BUYSMED                                       
         UNPK  WORK(2*L'PBNVSER#+1),PBNVSER#(L'PBNVSER#+1)                      
         MVC   BINVKEY+1(L'BINVKEY-1),WORK                                      
         MVC   BINVSEQ#,PBNVDSQN                                                
         MVC   BINVVIV#,PBNVINV#                                                
         AHI   RF,BINVTABL                                                      
                                                                                
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BNE   NXTIVB40                                                         
         STCM  RE,15,DUB+0         SAVE PREVIOUS COUNTER                        
         STCM  RF,15,DUB+4         SAVE PREVIOUS TABLE POINTER                  
         STCM  R1,15,FULL2         SAVE ELEM POINTER                            
         XC    IBUREC(IBUL),IBUREC                                              
         MVC   IBUMEDCD,BUYSMED                                                 
         MVC   IBUINVS#,PBNVSER#                                                
                                                                                
         MVC   WORK(IBUL),IBUREC                                                
         MVI   TSACTN,TSARDH       SET TO READ HIGH                             
         GOTOR BUFFER                                                           
         ICM   R1,15,FULL2         RESTORE ELEM POINTER                         
         CLC   IBUREC(L'IBUMEDCD+L'IBUINVS#),WORK                               
         BNE   *+14                                                             
         CLC   IBUIVDS#,EFFS       DETAIL ALREADY DOWNLOADED?                   
         BE    NXTIVB30                                                         
                                                                                
         XC    IBUREC(IBUL),IBUREC                                              
         MVC   IBUMEDCD,BUYSMED                                                 
         MVC   IBUINVS#,PBNVSER#                                                
         MVC   IBUIVDS#,PBNVDSQN                                                
         OC    IBUINVS#,IBUINVS#                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OC    IBUIVDS#,IBUIVDS#                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   IBUIVDS#,EFFS                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSAADD       ADD INVOICE DATA TO BUFFER                   
         GOTOR BUFFER                                                           
         CLI   TSERRS,0            TEST FOR ERRORS                              
         BE    *+14                                                             
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         BE    *+6                                                              
         DC    H'0'                OTHER ERRORS ARE NO GOOD                     
NXTIVB30 ICM   RE,15,DUB+0         RESTORE PREVIOUS COUNTER                     
         ICM   RF,15,DUB+4         RESTORE PREVIOUS TABLE POINTER               
         ICM   R1,15,FULL2         RESTORE ELEM POINTER                         
                                                                                
NXTIVB40 IC    R0,PBNVLEN                                                       
         AR    R1,R0               BUMP TO NEXT ELEMENT                         
         B     NXTIVB20                                                         
                                                                                
NXTIVB60 STCM  RE,3,BINVTAB#       SET NUMBER OF ARRAY ENTRIES                  
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         JNE   EXITY                                                            
         MVC   IBUKEY(IBUKEYL),TEMP2                                            
         MVI   TSACTN,TSARDH       SET TO READ HIGH                             
         GOTOR BUFFER                                                           
         CLC   IBUKEY(IBUKEYL),TEMP2                                            
         BE    *+6                                                              
         DC    H'0'                CANNOT RESTORE SEQUENCE                      
         MVI   TSACTN,TSANXT       SET TO GET NEXT RECORD                       
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
                                                                                
NXTIVB70 GOTOR XC_IVALS,3          CLEAR INSERTION VALUES                       
         J     EXITY                                                            
                                                                                
NXTIVB80 GOTOR XC_IVALS,3          CLEAR INSERTION VALUES                       
                                                                                
NXTIVB90 J     NOMORE              NO MORE RECORDS TO GO                        
         DROP  RB,R5,R4,R3,R1,RF                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET PROFILE VALUES                                                  *         
***********************************************************************         
                                                                                
GETPRF   J     *+12                                                             
         DC    C'*GETPRF*'                                                      
         LR    RB,RF                                                            
         USING GETPRF,RB                                                        
                                                                                
         L     RF,ALP                                                           
         LA    R0,PROFILEV                                                      
         ST    R0,LP_ADATA-LP_D(RF)                                             
         CLI   LP_RMODE-LP_D(RF),LP_RFRST                                       
         JNE   NOMORE                                                           
                                                                                
         BRAS  RE,GETPRPRF         GET PRINT PROFILE VALUES                     
                                                                                
         J     EXITY               EXIT TO SEND PROFILE VALUES                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET BUY CUSTOM COLUMN ELEMENT(S)                                    *         
***********************************************************************         
                                                                                
GETBCC   J     *+12                                                             
         DC    C'*GETBCC*'                                                      
         LR    RB,RF                                                            
         USING GETBCC,RB                                                        
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#BCCBLK-1)*L'LP_BLKS)                        
                                                                                
         L     R4,LP_BLKS+((B#BCCBLK-1)*L'LP_BLKS)                              
         USING BCCVARY,R4                                                       
                                                                                
         XC    BUYCCSQ#,BUYCCSQ#   INIT DATA TO BE REPLIED                      
         XC    BUYCCFDA,BUYCCFDA                                                
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GTBCC14                                                          
         L     R5,AIO2             BUY RECORD IS STILL IN IO2                   
         AHI   R5,PBUYFRST-PBUYRECD                                             
         CLI   0(R5),X'20'                                                      
         BNE   GTBCC18             NO BUY RECORD DETECTED                       
                                                                                
         MVC   TEMP(L'IOKEY),IOKEY                                              
                                                                                
GTBCC02  CLI   0(R5),0             END OF BUY RECORD?                           
         BE    GTBCC16             YES, DONE WITH BUY CUSTOM COLUMN             
         CLI   0(R5),BYCCIDQ                                                    
         BE    GTBCC04                                                          
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0               BUMP TO NEXT BUY ELEM                        
         B     GTBCC02                                                          
                                                                                
         USING BYCCELD,R5                                                       
                                                                                
GTBCC04  MVC   BUYCCSQ#,BYCCSQN    CUSTOM COLUMN SEQ # TO BE REPLIED            
                                                                                
         LA    R7,IOKEY                                                         
         USING PCOLPKEY,R7                                                      
         XC    PCOLPKEY,PCOLPKEY                                                
         MVC   PCOLPAGY,AGY                                                     
         MVI   PCOLPMED,C'A'       ALWAYS "A" (FOR CUSTOM COLUMN)               
         MVI   PCOLPRCD,X'D1'                                                   
         MVC   PCOLPSQN,BYCCSQN                                                 
         XC    PCOLPSQN,EFFS                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    GTBCC05                                                          
                                                                                
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING GCOLPKEY,RE                                                      
         MVC   GCOLPRID,=AL3(GCOLPRIQ)                                          
         MVI   GCOLPMED,C'A'                                                    
         MVI   GCOLPRCD,GCOLPRCQ                                                
         MVC   GCOLPSQN,BYCCSQN                                                 
         XC    GCOLPSQN,EFFS                                                    
         DROP  RE                                                               
         MVC   ELEM2,ELEM                                                       
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',GENDIR,ELEM,ELEM                        
         CLC   ELEM(L'GCOLPKEY),ELEM2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,DMCB,=C'GETREC',GENFIL,ELEM+36,AIO1,ELEM2               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R7,AIO1                                                          
         LA    R7,(GCOLFRST-GCOLKEY)(R7)                                        
         B     GTBCC05M                                                         
                                                                                
GTBCC05  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+6                                                              
         DC    H'0'                I/O ERROR                                    
                                                                                
         L     R7,IOADDR                                                        
         AHI   R7,PCOLELEM-PCOLREC                                              
         USING PCOLELEM,R7                                                      
GTBCC05M CLI   PCOLELEM,X'61'      ELEM PRESENT?                                
         BE    *+6                                                              
         DC    H'0'                INVALID CC RECORD                            
                                                                                
         CLI   PCOLTYP,C'T'        TEXT?                                        
         BNE   GTBCC06                                                          
         SR    RF,RF                                                            
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL         MINUS ELEM OVERHEAD                          
         BCTR  RF,0                FOR EX                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUYCCFDA(0),BYCCDATA                                             
         B     GTBCC12                                                          
                                                                                
GTBCC06  CLI   PCOLTYP,C'D'        DATE?                                        
         BE    *+12                                                             
         CLI   PCOLTYP,C'P'        PERIOD (DATE RANGE)?                         
         BNE   GTBCC08                                                          
         MVC   DUB(3),BYCCDATA                                                  
         GOTOR GETBCCDT            GET DATE (YYYY-MM-DD IN TEMP2)               
         MVC   BUYCCFDA(10),WORK   REPLY DATE IN MM-DD-YYYY FORMAT              
         SR    RF,RF                                                            
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL         MINUS ELEM OVERHEAD                          
         CHI   RF,3                                                             
         BE    GTBCC12             ONLY ONE DATE                                
         CHI   RF,6                                                             
         BE    *+6                                                              
         DC    H'0'                BAD DATE OR PERIOD CC ELEM!                  
         MVC   DUB(3),BYCCDATA+3                                                
         GOTOR GETBCCDT            GET DATE (YYYY-MM-DD IN TEMP2)               
         MVI   BUYCCFDA+10,C'-'    REPLY 2ND DATE AFTER DASH                    
         MVC   BUYCCFDA+11(10),WORK                                             
         B     GTBCC12                                                          
                                                                                
GTBCC08  CLI   PCOLTYP,C'N'        NUMERIC?                                     
         BE    *+8                                                              
         CLI   PCOLTYP,C'$'        DOLLAR?                                      
         BE    *+12                                                             
         CLI   PCOLTYP,C'%'        PERCENT?                                     
         BNE   GTBCC10                                                          
                                                                                
         XC    DUB,DUB             DUMMY CURRENY DEFINITION FLD                 
         MVC   DUB+3(1),PCOLDECS                                                
         LA    RF,DUB                                                           
         CURED (P8,BYCCDATA),(20,BUYCCFDA),(RF),ALIGN=LEFT,FLOAT=-              
         CLI   BUYCCFDA,C'.'                                                    
         BNE   *+14                                                             
         MVC   BUYCCFDA+1(20),BUYCCFDA                                          
         MVI   BUYCCFDA+0,C'0'                                                  
                                                                                
         CLI   PCOLDECS,0                                                       
         BE    GTBCC12                                                          
         CLI   PCOLDECS,5                                                       
         BE    GTBCC12                                                          
         LR    RF,R0               # OF SIGNIFICANT CHARACTERS                  
         BCTR  RF,0                                                             
         LA    RE,BUYCCFDA                                                      
         AR    RE,RF               POINT TO LAST CHARACTER                      
GTBCC09  CLI   0(RE),C'0'                                                       
         BNE   GTBCC12                                                          
         MVI   0(RE),C' '                                                       
         SHI   RE,1                                                             
         CLI   0(RE),C'.'                                                       
         BNE   GTBCC09                                                          
         MVI   0(RE),C' '          BLANK OUT DECIMAL POINT                      
                                                                                
         B     GTBCC12                                                          
                                                                                
GTBCC10  DC    H'0'                INVALID TYPE                                 
                                                                                
GTBCC12  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0               BUMP TO NEXT BUY ELEM                        
         ST    R5,FULL1            FOR NEXT ROUND                               
         B     GTBCC20                                                          
                                                                                
GTBCC14  L     R5,FULL1            PICK UP WHERE PREVIOUSLY LEFT                
         B     GTBCC02                                                          
                                                                                
GTBCC16  MVC   IOKEY,TEMP          RESTORE ORIGINAL KEY                         
GTBCC18  J     NOMORE                                                           
                                                                                
GTBCC20  J     EXITY               EXIT TO SEND FORMATTED DATA                  
                                                                                
GETBCCDT LR    R0,RE                                                            
         GOTOR VDATCON,DMCB,(3,DUB),(23,TEMP2)                                  
         MVC   WORK+0(2),TEMP2+5                                                
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),TEMP2+8                                                
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(4),TEMP2+0                                                
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RB,R7,R5,R4,R3                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* Get Vendor Invoice Number from Pay (Clearance Record)               *         
***********************************************************************         
                                                                                
GETPIV   J     *+12                                                             
         DC    C'*GETPIV*'                                                      
         LR    RB,RF                                                            
         USING GETPIV,RB                                                        
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#PIVBLK-1)*L'LP_BLKS)                        
                                                                                
         L     R4,LP_BLKS+((B#PIVBLK-1)*L'LP_BLKS)                              
         USING IPAD,R4                                                          
                                                                                
         XC    IPAD(IPAL),IPAD     Init data to be replied                      
                                                                                
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   GTPIV14                                                          
         L     R5,AIO2             Buy record is still in IO2                   
         AHI   R5,PBUYFRST-PBUYRECD                                             
         CLI   0(R5),X'20'                                                      
         JNE   GTPIV18             No Buy record detected                       
                                                                                
GTPIV02  CLI   0(R5),0             End of Buy record?                           
         JE    GTPIV18                                                          
         CLI   0(R5),PPAYELQ       Pay elem?                                    
         JE    GTPIV06                                                          
GTPIV04  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0               Bump to next Buy elem                        
         J     GTPIV02                                                          
                                                                                
         USING PPAYELEM,R5                                                      
GTPIV06  OC    PPDDATE,PPDDATE     Have Pay date?                               
         JZ    GTPIV04                                                          
                                                                                
         L     R2,AIO2             Buy record is still in IO2                   
         LR    R3,R5               Address of current pay elem                  
         BRAS  RE,INVCLSTA         Get info from Clearance record               
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0               Bump to next Buy elem                        
         ST    R5,FULL2            For next round                               
         J     GTPIV20                                                          
                                                                                
GTPIV14  L     R5,FULL2            Pick up where previously left off            
         J     GTPIV02                                                          
                                                                                
GTPIV18  J     NOMORE                                                           
                                                                                
GTPIV20  J     EXITY               Exit to send formatted data                  
                                                                                
         DROP  RB,R5,R4,R3                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET SPECIAL REP                                                     *         
***********************************************************************         
                                                                                
GETSREP  J     *+12                                                             
         DC    C'*GETSRE*'                                                      
         LR    RB,RF                                                            
         USING GETSREP,RB                                                       
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                        
                                                                                
         L     R4,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R4                                                      
                                                                                
         XC    SREP_MED,SREP_MED   INIT DATA TO BE REPLIED                      
         XC    SREP_REP,SREP_REP                                                
         XC    SREP_NAM,SREP_NAM                                                
         XC    SREP_ATT,SREP_ATT                                                
         XC    SREP_AD1,SREP_AD1                                                
         XC    SREP_AD2,SREP_AD2                                                
         XC    SREP_AD3,SREP_AD3                                                
         XC    SREP_STC,SREP_STC                                                
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GETSRE20                                                         
         MVC   TEMP(L'IOKEY),IOKEY                                              
         LHI   RF,1                                                             
         STH   RF,HALF2            COUNTER FOR # OF REP CODES                   
         LA    RE,SREPTAB                                                       
GETSRE10 LH    RF,HALF2                                                         
         CHI   RF,SREPTMXQ                                                      
         BNH   *+6                                                              
         DC    H'0'                EXCEEDED MAXIMUM ENTRIES IN TABLE            
         OC    0(SREPTABL,RE),0(RE)                                             
         BZ    GETSRE86                                                         
         ST    RE,FULL2                                                         
         B     GETSRE30                                                         
                                                                                
GETSRE20 L     RE,FULL2            ADDRESS OF REP CODE TO BE LOOKED UP          
         B     GETSRE10                                                         
                                                                                
GETSRE30 LA    R7,IOKEY                                                         
         USING PREPKEY,R7                                                       
         XC    PREPKEY,PREPKEY                                                  
         MVC   PREPKAGY,AGY                                                     
         MVC   PREPKMED,0(RE)      MEDIA CODE FROM TABLE ENTRY                  
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,1(RE)      REP CODE FROM TABLE ENTRY                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                SPECIAL REP RECORD NOT FOUND                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+6                                                              
         DC    H'0'                I/O ERROR                                    
                                                                                
         L     R7,IOADDR                                                        
         AHI   R7,PREPELEM-PREPREC                                              
         USING PREPELEM,R7                                                      
         CLI   PREPELEM,X'11'      ELEM PRESENT?                                
         BE    *+6                                                              
         DC    H'0'                INVALID REP RECORD                           
                                                                                
         MVC   SREP_MED,IOKEY+2    MEDIA CODE                                   
         MVC   SREP_REP,IOKEY+4    REP CODE                                     
         MVC   SREP_NAM,PREPNAME   NAME                                         
         MVC   SREP_ATT,PREPATTN   ATTENTION OF                                 
         MVC   SREP_AD1,PREPLIN1   ADDRESS LINE 1                               
         MVC   SREP_AD2,PREPLIN2   ADDRESS LINE 2                               
         MVC   SREP_AD3,PREPLIN3   ADDRESS LINE 3                               
         MVC   SREP_STC,PREPSTAC   STATE CODE                                   
                                                                                
         LH    RF,HALF2                                                         
         AHI   RF,1                                                             
         STH   RF,HALF2                                                         
         L     RE,FULL2                                                         
         LA    RE,SREPTABL(RE)     POINT TO NEXT TABLE ENTRY                    
         ST    RE,FULL2                                                         
         B     GETSRE90                                                         
                                                                                
GETSRE86 MVC   IOKEY,TEMP          RESTORE ORIGINAL KEY                         
GETSRE88 J     NOMORE                                                           
                                                                                
GETSRE90 J     EXITY               EXIT TO SEND FORMATTED DATA                  
                                                                                
         DROP  RB,R3,R4,R7                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* EIO SETUP RECORD REPLY                                              *         
***********************************************************************         
                                                                                
EIOSRPY  J     *+12                                                             
         DC    C'*EIOSRP*'                                                      
         LR    RB,RF                                                            
         USING EIOSRPY,RB                                                       
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#ESRRPY-1)*L'LP_BLKS)                        
                                                                                
         L     R4,LP_BLKS+((B#ESRRPY-1)*L'LP_BLKS)                              
         USING EIOSRD,R4                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   EIOSR20                                                          
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,ACLT           POINT TO WORK MAP POOL ENTRY                 
         JZ    NOMORE                                                           
         USING LW_D,RE                                                          
         LHI   R0,1                                                             
         LA    R1,LW_DATA1         SET N'ENTRIES TO 1                           
         CLI   LW_TYPE,LQ_TSINQ    TEST SINGLE ENTRY                            
         BE    *+12                                                             
         ICM   R0,3,LW_NUMN        NO - PICK UP N'ENTRIES FROM POOL             
         LA    R1,LW_DATA2                                                      
         DROP  RE                                                               
         STH   R0,NSER             SET N'ENTRIES TO BE PROCESSED                
         ST    R1,ANXTSER          SET A(NEXT ONE TO BE PROCESSED)              
         B     EIOSR40                                                          
                                                                                
EIOSR20  LH    R0,NSER             BUMP TO NEXT ENTRY IN POOL                   
         SHI   R0,1                                                             
         JZ    NOMORE                                                           
         STH   R0,NSER                                                          
         L     R1,ANXTSER                                                       
         AHI   R1,L'BUYSCLT                                                     
         ST    R1,ANXTSER                                                       
                                                                                
EIOSR40  ICM   RE,7,AMED                                                        
         MVC   EIO_MEDC,LW_DATA1-LW_D(RE)                                       
         MVC   EIO_CLTC,0(R1)                                                   
         MVI   EIO_UEIO,C'N'                                                    
         MVI   EIO_UESR,C'N'                                                    
                                                                                
         LA    R7,IOKEY                                                         
         USING SCHKKEY,R7                                                       
         XC    SCHKKEY,SCHKKEY                                                  
         MVC   SCHKAGY,AGY                                                      
         MVC   SCHKMED,EIO_MEDC                                                 
         MVI   SCHKRCD,SCHKRCDQ                                                 
         MVC   SCHKCLT,EIO_CLTC                                                 
         MVC   WORK2(L'SCHKKEY),IOKEY                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    EIOSR60                                                          
         MVC   IOKEY(L'SCHKKEY),WORK2                                           
         XC    SCHKCLT,SCHKCLT     TRY ALL CLIENT                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
                                                                                
EIOSR60  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+6                                                              
         DC    H'0'                I/O ERROR                                    
                                                                                
         L     R7,IOADDR                                                        
         AHI   R7,SCHFIRST-SCHREC                                               
         USING SCHHDRD,R7                                                       
         CLI   SCHHDRCD,SCHHDRQ    SETUP HEADER ELEMENT PRESENT?                
         BE    *+6                                                              
         DC    H'0'                INVALID EIO SETUP RECORD                     
                                                                                
         MVC   EIO_UEIO,SCHEIO                                                  
         CLI   EIO_UEIO,C' '                                                    
         BE    *+12                                                             
         CLI   EIO_UEIO,0                                                       
         BNE   *+8                                                              
         MVI   EIO_UEIO,C'N'       DEFAULT TO NO                                
                                                                                
         CLI   EIO_UEIO,C'N'                                                    
         BE    EIOSR70                                                          
         OC    SCHACTVD,SCHACTVD   HAVE ACTIVATION DATE?                        
         BZ    EIOSR70                                                          
         CLC   TODAYB,SCHACTVD     TODAY IS >= THAN ACTIVATION DATE?            
         BNL   EIOSR70                                                          
         MVI   EIO_UEIO,C'N'                                                    
                                                                                
EIOSR70  MVC   EIO_UESR,SCHESR                                                  
         CLI   EIO_UESR,C' '                                                    
         BE    *+12                                                             
         CLI   EIO_UESR,0                                                       
         BNE   *+8                                                              
         MVI   EIO_UESR,C'N'       DEFAULT TO NO                                
                                                                                
         CLI   EIO_UESR,C'N'                                                    
         BE    EIOSR80                                                          
         OC    SCHACTVD,SCHSRACT   HAVE ACTIVATION DATE?                        
         BZ    EIOSR80                                                          
         CLC   TODAYB,SCHSRACT     TODAY IS >= THAN ACTIVATION DATE?            
         BNL   EIOSR80                                                          
         MVI   EIO_UESR,C'N'                                                    
                                                                                
EIOSR80  MVC   EIO_EIOE,SCHEIOE    EIO BY ESTIMATE - Y/N/P                      
         CLI   EIO_EIOE,C'P'                                                    
         JE    *+12                                                             
         CLI   EIO_EIOE,C'Y'                                                    
         JNE   EIOSR84                                                          
         MVC   EIO_EIOD,SCHEACD    EIO BY ESTIMATE DATE                         
         CLI   EIO_EIOD+2,0                                                     
         BNE   *+8                                                              
         MVI   EIO_EIOD+2,1        DEFAULT TO 1ST DAY OF MONTH                  
                                                                                
EIOSR84  MVC   EIO_ESRE,SCHESRE    ESR BY ESTIMATE - Y/N/P                      
         CLI   EIO_ESRE,C'P'                                                    
         JE    *+12                                                             
         CLI   EIO_ESRE,C'Y'                                                    
         JNE   EIOSR86                                                          
         MVC   EIO_ESRD,SCHESRD    ESR BY ESTIMATE DATE                         
         CLI   EIO_ESRD+2,0                                                     
         BNE   *+8                                                              
         MVI   EIO_ESRD+2,1        DEFAULT TO 1ST DAY OF MONTH                  
                                                                                
EIOSR86  DS    0H                                                               
                                                                                
         J     EXITY                                                            
         DROP  RB,R3,R4,R7                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET AGENCY RECORD                                                   *         
***********************************************************************         
                                                                                
GETAGYR  J     *+12                                                             
         DC    C'*GETAGY*'                                                      
         LR    RB,RF                                                            
         USING GETAGYR,RB                                                       
                                                                                
         CLI   DL_TYPE,DL_PRBQ                                                  
         JE    NOMORE              DON'T EXECUTE FOR PROBE DOWNLOAD             
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#AGYBLK-1)*L'LP_BLKS)                        
                                                                                
         L     R4,LP_BLKS+((B#AGYBLK-1)*L'LP_BLKS)                              
         USING AGYRARYD,R4                                                      
                                                                                
         XC    AGY_MEDC,AGY_MEDC   INIT DATA TO BE REPLIED                      
         XC    AGY_NAME,AGY_NAME                                                
         XC    AGY_ADDR,AGY_ADDR                                                
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GETAGY20                                                         
         MVC   TEMP(L'IOKEY),IOKEY                                              
         B     GETAGY30                                                         
                                                                                
GETAGY20 B     GETAGY90            ONLY ONE REPLY AT THIS TIME                  
                                                                                
GETAGY30 LA    R7,IOKEY                                                         
         USING PAGYKEY,R7                                                       
         XC    PAGYKEY,PAGYKEY                                                  
         MVC   PAGYKAGY,AGY                                                     
         SR    RE,RE                                                            
         ICM   RE,7,AMED                                                        
         MVC   PAGYKMED,LW_DATA1-LW_D(RE)                                       
         MVI   PAGYKRCD,X'01'                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                AGENCY RECORD NOT FOUND                      
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+6                                                              
         DC    H'0'                I/O ERROR                                    
                                                                                
         L     R7,IOADDR                                                        
         AHI   R7,PAGYELEM-PAGYREC                                              
         USING PAGYELEM,R7                                                      
         CLI   PAGYELEM,X'01'      ELEM PRESENT?                                
         BE    *+6                                                              
         DC    H'0'                INVALID AGENCY RECORD                        
                                                                                
         MVC   AGY_MEDC,IOKEY+2    MEDIA CODE                                   
         MVC   AGY_NAME,PAGYNAME                                                
         MVC   AGY_ADDR,PAGYADDR                                                
                                                                                
         MVC   IOKEY,TEMP          RESTORE ORIGINAL KEY                         
         J     EXITY               EXIT TO SEND FORMATTED DATA                  
                                                                                
GETAGY90 J     NOMORE                                                           
                                                                                
         DROP  RB,R3,R4,R7                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* MESSAGE REPLY RECORD                                                *         
***********************************************************************         
                                                                                
MSG_RPY  J     *+12                                                             
         DC    C'*MSG_RP*'                                                      
         LR    RB,RF                                                            
         USING MSG_RPY,RB                                                       
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         LA    R4,TEMP2                                                         
         STCM  R4,15,LP_ADATA                                                   
         USING MSGREPYD,R4                                                      
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   MSG_RP20                                                         
         XC    FULL2,FULL2                                                      
         MVI   FULL2,4             PRINT SYSTEM                                 
         L     RF,FULL2                                                         
         ICM   RF,3,SVMSGNUM                                                    
         GOTOR GETMSG,DMCB,(RF),TEMP                                            
         MVC   MSG_MAPC,SVMSGMAP                                                
         MVC   MSG_TEXT,TEMP                                                    
         B     MSG_RP30                                                         
                                                                                
MSG_RP20 B     MSG_RP90            ONLY ONE REPLY AT THIS TIME                  
                                                                                
MSG_RP30 J     EXITY               EXIT TO SEND FORMATTED DATA                  
                                                                                
MSG_RP90 J     NOMORE                                                           
                                                                                
         DROP  RB,R3,R4                                                         
         EJECT                                                                  
                                                                                
                                                                                
***********************************************************************         
* RUN-NOT-ORDERED INVOICE RECORD                                      *         
***********************************************************************         
                                                                                
NXTRNO   J     *+12                                                             
         DC    C'*NXTRNO*'                                                      
         LR    RB,RF                                                            
         USING NXTRNO,RB                                                        
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         GOTOR XC_IVALS,0          CLEAR ALL REPLY VALUES                       
         USING INVVALSD,R5                                                      
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   N_RNO20                                                          
                                                                                
         ICM   R1,7,AMED                                                        
         MVC   BYTE2,LW_DATA1-LW_D(R1)                                          
         GOTOR (#VALMED,AVALMED),DMCB,BYTE2,L'PAGYKMED,BYTE2                    
                                                                                
         ICM   R1,7,ACLT                                                        
         MVC   TEMP2(L'PCLTKCLT),LW_DATA1-LW_D(R1)                              
         OC    TEMP2(L'PCLTKCLT),SPACES                                         
         CLC   TEMP2(L'PCLTKCLT),=C'***'                                        
         BE    N_RNO10                                                          
         GOTOR (#VALCLT,AVALCLT),DMCB,TEMP2,L'PCLTKCLT,TEMP2                    
         JNE   NXTIV#90                                                         
N_RNO10  OC    BUYPUB,SPACES                                                    
         CLC   BUYPUB,SPACES                                                    
         BE    N_RNO96             NO PUB, CANNOT LOOK INVOICE RECS             
         LHI   RF,L'BUYPUB                                                      
         LA    RE,BUYPUB+L'BUYPUB-1                                             
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         GOTOR VPUBVAL,DMCB,((RF),BUYPUB),DUB                                   
         CLI   0(R1),FF                                                         
         BE    N_RNO96             INVALID PUB, NO NEED TO CONTINUE             
                                                                                
         XC    IOKEY,IOKEY         SET UP INVOICE PASSIVE KEY                   
I        USING PNV1KEY,IOKEY                                                    
         MVC   I.PNV1AGY,AGY                                                    
         MVC   I.PNV1MED,BYTE2                                                  
         MVI   I.PNV1RCD,PNV1RCDQ                                               
         MVC   I.PNV1CLT,TEMP2                                                  
         MVC   I.PNV1PBCD,DUB                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BNE   N_RNO96                                                          
N_RNO16  CLC   I.PNV1KEY(PNV1SER#-PNV1KEY),IOKEYSAV                             
         BNE   N_RNO96                                                          
         MVC   INVWKKEY,IOKEY      SAVE IT FOR NEXT ROUND                       
         B     N_RNO30                                                          
                                                                                
N_RNO20  MVC   IOKEY(L'INVWKKEY),INVWKKEY                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         BE    N_RNO16                                                          
         DC    H'0'                                                             
                                                                                
N_RNO30  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         L     RE,AIO1                                                          
         MVC   MINMAST,0(RE)       SAVE MINIO MASTER KEY                        
         MVC   MINMKEY(L'PNVKEY),MINMAST                                        
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   N_RNO40             ONLY NEED TO INIT ONCE                       
         GOTOR MININIT             INIT MINIO                                   
         MVC   MINMKEY(L'PNVKEY),MINMAST                                        
         GOTOR VMINIO,MINPARMS,('MINOPN',MINBLKD)                               
         CLI   MINERR,0                                                         
         BE    N_RNO40                                                          
         DC    H'0'                                                             
                                                                                
N_RNO40  LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVHDRD,RE                                                       
         MVI   PNVHKCDE,PNVHKIDQ   INVOICE HEADER ELEM ID                       
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         BNZ   *+6                                                              
         DC    H'0'                THERE MUST BE AN INVOICE HDR ELEM!           
         TM    PNVHSTUS,PNVHDLQ    DELETED?                                     
         BNZ   N_RNO20             DELETED, PROC NEXT ONE                       
         GOTOR FIVH                FORMAT INVOICE HEADER VALUES                 
                                                                                
N_RNO90  J     EXITY               EXIT TO SEND FORMATTED DATA                  
                                                                                
N_RNO96  GOTOR XC_IVALS,0          CLEAR ALL REPLY VALUES                       
                                                                                
N_RNO_X  J     NOMORE                                                           
                                                                                
         DROP  RB,R3,R5,I,RE                                                    
         EJECT                                                                  
                                                                                
***********************************************************************         
* RUN-NOT-ORDERED INVOICE LINE ITEMS                                  *         
***********************************************************************         
                                                                                
GETRNO   J     *+12                                                             
         DC    C'*GETRNO*'                                                      
         LR    RB,RF                                                            
         USING GETRNO,RB                                                        
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         ST    R5,LP_ADATA                                                      
         GOTOR XC_IVALS,1          CLEAR DETAIL REPLY VALUES                    
         USING INVVALSD,R5                                                      
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   G_RNO20                                                          
         CLI   MINMKEY+L'PNVKAGY+L'PNVKMED,PNVKRCDQ                             
         BE    *+6                                                              
         DC    H'0'                MINIO MASTER KEY IS SET AT HDR LV            
         LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVDTLD,RE                                                       
         MVI   PNVDKCDE,PNVDKIDQ   INVOICE DETAIL ELEM ID                       
         MVI   1(RE),1             MATCH ONLY FIRST BYTE (FOR MGETEL)           
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    G_RNO96             NOT FOUND, NO NEED TO REPLY                  
         B     G_RNO24                                                          
                                                                                
G_RNO20  CLI   MINMKEY+L'PNVKAGY+L'PNVKMED,PNVKRCDQ                             
         BE    *+6                                                              
         DC    H'0'                MUST HAVE CORRECT MASTER MINIO KEY           
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(L'INVELMKY),INVELMKY                                        
         SR    RF,RF                                                            
         ICM   RF,3,INVELMKY+2                                                  
         AHI   RF,1                BUMP UP DETAIL SEQ NUMBER                    
         STCM  RF,3,ELEM+2                                                      
         MVI   ELEM+1,L'INVELMKY                                                
         GOTOR MGETEL,ELEM         GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    G_RNO96             NOT FOUND, NO NEED TO REPLY                  
                                                                                
G_RNO24  MVC   INVELMKY,0(RE)      SAVE ELEM KEY FOR NEXT ROUND                 
         CLI   PNVDKTYP,PNVDKDSQ   DETAIL DESCRIPTION ELEMENT?                  
         BNE   G_RNO36             NO, TRY NEXT ELEM                            
         TM    PNVDSTAT,PNVDDLQ    DETAIL ELEM IS DELETED?                      
         BNZ   G_RNO36             YES, TRY NEXT ELEM                           
         OC    PNVDSER#,PNVDSER#   RUN-NOT-ORDERED?                             
         BNZ   G_RNO36             NO, TRY NEXT ELEM                            
         CLC   PNVDDTE,STRDATE     LINE ITEM DATE >= START DATE?                
         BL    G_RNO36                                                          
         CLC   PNVDDTE,ENDDATE     LINE ITEM DATE <= END DATE?                  
         BH    G_RNO36                                                          
         ICM   R1,7,APRD                                                        
         MVC   TEMP2(L'PNVDPRD),LW_DATA1-LW_D(R1)                               
         OC    TEMP2(L'PNVDPRD),SPACES                                          
         CLC   TEMP2(L'PNVDPRD),SPACES                                          
         BE    *+14                                                             
         CLC   PNVDPRD,TEMP2       PRD CODE MATCH THAT OF FILTER?               
         BNE   G_RNO36                                                          
         CLI   FLTPBOPT,C' '       FILTERING ON PUB OPTION?                     
         BNH   G_RNO28                                                          
         CLI   FLTPBOPT,FLTPOBAS   ONLY WANT BASE PUB?                          
         BNE   *+14                                                             
         OC    PNVDZONE(L'PNVDZONE+L'PNVDEDN),PNVDZONE                          
         BNZ   G_RNO36                                                          
         CLI   FLTPBOPT,FLTPOZON   PUB WITH ZONE AND/OR EDITIONS?               
         BNE   *+14                                                             
         OC    PNVDZONE(L'PNVDZONE+L'PNVDEDN),PNVDZONE                          
         BZ    G_RNO36                                                          
                                                                                
G_RNO28  GOTOR FIVD                FORMAT INVOICE DETAIL VALUES                 
         MVI   INVDLSW,YESQ        SET TO DOWNLOAD INVOICES                     
         MVI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL             
         MVI   INVCMSW,PNVCKDTQ    SW SET TO DO DETAIL COMMENT LATER            
         B     G_RNO90                                                          
                                                                                
G_RNO36  GOTOR MNXTEL,ELEM                                                      
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    G_RNO96             NOT FOUND, NO NEED TO REPLY                  
         B     G_RNO24                                                          
                                                                                
G_RNO90  J     EXITY               EXIT TO SEND FORMATTED DATA                  
                                                                                
G_RNO96  GOTOR XC_IVALS,0          CLEAR ALL REPLY VALUES                       
                                                                                
G_RNO_X  MVI   INVCMSW,0           RESET INV COMMENT SWITCH                     
         J     NOMORE                                                           
                                                                                
         DROP  RB,R3,R5,RE                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO LOOK UP PRINTPAK PROFILE VALUES                          *         
***********************************************************************         
                                                                                
GETPRPRF NTR1  BASE=*,LABEL=*                                                   
         XC    PROFILEV,PROFILEV                                                
         XC    TEMP(12),TEMP                                                    
         MVI   TEMP,C'P'                                                        
         OC    PROFNAME,SPACES                                                  
         CLI   PROFNAME+2,C' '     PROFILE NAME IS TWO CHARS?                   
         BE    *+18                                                             
         MVC   TEMP+01(3),PROFNAME                                              
         NI    TEMP,X'BF'          MAKE SYSTEM LOWER CASE                       
         B     *+10                                                             
         MVC   TEMP+02(2),PROFNAME                                              
         MVC   TEMP+04(L'AGY),AGY                                               
                                                                                
         MVI   BYTE1,X'80'+X'40'   RETURN DEFAULT PROFILE                       
         CLC   PROFNAME(2),=C'FX'  FOREIGN EXCHANGE PROFILE?                    
         BNE   *+8                                                              
         MVI   BYTE1,X'80'+X'10'   USE AGENCY PROFILE (NOT USER ID)             
                                                                                
                                                                                
         ICM   R1,7,AMED                                                        
         CLI   LW_TYPE-LW_D(R1),LW_TSINQ                                        
         BNE   GPRPRF50                                                         
         MVC   TEMP+06(1),LW_DATA1-LW_D(R1)                                     
                                                                                
         TM    DL_FLAG1,FLTPAYEQ   FILTERING ON PAYEE?                          
         BZ    *+14                                                             
         MVC   TEMP+07(3),PBUYKCLT                                              
         B     GPRPRF42                                                         
                                                                                
         ICM   R1,7,ACLT                                                        
         CLI   LW_TYPE-LW_D(R1),LW_TSINQ                                        
         BNE   GPRPRF50                                                         
         CLC   =C'***',LW_DATA1-LW_D(R1)                                        
         BE    GPRPRF50                                                         
         MVC   TEMP+07(3),LW_DATA1-LW_D(R1)                                     
                                                                                
GPRPRF42 GOTOR (#GETCLT,AGETCLT),DMCB,AGY,TEMP+06,TEMP+07                       
         BE    *+6                                                              
         DC    H'0'                CAN'T READ CLIENT RECORD                     
                                                                                
         CLC   PROFNAME(2),=C'FX'  FOREIGN EXCHANGE PROFILE?                    
         BE    GPRPRF50            NOT DEFINED FOR CLT OFFICE                   
                                                                                
         L     RE,AIO1                                                          
         USING PCLTRECD,RE                                                      
         CLI   PCLTOFF,C' '        OFFICE CODE?                                 
         BNH   *+14                                                             
         MVI   TEMP+10,C'*'                                                     
         MVC   TEMP+11(1),PCLTOFF                                               
         DROP  RE                                                               
                                                                                
GPRPRF50 GOTOR VGETPROF,DMCB,(BYTE1,TEMP),PROFILEV,VDATAMGR                     
                                                                                
         L     RE,ALP                                                           
         USING LP_D,RE                                                          
         CLC   LP_VRSN1,=AL1(3,6,0,0)                                           
         BL    GPRPRF60                                                         
         CLC   PROFILEV,SPACES                                                  
         BH    GPRPRF60                                                         
         MVI   PROFILEV,C'N'       PATCH TO "N"                                 
         DROP  RE                                                               
                                                                                
GPRPRF60 DS    0H                                                               
                                                                                
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO LOOK UP MESSAGES                                         *         
***********************************************************************         
                                                                                
GETMSG   NTR1  BASE=*,LABEL=*                                                   
         ICM   R0,15,0(R1)         MSG SYS, TYPE, NUMBER                        
         ICM   RE,15,4(R1)         OUTPUT AREA                                  
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,60                                                        
         STCM  RE,7,GTAOUT                                                      
         STCM  R0,B'1000',GTMSYS   SET MESSAGE SYSTEM                           
         STCM  R0,B'0100',GTMTYP   SET MESSAGE TYPE                             
         STCM  R0,B'0011',GTMSGNO  SET MESSAGE NUMBER                           
         CLI   GTMSYS,0                                                         
         BNE   *+8                                                              
         MVI   GTMSYS,FF           SET DEFAULT MESSAGE SYSTEM                   
         CLI   GTMTYP,0                                                         
         BNE   *+8                                                              
         MVI   GTMTYP,GTMERR       DEFAULT TYPE IS ERROR                        
         OI    GT1INDS,GT1OWRK                                                  
         GOTOR VGETTXT,(R1)                                                     
         J     EXIT                                                             
         DROP  RB,R1                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* NUMBER OF INVOICE COMMENTS FILTER (HEADER AND DETAIL)               *         
* BYTE3 = TYPE OF COMMENT FILTERING ON                                *         
***********************************************************************         
                                                                                
IVCM#FLT NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   BYTE3,PNVCKHDQ      HEADER COMMENTS?                             
         BE    *+8                                                              
         CLI   BYTE3,PNVCKDTQ      DETAIL COMMENTS?                             
         BE    *+6                                                              
         DC    H'0'                BAD ELEM CODE                                
                                                                                
         USING PNVCOMD,RE                                                       
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(L'PNVHKEY,RE),0(RE)                                            
         BZ    IVCM#FX                                                          
         MVC   WORK2(L'PNVHKEY),0(RE)                                           
         MVI   BYTE4,0             LAST COMMENT GROUP NUMBER                    
                                                                                
IVCM#F20 CLC   BYTE3,PNVCKCDE      NO MORE COMMENT ELEMS?                       
         BNE   IVCM#F40                                                         
         CLI   PNVCKTYP,PNVCKCMQ   GENERAL COMMENTS?                            
         BE    IVCM#F30                                                         
         CLI   PNVCKTYP,PNVCKPBQ   DIALOGUE BETWEEN PAYER/BUYER?                
         BNE   IVCM#F40                                                         
                                                                                
IVCM#F30 MVC   BYTE4,PNVCKCGP      SAVE COMMENT GROUP NUMBER                    
         GOTOR MNXTEL,ELEM                                                      
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVCOMLQ,RE),0(RE)                                             
         BNZ   IVCM#F20                                                         
                                                                                
IVCM#F40 CLI   BYTE4,0             GROUP OF COMMENTS FOUND?                     
         BE    IVCM#F90                                                         
         SR    RE,RE                                                            
         IC    RE,FILT#IHC                                                      
         CLI   BYTE3,PNVCKHDQ      HEADER COMMENTS?                             
         BE    *+8                                                              
         IC    RE,FILT#IDC                                                      
         SR    RF,RF                                                            
         IC    RF,BYTE4            LAST COMMENT GROUP NUMBER                    
                                                                                
         CHI   RE,0                NO NEED TO DOWNLOAD INV COMMENTS?            
         BE    IVCM#FX                                                          
         CR    RE,RF               FILTER VALUE > LAST COMMENT GRP #?           
         BNL   IVCM#F90                                                         
                                                                                
         SR    RF,RE                                                            
         STC   RF,BYTE4            NUMBER OF COMMENT GROUP TO SKIP              
         BRAS  RE,IVCM#RES         RESTORE STARTING POINT                       
                                                                                
IVCM#F60 GOTOR MNXTEL,ELEM                                                      
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVCOMLQ,RE),0(RE)                                             
         BZ    IVCM#FX                                                          
         CLC   BYTE4,PNVCKCGP      SKIPPED ENOUGH COMMENT GROUPS?               
         BNL   IVCM#F60                                                         
         CLI   PNVCKCSQ,1          NEW SET OF COMMENT FOR SURE?                 
         BE    *+6                                                              
         DC    H'0'                                                             
         B     IVCM#FX                                                          
                                                                                
IVCM#F90 BRAS  RE,IVCM#RES         RESTORE STARTING POINT                       
                                                                                
IVCM#FX  J     EXIT                                                             
                                                                                
IVCM#RES LR    R0,RE                                                            
         XC    ELEM,ELEM           RESTORE STARTING POINT                       
         MVC   ELEM(L'PNVHKEY),WORK2                                            
         GOTOR MGETEL,ELEM                                                      
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(L'PNVHKEY,RE),0(RE)                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   WORK2(L'PNVHKEY),0(RE)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
         DROP  RB,RE                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET PUB BUFFER                                                      *         
* WORK+0         (L'PBUYKMED) = MEDIA CODE                            *         
* WORK+L'PBUYKMED(L'PBUPZE  ) = PUB CODE (BASE PUB, ZONE, EDITION)    *         
***********************************************************************         
                                                                                
SETPBUFF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLC   PBUMED,WORK         TEST CHANGE OF MEDIA/PUBLICATION             
         BNE   SETPB10                                                          
         CLC   PBUPZE,WORK+L'PBUYKMED                                           
         BNE   SETPB10                                                          
         TM    DL_FLAG1,FLTPAYEQ   FILTERING ON PAYEE?                          
         BZ    SETPBX                                                           
         NI    DL_FLAG1,X'FF'-SKIPPYEQ                                          
         CLC   PBUPAYCD,FILTSREP   PAYEE CODE MATCH THAT OF FILTER?             
         BE    SETPBX                                                           
         OI    DL_FLAG1,SKIPPYEQ   NO MATCH, SKIP PUB & INSERTION               
         B     SETPBX                                                           
                                                                                
SETPB10  XC    PROFILEV,PROFILEV   INIT PROFILE VALUES                          
                                                                                
         L     R0,AIO3             SAVE AIO1 (MINIO USE IT)                     
         LHI   R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LHI   RF,IOLENQ                                                        
         MVCL  R0,RE                                                            
                                                                                
         MVC   PROFNAME(2),=C'AB'  GET BY PROFILE                               
         BRAS  RE,GETPRPRF         GET PRINT PROFILE VALUES                     
                                                                                
         L     R0,AIO1             RESTORE AIO1 (GETCLT CLOBBERED IT)           
         LHI   R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LHI   RF,IOLENQ                                                        
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,PBUREC           RESOLVE PUBLICATION DETAILS                  
         LHI   R1,PBUL                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   PBUMED,WORK                                                      
         MVC   PBUPZE,WORK+L'PBUYKMED                                           
         MVI   PBUNAME,C'?'                                                     
         MVC   PBUNAME+1(L'PBUNAME-1),PBUNAME                                   
                                                                                
         MVC   WORK2(L'IOKEY),IOKEY                                             
         LA    R3,IOKEY                                                         
         USING PUBRECD,R3          READ PUBLICATION RECORD                      
         XC    PUBKEY,PUBKEY                                                    
         MVC   PUBKMED,WORK                                                     
         MVC   PUBKPUB(LPUBKPZE),WORK+L'PBUYKMED                                
         MVC   PUBKAGY,AGY                                                      
         MVI   PUBKCOD,PUBKCODQ                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPUBDIR+IO3'                            
         BNE   SETPB80                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPUBFIL+IO3'                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,IOADDR                                                        
         MVC   PBUNAME,SPACES                                                   
         MVC   PBUNAME(L'PUBNAME),PUBNAME                                       
         MVC   PBUZNAME,PUBZNAME   ZONE NAME                                    
         MVC   PBUCITY,PUBCITY     CITY                                         
         MVC   PBUSTATE,PUBSTATE   STATE                                        
         MVC   PBUCODE,BUYPUB      SET PUBLICATION CODE (PRINTABLE)             
         MVC   PBUREP,PUBPLSH      SET PUBLISHER/REP CODE                       
         MVC   PBUADDL1,PUBLINE1   ADDRESS LINE 1                               
         MVC   PBUADDL2,PUBLINE2   ADDRESS LINE 2                               
         MVC   PBUADDL3,PUBLINE3   ADDRESS LINE 3                               
         MVC   PBUZIPCD,PUBNWZIP   ZIP CODE                                     
         MVC   PBUNSTCD,PUBSTACD   NUMERIC STATE CODE                           
                                                                                
         LA    R3,(PUBNAMEL-PUBREC)(R3)                                         
         MVI   ELCODE,X'14'        PUBLICATION REP ELEM CODE                    
         USING PUBREPEL,R3                                                      
SETPB30H BRAS  RE,NXTELEM                                                       
         BNE   SETPB30M                                                         
         CLC   PUBRPOFF,=X'FFFFFF' AGENCY SPECIFIC?                             
         BE    SETPB30K                                                         
         CLC   PUBRPOFF,TEMP+07    CLIENT SPECIFIC?                             
         BE    SETPB30K                                                         
         CLI   PUBRPOFF,X'FF'      OFFICE SPECIFIC?                             
         BNE   SETPB30H                                                         
         CLC   PUBRPOFF+1(L'GADR_OFC),TEMP+11                                   
         BNE   SETPB30H                                                         
SETPB30K MVC   PBUPAYCD,PUBPAREP                                                
SETPB30M L     R3,IOADDR                                                        
                                                                                
         MVC   PBUNAME2,PBUNAME    SET MAIN PUB AS 2ND ADDRESS                  
         MVC   PBUZNAM2,PBUZNAME                                                
         MVC   PBUCITY2,PBUCITY                                                 
         MVC   PBUSTAT2,PBUSTATE                                                
         MVC   PBUAL1_2,PBUADDL1                                                
         MVC   PBUAL2_3,PBUADDL2                                                
         MVC   PBUAL3_2,PBUADDL3                                                
         MVC   PBUZIPC2,PBUZIPCD                                                
                                                                                
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BNE   SETPB60                                                          
         CLI   PROFILEV+06,C'Y'    USE PAY ADDRESS HIERARCHY?                   
         BE    *+8                                                              
         CLI   PROFILEV+06,C'2'    2ND ADDRESS OPTION?                          
         BNE   SETPB60                                                          
         USING GETADRPD,R1                                                      
         XC    TEMP2,TEMP2                                                      
         LA    R1,TEMP2                                                         
         MVC   GADR_AGY,AGY        AGENCY                                       
         MVC   GADR_MED,TEMP+06    MEDIA                                        
         MVC   GADR_CLT,TEMP+07    CLIENT                                       
         MVC   GADR_OFC,TEMP+11    OFFICE CODE                                  
         XC    DUB2,DUB2           PAYEE ADDRESS LEVEL FLAG                     
         GOTOR VPPGETAD,DMCB,(C'P',TEMP2),(R3),VDATAMGR                         
         CLI   0(R1),X'08'         PAY ADDRESS REC FOUND ?                      
         BNE   SETPB50                                                          
         BRAS  RE,SETPBCLR                                                      
         MVC   DUB2(03),1(R1)      SAVE PAYEE ADDRESS LEVEL                     
         L     RE,4(R1)                                                         
         USING PGETADRD,RE                                                      
         MVC   PBUNAME,PGADNAME                                                 
         MVC   PBUADDL1,PGADLIN1                                                
         MVC   PBUADDL2,PGADLIN2                                                
         MVC   PBUADDL3,PGADLIN3                                                
                                                                                
SETPB50  L     R3,IOADDR                                                        
         LA    R3,(PUBNAMEL-PUBREC)(R3)                                         
         MVI   ELCODE,X'14'        CLIENT/OFFICE REPS ELEM CODE                 
SETPB52  BRAS  RE,NXTELEM                                                       
         BNE   SETPB60                                                          
         LA    R1,TEMP2                                                         
         CLC   PUBRPOFF,=X'FFFFFF' AGENCY SPECIFIC?                             
         BE    SETPB56                                                          
         CLC   PUBRPOFF,GADR_CLT   CLIENT SPECIFIC?                             
         BE    SETPB54                                                          
         CLI   PUBRPOFF,X'FF'      OFFICE SPECIFIC?                             
         BNE   SETPB52                                                          
         CLC   PUBRPOFF+1(L'GADR_OFC),GADR_OFC                                  
         BNE   SETPB52                                                          
                                                                                
SETPB54  OC    PUBPAREP(12),PUBPAREP                                            
         BZ    SETPB52                                                          
                                                                                
SETPB56  CLC   PUBPAREP,=4C'0'                                                  
         BE    SETPB60                                                          
         OC    PUBPAREP,PUBPAREP                                                
         BZ    SETPB60                                                          
                                                                                
         MVC   PBUPAYCD,PUBPAREP   REPLY PAYING REP CODE                        
                                                                                
         OC    DUB2(3),DUB2        ANYTHING IN SAVED ADDRESS LEVEL?             
         BZ    *+14                                                             
         CLC   DUB2(3),PUBRPOFF    ADDR IS MORE SPECIFIC THAN REP?              
         BL    SETPB60                                                          
                                                                                
         LA    RE,IOKEY            READ REP RECORD                              
         LA    R1,TEMP2                                                         
         USING PREPRECD,RE                                                      
         XC    PREPKEY,PREPKEY                                                  
         MVC   PREPKAGY,AGY                                                     
         MVC   PREPKMED,GADR_MED                                                
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,PUBPAREP                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO3'                            
         BNE   SETPB60                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO3'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,SETPBCLR                                                      
         L     RE,IOADDR                                                        
         LA    RE,(PREPELEM-PREPREC)(RE)                                        
         CLI   0(RE),X'11'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID REP RECORD                           
         USING PREPELEM,RE                                                      
         MVC   PBUNAME,PREPNAME                                                 
         MVC   PBUADDL1,PREPLIN1                                                
         MVC   PBUADDL2,PREPLIN2                                                
         MVC   PBUADDL3,PREPLIN3                                                
         MVC   PBUSTATE,PREPSTAC                                                
         MVC   PBUPAYCD,PUBPAREP   REPLY PAYING REP CODE                        
                                                                                
SETPB60  TM    DL_FLAG1,FLTPAYEQ   FILTERING ON PAYEE?                          
         BZ    SETPB70                                                          
         NI    DL_FLAG1,X'FF'-SKIPPYEQ                                          
         CLC   PBUPAYCD,FILTSREP   PAYEE CODE MATCH THAT OF FILTER?             
         BE    SETPB70                                                          
         OI    DL_FLAG1,SKIPPYEQ   NO MATCH, SKIP PUB & INSERTION               
SETPB70  MVC   IOKEY,WORK2         RESTORE SAVED BUY KEY                        
                                                                                
SETPB80  TM    GIND1,GIONLINE      TEST RUNNING ONLINE                          
         BNZ   SETPBX              YES                                          
         TM    DL_FLAG1,SKIPPYEQ   PAYEE NOT MATCH, SKIP?                       
         BNZ   SETPBX                                                           
         GOTOR VBUFFRIN,DMCB,('BUFFAPUT',PUBBUFF),PBUD,ACOMFACS                 
         BE    SETPBX                                                           
         DC    H'0'                                                             
                                                                                
SETPBX   J     EXIT                                                             
*                                                                               
SETPBCLR XC    PBUNAME,PBUNAME     SET PUBLICATION NAME                         
         XC    PBUZNAME,PBUZNAME   ZONE NAME                                    
         XC    PBUCITY,PBUCITY     CITY                                         
         XC    PBUSTATE,PBUSTATE   STATE                                        
         XC    PBUREP,PBUREP       SET PUBLISHER/REP CODE                       
         XC    PBUADDL1,PBUADDL1   ADDRESS LINE 1                               
         XC    PBUADDL2,PBUADDL2   ADDRESS LINE 2                               
         XC    PBUADDL3,PBUADDL3   ADDRESS LINE 3                               
         XC    PBUZIPCD,PBUZIPCD   ZIP CODE                                     
         BR    RE                                                               
*                                                                               
NXTELEM  SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLC   ELCODE,0(R3)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R3),0                                                          
         JNE   NXTELEM                                                          
         LTR   R3,R3               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         DROP  RB,R3,R1,RE                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* CLEAR INVOICE VALUES, SO NO VALUES WILL BE REPLIED                  *         
* R1 -> 0 = CLEAR ALL INVOICE VALUES (HDR,DET,COMMENTS,INS)           *         
*       1 = CLEAR DETAIL VALUES ONLY                                  *         
*       2 = CLEAR HEADER/DETAIL COMMENT(S)                            *         
*       3 = CLEAR INSERTION DATA                                      *         
*       4 = CLEAR INVOICE HEADER VALUES ONLY                          *         
*       5 = CLEAR INVOICE SPECIAL REP DATA AND TABLE                  *         
*       8 = CLEAR ALL, EXCEPT SPECIAL REP DATA AND TABLE              *         
*       9 = CLEAR ALL, EXCEPT HEADER                                  *         
***********************************************************************         
                                                                                
XC_IVALS NTR1  BASE=*,LABEL=*                                                   
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         LR    R5,R1                                                            
                                                                                
         CHI   R5,8                CLEAR ALL, EXCEPT SPECIAL REP TAB?           
         BE    XC_IV12                                                          
         CHI   R5,0                CLEAR ALL INVOICE VALUES?                    
         BNE   XC_IV20                                                          
XC_IV12  L     R1,AINVVAL                                                       
         USING INVVALSD,R1                                                      
         XC    INVSUMYS(INVSYLQ),INVSUMYS                                       
         LA    R1,INVHDRVS         BEGINNING OF INV HDR VALUES                  
         LR    R0,R1                                                            
         LHI   R1,INVHVLQ          L'HEADER VALUES                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CHI   R5,4                CLEAR INVOICE HEADER VALUES ONLY?            
         BE    XC_IV90                                                          
                                                                                
XC_IV14  L     R1,AINVVAL                                                       
         USING INVVALSD,R1                                                      
         LA    R1,INVCOMVS         BEGINNING OF COMMENT VALUES                  
         LR    R0,R1                                                            
         LHI   R1,INVCVLQ          L'COMMENT VALUES                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R0,LP_BLKS+((B#INVCOM-1)*L'LP_BLKS)                              
         LHI   R1,IVCOMMMX*L'IVCOMMNT+1                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CHI   R5,2                CLEAR COMMENT(S) ONLY?                       
         BE    XC_IV90                                                          
                                                                                
XC_IV16  CHI   R5,8                CLEAR ALL, EXCEPT SPECIAL REP TAB?           
         BE    XC_IV24                                                          
         L     R1,AINVVAL                                                       
         USING INVVALSD,R1                                                      
         LA    R1,SREP_STR         BEGINNING OF SPECIAL REP VALUES              
         LR    R0,R1                                                            
         LHI   R1,SREPTLNQ         L'SPECIAL REP VALUES                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CHI   R5,5                CLEAR INVOICE SPECIAL REP?                   
         BE    XC_IV90                                                          
                                                                                
         B     XC_IV24             CLEAR DETAIL VALUES                          
                                                                                
XC_IV20  CHI   R5,1                CLEAR ONLY DETAIL VALUES?                    
         BNE   XC_IV40                                                          
XC_IV24  L     R1,AINVVAL                                                       
         USING INVVALSD,R1                                                      
         LA    R1,INVDETVS         BEGINNING OFR INV DET VALUES                 
         LR    R0,R1                                                            
         LHI   R1,INVDVLQ          L'DETAIL VALUES                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CHI   R5,8                CLEAR ALL, EXCEPT SPECIAL REP TAB?           
         BE    XC_IV52                                                          
         CHI   R5,9                CLEAR ALL, EXCEPT HEADER?                    
         BE    XC_IV52                                                          
         B     XC_IV90                                                          
                                                                                
XC_IV40  CHI   R5,2                CLEAR COMMENT(S)?                            
         BE    XC_IV14                                                          
                                                                                
XC_IV50  CHI   R5,3                CLEAR INSERTION DATA?                        
         BNE   XC_IV60                                                          
                                                                                
XC_IV52  GOTOR XCBUYVAL            BEGINNING OF BUY VALUES                      
         ZAP   BUYCDPCT,PZERO      PACK ZERO WILL NOT BE REPLIED                
         GOTOR XC_BLCK#,B#BUY                                                   
         B     XC_IV90                                                          
                                                                                
XC_IV60  CHI   R5,4                CLEAR INVOICE HEADER VALUES ONLY?            
         BE    XC_IV12                                                          
                                                                                
         CHI   R5,5                CLEAR SPECIAL REP?                           
         BE    XC_IV16                                                          
                                                                                
XC_IV70  CHI   R5,9                CLEAR ALL, EXCEPT HEADER?                    
         BE    XC_IV14                                                          
                                                                                
XC_IV90  J     EXIT                                                             
         DROP  RB,R3,R1                                                         
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
XC_BLCK# STM   RE,R1,12(RD)        CLEAR BLOCK (R1 HAS BLOCK NUMBER)            
         LTR   R1,R1                                                            
         JNZ   *+6                                                              
         DC    H'0'                BAD PARAMETER                                
         MHI   R1,L'LP_BLKS        DISPLACEMENT TO TARGET BLOCK                 
         L     RE,ALP                                                           
         LA    RE,LP_BLKS-L'LP_BLKS-LP_D(R1,RE)                                 
         ICM   R0,15,0(RE)         POINT TO START OF BLOCK                      
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
XCBUYVAL STM   RE,R1,12(RD)        CLEAR BUYVALS                                
         LA    R0,BUYVALS                                                       
         LHI   R1,BUYVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
S_BUYARY NTR1  BASE=*,LABEL=*      SET BUY DATA ARRAY                           
                                                                                
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     R4,LP_BLKS+((B#INSARY-1)*L'LP_BLKS)                              
         USING INSVARY,R4                                                       
                                                                                
         TM    FLTMATST,FLTMSNOI   NO INVOICES?                                 
         BNZ   S_BA020                                                          
         GOTOR XC_BARYD,BINVARYQ   INIT BUY INVOICE DATA ARRAY                  
         LA    R3,PBUYFRST         POINT TO FIRST BUY ELEMENT                   
         USING PBNVELMD,R3                                                      
         SR    R5,R5               NUMBER OF BUY INVOICE ELEM                   
         LA    R7,BINVTAB                                                       
         USING BINVTAB,R7                                                       
         MVI   ELCODE,PBNVELQ      BUY INVOICE ELEM CODE                        
                                                                                
S_BA010  BRAS  RE,NXTELEM                                                       
         BNE   S_BA016                                                          
                                                                                
         CLC   INVSURCE,SPACES                                                  
         JNH   S_BA012             No invoice source to filter on               
         LHI   R0,L'INVSURCE                                                    
         LA    RF,INVSURCE         Point to filtering entries                   
         CLC   PBNVISRC,0(RF)      Invoice source match?                        
         JE    S_BA012                                                          
         LA    RF,1(RF)            Point to next filter entry                   
         JCT   R0,*-14                                                          
         J     S_BA010             Failed filter, try next elem                 
                                                                                
S_BA012  MVC   WORK2(L'PBUYKMED),PBUYKMED                                       
         MVC   WORK2+L'PBUYKMED(L'PBNVSER#),PBNVSER#                            
         BRAS  RE,TSTINVS#         INVOICE SERIAL # IS GOOD?                    
         BNE   S_BA010                                                          
                                                                                
         AHI   R5,1                1 BUY INVOICE ELEM FOUND                     
         CHI   R5,BINVTABM                                                      
         BNH   *+6                                                              
         DC    H'0'                TOO MANY INVOICE ELEM!                       
         MVC   BINVKEY(L'PBUYKMED),PBUYKMED                                     
         UNPK  WORK(2*L'PBNVSER#+1),PBNVSER#(L'PBNVSER#+1)                      
         MVC   BINVKEY+1(L'BINVKEY-1),WORK                                      
         MVC   BINVSEQ#,PBNVDSQN                                                
         MVC   BINVVIV#,PBNVINV#                                                
         AHI   R7,BINVTABL         POINT TO NEXT ARRAY ENTRY                    
                                                                                
* FOR INVOICE DOWNLOAD MODES, NO NEED TO ADD INVOICE KEY TO BUFFER              
                                                                                
         CLI   INVDLMOD,IVDLMD_#   DOWNLOAD BY INVOICE NUMBER?                  
         BE    S_BA010                                                          
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY(S)?                  
         BE    S_BA010                                                          
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY INV START/END DATES?             
         BE    S_BA010                                                          
         CLI   INVDLMOD,IVDLMD_C   Download by Inv Creation dates?              
         JE    S_BA010                                                          
         CLI   INVDLSW,YESQ        DOWNLOAD INVOICE DATA?                       
         BNE   S_BA010                                                          
                                                                                
         XC    IBUREC(IBUL),IBUREC                                              
         MVC   IBUMEDCD,PBUYKMED   MEDIA CODE                                   
         MVC   IBUINVS#,PBNVSER#   INVOICE SERIAL NUMBER                        
         MVI   TSACTN,TSAADD       ADD INVOICE KEY TO BUFFER                    
         GOTOR BUFFER                                                           
         CLI   TSERRS,0            TEST FOR ERRORS                              
         BNE   *+20                                                             
         ICM   R0,15,T_INVCNT                                                   
         AHI   R0,1                                                             
         STCM  R0,15,T_INVCNT      TOTAL # OF INVOICES COUNTER                  
         B     S_BA010                                                          
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         BE    S_BA010                                                          
         DC    H'0'                OTHER ERRORS ARE NO GOOD                     
                                                                                
S_BA016  STCM  R5,3,BINVTAB#       NUMBER OF BUY INVOICE ELEM                   
                                                                                
S_BA020  GOTOR XC_BARYD,BPO#ARYQ   INIT ZZZ BUY PURCHASE ORDER ARRAY            
         LA    R3,PBUYFRST         POINT TO FIRST BUY ELEMENT                   
         USING PBYPOELD,R3                                                      
         SR    R5,R5               NUMBER OF BUY PURCHASE ORDER ELEM            
         LA    R7,BPO#TAB                                                       
         USING BPO#TAB,R7                                                       
         MVI   ELCODE,PBYPOELQ     BUY PURCHASE ORDER ELEM CODE                 
S_BA024  BRAS  RE,NXTELEM                                                       
         BNE   S_BA026                                                          
         TM    PBYPOSTA,BYPOZZZQ   PO# ELEM FOR ZZZ BUY?                        
         BZ    S_BA024                                                          
         AHI   R5,1                1 ZZZ BUY PO# ELEM FOUND                     
         CHI   R5,BPO#TABM                                                      
         BNH   *+6                                                              
         DC    H'0'                TOO MANY PURCHASE ORDER ELEM!                
         MVC   BPO#SEQ#,PBYPOSQ#                                                
         MVC   BPO#PRDC,PBYPOPRD                                                
         AHI   R7,BPO#TABL         POINT TO NEXT ARRAY ENTRY                    
         B     S_BA024                                                          
S_BA026  STCM  R5,3,BPO#TAB#       NUMBER OF BUY PURCHASE ORDER ELEM            
                                                                                
         J     EXIT                                                             
                                                                                
XC_BARYD STM   RE,R1,12(RD)        CLEAR BUY DATA ARRAY                         
         LTR   R1,R1                                                            
         JNZ   *+6                                                              
         DC    H'0'                BAD PARAMETER                                
         CHI   R1,BINVARYQ         CLEAR BUY INVOICE DATA ARRAY?                
         JNE   XC_BARY1                                                         
         LA    R0,BINVTAB#                                                      
         LHI   R1,BINVTABL*BINVTABM+L'BINVTAB#                                  
         J     XC_BARYX                                                         
XC_BARY1 CHI   R1,BPO#ARYQ         CLEAR ZZZ BUY PURCHASE ORDER ARRAY?          
         JNE   XC_BARY2                                                         
         LA    R0,BPO#TAB#                                                      
         LHI   R1,BPO#TABL*BPO#TABM+L'BPO#TAB#                                  
         J     XC_BARYX                                                         
XC_BARY2 DC    H'0'                UNDEFINED BUY DATA ARRAY                     
XC_BARYX SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
         DROP  RB,R1,R3,R4,R7                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* MODIFY FILTER FIELDS                                                *         
***********************************************************************         
                                                                                
MODFYFIL NTR1  BASE=*,LABEL=NO                                                  
                                                                                
         OC    FILTSREP,FILTSREP                                                
         BZ    MODF_20                                                          
         MVI   TEMP2,C'0'                                                       
         MVC   TEMP2+1(L'TEMP2-1),TEMP2                                         
         LA    RE,FILTSREP+(L'FILTSREP-1)                                       
         LA    RF,L'FILTSREP                                                    
         CLI   0(RE),C' '                                                       
         BNE   *+12                                                             
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     *-12                                                             
         CHI   RF,L'FILTSREP                                                    
         JE    MODF_20                                                          
         LA    R1,L'FILTSREP                                                    
         SR    R1,RF                                                            
         LA    RE,TEMP2                                                         
         AR    RE,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),FILTSREP                                                 
         MVC   FILTSREP,TEMP2      RIGHT JUSTIFIED WITH PADDED ZEROES           
                                                                                
MODF_20  CLI   FLTPBOPT,C' '       FILTERING ON PUB OPTION?                     
         BH    MODF_40                                                          
         CLI   DL_TYPE,DL_IMRQ     INVOICE MATCH REPORT DL (DRR2)?              
         BNE   MODF_40                                                          
         MVI   FLTPBOPT,FLTPOBAS   ONLY WANT BASE PUB (DRR2 DEFAULT)            
                                                                                
MODF_40  DS    0H                  FUTURE FILTER MODIFICATIONS                  
                                                                                
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* FORMAT INVOICE HEADER DATA                                          *         
***********************************************************************         
                                                                                
FIVH     NTR1  ,                                                                
         L     R5,AINVVAL                                                       
         USING INVVALSD,R5                                                      
         LA    R7,ELEM2            POINT TO ELEMENT                             
         USING PNVHDRD,R7                                                       
         CLI   PNVHKCDE,PNVHKIDQ   HEADER ELEMENT CODE?                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,MINMKEY                                                       
         USING PNVKEY,RF                                                        
         MVC   INVKEY(L'PNVKMED),PNVKMED                                        
         XC    WORK,WORK                                                        
         UNPK  WORK(2*L'PNVKSER#+1),PNVKSER#(L'PNVKSER#+1)                      
         MVI   WORK+2*L'PNVKSER#,C' '                                           
         MVC   INVKEY+L'PNVKMED(2*L'PNVKSER#),WORK                              
         MVC   INVCLTC,PNVHCLT                                                  
         GOTOR VPUBEDIT,DMCB,(X'08',PNVHPUB),(C'S',INVHPUBC)                    
         MVC   INVVNDR#,PNVHINV#                                                
         MVC   INVSTAT,PNVHSTAT                                                 
         MVC   INVPSTR,PNVHSTRT                                                 
         MVC   INVPEND,PNVHEND                                                  
         MVC   INVDATE,PNVHDATE                                                 
         MVC   INVTOTL,PNVHTOT                                                  
         MVC   INVTTYP,PNVH$TYP                                                 
         MVC   INVSREP,PNVHSREP                                                 
         MVC   INVHTAX,PNVHTAX                                                  
         XC    INVHSRC,INVHSRC                                                  
         CLI   PNVHIVSR,INVADBYQ   Source is AdBuyer (default)?                 
         JNE   *+10                                                             
         MVC   INVHSRC,=C'ADB'                                                  
         CLI   PNVHIVSR,INVADB_Q   Source is AdBuyer?                           
         JNE   *+10                                                             
         MVC   INVHSRC,=C'ADB'                                                  
         CLI   PNVHIVSR,INVIPS_Q   Source is IPS?                               
         JNE   *+10                                                             
         MVC   INVHSRC,=C'IPS'                                                  
         CLI   PNVHIVSR,INVPRM_Q   Source is Prisma?                            
         JNE   *+10                                                             
         MVC   INVHSRC,=C'PRM'                                                  
         CLI   PNVHIVSR,INVRAD_Q   Source is Radia?                             
         JNE   *+10                                                             
         MVC   INVHSRC,=C'RAD'                                                  
                                                                                
         XC    INVHCDT,INVHCDT     Init invoice creation date                   
FIVH36   GOTOR MNXTEL,ELEM         Looping to find activity elem                
         ICM   RE,15,MINELEM                                                    
         OC    0(08,RE),0(RE)                                                   
         JZ    FIVH40              Done looping, restore header elem            
         USING PNVACTHD,RE                                                      
         CLI   PNVAKCDE,PNVAKHDQ   Header activity element?                     
         JNE   FIVH36                                                           
         CLI   PNVAKACT,PNVAKACQ   Activity element?                            
         JNE   FIVH36                                                           
         TM    PNVAHCH1,PNVAHADD   Invoice header added?                        
         JZ    FIVH36                                                           
         MVC   INVHCDT,PNVAHDTE    Save date of invoice creation                
         DROP  RE                                                               
                                                                                
FIVH40   XC    ELEM,ELEM           Point to header element again                
         MVI   ELEM,PNVHKIDQ       Invoice header elem ID                       
         GOTOR MGETEL,ELEM         Get element to record                        
         ICM   RE,15,MINELEM                                                    
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         JNZ   *+6                                                              
         DC    H'0'                Must restore header elem pointer             
                                                                                
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         JNE   EXIT                                                             
         TM    DL_FLAG1,IVIRPLYQ   E#INSDET REPLY RECORD (DRR)?                 
         JNZ   EXIT                                                             
                                                                                
         MVC   WORK(L'INVMED),INVKEY                                            
         MVC   WORK+L'INVMED(L'INVSREP),INVSREP                                 
         GOTOR SETSREPT                                                         
                                                                                
         J     EXIT                                                             
         DROP  R7,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT INVOICE DETAIL DATA                                          *         
***********************************************************************         
                                                                                
FIVD     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         USING INVVALSD,R5                                                      
         LA    R7,ELEM2            POINT TO ELEMENT                             
         USING PNVDTLD,R7                                                       
                                                                                
         CLI   PNVDKCDE,PNVDKIDQ   DETAIL ELEMENT CODE?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PNVDKTYP,PNVDKDSQ   TYPE IS DETAIL DESCRIPTION?                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   DL_TYPE,DL_PRBQ     PROBE DOWNLOAD?                              
         BNE   *+20                                                             
         ICM   RE,15,T_IVDCNT                                                   
         AHI   RE,1                                                             
         STCM  RE,15,T_IVDCNT                                                   
         B     FIVD0022            DON'T EXECUTE FOR PROBE DOWNLOAD             
                                                                                
         XC    INVINSKY,INVINSKY                                                
                                                                                
         TM    PNVDSTAT,PNVDDLQ    DETAIL ELEM IS DELETED?                      
         BZ    FIVD0002            BZ=NOT DELETED                               
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BE    FIVD0022            NO NEED TO REPLY DELETED FOR DRR             
         CLC   LP_VRSN1,=AL1(3,3,0,19)                                          
         BNL   *+12                                                             
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY?                     
         BNE   FIVD0022            DO NOT REPLY ANY DETAIL DATA                 
         MVC   INVISQN,PNVDKSQN                                                 
         MVI   INVDGSTA,GST_DELQ   DETAIL IS DELETED                            
         CLC   LP_VRSN1,=AL1(3,3,0,19)                                          
         BL    *+16                                                             
         TM    PNVDSTAT,PNVDMTQ    DETAIL IS MATCHED?                           
         BZ    *+8                                                              
         MVI   INVDGSTA,GST_CLRQ   DETAIL IS CLEARED (DELETED+MATCHED)          
         GOTOR FISK                                                             
         B     FIVD0022            DONE WITH DETAIL DATA REPLY                  
                                                                                
FIVD0002 MVC   INVISQN,PNVDKSQN                                                 
                                                                                
         GOTOR FISK                                                             
                                                                                
         TM    DL_FLAG1,IVIRPLYQ   E#INSDET REPLY RECORD (DRR)?                 
         BZ    *+16                                                             
         CLC   PNVDKSQN,IBUIVDS#   INVOICE DETAIL SEQUENCE# MATCH?              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   INVDLMOD,IVDLMD_#   DOWNLOAD BY INVOICE NUMBER?                  
         BE    FIVD0004                                                         
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY(S)?                  
         BE    FIVD0004                                                         
         CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BNE   FIVD0006                                                         
         CLI   INSDLSW,NOQ         NEED TO DOWNLOAD INSERTION DATA?             
         BE    FIVD0006                                                         
         CLI   INVDLMOD,IVDLMD_B   DL MODE IS DRIVEN BY BUY (INSERTION)         
         BE    FIVD0006                                                         
                                                                                
FIVD0004 TM    DL_FLAG1,IVIRPLYQ   E#INSDET REPLY RECORD (DRR)?                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         OC    INVINSKY,INVINSKY   INSERTION KEY                                
         BZ    FIVD0006                                                         
         XC    IBUREC(IBUL),IBUREC                                              
         MVI   TSACTN,TSARDH       SET TO READ HIGH                             
FIVD004D GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BNZ   FIVD004H                                                         
         CLC   IBUINSKY,INVINSKY   INSERTION KEY ALREADY IN BUFFER?             
         BE    FIVD0006                                                         
         MVI   TSACTN,TSANXT       SET TO GET NEXT RECORD                       
         B     FIVD004D                                                         
                                                                                
FIVD004H XC    IBUREC(IBUL),IBUREC                                              
         MVC   IBUMEDCD,INVMED     INVOICE KEY                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'INVSER#),INVSER#                                          
         MVI   WORK+L'INVSER#,C'0'                                              
         PACK  DUB,WORK(L'INVSER#+1)                                            
         MVC   IBUINVS#,DUB+2                                                   
         MVC   IBUIVDS#,EFFS       FOR DRR REPLY USES                           
         MVC   IBUINSKY,INVINSKY   INSERTION KEY                                
         MVI   TSACTN,TSAADD       ADD INVOICE DATA TO BUFFER                   
                                                                                
         GOTOR BUFFER                                                           
         CLI   TSERRS,0            TEST FOR ERRORS                              
         BE    FIVD0006                                                         
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         BE    FIVD0006                                                         
         DC    H'0'                OTHER ERRORS ARE NO GOOD                     
                                                                                
FIVD0006 OC    PNVDRATE,PNVDRATE   NULLS?                                       
         BZ    FIVD0014            YES, NO RATE TO BE REPLIED                   
         CP    PNVDRATE,PZERO      PACKED ZERO?                                 
         BNE   *+14                                                             
         MVC   INVRATE(L'FREE),FREE                                             
         B     FIVD0014                                                         
                                                                                
         LA    R4,INVRATE                                                       
* * * *  CLI   INVMED,C'N'         NEWSPAPER?                                   
* * * *  BNE   *+8                                                              
* * * *  CLI   PNVDCTPN,C'T'       TOTAL RATE?                                  
* * * *  BNE   *+14                                                             
* * * *  MVC   0(1,R4),PNVDCTPN                                                 
* * * *  LA    R4,1(R4)                                                         
         CLI   PNVDCSTP,C'N'       NET AMOUNT                                   
         BNE   FIVD0008                                                         
         MVI   0(R4),C'N'                                                       
         B     FIVD0010                                                         
FIVD0008 CLI   PNVDCSIN,0                                                       
         BE    FIVD0012                                                         
         CLI   PNVDCSIN,C' '                                                    
         BE    FIVD0012                                                         
         MVC   0(L'PNVDCSIN,R4),PNVDCSIN                                        
FIVD0010 LA    R4,1(R4)                                                         
                                                                                
FIVD0012 XC    DUB,DUB             DUMMY CURRENY DEFINITION FLD                 
         MVI   DUB+3,2             DEFAULT IS 2 DECIMALS                        
         CLI   PNVDRDEC,0                                                       
         BE    *+10                                                             
         MVC   DUB+3(1),PNVDRDEC                                                
         LA    RF,DUB                                                           
         CURED (P6,PNVDRATE),(15,(R4)),(RF),ALIGN=LEFT,FLOAT=-                  
         OC    INVRATE,SPACES                                                   
                                                                                
FIVD0014 OC    PNVDPREM,PNVDPREM                                                
         BNZ   *+12                SKIP IF NO PREM                              
         CLI   PNVDNCL,0                                                        
         BE    FIVD0020            AND NO COLORS                                
                                                                                
         OC    PNVDPREM,PNVDPREM                                                
         BZ    FIVD0016            NO PREIMUM, DO COLOR                         
                                                                                
         CP    PNVDPREM,PZERO      ZERO COST?                                   
         BNZ   *+14                                                             
         MVC   INVPREM(L'FREE),FREE                                             
         B     FIVD0020            SET PREMIUM TO 'FREE'                        
                                                                                
FIVD0016 LA    R4,INVPREM                                                       
                                                                                
         CLI   PNVDNCL,0           COLORS PRESENT?                              
         BE    FIVD0018                                                         
         MVC   0(1,R4),PNVDNCL     NUMBER OF COLORS                             
         OI    0(R4),X'F0'         FILL IN ZONE                                 
         MVI   1(R4),C'C'          INDICATE COLORS                              
         LA    R4,2(R4)            BUMP OUTPUT POINTER                          
         OC    PNVDPREM,PNVDPREM                                                
         BZ    FIVD0020            DONE IF NO PREMIUM COST                      
         MVI   0(R4),C'/'                                                       
         AHI   R4,1                                                             
                                                                                
FIVD0018 CLI   PNVDPRTP,C' '       COST TYPE PRESENT?                           
         BNH   *+14                                                             
         MVC   0(1,R4),PNVDPRTP                                                 
         AHI   R4,1                                                             
                                                                                
         CLI   PNVDPRIN,C' '       COST INDICATOR PRESENT?                      
         BNH   *+14                                                             
         MVC   0(1,R4),PNVDPRIN                                                 
         AHI   R4,1                                                             
                                                                                
         EDITR (P6,PNVDPREM),(15,(R4)),2,ALIGN=LEFT,FLOAT=-,IZERO=Y             
         OC    INVPREM,SPACES                                                   
                                                                                
FIVD0020 MVC   INVNET,PNVDNET                                                   
         MVC   INVGROSS,PNVDGRS                                                 
         MVC   INV#LINE,PNVD#LIN                                                
         MVC   INVINSDT,PNVDDTE                                                 
         MVC   INVSPDSP,PNVDSPC                                                 
         MVC   INVADCAP(L'PNVDACAP),PNVDACAP                                    
         MVC   INVADCAP+L'PNVDACAP(L'PNVDACP2),PNVDACP2                         
         MVC   INVCLTCD,PNVDCLT                                                 
         MVC   INVPRDCD,PNVDPRD                                                 
         XC    INVDSRC,INVDSRC                                                  
         CLI   PNVDIVSR,INVADBYQ   Source is AdBuyer (default)?                 
         JNE   *+10                                                             
         MVC   INVDSRC,=C'ADB'                                                  
         CLI   PNVDIVSR,INVADB_Q   Source is AdBuyer?                           
         JNE   *+10                                                             
         MVC   INVDSRC,=C'ADB'                                                  
         CLI   PNVDIVSR,INVIPS_Q   Source is IPS?                               
         JNE   *+10                                                             
         MVC   INVDSRC,=C'IPS'                                                  
         CLI   PNVDIVSR,INVPRM_Q   Source is Prisma?                            
         JNE   *+10                                                             
         MVC   INVDSRC,=C'PRM'                                                  
         CLI   PNVDIVSR,INVRAD_Q   Source is Radia?                             
         JNE   *+10                                                             
         MVC   INVDSRC,=C'RAD'                                                  
                                                                                
         MVC   IVNDKEY,INVKEY                                                   
         MVC   INVDVI#,INVVNDR#                                                 
         MVC   INVDSTA,INVSTAT                                                  
         MVC   INVDTYP,INVTTYP                                                  
                                                                                
         MVC   INVDDCMC,PNVDDCM                                                 
                                                                                
         OC    PNVDPUB,PNVDPUB                                                  
         BZ    FIVD0022                                                         
         GOTOR VPUBEDIT,DMCB,(0,PNVDPUB),(C'S',INVDPUBC)                        
                                                                                
FIVD0022 J     EXIT                EXIT TO SEND FORMATTED BUY RECORD            
                                                                                
FISK     OC    PNVDSER#,PNVDSER#   FORMAT INSERTION KEY                         
         BZR   RE                                                               
         CP    PNVDSER#,PZERO                                                   
         BER   RE                                                               
         LA    RF,MINMKEY                                                       
         USING PNVKEY,RF                                                        
         MVC   INVINSKY(L'PNVKMED),PNVKMED                                      
         MVC   INVINSKY+L'PNVKMED(L'PNVDCLT),PNVDCLT                            
         XC    WORK,WORK                                                        
         UNPK  WORK(2*L'PNVDSER#-1),PNVDSER#                                    
         OI    WORK+(2*L'PNVDSER#-1),X'F0'                                      
         MVC   INVINSKY+L'PNVKMED+L'PNVDCLT(2*L'PNVDSER#-1),WORK                
         BR    RE                                                               
         DROP  RB,R7,R5,R3,RF                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROCESS PID AND DATE IN DETAIL ELEMS                                *         
***********************************************************************         
                                                                                
PRCPIDDT NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         USING INVVALSD,R5                                                      
         XC    INVDDCPI,INVDDCPI   CLEAR DCOM PID                               
         XC    INVDDCDT,INVDDCDT   CLEAR DCOM DATE                              
                                                                                
         CLI   DL_TYPE,DL_PRBQ                                                  
         JE    PPIDDT_X            DON'T REPLY FOR PROBE DOWNLOAD               
                                                                                
         CLC   LP_VRSN1,=AL1(3,3,0,7)                                           
         JL    PPIDDT_X                                                         
                                                                                
         MVC   WORK2,ELEM          SAVE ORIGINAL MINIO ELEM KEY                 
         XC    HALF2,HALF2         TO SAVE BINARY PID                           
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM             POINT TO ELEMENT                             
         USING PNVACTHD,RE                                                      
         MVI   PNVAKCDE,PNVAKDTQ   DETAIL ACTIVITY ELEM ID                      
         MVI   PNVAKLEN,PNVACTLQ   DETAIL ACTIVITY ELEM LENGTH                  
         LHI   RF,1                                                             
         STCM  RF,3,PNVAKDSQ       START AT DETAIL SEQ 1                        
         MVI   PNVAKACT,PNVAKACQ   DETAIL ACTIVITY ELEM CODE                    
         STCM  RF,3,PNVAKCSQ       START AT ACTIVITY SEQ 1                      
                                                                                
PPIDDT10 GOTOR MGETEL,ELEM                                                      
PPIDDT14 ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVACTLQ,RE),0(RE)                                             
         BZ    PPIDDT80                                                         
                                                                                
         TM    PNVAKACT,PNVAKACQ   ACTIVITY ELEM CODE?                          
         BO    PPIDDT20                                                         
         GOTOR MNXTEL,ELEM                                                      
         B     PPIDDT14                                                         
                                                                                
PPIDDT20 TM    PNVAHCH4,PNVADDCM   DCOM CHANGED?                                
         BZ    PPIDDT30                                                         
         MVC   HALF2,PNVAHPID      BINARY PID FOR LATEST DCOM CHANGE            
         MVC   INVDDCDT,PNVAHDTE   DATE FOR LATEST DCOM CHANGE                  
PPIDDT30 SR    RF,RF                                                            
         ICM   RF,3,PNVAKCSQ                                                    
         AHI   RF,1                BUMP TO NEXT ACTIVITY SEQ #                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(L'PNVAKEY),PNVAKEY                                          
         LA    RE,ELEM                                                          
         STCM  RF,3,PNVAKCSQ                                                    
         B     PPIDDT10                                                         
                                                                                
PPIDDT80 OC    HALF2,HALF2         HAVE BINARY PID?                             
         BZ    PPIDDT90                                                         
         BRAS  RE,GETPID                                                        
         MVC   INVDDCPI,DUB2       PID FOR LATEST DCOM CHANGE                   
                                                                                
PPIDDT90 XC    ELEM,ELEM           RESTORE MINIO ELEM SEQ                       
         MVC   ELEM(L'PNVHKEY),WORK2                                            
         GOTOR MGETEL,ELEM                                                      
                                                                                
PPIDDT_X J     EXIT                                                             
                                                                                
         DROP  RB,R5,R3,RE                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Process invoice summary values                                      *         
***********************************************************************         
*                                                                               
PRCIVSMY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,AINVVAL                                                       
         USING INVVALSD,R5                                                      
*                                                                               
         CLI   DL_TYPE,DL_PRBQ                                                  
         JE    PIVSMY_X            Probe download, skip                         
         OC    INVINSKY,INVINSKY                                                
         JZ    PIVSMY_X            No buy serial key, skip                      
         GOTOR GETBUY,INVINSKY                                                  
         JNE   PIVSMY_X            Buy not found, skip                          
         L     R2,IOADDR                                                        
         TM    (PBDCNDA-PBUYKEY)(R2),X'80'                                      
         JZ    PIVSMY_X            Buy is not Canadian, skip                    
         TM    (PBUYCNTL-PBUYKEY)(R2),X'80'                                     
         JNZ   PIVSMY_X            Buy is deleted, skip                         
*                                                                               
         GOTOR VGETINS,DMCB,0(R2),PVALUES,7(R2),(C'F',0),=C'PST',0              
         L     RE,16(R1)                                                        
         USING GVALUES,RE                                                       
         L     RF,GSTTAX            Get GST tax amount                          
         ICM   R1,15,INVTWTAX                                                   
         AR    R1,RF                Add GST tax to total with taxes             
         STCM  R1,15,INVTWTAX                                                   
         SR    R1,R1                Init for PST tax amount                     
         LA    RF,PSTAREA                                                       
         LHI   R0,10                10 sets of PST codes                        
         A     R1,(PSTTAX-PSTAREA)(RF)                                          
         LA    RF,PSTAREAL(RF)                                                  
         JCT   R0,*-8                                                           
         ICM   RF,15,INVTWTAX                                                   
         AR    RF,R1                                                            
         STCM  RF,15,INVTWTAX       Add PST tax to total with taxes             
         DROP  RE                                                               
*                                                                               
         GOTOR VGETINS,DMCB,0(R2),PVALUES,7(R2),0,0,0                           
         SR    RE,RE                                                            
         A     RE,PYABLE           Gross-Comm-CD                                
         S     RE,PAID                                                          
         ICM   RF,15,INVTWTAX                                                   
         AR    RF,RE                                                            
         STCM  RF,15,INVTWTAX                                                   
*                                                                               
PIVSMY_X J     EXIT                                                             
*                                                                               
         DROP  RB,R5,R3                                                         
         EJECT                                                                  
                                                                                
                                                                                
***********************************************************************         
* LOOK UP INVOICE ELEMENTS IN INSERTION RECORD AND FORMAT THEM        *         
***********************************************************************         
                                                                                
SETSREPT NTR1  BASE=*,LABEL=*      SET SPECAIL REP TABLE                        
                                                                                
         OC    WORK+L'INVMED(L'INVSREP),WORK+L'INVMED                           
         JZ    EXIT                                                             
         CLC   WORK+L'INVMED(L'INVSREP),SPACES                                  
         JE    EXIT                                                             
                                                                                
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     R4,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R4                                                      
                                                                                
         LA    RE,SREPTAB                                                       
         LHI   RF,1                TABLE ENTRIES COUNTER                        
                                                                                
SETSRE20 CHI   RF,SREPTMXQ         TABLE MAX REACHED?                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLC   1(L'SREPKREP,RE),WORK+L'INVMED                                   
         JE    EXIT                                                             
         OC    0(SREPTABL,RE),0(RE)                                             
         BZ    *+16                                                             
         AHI   RF,1                                                             
         LA    RE,SREPTABL(RE)     POINT TO NEXT TABLE ENTRY                    
         B     SETSRE20                                                         
         MVC   0(L'SREPKMED,RE),WORK                                            
         MVC   1(L'SREPKREP,RE),WORK+L'INVMED                                   
                                                                                
         J     EXIT                                                             
         DROP  RB,R1,R4                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* OPTIONALLY FILTER AND FORMAT BUY FOR DOWNLOADING                    *         
*                                                                     *         
* NTRY - R1=ZERO TO FORMAT ONLY, 1 TO FILTER AND FORMAT               *         
* EXIT - CC=NOT EQUAL IF BUY DIDN'T PASS A FILTER TEST                *         
***********************************************************************         
                                                                                
FBUY     NTR1  BASE=*,LABEL=*                                                   
         STC   R1,FLAG             SAVE CALL TYPE                               
         GOTOR XCBUYVAL                                                         
                                                                                
         BRAS  RE,CKBUYORG         CHECK FOR BUY ORIGIN                         
         JNE   EXITN                                                            
                                                                                
         MVI   BUYBSTAT,BUYBSLIV   SET BUY STATUS                               
         CLI   PBDBFD,C'T'         IS THIS A TEST BUY                           
         BNE   *+8                                                              
         MVI   BUYBSTAT,BUYBSTST                                                
         TM    PBDSTAT2,X'40'      STEWARDSHIP INSERTION?                       
         BZ    *+8                                                              
         MVI   BUYBSTAT,BUYBSTEW                                                
         TM    PBUYCNTL,X'80'      IS THIS BUY DELETED                          
         BZ    *+8                                                              
         OI    BUYBSTAT,BUYBSDEL                                                
                                                                                
         CLI   FLAG,0              TEST APPLYING FILTERS                        
         BE    FBUY0030            NO                                           
         BRAS  RE,MODFYFIL         MODIFY FILTERS                               
                                                                                
         CLI   FLTPBOPT,C' '       FILTERING ON PUB OPTION?                     
         BNH   FBUY01_6                                                         
         CLI   FLTPBOPT,FLTPOBAS   ONLY WANT BASE PUB?                          
         BNE   *+14                                                             
         OC    PBUYKZON(L'PBUYKZON+L'PBUYKEDT),PBUYKZON                         
         JNZ   EXITN                                                            
         CLI   FLTPBOPT,FLTPOZON   PUB WITH ZONE AND/OR EDITIONS?               
         BNE   *+14                                                             
         OC    PBUYKZON(L'PBUYKZON+L'PBUYKEDT),PBUYKZON                         
         JZ    EXITN                                                            
                                                                                
FBUY01_6 TM    FILTSTAT,FILTSALL                                                
         BNM   FBUY0005                                                         
                                                                                
         TM    BUYBSTAT,BUYBSLIV   TEST LIVE BUY                                
         BZ    FBUY0002                                                         
         TM    BUYBSTAT,BUYBSDEL   TEST LIVE DELETE                             
         BNZ   *+16                                                             
         TM    FILTSTAT,FILTSLUD   TEST WANT LIVE UNDELETED BUYS                
         JZ    EXITN                                                            
         B     FBUY0005                                                         
         TM    FILTSTAT,FILTSLDE   TEST WANT LIVE DELETED BUYS                  
         JZ    EXITN                                                            
         B     FBUY0005                                                         
                                                                                
FBUY0002 TM    BUYBSTAT,BUYBSTST   TEST TEST BUY                                
         BZ    FBUY0004                                                         
         TM    BUYBSTAT,BUYBSDEL   TEST TEST DELETE                             
         BNZ   *+16                                                             
         TM    FILTSTAT,FILTSTUD   TEST WANT TEST UNDELETED BUYS                
         JZ    EXITN                                                            
         B     FBUY0005                                                         
         TM    FILTSTAT,FILTSTDE   TEST WANT TEST DELETED BUYS                  
         JZ    EXITN                                                            
         B     FBUY0005                                                         
                                                                                
FBUY0004 TM    BUYBSTAT,BUYBSTEW   STEWARDSHIP BUY?                             
         BZ    FBUY0005                                                         
         TM    BUYBSTAT,BUYBSDEL   STEWARDSHIP DELETED?                         
         BNZ   *+16                                                             
         TM    FILTSTAT,FILTSSUD   WANT STEWARDSHIP UNDELETED BUYS?             
         JZ    EXITN                                                            
         B     FBUY0005                                                         
         TM    FILTSTAT,FILTSSDE   WANT STEWARDSHIP DELETED BUYS                
         JZ    EXITN                                                            
                                                                                
FBUY0005 MVC   WORK2(L'PBDJOB),PBDJOB                                           
         OC    WORK2(L'PBDJOB),SPACES                                           
         OC    FILTADNO,FILTADNO   TEST AD NUMBER FILTER SET                    
         BZ    FBUY0006                                                         
         CLC   WORK2(L'PBDJOB),FILTADNO                                         
         JNE   EXITN                                                            
                                                                                
FBUY0006 MVI   BYTE2,0                                                          
         TM    FLTCLRST,FLTCSCLR+FLTCSNCL                                       
         BNM   FBUY0016                                                         
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
FBUY0008 CLI   0(R3),0             END OF BUY RECORD?                           
         BE    FBUY0012                                                         
         USING PPAYELEM,R3                                                      
         CLI   PPAYELEM,PPAYELQ    TEST PAYMENT ELEMENT                         
         BNE   FBUY0010                                                         
         OC    PPDDATE,PPDDATE                                                  
         BZ    FBUY0010                                                         
         MVI   BYTE2,C'C'          STATUS IS CLEARED                            
         B     FBUY0012                                                         
FBUY0010 SR    R0,R0               BUMP TO NEXT ELEMENT ON RECORD               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FBUY0008                                                         
FBUY0012 CLI   BYTE2,C'C'          CLEARED?                                     
         BNE   FBUY0014                                                         
         TM    FLTCLRST,FLTCSCLR   FILTER WANTS CLEARED?                        
         JZ    EXITN                                                            
         B     FBUY0016                                                         
FBUY0014 TM    FLTCLRST,FLTCSNCL   FILTER WANTS NOT CLEARED?                    
         JZ    EXITN                                                            
                                                                                
FBUY0016 CLI   DL_TYPE,DL_IMRQ     INVOICE MATCH REPORT DL (DRR2)?              
         BNE   FBUY0017                                                         
         MVI   INVDLSW,YESQ        SET TO DOWNLOAD INVOICES                     
         MVI   DL_TYPE,DL_DRRQ     SET TO DRR DOWNLOAD                          
         TM    FLTMATST,FLTMSPEN+FLTMSMAT+FLTMSDIS                              
         BZ    FBUY0026                                                         
         BM    FBUY0017                                                         
         OI    FLTMATST,FLTMSINV   SET TO DL INSERTIONS WITH INVOICES           
         B     FBUY0026                                                         
                                                                                
FBUY0017 TM    FLTMATST,FLTMSPEN+FLTMSMAT+FLTMSDIS                              
         BNM   FBUY0026                                                         
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
FBUY0018 CLI   0(R3),0             END OF BUY RECORD?                           
         BNE   *+16                                                             
         TM    FLTMATST,FLTMSPSI   PLUS NO INVOICES?                            
         BNZ   FBUY0026                                                         
         J     EXITN               INVOICE MATCHING ELEM NOT FOUND              
         USING PBMATELD,R3                                                      
         CLI   PBMATELM,PBMATELQ   INVOICE MATCHING STATUSES ELEM?              
         BE    FBUY0020                                                         
FBUY0019 SR    R0,R0               BUMP TO NEXT ELEMENT ON RECORD               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FBUY0018                                                         
FBUY0020 CLI   PBMTSTAT,PBMTSPDQ   PENDING?                                     
         BNE   FBUY0022                                                         
         TM    FLTMATST,FLTMSPEN   FILTER WANTS PENDING?                        
         JZ    EXITN                                                            
         B     FBUY0026                                                         
FBUY0022 CLI   PBMTSTAT,PBMTSMTQ   MATCHED?                                     
         BNE   FBUY0024                                                         
         TM    FLTMATST,FLTMSMAT   FILTER WANTS MATCHED?                        
         JZ    EXITN                                                            
         B     FBUY0026                                                         
FBUY0024 CLI   PBMTSTAT,PBMTSDSQ   DISCREPANT?                                  
         BNE   FBUY0019            TRY NEXT ELEM                                
         TM    FLTMATST,FLTMSDIS   FILTER WANTS DISCREPANT?                     
         JZ    EXITN                                                            
                                                                                
FBUY0026 TM    FLTMATST,FLTMSNOI   NO INVOICES?                                 
         BZ    *+12                                                             
         BRAS  RE,CKINSINV         CK FOR INVOICE DATA IN INSERTION             
         JE    EXITN                                                            
                                                                                
         TM    FLTMATST,FLTMSINV   WITH INVOICES?                               
         BZ    *+12                                                             
         BRAS  RE,CKINSINV         CK FOR INVOICE DATA IN INSERTION             
         JNE   EXITN                                                            
                                                                                
         OC    FILTSREP,FILTSREP   SPECIAL REP FILTER PRESENT?                  
         BZ    *+12                                                             
         BRAS  RE,CKSREP           CK FOR SPECIAL REP                           
         JNE   EXITN                                                            
                                                                                
         TM    FLTINFLT,FLTNOMSD   NO MAT CLOSING OR SPACE CLOSING?             
         BZ    *+10                                                             
         OC    PBDMDATE,PBDMDATE   HAVE MATERIAL CLOSING DATE?                  
         BZ    *+10                                                             
         OC    PBDCDATE,PBDCDATE   HAVE SPACE CLOSING DATE?                     
         BZ    *+8                                                              
         J     EXITN                                                            
                                                                                
         BRAS  RE,CKINSPUB         CK FOR INSERTION PUB'S NATIONALITY           
         JNE   EXITN                                                            
                                                                                
FBUY0028 DS    0X                  FOR FUTURE FILTERS                           
                                                                                
FBUY0030 GOTOR VPUBEDIT,DMCB,(X'08',PBUYKPUB),(C'S',BUYPUB)                     
                                                                                
         BRAS  RE,SETFXVAL         SET FOREIGN EXCHANGE VALUES                  
                                                                                
* * * *  GOTOR VGETINS,DMCB,PBUYRECD,PVALUES,PBUYKPRD,(C'A',0),,ALL             
                                                                                
         GOTOR VGETINS,DMCB,PBUYRECD,PVALUES,PBUYKPRD,(C'F',0),0,ALL            
                                                                                
         MVC   BUYGRSAC,GROSS      EXTRACT GROSS ADDITIONAL CHARGES             
         L     R1,GROSS                                                         
         S     R1,AGYCOM                                                        
         STCM  R1,15,BUYNETAC      SET NET ADDITIONAL CHARGES                   
         MVC   BUYGLCAC,BLABLE     EXTRACT BILLABLE ADDITIONAL CHARGES          
         MVC   BUYNLCAC,PYABLE     EXTRACT PAYABLE ADDITIONAL CHARGES           
                                                                                
* * * *  GOTOR VGETINS,DMCB,PBUYRECD,PVALUES,PBUYKPRD,(0,0)                     
                                                                                
         GOTOR VGETINS,DMCB,PBUYRECD,(C'O',PVALUES),PBUYKPRD,0,0,0              
         MVC   BYC2GRSB,BGROSS     Set COS2 gross billed                        
         L     R1,BGROSS                                                        
         S     R1,BAGYCOM                                                       
         STCM  R1,15,BYC2NETB      Set COS2 net billed                          
         MVC   BYC2GCDB,BILLED     Set COS2 gross less CD billed                
         S     R1,BCSCHDSC                                                      
         STCM  R1,15,BYC2NCDB      Set COS2 net less CD billed                  
                                                                                
         GOTOR VGETINS,DMCB,PBUYRECD,PVALUES,PBUYKPRD,(C'F',0),0,0              
                                                                                
         L     R1,GROSS                                                         
         S     R1,AGYCOM                                                        
                                                                                
         STCM  R1,15,BUYNETOR      SET NET ORDERED                              
                                                                                
         L     R1,PGROSS                                                        
         S     R1,PAGYCOM                                                       
         STCM  R1,15,BUYNETPD      SET NET PAID                                 
                                                                                
         S     R1,PCSHDSC                                                       
         STCM  R1,15,BUYPLNCD      SET PAID NET OF CASH DISCOUNT                
                                                                                
         L     R1,BGROSS                                                        
         S     R1,BAGYCOM                                                       
         STCM  R1,15,BUYNETBD      SET NET BILLED                               
                                                                                
         S     R1,BCSCHDSC                                                      
         STCM  R1,15,BUYBLNCD      SET BILLED NET OF CASH DISCOUNT              
                                                                                
         MVC   WORK2(20),GROSS     SAVE DALLOR AMOUNTS                          
         TM    BUYBSTAT,BUYBSDEL   INSERTION IS DELETED?                        
         BZ    *+10                                                             
         XC    GROSS(20),GROSS     SET GROSS TO ZERO                            
                                                                                
         ICM   RE,15,GROSS                                                      
         ICM   RF,15,PGROSS                                                     
         SR    RE,RF                                                            
         STCM  RE,15,BUYGPYBL      GROSS-SUM(PPGROSS)                           
                                                                                
         ICM   RE,15,PYABLE                                                     
         ICM   RF,15,CSHDSC                                                     
         AR    RE,RF                                                            
         ST    RE,FULL1                                                         
         ICM   RE,15,PGROSS                                                     
         ICM   RF,15,PAGYCOM                                                    
         SR    RE,RF                                                            
         L     RF,FULL1                                                         
         SR    RF,RE                                                            
         STCM  RF,15,BUYNPYBL      PYABLE+CSHDSC-SUM(PPGROSS-PPAGYCOM)          
                                                                                
         MVC   GROSS(20),WORK2     RESTORE DOLLAR AMOUNTS                       
                                                                                
         LA    R0,PBUYRECD                                                      
         ST    R0,PBYOINPT                                                      
         LA    R0,PVALUES                                                       
         ST    R0,PBYOVALS                                                      
         MVC   PBYODTCN,VDATCON                                                 
         GOTOR VPPBYOUT,DMCB,PPBYOUTD                                           
         MVC   BUYSPACE,PBYOSPC1   SET SPACE/DESCRIPTION                        
                                                                                
         CLI   PBUYKMED,ODORMEDQ   TEST IF OUTDOOR                              
         BNE   FBUY0032                                                         
         CLI   PBDSPACE,FF         YES - TEST FOR SHOWING                       
         BNE   *+10                                                             
         CP    PBDSHOW,PZERO                                                    
         BNE   *+10                                                             
         CP    PBDREG,PZERO                                                     
         BNE   *+10                                                             
         CP    PBDILLUM,PZERO                                                   
         BE    FBUY0032                                                         
         MVC   BUYSPACE,SPACES     YES - CLEAR SPACE/DESCRIPTION                
                                                                                
FBUY0032 CLI   PBUYKMED,NEWSMEDQ                                                
         BNE   *+8                                                              
         GOTOR FMTNSD              FORMAT NEWSPAPER SPACE/DESCRIPTION           
                                                                                
         OC    BUYSPACE,SPACES     ENSURE NO TRAILING BINARY ZEROES             
                                                                                
         CLI   FLAG,0              TEST APPLYING FILTERS                        
         BE    FBUY0034            NO                                           
         OC    FILTDESC,FILTDESC   TEST SPACE/DESCRIPTION FILTER SET            
         BZ    FBUY0034                                                         
         CLC   BUYSPACE,FILTDESC   YES - APPLY FILTER                           
         JNE   EXITN                                                            
                                                                                
FBUY0034 LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
                                                                                
         GOTOR FMTLIN              FORMAT INSERTION DATE/LINE NUMBER            
                                                                                
         CLC   PBUYKPRD,ZZZ        SEE IF ZZZ INSERTION                         
         BNE   *+8                                                              
         GOTOR FMTPRD              FORMAT ALLOCATIONS                           
                                                                                
         MVC   BUYCDPCT,PBDCD      SET CASH DISCOUNT PERCENTAGE                 
         ZAP   BUYACP,PBDACP       SET AGENCY COMMISSION PERCENT                
         CP    BUYACP,=P'-1'       TEST SPECIAL VALUE FOR 100%                  
         BNE   *+10                                                             
         ZAP   BUYACP,=P'100000'                                                
                                                                                
         TM    PBDSTAT,X'10'       TEARSHEET RECEIVED?                          
         BZ    *+8                                                              
         MVI   BUYTSRST,YESQ       YES                                          
                                                                                
         MVC   BUYRTCOD,PBDRCODE   RATE CODE                                    
                                                                                
         MVC   BUYSFH,SPACES                                                    
         TM    PBDSTAT,X'0C'       TEST SPECIAL FINANCIAL HANDLING              
         BZ    FBUY0036                                                         
         MVC   BUYSFH,=C'HOLD'                                                  
         TM    PBDSTAT,X'08'                                                    
         BNZ   FBUY0036                                                         
         MVC   BUYSFH,=C'REL '                                                  
                                                                                
FBUY0036 CLI   PBDSPACE,FF         TEST OUTDOOR                                 
         BNE   FBUY0038                                                         
         ZAP   BUYREGDS,PBDREG     SET NUMBER OF REGULAR DISPLAYS               
         ZAP   BUYILLPA,PBDILLUM   SET NUMBER OF ILLUMINATED DISPLAYS           
         MVC   BUYSHOWS,SPACES                                                  
         CP    PBDSHOW,=P'99999'   SPECIAL VALUE FOR SPC (SPECIALS)             
         BNE   *+14                                                             
         MVC   BUYSHOWS(3),=C'SPC'                                              
         B     FBUY0038                                                         
         EDITR PBDSHOW,(5,BUYSHOWS),ALIGN=LEFT                                  
                                                                                
FBUY0038 GOTOR FMT_COST,1          FORMAT RATE                                  
         MVC   BUYRATE,TEMP2+(FC_OCOST-F_COST_D)                                
                                                                                
         CLI   PBUYKMED,NEWSMEDQ   TEST NEWSPAPERS                              
         BNE   FBUY0042                                                         
         GOTOR FMTNPR              FORMAT NEWSPAPER PREMIUMS                    
         GOTOR FMTNCL              FORMAT NEWSPAPER CONTRACT LINEAGE            
                                                                                
FBUY0042 CLI   0(R3),0             TEST END OF RECORD                           
         BE    FBUY0080                                                         
                                                                                
         USING PSERELEM,R3                                                      
         CLI   PSERELEM,PSEREQ     TEST SERIAL NUMBER ELEMENT                   
         BNE   FBUY0044                                                         
         MVC   BUYSMED,PBUYKMED    YES - BUILD SERIAL NUMBER                    
         MVC   BUYSCLT,PBUYKCLT                                                 
         UNPK  BUYSSER,PSERNUM                                                  
         OI    BUYSSER+L'BUYSSER-1,X'F0'                                        
         B     FBUY0078                                                         
                                                                                
         USING PBINVELM,R3                                                      
FBUY0044 CLI   PBINVELM,PBINVEQ    TEST INVOICE ELEMENT                         
         BNE   FBUY0046                                                         
         MVC   BUYMCHIN,PBINVNUM   SET PAID INVOICE NUMBER                      
         MVC   BUYMCHDT,PBINVMDT   SET MATCHED DATE                             
         B     FBUY0078                                                         
                                                                                
         USING PPAYELEM,R3                                                      
FBUY0046 CLI   PPAYELEM,PPAYELQ    TEST PAYMENT ELEMENT                         
         BNE   FBUY0048                                                         
         OC    PPDDATE,PPDDATE                                                  
         BZ    FBUY0078                                                         
         STCM  R3,15,BUYAPAYE      SAVE A(PAYMENT ELEMENT)                      
         MVI   BUYCLRST,C'C'       CLEARED IF PAY ELEM W/ DATE IS FOUND         
         B     FBUY0078                                                         
                                                                                
         USING PBILELEM,R3                                                      
FBUY0048 CLI   PBILELEM,PBILELQ    Normal billing element?>                     
         BE    *+12                                                             
         CLI   PBILELEM,PORBELQ    Open rate billing element?                   
         BNE   FBUY0050                                                         
         OC    PBLDATE,PBLDATE     Have billing date?                           
         BZ    FBUY0078                                                         
         STCM  R3,15,BUYABILE      Save A(BILLING ELEMENT)                      
         B     FBUY0078                                                         
                                                                                
         USING PACELEM,R3                                                       
FBUY0050 CLI   PACELEM,PACELQ      TEST ADDITIONAL CHARGE ELEMENT               
         BNE   FBUY0062                                                         
                                                                                
         SR    R5,R5               ADD ENTRY TO ADDITIONAL CHARGE TABLE         
         ICM   R5,3,BUYATAB#                                                    
         LA    RE,1(R5)                                                         
         STCM  RE,3,BUYATAB#       SET N'TABLE ENTRIES                          
         MHI   R5,BUYATABL                                                      
         LA    R5,BUYATAB(R5)                                                   
         USING BUYATAB,R5                                                       
         MVC   BUYATCOD,PACCODE    SET ADDITIONAL CHARGE CODE                   
                                                                                
         CLI   PACGN,0                                                          
         BNE   *+14                                                             
         OC    PACAMT,PACAMT                                                    
         BZ    FBUY0060            NO AMT PRESENT                               
                                                                                
         MVC   BUYATCRG(1),PACGN                                                
         CLI   PACGN,C'G'          GROSS AMOUNT?                                
         BE    FBUY0054                                                         
         CLI   PACGN,C'N'          NET AMOUNT?                                  
         BE    FBUY0056                                                         
         DC    H'0'                NO OTHER TYPE OF AMT FOR NOW                 
                                                                                
FBUY0054 ZAP   DUB2,PACAMT         AMT IN GROSS, NO CALCULATIONS NEEDED         
         B     FBUY0058                                                         
                                                                                
FBUY0056 CLI   PACAC,C'N'          SUBJECT TO COMMISSION?                       
         BE    FBUY0054            NO, NET AND GROSS ARE SAME                   
                                                                                
         ZAP   FULL1,BUYACP        DEFAULT IS TO USE BUY AGE COMM               
         OC    PACACOM,PACACOM     ADDTNL CHRGS AGY COMM IS ZERO?               
         BZ    *+10                                                             
         ZAP   FULL1,PACACOM       NO - USE IT INSTEAD                          
         ZAP   WORK(12),PACAMT                                                  
         ZAP   DUB2(4),=P'100000'                                               
         SP    DUB2(4),FULL1       NET PCT                                      
         MP    WORK(12),DUB2(4)                                                 
         SRP   WORK(12),64-5,5                                                  
         ZAP   DUB2,WORK(12)                                                    
                                                                                
FBUY0058 EDITR (P8,DUB2),(11,BUYATCRG+1),2,ALIGN=LEFT,FLOAT=-,IZERO=Y           
                                                                                
FBUY0060 MVC   BUYATCPC,PACACOM    SET COMMISSION %                             
         MVC   BUYATCOM,PACAC      SUBJECT TO COMMISSION                        
         MVC   BUYATSCD,PACCD      SUBJECT TO CASH DISCOUNT                     
         B     FBUY0078                                                         
         DROP  R5                                                               
                                                                                
         USING PCOMEL,R3                                                        
FBUY0062 LA    R1,BUYREGC#                                                      
         CLI   PCOMEL,PRGCELQ      REGULAR COMMENTS                             
         BE    *+12                                                             
         LA    R1,BUYINSC#                                                      
         CLI   PCOMEL,PIOCELQ      TEST INSERTION ORDER COMMENTS                
         BE    *+12                                                             
         LA    R1,BUYPOSI#                                                      
         CLI   PCOMEL,PPOCELQ      TEST POSITION COMMENTS                       
         BE    *+12                                                             
         LA    R1,BUYTSHC#                                                      
         CLI   PCOMEL,PTSHELQ      TEST TEARSHEET COMMENTS                      
         BE    *+12                                                             
         LA    R1,BUYSRCM#                                                      
         CLI   PCOMEL,PSRCELQ      TEST SPECIAL REMITTANCE COMMENTS             
         BNE   FBUY0064                                                         
                                                                                
         SR    RE,RE                                                            
         IC    RE,0(R1)            RE=N'COMMENT LINES SO FAR                    
         CLI   PCOMEL,PTSHELQ      TEARSHEET COMMENTS ELEM?                     
         BNE   *+12                                                             
         CHI   RE,TSCMAXQ          MAX TEARSHEET COMMENT LINES REACHED?         
         BE    FBUY0078                                                         
         CHI   RE,COMMAXQ                                                       
         BE    FBUY0078                                                         
         LA    RF,1(RE)                                                         
         STC   RF,0(R1)            SET N'COMMENT LINES NOW                      
         CLI   PCOMEL,PRGCELQ      REGULAR COMMENTS                             
         BNE   *+12                                                             
         MHI   RE,REGCLNQ                                                       
         LHI   RF,REGCLNQ                                                       
         CLI   PCOMEL,PIOCELQ      INSERTION ORDER COMMENTS?                    
         BNE   *+12                                                             
         MHI   RE,IOCMLNQ                                                       
         LHI   RF,IOCMLNQ                                                       
         CLI   PCOMEL,PPOCELQ      POSITION INSTRUCTION COMMENTS?               
         BNE   *+12                                                             
         MHI   RE,PICMLNQ                                                       
         LHI   RF,PICMLNQ                                                       
         CLI   PCOMEL,PTSHELQ      TEARSHEET COMMENTS?                          
         BNE   *+12                                                             
         MHI   RE,TSCMLNQ                                                       
         LHI   RF,TSCMLNQ                                                       
         CLI   PCOMEL,PSRCELQ      SPECIAL REMITTANCE COMMENTS?                 
         BNE   *+12                                                             
         MHI   RE,SRCMLNQ                                                       
         LHI   RF,SRCMLNQ                                                       
         LA    RE,1(RE,R1)         RE=A(COMMENT LINE)                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SPACES      CLEAR COMMENT LINE                           
         SR    RF,RF                                                            
         IC    RF,PCOMLEN                                                       
         SHI   RF,PCOMLN1Q+1                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),PCOMMENT    MOVE COMMENT TO OUTPUT AREA                  
         B     FBUY0078                                                         
                                                                                
         USING PIOELEM,R3                                                       
FBUY0064 CLI   PIOELEM,PIOELQ      TEST INSERTION ORDER ELEMENT                 
         BNE   *+22                                                             
         OC    PIODATE,PIODATE                                                  
         BZ    FBUY0078                                                         
         STCM  R3,15,BUYAIORE      SAVE A(INSERTION ORDER ELEMENT)              
         B     FBUY0078                                                         
                                                                                
         USING PWIOELEM,R3                                                      
         CLI   PWIOELEM,PWIOELCQ   TEST WEB INSERTION ORDER ELEMENT             
         BNE   *+22                                                             
         OC    PWIODATE,PWIODATE                                                
         BZ    FBUY0078                                                         
         STCM  R3,15,BUYAWIOE      SAVE A(WEB INSERTION ORDER ELEM)             
         B     FBUY0078                                                         
                                                                                
         USING PESRELEM,R3                                                      
         CLI   PESRELCO,PESRELCQ   ESR ELEM?                                    
         BNE   *+22                                                             
         OC    PESRDATE,PESRDATE                                                
         BZ    FBUY0078                                                         
         STCM  R3,15,BUYAESRE      SAVE A(ESR ELEM)                             
         B     FBUY0078                                                         
                                                                                
         USING PCOS2FEL,R3                                                      
         CLI   PCOS2FEL,PC2FACTQ   COS2 FACTOR ELEM CODE?                       
         BNE   FBUY0066                                                         
         OC    PCOS2FAC,PCOS2FAC                                                
         BZ    FBUY0078                                                         
         CP    PCOS2FAC,PZERO                                                   
         BE    FBUY0078                                                         
         EDITR PCOS2FAC,BUYCOS2F,6,FLOAT=-,ALIGN=LEFT                           
         LA    RE,PVALUESX-PVALUES                                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM2(0),PVALUES                                                 
         GOTOR VGETINS,DMCB,PBUYRECD,(C'O',PVALUES),PBUYKPRD,0,0,0              
         L     R1,GROSS                                                         
         STCM  R1,15,BUYC2GRS      SET COS2 FACTOR GROSS AMOUNT                 
         S     R1,AGYCOM                                                        
         STCM  R1,15,BUYC2NET      SET COS2 FACTOR NET AMOUNT                   
         LA    RE,PVALUESX-PVALUES                                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PVALUES(0),ELEM2    RESTORE PVALUES                              
         B     FBUY0078                                                         
                                                                                
         USING PICONELD,R3                                                      
FBUY0066 CLI   PICONEL,PICONEQ     TEST INTERNET CONTRACT ELEMENT               
         BNE   FBUY0072                                                         
         MVC   BUYICNUM,PICCONN    CONTRACT NUMBER                              
         MVC   BUYICIMP,PICIMPS    TOTAL IMPRESSIONS                            
         MVC   BUYICCPM,PICCPM     TOTAL CPM                                    
         MVC   WORK2(L'BUYICRAT),SPACES                                         
         EDITR PICRATE,(10,WORK2),2,ALIGN=LEFT                                  
         CLI   PICRIND,C'N'        NET INDICATOR                                
         BNE   FBUY0068                                                         
         MVC   BUYICRAT(L'PICRIND),PICRIND                                      
         MVC   BUYICRAT+L'PICRIND(L'BUYICRAT-L'PICRIND),WORK2                   
         B     FBUY0070                                                         
                                                                                
FBUY0068 MVC   BUYICRAT,WORK2                                                   
FBUY0070 MVC   BUYICINS,PICINS     NUMBER OF INSERTIONS                         
         B     FBUY0078                                                         
                                                                                
         USING PPAGEVEL,R3                                                      
FBUY0072 CLI   PPAGEVEL,PPAGEVQ    TEST PAGE VIEWS ELEMENT                      
         BNE   *+14                                                             
         MVC   BUYVIEWS,PPAGEVS    SET NUMBER OF PAGE VIEWS                     
         B     FBUY0078                                                         
                                                                                
         USING PBSHPDEL,R3                                                      
         CLI   PBSHPDEL,PBSHPDEQ   TEST SHIP DATE ELEMENT                       
         BNE   *+14                                                             
         MVC   BUYSHPDT,PBSHDATE   SET SHIP DATE                                
         B     FBUY0078                                                         
                                                                                
         USING PBSREPEL,R3                                                      
         CLI   PBSREPEL,PBREPEQ    TEST SPECIAL REP ELEMENT                     
         BNE   *+14                                                             
         MVC   BUYSREP,PBSREP      SET SPECIAL REP                              
         B     FBUY0078                                                         
                                                                                
         USING PCLCKTEL,R3                                                      
         CLI   PCLCKTEL,PCLCKTEQ   TEST CLICK THRUS ELEMENT                     
         BNE   *+14                                                             
         MVC   BUYCLICK,PCLCKTS                                                 
         B     FBUY0078                                                         
                                                                                
         USING PBREFEL,R3                                                       
         CLI   PBREFEL,PBREFELQ    TEST REFERENCE NUMBER ELEMENT                
         BNE   *+14                                                             
         MVC   BUYREFNO,PBREFNO    SET REFERENCE NUMBER                         
         B     FBUY0078                                                         
                                                                                
         USING PBDECEL,R3                                                       
         CLI   PBDECEL,PBDECELQ    TEST DAILY EFF. CIRCULATION ELEMENT          
         BNE   *+14                                                             
         MVC   BUYDECIR,PBDEC      SET NUMBER OF FREE STANDING INSERTS          
         B     FBUY0078                                                         
                                                                                
         USING PBRPTEL,R3                                                       
         CLI   PBRPTEL,PBRPTELQ    TEST NUMBER OF REPAINTS ELEMENT              
         BNE   *+14                                                             
         MVC   BUYREPTS,PBRPTNO    SET NUMBER OF FREE STANDING INSERTS          
         B     FBUY0078                                                         
                                                                                
         USING PIMPRSEL,R3                                                      
         CLI   PIMPRSEL,PIMPRSEQ   TEST ESTIMATED IMPRESSIONS ELEMENT           
         BNE   *+14                                                             
         MVC   BUYEIMPS,PIMPRS     SET NUMBER OF ESTIMATED IMPRESSIONS          
         B     FBUY0078                                                         
                                                                                
         USING PAIMSPEL,R3                                                      
         CLI   PAIMSPEL,PAIMSPEQ   TEST ACTUAL IMPRESSIONS ELEMENT              
         BNE   *+14                                                             
         MVC   BUYAIMPS,PAIMPRS    SET NUMBER OF ACTUAL IMPRESSIONS             
         B     FBUY0078                                                         
                                                                                
         USING PECPMELD,R3                                                      
         CLI   PECPMEL,PECPMEQ     ESTIMATED CPM                                
         BNE   *+14                                                             
         MVC   BUYECPM,PECPM                                                    
         B     FBUY0078                                                         
                                                                                
         USING PACPMELD,R3                                                      
         CLI   PACPMEL,PACPMEQ     ACTUAL CPM                                   
         BNE   *+14                                                             
         MVC   BUYACPM,PACPM                                                    
         B     FBUY0078                                                         
                                                                                
         USING PWSJELD,R3                                                       
         CLI   PWSJELEM,PWSJEQ     WSJ INSERTION                                
         BNE   *+12                                                             
         MVI   BUYWSJ,YESQ                                                      
         B     FBUY0078                                                         
                                                                                
         USING PEXDAYEL,R3                                                      
         CLI   PEXDAYEL,PEXDAYEQ   MATERIALS CLOSING EXTENSION DAYS             
         BNE   *+14                                                             
         MVC   BUYMCXDY,PEXDAYS                                                 
         B     FBUY0078                                                         
                                                                                
         USING PEXDATEL,R3                                                      
         CLI   PEXDATEL,PEXDTELQ   MATERIALS CLOSING EXTENSION DATE             
         BNE   *+14                                                             
         MVC   BUYMCLXD,PEXDATE                                                 
         B     FBUY0078                                                         
                                                                                
         USING PISITEEL,R3                                                      
         CLI   PISITEEL,PISITELQ   INTERNET SITE LOCATION                       
         BNE   *+14                                                             
         MVC   BUYISITE,PISITE                                                  
         B     FBUY0078                                                         
                                                                                
         USING PISNMELM,R3                                                      
         CLI   PISNMELM,PISNMELQ   ISSUE NAME?                                  
         BNE   *+14                                                             
         MVC   BUYISSNM,PISNAME                                                 
         B     FBUY0078                                                         
                                                                                
         USING PIUPEL,R3                                                        
         CLI   PIUPEL90,PIUPELEQ   PBU UPLOAD ELEM?                             
         JNE   *+28                                                             
         MVC   BUYUPLID(8),PIUPUSEQ                                             
         CLI   PIUPELLN,PIUPELXQ   Have extended unique sequence#?              
         JL    *+14                                                             
         MVC   BUYUPLID+8(7),PIUPUQXT                                           
         B     FBUY0078                                                         
                                                                                
         USING PBMATELD,R3                                                      
         CLI   PBMATELM,PBMATELQ   INVOICE MATCHING STATUSES ELEM?              
         BNE   *+20                                                             
         MVC   BUYMATST,PBMTSTAT   INVOICE MATCHING STATUS                      
         MVC   BUYDISST,PBMTDSTA   INVOICE MATCHING DISCREPANCY STATUS          
         B     FBUY0078                                                         
                                                                                
         USING PBYPOELD,R3                                                      
         CLI   PBYPOELC,PBYPOELQ   PURCHASE ORDER NUMBER ELEM?                  
         BNE   *+22                                                             
         TM    PBYPOSTA,BYPOZZZQ   PURCHASE ORDER # FOR ZZZ BUY?                
         BNZ   *+14                                                             
         MVC   BUYPOSQ#,PBYPOSQ#   PURCHASE ORDER SEQUENCE NUMBER               
         B     FBUY0078                                                         
                                                                                
         USING PBYMELMD,R3                                                      
         CLI   PBYMELCO,PBYMELCQ   BUY MOVE ELEM?                               
         BNE   *+32                                                             
         TM    PBYMSTAT,PBYMFROQ   INSERTION MOVED "FROM"?                      
         BZ    *+14                                                             
         ZAP   BUYS#FRO,PBYMSER#   BUY SERIAL# OF "FROM" INSERTION              
         B     FBUY0078                                                         
         ZAP   BUYS#_TO,PBYMSER#   BUY SERIAL# OF "TO" INSERTION                
         B     FBUY0078                                                         
                                                                                
         BRAS  RE,LASTBYAC         SEARCH FOR LATEST BUY ACTIVITIES             
         BE    FBUY0078                                                         
                                                                                
         BRAS  RE,BUYORC2$         Open Rate (COS2 $) element                   
         BE    FBUY0078                                                         
                                                                                
         USING PTSHTEL,R3                                                       
         CLI   PTSHTEL,PTSHTELQ    TEST TEARSHEET ELEMENT                       
         BNE   FBUY0074                                                         
         MVC   BUYPAGEN,PTSHPAGE   SET PAGE NOTATION                            
         MVC   BUYTAPPR,PTSHSTAT   SET TEARSHEET APPROVAL                       
         MVC   BUYREPRO,PTSHREPO   SET REPRODUCTION QUALITY                     
         MVC   BUYTSTAT,PTSHIND1                                                
         B     FBUY0078                                                         
                                                                                
         USING PBYPSTEL,R3                                                      
FBUY0074 CLI   PBYPSTEL,PBYPSTEQ                                                
         BNE   FBUY0076                                                         
         USING PSTBLKD,WORKAREA                                                 
         XC    PSTBLKD(PSTLNQ),PSTBLKD                                          
         MVI   PSTACT,PSTFMTQ                                                   
         LA    R1,PBYPSTC                                                       
         ST    R1,PSTADIN          ADDRESS OF DATA                              
         LA    R1,WORK                                                          
         ST    R1,PSTADOUT                                                      
         GOTOR VPSTVAL,DMCB,PSTBLKD                                             
         MVC   BUYPST,WORK                                                      
         B     FBUY0078                                                         
                                                                                
         USING PBFSIEL,R3                                                       
FBUY0076 CLI   PBFSIEL,PBFSIELQ    FREE STANDING INSERTS                        
         BNE   FBUY0078                                                         
         MVC   BUYFSINS,SPACES                                                  
         LA    R5,BUYFSINS                                                      
         CLI   PBFSIIND,X'01'      Looked-up value from FSI record?             
         JNE   *+12                                                             
         LA    R5,BUYFSINS+1                                                    
         MVI   BUYFSINS,C'L'       To indicate the value is looked-up           
         EDITR PBFSI,(9,0(R5)),ALIGN=LEFT                                       
         B     FBUY0078                                                         
                                                                                
FBUY0078 SR    R0,R0               BUMP TO NEXT ELEMENT ON RECORD               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FBUY0042                                                         
                                                                                
FBUY0080 BRAS  RE,SETPYBLE         SET PAYABLE STATUS                           
         BRAS  RE,SETACSTA         SET ADDITIONAL CHARGE STATUS                 
         CLI   FLAG,0              TEST APPLYING FILTERS                        
         BE    FBUY0081            NO                                           
         OC    FILTICNO,FILTICNO   TEST INTERNET CONTRACT# FILTER SET           
         BZ    *+14                                                             
         CLC   BUYICNUM,FILTICNO   YES - APPLY FILTER                           
         JNE   EXITN                                                            
         TM    FLTCLRST,FLTPAYBL+FLTNPYBL                                       
         BNM   FBUY0081                                                         
         TM    BUYSTAT2,BYS2PYBL   INSERTION IS PAYABLE?                        
         BZ    *+12                                                             
         TM    FLTCLRST,FLTPAYBL   FILTER WANTS PAYABLE?                        
         JZ    EXITN                                                            
         TM    BUYSTAT2,BYS2NPYB   INSERTION IS NON-PAYABLE?                    
         BZ    *+12                                                             
         TM    FLTCLRST,FLTNPYBL   FILTER WANTS NON-PAYABLE?                    
         JZ    EXITN                                                            
                                                                                
FBUY0081 ICM   R3,15,BUYABILE      FORMAT BILLING VALUES                        
         BZ    FBUY0082                                                         
         USING PBILELEM,R3                                                      
         MVC   BUYBINVD,PBLDATE    SET BILLED DATE                              
         GOTOR FMTINO,DMCB,(PBUYKMED,PBILELEM),BUYBINVN                         
                                                                                
FBUY0082 ICM   R3,15,BUYAPAYE      FORMAT PAYMENT VALUES                        
         BZ    FBUY0084                                                         
         USING PPAYELEM,R3                                                      
         GOTOR FMTPYE,DMCB,PPAYELEM,BUYPREP                                     
         MVC   BUYPAYDT,PPDDATE    SET PAID DATE                                
         MVC   BUYPAYSQ,PPDSEQNO   SET PAYMENT SEQUENCE NUMBER                  
         MVC   WORK(L'BUYPAYDT),BUYPAYDT                                        
         MVC   WORK+L'BUYPAYDT(L'BUYPAYSQ),BUYPAYSQ                             
         GOTOR GETCHK              RESOLVE CHECK NUMBER                         
         MVC   BUYCHKNO,WORK                                                    
         MVC   BUYCHKDT,WORK+L'PPCLCHK                                          
                                                                                
FBUY0084 ICM   R3,15,BUYAIORE      FORMAT INSERTION ORDER VALUES                
         BZ    FBUY0085                                                         
         USING PIOELEM,R3                                                       
         MVC   BUYORDDT,PIODATE    SET INSERTION ORDER DATE                     
         GOTOR FMTION,DMCB,(PBUYKMED,PIOELEM),BUYORDNO                          
         MVI   BUYORDNO+12,C' '                                                 
         MVC   BUYORDNO+13(3),PP@IONEW                                          
         CLI   PIOTYP,C'N'                                                      
         BE    FBUY0085                                                         
         MVC   BUYORDNO+13(3),PP@IOCHA                                          
         CLI   PIOTYP,C'C'                                                      
         BE    FBUY0085                                                         
         MVC   BUYORDNO+13(3),PP@IOCAN                                          
         CLI   PIOTYP,C'D'                                                      
         BE    FBUY0085                                                         
         MVC   BUYORDNO+13(3),SPACES                                            
         DROP  R3                                                               
                                                                                
FBUY0085 ICM   R3,15,BUYAWIOE      FORMAT WEB INSERTION ORDER VALUES            
         BZ    FBUY0086                                                         
         USING PWIOELEM,R3                                                      
         TM    PWIOSTAT,PWIOESTQ   EIO BY ESTIMATE?                             
         BZ    *+8                                                              
         OI    BUYIOSTA,BIOSESTQ   EIO BY ESTIMATE                              
         MVC   BUYORDDT,PWIODATE   SET WEB INSERTION ORDER DATE                 
         TM    PWIOSTAT,PWIOEPRQ   EIO BY ESTIMATE PERIOD?                      
         BZ    *+8                                                              
         OI    BUYIOSTA,BIOSEPRQ   EIO BY ESTIMATE PERIOD                       
         MVC   BUYORDDT,PWIODATE   SET WEB INSERTION ORDER DATE                 
         MVC   BUYORDNO,SPACES                                                  
         GOTOR FMTWION,DMCB,PBUYKEY,PWIOELEM,BUYORDNO                           
                                                                                
FBUY0086 ICM   R3,15,BUYAESRE      FORMAT ESR VALUES                            
         BZ    FBUY0087                                                         
         USING PESRELEM,R3                                                      
         TM    PESRSTAT,PESRESTQ   ESR BY ESTIMATE?                             
         BZ    *+8                                                              
         OI    BUYSRSTA,BSRSESTQ   ESR BY ESTIMATE                              
         TM    PESRSTAT,PESREPRQ   ESR BY ESTIMATE PERIOD?                      
         BZ    *+8                                                              
         OI    BUYSRSTA,BSRSEPRQ   ESR BY ESTIMATE PERIOD                       
         MVC   BUYESRDT,PESRDATE   SET ESR DATE                                 
         MVC   BUYESRNO,SPACES                                                  
         GOTOR FMTESRN,DMCB,PBUYKEY,PESRELEM,BUYESRNO                           
         DROP  R3                                                               
                                                                                
FBUY0087 GOTOR GETCAP              YES - READ JOB RECORD FOR CAPTION            
                                                                                
         CLI   FLAG,0              TEST APPLYING FILTERS                        
         BE    FBUY0090            NO                                           
         OC    FILTADID,FILTADID   FILTERING ON AD-ID?                          
         BZ    FBUY0090                                                         
         CLC   PBDJOB,SPACES       AD CODE PRESENT?                             
         JNH   EXITN                                                            
         CLC   LASTADID,FILTADID   AD-ID MATCH THAT OF FILTER?                  
         JNE   EXITN                                                            
                                                                                
FBUY0090 CLI   PBDJOB,FF           AD-ID ONLY?                                  
         BNE   *+10                                                             
         XC    PBDJOB,PBDJOB       DO NOT SENT BINARY AD CODE                   
                                                                                
         BRAS  RE,FMTPLCOS         FORMAT PLANNED COST                          
                                                                                
         GOTOR S_BUYARY            SET BUY DATA ARRAY                           
                                                                                
         MVC   WORK(L'PBUYKMED),PBUYKMED                                        
         MVC   WORK+L'PBUYKMED(L'PBUPZE),PBUYKPUB                               
         GOTOR SETPBUFF                                                         
         TM    DL_FLAG1,SKIPPYEQ   PAYEE NOT MATCH, SKIP?                       
         JNZ   EXITN                                                            
                                                                                
         OC    BUYSER,BUYSER       TEST SERIAL NUMBER SET                       
         JNZ   FMTBUYX                                                          
         MVC   BUYSMED,PBUYKMED    NO - SEND FULL BUY KEY                       
         MVC   BUYSCLT,PBUYKCLT                                                 
         MVC   BUYSPRD,PBUYKPRD                                                 
         GOTOR VHEXOUT,DMCB,PBUYKPUB,BUYSPUB,L'BUYSPUB/2,=C'N'                  
         GOTOR VHEXOUT,DMCB,PBUYKLIN,BUYSLIN,L'BUYSLIN/2,=C'N'                  
                                                                                
FMTBUYX  J     EXITY                                                            
                                                                                
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
END_FMTB EQU   *                                                                
                                                                                
***********************************************************************         
* SEARCH FOR LATEST BUY ACTIVITIES                                    *         
***********************************************************************         
                                                                                
LASTBYAC NTR1  BASE=*,LABEL=*                                                   
         USING PCHGELEM,R3                                                      
         CLI   PCHGELCD,PCHGELEQ   BUY ACTIVITY ELEM?                           
         JNE   EXITN                                                            
         LA    R5,PCHG_XSS                                                      
         CLI   PCHGLEN,PCHGNEWS    ELEM W/ PID, NO COST CHG?                    
         BE    LBYAC20                                                          
         LA    R5,PCHG_XLS                                                      
         CLI   PCHGLEN,PCHGNEWL    ELEM W/ PID, COST CHG?                       
         BE    LBYAC20                                                          
         J     EXITY                                                            
LBYAC20  TM    2(R5),PCHGQIVQ      CHG BYTE 5, REQUEST INVOICE?                 
         BZ    LBYAC30                                                          
         MVC   BUYQIDAT,PCHGDAT                                                 
         MVC   HALF2,0(R5)                                                      
         BRAS  RE,GETPID                                                        
         MVC   BUYQIPID,DUB2                                                    
                                                                                
LBYAC30  TM    2(R5),PCHGRIVQ      CHG BYTE 5, RECIEVE INVOICE?                 
         BZ    LBYAC40                                                          
         MVC   BUYRIDAT,PCHGDAT                                                 
         MVC   HALF2,0(R5)                                                      
         BRAS  RE,GETPID                                                        
         MVC   BUYRIPID,DUB2                                                    
                                                                                
LBYAC40  DS    0H                                                               
         J     EXITY                                                            
                                                                                
         DROP  RB,R3                                                            
                                                                                
GETPID   LR    R0,RE                                                            
         OI    DL_FLAG1,GETPIDOQ                                                
         GOTOR GETWHO,DMCB,HALF2,(8,DUB2)                                       
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* CHECK FOR BUY ORIGIN                                                *         
***********************************************************************         
                                                                                
CKBUYORG NTR1  BASE=*,LABEL=*                                                   
         TM    PBDSTAT2,X'80'      INSERTION ADDED BY ADBUYER?                  
         BZ    *+8                                                              
         MVI   BUYORGIN,PPIDADBQ                                                
         TM    PBDSTAT2,X'20'      INSERTION ADDED BY IDESK?                    
         BZ    *+8                                                              
         MVI   BUYORGIN,PPIDIDKQ                                                
                                                                                
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
         MVI   ELCODE,PIUPELEQ                                                  
         BRAS  RE,NXTELEM          INSERTION ADDED BY PBU?                      
         BNE   *+8                                                              
         MVI   BUYORGIN,PPIDPBUQ                                                
                                                                                
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
         MVI   ELCODE,PPIDELQ                                                   
         BRAS  RE,NXTELEM                                                       
         BNE   CKBORG20                                                         
         USING PPIDELM,R3                                                       
         CLI   PPIDELL,PPIDLEQ     NEW STYLE PID ELEM?                          
         BL    CKBORG20                                                         
         MVC   BUYORGIN,PPIDPRG    Save Buy Origin                              
                                                                                
CKBORG20 CLI   FLTBYORG,0          FILTERING ON BUY ORIGIN?                     
         BE    CKBORG_X                                                         
                                                                                
         TM    FLTBYORG,FLTPPAKQ   PRINTPAK?                                    
         BZ    *+12                                                             
         CLI   BUYORGIN,PPIDPPKQ   INSERTION IS ADDED BY PRINTPAK?              
         JNE   EXITN                                                            
                                                                                
         TM    FLTBYORG,FLTSFMCQ   SFM BUY COPY?                                
         BZ    *+12                                                             
         CLI   BUYORGIN,PPIDCPYQ   INSERTION IS ADDED BY SFM BUY COPY?          
         JNE   EXITN                                                            
                                                                                
         TM    FLTBYORG,FLTSFMMQ   SFM BUY MOVE?                                
         BZ    *+12                                                             
         CLI   BUYORGIN,PPIDMOVQ   INSERTION IS ADDED BY SFM BUY MOVE?          
         JNE   EXITN                                                            
                                                                                
         TM    FLTBYORG,FLTADBYQ   ADBUYER?                                     
         BZ    *+12                                                             
         CLI   BUYORGIN,PPIDADBQ   INSERTION IS ADDED BY ADBUYER?               
         JNE   EXITN                                                            
                                                                                
         TM    FLTBYORG,FLTIDSKQ   IDESK?                                       
         BZ    *+12                                                             
         CLI   BUYORGIN,PPIDIDKQ   INSERTION IS ADDED BY IDESK?                 
         JNE   EXITN                                                            
                                                                                
         TM    FLTBYORG,FLT_PBUQ   PBU?                                         
         BZ    *+12                                                             
         CLI   BUYORGIN,PPIDPBUQ   INSERTION IS ADDED BY PBU?                   
         JNE   EXITN                                                            
                                                                                
         TM    FLTBYORG,FLT_IMPQ   AdBuyer Imports?                             
         BZ    *+12                                                             
         CLI   BUYORGIN,PPIDIMPQ   Insertion is added by AB Imports?            
         JNE   EXITN                                                            
                                                                                
CKBORG_X DS    0H                                                               
         J     EXITY                                                            
                                                                                
         DROP  RB,R3                                                            
                                                                                
***********************************************************************         
* SET PAYABLE STATUS                                                  *         
***********************************************************************         
                                                                                
SETPYBLE NTR1  BASE=*,LABEL=*                                                   
         NI    BUYSTAT2,X'FF'-(BYS2PYBL+BYS2NPYB)                               
         TM    BUYBSTAT,BUYBSTST   TEST BUY?                                    
         BNZ   SETPYB52                                                         
         TM    BUYBSTAT,BUYBSTEW   STEWARD BUY?                                 
         BNZ   SETPYB52                                                         
         CLI   BUYCLRST,C'C'                                                    
         BNE   SETPYB50                                                         
         OC    GROSS,GROSS         FREE BUY?                                    
         BNZ   *+12                                                             
         TM    BUYBSTAT,BUYBSDEL   INSERTION IS DELETED?                        
         BNZ   SETPYB52            Cleared, Free, Deleted-> Not Payable         
                                                                                
         OC    BUYNPYBL,BUYNPYBL   HAVE NET PAYABLE AMOUNT?                     
         BZ    SETPYB52                                                         
         B     SETPYB54                                                         
                                                                                
SETPYB50 TM    BUYBSTAT,BUYBSDEL   INSERTION IS DELETED?                        
         BZ    SETPYB54                                                         
SETPYB52 OI    BUYSTAT2,BYS2NPYB   SET TO NON-PAYABLE                           
         B     SETPYB_X                                                         
                                                                                
SETPYB54 OI    BUYSTAT2,BYS2PYBL   SET TO PAYABLE                               
                                                                                
SETPYB_X J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET ADDITIONAL CHARGE STATUS                                        *         
***********************************************************************         
                                                                                
SETACSTA NTR1  BASE=*,LABEL=*                                                   
         SR    RE,RE                                                            
         ICM   RE,3,BUYATAB#                                                    
         CHI   RE,0                ANY ADDITIONAL CHARGE TO LOOK UP?            
         BNH   S_ACST_X                                                         
         OI    BUYSTAT3,BYS3ACHQ   INSERTION HAS ADDITIONAL CHARGES             
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
         MVI   ELCODE,PPAYELQ                                                   
         USING PPAYELEM,R3                                                      
S_ACST10 BRAS  RE,NXTELEM                                                       
         BNE   S_ACST30                                                         
         CLI   PPAYELEM+1,PPACCODE-PPAYELEM                                     
         BL    S_ACST10                                                         
         LA    RE,BUYATAB                                                       
         SR    RF,RF                                                            
         ICM   RF,3,BUYATAB#                                                    
         USING BUYATAB,RE                                                       
S_ACST20 CLC   PPACCODE,BUYATCOD                                                
         BE    *+16                                                             
         LA    RE,BUYATABL(RE)                                                  
         BCT   RF,S_ACST20                                                      
         B     S_ACST10                                                         
         OI    BUYATSTA,BUYATPYQ                                                
         B     S_ACST10                                                         
         DROP  RE                                                               
*                                                                               
S_ACST30 LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
         MVI   ELCODE,PBILELQ                                                   
         USING PBILELEM,R3                                                      
S_ACST40 BRAS  RE,NXTELEM                                                       
         BNE   S_ACST60                                                         
         CLI   PBILELEM+1,PBACCODE-PBILELEM                                     
         BL    S_ACST40                                                         
         USING BUYATAB,RE                                                       
         LA    RE,BUYATAB                                                       
         SR    RF,RF                                                            
         ICM   RF,3,BUYATAB#                                                    
         USING BUYATAB,RE                                                       
S_ACST50 OC    PBACCODE,PBACCODE   Have additional charge code?                 
         JZ    S_ACST40                                                         
         CLC   PBACCODE,BUYATCOD                                                
         BE    *+16                                                             
         LA    RE,BUYATABL(RE)                                                  
         BCT   RF,S_ACST50                                                      
         B     S_ACST40                                                         
         OI    BUYATSTA,BUYATBLQ                                                
         B     S_ACST40                                                         
         DROP  RE                                                               
                                                                                
S_ACST60 DS    0H                                                               
                                                                                
S_ACST_X J     EXIT                                                             
         DROP  RB,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET FOREIGN EXCHANGE VALUES                                         *         
***********************************************************************         
                                                                                
SETFXVAL NTR1  BASE=*,LABEL=*      SET FOREIGN EXCHANGE VALUES                  
                                                                                
         XC    FX_VAL_S(FX_VALNQ),FX_VAL_S                                      
                                                                                
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
         MVI   ELCODE,BYCCIDQ                                                   
S_FXV20  BRAS  RE,NXTELEM                                                       
         BNE   S_FXV_X                                                          
         USING BYCCELM,R3                                                       
         CLC   BYCCSQN,=X'200F'    FX STANDARD CUSTOM COLUMN?                   
         BNE   S_FXV20                                                          
         EDITR (P8,BYCCDATA),(8,FX_RATE_),5,FLOAT=-,ALIGN=LEFT,IZERO=Y          
                                                                                
         GOTOR VGETINS,DMCB,PBUYRECD,PVALUES,PBUYKPRD,(C'A',0),0,=C'FX'         
                                                                                
         MVC   FXAC_AMT,AGYCOM     FX AGENCY AMOUNT                             
         MVC   FXCD_AMT,CSHDSC     FX CASH DISCOUNT AMOUNT                      
                                                                                
         L     RE,GROSS                                                         
         S     RE,AGYCOM                                                        
         STCM  RE,15,FXNETORD      FX NET ORDERED                               
         MVC   FXGRSORD,GROSS      FX GROSS ORDERED                             
                                                                                
         ICM   RE,15,PYABLE                                                     
         ICM   RF,15,CSHDSC                                                     
         AR    RE,RF                                                            
         ST    RE,FULL2                                                         
         ICM   RE,15,PGROSS                                                     
         ICM   RF,15,PAGYCOM                                                    
         SR    RE,RF                                                            
         L     RF,FULL2                                                         
         SR    RF,RE                                                            
         STCM  RF,15,FXNETPYB      FX NET PAYABLE                               
                                                                                
         ICM   RE,15,GROSS                                                      
         ICM   RF,15,PGROSS                                                     
         SR    RE,RF                                                            
         STCM  RE,15,FXGRSPYB      FX GROSS PAYABLE                             
                                                                                
         ICM   RE,15,PGROSS                                                     
         S     RE,PAGYCOM                                                       
         STCM  RE,15,FXNET_PD      FX NET PAID                                  
         MVC   FXGRS_PD,PGROSS     FX GROSS PAID                                
         S     RE,PCSHDSC                                                       
         STCM  RE,15,FXNLCDPD      FX NET LESS CD PAID                          
         MVC   FXGLCDPD,PAID       FX GROSS LESS CD PAID                        
                                                                                
         MVC   FXNLCDOR,PYABLE     FX NET LESS CD ORDERED                       
         MVC   FXGLCDOR,BLABLE     FX GROSS LESS CD ORDERED                     
                                                                                
         GOTOR VGETINS,DMCB,PBUYRECD,PVALUES,PBUYKPRD,(0,0),0,0                 
                                                                                
         L     RE,GROSS                                                         
         S     RE,AGYCOM                                                        
         STCM  RE,15,S_NETORD      NET ORDERED + FX NET ORDERED                 
         MVC   S_GRSORD,GROSS      GROSS ORDERED + FX GROSS ORDERED             
                                                                                
         ICM   RE,15,PYABLE                                                     
         ICM   RF,15,CSHDSC                                                     
         AR    RE,RF                                                            
         ST    RE,FULL2                                                         
         ICM   RE,15,PGROSS                                                     
         ICM   RF,15,PAGYCOM                                                    
         SR    RE,RF                                                            
         L     RF,FULL2                                                         
         SR    RF,RE                                                            
         STCM  RF,15,S_NETPYB      NET PAYABLE + FX NP                          
                                                                                
         ICM   RE,15,GROSS                                                      
         ICM   RF,15,PGROSS                                                     
         SR    RE,RF                                                            
         STCM  RE,15,S_GRSPYB      GROSS PAYABLE + FX GP                        
                                                                                
         ICM   RE,15,PGROSS                                                     
         S     RE,PAGYCOM                                                       
         STCM  RE,15,S_NET_PD      NET PAID + FX NET PAID                       
         MVC   S_GRS_PD,PGROSS     GROSS PAID + FX GROSS PAID                   
         S     RE,PCSHDSC                                                       
         STCM  RE,15,S_NLCDPD      NET LESS CD PAID + FX NLCD PAID              
         MVC   S_GLCDPD,PAID       GROSS LESS CD PAID + FX GLCD PAID            
                                                                                
         MVC   S_NLCDOR,PYABLE     NLCD ORDERED + FX NLCD ORDERED               
         MVC   S_GLCDOR,BLABLE     GLCD ORDERED + FX GLCD ORDERED               
*                                                                               
S_FXV_X  J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* FORMAT NEWSPAPER SPACE/DESCRIPTION                                  *         
***********************************************************************         
                                                                                
FMTNSD   NTR1  BASE=*,LABEL=*                                                   
         MVC   BUYSPACE,SPACES                                                  
                                                                                
         OC    PBDSPACE,PBDSPACE   ANYTHING IN SPACE?                           
         BZ    FMTNSD02                                                         
         CLC   PBDSPACE,SPACES     ANYTHING IN SPACE?                           
         BE    FMTNSD02                                                         
                                                                                
         CLC   =X'7B00',PBDSPACE   # AND ZERO - TREAT AS NONE-SPACE BUY         
         BE    FMTNSD02                                                         
         CLC   =C'# ',PBDSPACE     # AND SPACE- TREAT AS NONE-SPACE BUY         
         BE    FMTNSD02                                                         
                                                                                
         CLI   PBDSPACE,C'*'       OTHER SPECIAL CHARS LOWER THAN C'*'?         
         BL    *+14                                                             
         CLC   =C'* ',PBDSPACE     TEST SPACE BUY                               
         BNL   FMTNSD02                                                         
         MVC   BUYSPACE(8),PBDSPACE                                             
         B     FMTNSDX                                                          
                                                                                
FMTNSD02 LA    R5,WORK2                                                         
         MVC   WORK2,SPACES                                                     
         CLI   PBDSPACE,C'*'                                                    
         BNE   *+12                                                             
         MVI   0(R5),C'*'                                                       
         AHI   R5,1                                                             
         CLI   PBDSPACE,C'#'       SPECIAL FOR NO ASC CHECKING                  
         BNE   *+12                                                             
         MVI   0(R5),C'#'                                                       
         AHI   R5,1                                                             
         ZAP   DUB,PBDUNITS                                                     
         BNZ   *+16                                                             
         MVI   0(R5),C'0'                                                       
         AHI   R5,1                                                             
         B     *+10                                                             
         GOTOR FMTNSDE1                                                         
         AR    R5,R0                                                            
                                                                                
         MVC   0(1,R5),PBDUIND                                                  
         OI    0(R5),C' '          TO SET X'89' TO X'C9'                        
         AHI   R5,1                                                             
                                                                                
         OC    PBDCLMS,PBDCLMS                                                  
         BZ    FMTNSD04                                                         
         ZAP   DUB,PBDCLMS                                                      
         BZ    FMTNSD04                                                         
         MVI   0(R5),C'/'                                                       
         AHI   R5,1                                                             
         GOTOR FMTNSDE2                                                         
                                                                                
FMTNSD04 MVC   BUYSPACE(8),WORK2                                                
                                                                                
FMTNSDX  J     EXIT                                                             
                                                                                
FMTNSDE1 CLI   PBDUIND,C'I'-X'40'                                               
         BNE   FMTNSDE2                                                         
         EDITR (P8,DUB),(6,0(R5)),2,ALIGN=LEFT                                  
         BR    RE                                                               
                                                                                
FMTNSDE2 EDITR (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         BR    RE                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT NEWSPAPER CONTRACT LINEAGE EQUIVALENCY                       *         
***********************************************************************         
                                                                                
FMTNCL   NTR1  BASE=*,LABEL=*                                                   
         CLC   =X'7B00',PBDSPACE                                                
         BE    FMTNCLX                                                          
         CLC   =C'# ',PBDSPACE                                                  
         BE    FMTNCLX                                                          
         CLC   =C'* ',PBDSPACE     SEE IF SPACE BUY                             
         BNL   FMTNCLX             IF NOT - NO CLE                              
                                                                                
         MVC   BUYCLE,SPACES                                                    
         LA    R5,BUYCLE           SET CONTRACT LINEAGE EQUIVALENCY             
         CLI   PBDUIND,C'I'-X'40'  SEE IF LOWER CASE I                          
         BNE   FMTNCL02                                                         
         EDITR PBDUNITS,(6,0(R5)),2,ALIGN=LEFT                                  
         AR    R5,R0                                                            
         MVI   0(R5),C'I'                                                       
         B     FMTNCLX                                                          
                                                                                
FMTNCL02 EDITR PBDUNITS,(5,0(R5)),ALIGN=LEFT                                    
         AR    R5,R0                                                            
         CLI   PBDUIND,C'I'                                                     
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         AHI   R5,1                                                             
         CP    PBDCLMS,PZERO                                                    
         BE    FMTNCLX                                                          
         MVI   0(R5),C'/'                                                       
         AHI   R5,1                                                             
         EDITR PBDCLMS,(5,0(R5)),ALIGN=LEFT                                     
                                                                                
FMTNCLX  J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO FORMAT INSERTION DATE/SUBLINE                         *         
***********************************************************************         
                                                                                
FMTLIN   NTR1  BASE=*,LABEL=*                                                   
         MVC   BUYLINE(L'BUYLINE),SPACES                                        
         LA    R5,BUYLINE                                                       
         CLI   PBDBFD,0                                                         
         BE    FMTLIN02                                                         
         MVC   0(1,R5),PBDBFD                                                   
         AHI   R5,1                                                             
                                                                                
FMTLIN02 GOTOR VDATCON,DMCB,(3,PBUYKDAT),(7,0(R5))                              
         AHI   R5,5                SET NEXT OUTPUT ADDRESS                      
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+14                                                             
         SHI   R5,2                                                             
         MVC   0(2,R5),SPACES                                                   
                                                                                
         CLI   PBUYKLIN,1          TEST SUBLINE TO DISPLAY                      
         BE    FMTLIN06                                                         
         MVI   0(R5),C'-'                                                       
         SR    R0,R0                                                            
         IC    R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         CHI   R0,100                                                           
         BL    FMTLIN04                                                         
                                                                                
         DP    DUB,=P'10'          DISPLAY 100 - 239 AS A0 - N9                 
         OI    DUB+L'DUB-1,X'0F'                                                
         CLI   DUB+L'DUB-1,X'0F'                                                
         BE    *+10                                                             
         UNPK  2(1,R5),DUB+7(1)                                                 
         ZAP   DUB,DUB(6)                                                       
         CVB   RF,DUB                                                           
         SHI   RF,10                                                            
         LA    RF,LINETAB(RF)                                                   
         MVC   1(1,R5),0(RF)                                                    
         B     FMTLIN06                                                         
                                                                                
FMTLIN04 OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(2,R5),DUB                                                      
         CLI   1(R5),C'0'                                                       
         BNE   FMTLIN06                                                         
         MVC   1(2,R5),2(R5)                                                    
                                                                                
FMTLIN06 L     R1,ALP              DDLINK CONTROL BLOCK                         
         USING LP_D,R1                                                          
         CLC   LP_VRSN1,=AL1(1,2,0,0)                                           
         BNL   FMTLIN08                                                         
         CLC   LP_VRSN1,=AL1(1,1,0,0)                                           
         BL    FMTLIN08                                                         
         CLI   BUYLINE,C'T'        TEST INSERTION INDICATOR?                    
         BNE   FMTLIN08            STRIP LEADING T                              
         MVC   BUYLINE(L'BUYLINE-1),BUYLINE+1                                   
         MVI   BUYLINE+L'BUYLINE-1,C' '                                         
         B     FMTLIN20                                                         
         DROP  R1                                                               
                                                                                
FMTLIN08 CLI   BUYLINE,C'T'        TEST INSERTION INDICATOR?                    
         BNE   FMTLIN20                                                         
         TM    PBDSTAT2,X'40'      STEWARDSHIP INSERTION?                       
         BZ    FMTLIN20                                                         
         MVI   BUYLINE,C'S'                                                     
                                                                                
FMTLIN20 MVC   BUYTYPE,SPACES      ADBUYER 'FEB29' RUNTIME ERROR FIX            
         CLC   PBUYKDAT+1(2),=X'021D'                                           
         BNE   FMTLIN30                                                         
         MVC   BUYTYPE(05),=C'DAILY'                                            
         CLI   PBDFREQ,C'M'                                                     
         BNE   *+14                                                             
         MVC   BUYTYPE(07),=C'MONTHLY'                                          
         B     FMTLIN30                                                         
         CLI   PBDBFD,C'W'                                                      
         BNE   *+10                                                             
         MVC   BUYTYPE(07),=C'WEEK OF'                                          
         CLI   PBDBFD,C'B'                                                      
         BNE   *+10                                                             
         MVC   BUYTYPE(13),=C'BEST FOOD DAY'                                    
                                                                                
FMTLIN30 DS    0H                                                               
                                                                                
FMTLINX  J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO FORMAT POL PRODUCT ALLOCATIONS                        *         
***********************************************************************         
                                                                                
FMTPRD   NTR1  BASE=*,LABEL=*                                                   
         MVC   BUYALLOC,SPACES                                                  
         LA    R5,BUYALLOC                                                      
         TM    PBDWTSUM,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTPRD02            NO-EQUAL                                     
         MVC   BYTE1,PBDWTSUM                                                   
         NI    BYTE1,FF-X'80'                                                   
         GOTOR FMTPRDED,BYTE1                                                   
         MVI   0(R5),C'/'                                                       
         AHI   R5,1                                                             
                                                                                
FMTPRD02 LA    R3,PBUYFRST                                                      
         USING PALCEL,R3                                                        
         SR    R0,R0                                                            
FMTPRD04 CLI   PALCEL,PALCELQ                                                   
         BNE   FMTPRD08                                                         
                                                                                
         MVC   0(L'PALCPRD,R5),PALCPRD                                          
         CLI   PALCPRD+L'PALCPRD-1,C' '                                         
         BNE   *+6                                                              
         BCTR  R5,0                                                             
         AHI   R5,L'PALCPRD                                                     
         TM    PBDWTSUM,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTPRD06                                                         
                                                                                
         MVI   0(R5),C'-'                                                       
         AHI   R5,1                                                             
         GOTOR FMTPRDED,PALCCSHR   COST SHARE                                   
         CLI   PALCCSHR,0                                                       
         BNE   *+12                                                             
         MVI   0(R5),C'0'                                                       
         AHI   R5,1                                                             
         CLC   PALCCSHR,PALCSSHR   TEST COST SHARE=SPACE SHARE                  
         BE    FMTPRD06                                                         
         MVI   0(R5),C'-'                                                       
         AHI   R5,1                                                             
         GOTOR FMTPRDED,PALCSSHR   SPACE SHARE                                  
                                                                                
FMTPRD06 MVI   0(R5),COMMA         INSERT COMMA BETWEEN PRODUCTS                
         AHI   R5,1                                                             
                                                                                
FMTPRD08 IC    R0,PALCLEN          BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         CLI   PALCEL,0            TEST END OF RECORD                           
         BNE   FMTPRD04            NO                                           
         BCTR  R5,0                                                             
         CLI   0(R5),COMMA         REMOVE ANY TRAILING COMMA                    
         BNE   FMTPRDX                                                          
         MVI   0(R5),C' '                                                       
                                                                                
FMTPRDX  J     EXIT                                                             
                                                                                
FMTPRDED EDITR (B1,0(R1)),(3,0(R5)),ALIGN=LEFT                                  
         AR    R5,R0               BUMP TO NEXT OUTPUT POSITION                 
         BR    RE                                                               
         DROP  R3,RB                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMT_COST NTR1  BASE=*,LABEL=*      FORMAT COST                                  
*                                                                               
F        USING F_COST_D,TEMP2                                                   
*                                                                               
         XC    F.F_COST_D(FC_BLKLQ),F.F_COST_D                                  
         MVC   F.FC_TMPWK,SPACES                                                
         MVC   F.FC_OCOST,SPACES                                                
*                                                                               
         LR    R3,R2               POINT TO BUY RECORD                          
         LA    R3,33(R3)           POINT TO 1ST BUY ELEM                        
         MVC   F.FC_RTIND,PBDRLIND-PBDELEM(R3)                                  
         MVC   F.FC_ACPCT,PBDACP-PBDELEM(R3)                                    
*                                                                               
         CHI   R1,1                FORMATTING RATE?                             
         BNE   F_COS12                                                          
         ZAP   F.FC_COST_,PBDCOS-PBDELEM(L'PBDCOS,R3)                           
         MVC   F.FC_NETSW,PBDCTYP-PBDELEM(R3)                                   
         MVC   F.FC_COTYP,PBDCOSTY-PBDELEM(R3)                                  
         MVC   F.FC_COIND,PBDCOSIN-PBDELEM(R3)                                  
         B     F_COS30                                                          
*                                                                               
F_COS12  CHI   R1,2                FORMATTING PLANNED COST?                     
         BNE   F_COS20                                                          
         MVI   ELCODE,BYPCIDQ                                                   
         BRAS  RE,NXTELEM          PLANNED COST ELEM FOUND?                     
         BNE   F_COS_X                                                          
         USING BYPCELD,R3                                                       
         ZAP   F.FC_COST_,BYPCCST  COST                                         
         MVC   F.FC_NETSW,BYPCNIND NET COST SWITCH (ENTERED AS NET)             
         MVC   F.FC_COTYP,BYPCTYP  COST TYPE (U=UNIT COST)                      
         MVC   F.FC_COIND,BYPCIND  COST INDICATOR                               
         B     F_COS30                                                          
*                                                                               
F_COS20  DC    H'0'                NO OTHER COST AT THIS TIME                   
*                                                                               
F_COS30  CLI   PBUYKMED-PBUYKEY(R2),C'N'                                        
         BNE   F_COS40                                                          
         ZAP   DUB,F.FC_COST_      FORMAT COST FOR NEWSPAPER                    
         CVB   R1,DUB                                                           
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         JE    *+12                                                             
         CLI   F.FC_COIND,C'R'     ROADSIDE (DISPLAY AS NET)?                   
         JNE   F_COS32                                                          
         ZAP   DUB,F.FC_ACPCT                                                   
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
F_COS32  CLI   F.FC_COTYP,C'U'     UNIT RATE?                                   
         BE    F_COS37                                                          
         C     R1,=F'99999999'     TOTAL RATE OVER 999,999.99?                  
         BNH   F_COS34                                                          
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'          HAVE ENTERED PENNIES WHEN BUYING             
         LTR   R1,R1               (NO ROOM)                                    
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
F_COS33  EDITR (R1),(9,F.FC_TMPWK+5),0,FLOAT=-,ALIGN=LEFT                       
         B     F_COS38                                                          
*                                                                               
F_COS34  CHI   R1,0                NEGATIVE RATE?                               
         BNL   F_COS36                                                          
         C     R1,=F'-99999999'    TOTAL RATE LESS THAN 99,999,999?             
         BH    F_COS35                                                          
         MHI   R1,-1               DROP PENNIES AND DIMES                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         MHI   R1,-1                                                            
         B     F_COS33                                                          
*                                                                               
F_COS35  C     R1,=F'-999999'      TOTAL RATE LESS THAN 9,999.99?               
         BNL   F_COS36                                                          
         MHI   R1,-1               DROP PENNIES, LEAVE DIMES                    
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MHI   R1,-1                                                            
         EDITR (R1),(9,F.FC_TMPWK+5),1,FLOAT=-,ALIGN=LEFT                       
         B     F_COS38                                                          
*                                                                               
F_COS36  EDITR (R1),(9,F.FC_TMPWK+5),2,FLOAT=-,ALIGN=LEFT                       
         B     F_COS38                                                          
*                                                                               
F_COS37  EDITR (R1),(11,F.FC_TMPWK+5),5,FLOAT=-,ALIGN=LEFT                      
*                                                                               
         LA    R1,F.FC_TMPWK+5-3   START OF OUTPUT                              
         AR    R1,R0               + LENGTH                                     
         SHI   R1,3                BACK UP TO LAST 3 BYTES                      
         CLC   =C'000',0(R1)                                                    
         BNE   *+10                                                             
         MVC   0(3,R1),SPACES      MOVE SOME BLANKS                             
*                                                                               
F_COS38  LA    R1,F.FC_TMPWK+5                                                  
         CLI   F.FC_COTYP,C'U'     UNIT RATE?                                   
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COTYP                                               
         CLI   F.FC_COIND,C' '     DEFAULT COST TYPE?                           
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COIND                                               
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_NETSW                                               
         TM    F.FC_RTIND,X'08'    FROZEN RATE?                                 
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
         MVC   F.FC_OCOST(11),0(R1)                                             
         CP    F.FC_COST_,=P'0'                                                 
         BNE   F_COS_X                                                          
         MVC   F.FC_OCOST,SPACES                                                
         MVC   F.FC_OCOST(L'FREE),FREE                                          
         CLI   F.FC_COIND,C'S'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'SFREE'                                          
         B     F_COS_X                                                          
*                                                                               
F_COS40  ZAP   DUB,F.FC_COST_      FORMAT COST FOR NON-NEWSPAPER                
         CVB   R1,DUB                                                           
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         JE    *+12                                                             
         CLI   F.FC_COIND,C'R'     ROADSIDE (DISPLAY AS NET)?                   
         JNE   F_COS42                                                          
         ZAP   DUB,F.FC_ACPCT                                                   
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               =NET PCT                                     
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
F_COS42  EDITR (R1),(10,F.FC_TMPWK+2),2,ALIGN=LEFT,FLOAT=-                      
         LA    R1,F.FC_TMPWK+2                                                  
         CLI   F.FC_COIND,C' '     DEFAULT COST TYPE?                           
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COIND                                               
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_NETSW                                               
         TM    F.FC_RTIND,X'08'    FROZEN RATE?                                 
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
         MVC   F.FC_OCOST(11),0(R1)                                             
         CP    F.FC_COST_,PZERO                                                 
         BNE   F_COS_X                                                          
         MVC   F.FC_OCOST(11),SPACES                                            
         MVC   F.FC_OCOST(L'FREE),FREE                                          
         CLI   F.FC_COIND,C'S'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'SFREE'                                          
         CLI   F.FC_COIND,C'R'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'RFREE'                                          
*                                                                               
F_COS_X  J     EXIT                                                             
*                                                                               
         DROP  F                                                                
         DROP  RB,R3                                                            
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
FMTNPR   NTR1  BASE=*,LABEL=*      FORMAT NEWSPAPER PREMIUMS                    
         MVC   BUYPREM,SPACES                                                   
         LA    R3,BUYPREM                                                       
         CLI   PBDCL,0                                                          
         BE    FMTNPR02                                                         
         MVC   0(1,R3),PBDCL                                                    
         MVI   1(R3),C'C'                                                       
         MVI   2(R3),C'/'                                                       
         AHI   R3,3                                                             
         B     FMTNPR04                                                         
                                                                                
FMTNPR02 CP    PBDPRCOS,PZERO                                                   
         BE    FMTNPR90                                                         
                                                                                
FMTNPR04 ZAP   DUB,PBDPRCOS                                                     
         CVB   R1,DUB              R1=PREMIUM COST                              
         CLI   PBDPCTYP,C'N'       NET INPUT SO DISPLAY AS NET                  
         BNE   FMTNPR06                                                         
                                                                                
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
FMTNPR06 CLI   PBDPRIN,C' '        TEST DEFAULT IND                             
         BNH   *+14                                                             
         MVC   0(1,R3),PBDPRIN                                                  
         AHI   R3,1                                                             
         CLI   PBDPCTYP,C' '       TEST PREMIUM COST TYPE                       
         BNH   *+14                                                             
         MVC   0(1,R3),PBDPCTYP                                                 
         AHI   R3,1                                                             
         CLI   PBDCL,0             CHECK FOR COLOR                              
         BE    FMTNPR40                                                         
                                                                                
         CLI   PBDPRIN,C' '        CHECK FOR PR RATE IND                        
         BH    FMTNPR08            IF SPACE I CAN DISPLAY 8 DIGITS              
         CLI   PBDPCTYP,C' '       CHECK FOR PR COST TYPE                       
         BNH   FMTNPR20            IF SPACE I CAN DISPLAY 8 DIGITS              
                                                                                
FMTNPR08 C     R1,=F'-99999'       CHECK FOR NEGATIVE > -999.99                 
         BL    FMTNPR10                                                         
         C     R1,=F'1000000'      SEE IF OVER 10,000.00                        
         BL    FMTNPR12                                                         
                                                                                
FMTNPR10 CVD   R1,DUB              SEE IF PENNY CAN BE DROPPED                  
         C     R1,=F'-10000000'    CK FOR -100,000.00                           
         BNH   F_NPR10H                                                         
         C     R1,=F'10000000'     CK FOR 100,000.00                            
         BL    F_NPR10G                                                         
                                                                                
F_NPR10H DP    DUB,P100                                                         
         ZAP   DUB,DUB(8-2)        NO DECIMAL                                   
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R3)),0,FLOAT=-,ALIGN=LEFT,IZERO=Y                      
         B     FMTNPR90                                                         
                                                                                
F_NPR10G DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(8-2)        ONE DECIMAL                                  
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R3)),1,FLOAT=-,ALIGN=LEFT,IZERO=Y                      
         B     FMTNPR90                                                         
                                                                                
FMTNPR12 CLI   PBDCL,0                                                          
         BE    FMTNPR16                                                         
         CLI   BUYPREM+3,C'0'                                                   
         BH    FMTNPR16                                                         
         C     R1,=F'999999'       GREATER THAN 9,999.99?                       
         BNH   FMTNPR16                                                         
         CVD   R1,DUB                                                           
         DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(6)                                                       
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R3)),1,FLOAT=-,ALIGN=LEFT,IZERO=Y                      
         B     FMTNPR90                                                         
                                                                                
FMTNPR16 EDITR (R1),(7,0(R3)),2,FLOAT=-,ALIGN=LEFT,IZERO=Y                      
         B     FMTNPR90                                                         
                                                                                
FMTNPR20 C     R1,=F'-9999999'     SEE IF NEGATIVE > -99,999.99                 
         BL    FMTNPR26                                                         
         C     R1,=F'10000000'     SEE IF OVER 100,000.00                       
         BL    FMTNPR30                                                         
                                                                                
FMTNPR26 CVD   R1,DUB              SEE IF PENNY CAN BE DROPPED                  
         C     R1,=F'-10000000'    CK FOR -100,000.00                           
         BNH   F_NPR26H                                                         
         C     R1,=F'100000000'    CK FOR 1,000,000.00                          
         BL    F_NPR26G                                                         
                                                                                
F_NPR26H DP    DUB,P100                                                         
         ZAP   DUB,DUB(8-2)        NO DECIMAL                                   
         CVB   R1,DUB                                                           
         EDITR (R1),(8,0(R3)),0,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK                 
         B     FMTNPR90                                                         
                                                                                
F_NPR26G DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(8-2)        ONE DECIMAL                                  
         CVB   R1,DUB                                                           
         EDITR (R1),(8,0(R3)),1,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK                 
         B     FMTNPR90                                                         
                                                                                
FMTNPR30 EDITR (R1),(8,0(R3)),2,FLOAT=-,ALIGN=LEFT,IZERO=Y                      
         B     FMTNPR90                                                         
                                                                                
FMTNPR40 EDITR (R1),(10,0(R3)),2,FLOAT=-,ALIGN=LEFT,IZERO=Y                     
                                                                                
FMTNPR90 J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
                                                                                
FMTPLCOS NTR1  BASE=*,LABEL=*      FORMAT PLANNED COST                          
                                                                                
         MVC   BUYPLCOS,SPACES                                                  
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         CLC   LP_VRSN1,=AL1(3,4,0,31)                                          
         BL    F_PLCO_X                                                         
         GOTOR FMT_COST,2          FORMAT PLANNED COST                          
         MVC   BUYPLCOS,TEMP2+(FC_OCOST-F_COST_D)                               
         CLC   BUYPLCOS,SPACES                                                  
         BH    F_PLCO70                                                         
         OC    PBDPLCOS,PBDPLCOS   HAVE PLANNED COST?                           
         BZ    F_PLCO_X                                                         
         CLI   PBDPLCOS,X'FF'      HAVE PLANNED COST?                           
         BE    F_PLCO_X                                                         
         ICM   R1,15,PBDPLCOS                                                   
         EDITR (R1),(L'BUYPLCOS,BUYPLCOS),2,COMMAS=YES,ALIGN=LEFT               
                                                                                
F_PLCO70 XC    PBDPLCOS,PBDPLCOS   DO NOT REPLY BINARY DATA                     
                                                                                
         MVC   TEMP2(PVALUESX-PVALUES),PVALUES                                  
         GOTOR VGETINS,DMCB,PBUYRECD,(C'P',PVALUES),PBUYKPRD,0,0,0              
         CLI   DMCB+8,C'X'                                                      
         BE    *+20                                                             
         ICM   RE,15,GROSS                                                      
         STCM  RE,15,BUYPCGRS                                                   
         S     RE,AGYCOM                                                        
         STCM  RE,15,BUYPCNET                                                   
         MVC   PVALUES(PVALUESX-PVALUES),TEMP2                                  
                                                                                
F_PLCO_X J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK FOR INVOICE DATA IN INSERTION (CC EQUAL -> FOUND)             *         
***********************************************************************         
                                                                                
CKINSINV NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
CKINSI10 CLI   0(R3),0             END OF BUY RECORD?                           
         BNE   CKINSI20                                                         
         TM    FLTMATST,FLTMSPSI   PLUS NO INVOICES?                            
         JNZ   EXITY                                                            
         J     EXITN                                                            
                                                                                
CKINSI20 CLI   0(R3),PBNVELQ       INVOICE HEADER/DETAIL ELEM?                  
         JE    EXITY                                                            
         CLI   0(R3),PBMATELQ      INVOICE MATCHING STATUSES ELEM?              
         BNE   CKINSI30                                                         
         USING PBMATELM,R3                                                      
         CLI   PBMTSTAT,0                                                       
         JNE   EXITY                                                            
         CLI   PBMTDSTA,0                                                       
         JNE   EXITY                                                            
                                                                                
CKINSI30 SR    R0,R0               BUMP TO NEXT ELEMENT ON RECORD               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CKINSI10                                                         
                                                                                
         DROP  RB,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* FILTERING ON INSERTION PUB'S NATIONALITY                            *         
***********************************************************************         
                                                                                
CKINSPUB NTR1  BASE=*,LABEL=*                                                   
                                                                                
         TM    FLTINFLT,FLTCANAD   FILTERING FOR CANADIAN PUBS ONLY?            
         BNZ   CKINSP10                                                         
         TM    FLTINFLT,FLTUSAPB   FILTERING FOR USA PUBS ONLY?                 
         BNZ   CKINSP10                                                         
         J     EXITY                                                            
                                                                                
CKINSP10 CLC   PBUMED,PBUYKMED     TEST CHANGE OF MEDIA/PUBLICATION             
         BNE   *+14                                                             
         CLC   PBUPZE,PBUYKPUB                                                  
         JE    EXITY                                                            
                                                                                
         MVC   WORK2(L'IOKEY),IOKEY                                             
         MVC   WORK2+L'IOKEY(L'IOADDR),IOADDR                                   
         LA    R3,IOKEY                                                         
         USING PUBRECD,R3          READ PUBLICATION RECORD                      
         XC    PUBKEY,PUBKEY                                                    
         MVC   PUBKMED,PBUYKMED                                                 
         MVC   PUBKPUB(LPUBKPZE),PBUYKPUB                                       
         MVC   PUBKAGY,AGY                                                      
         MVI   PUBKCOD,PUBKCODQ                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPUBDIR+IO3'                            
         JNE   EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPUBFIL+IO3'                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,IOADDR                                                        
         MVC   IOKEY,WORK2         RESTORE SAVED BUY KEY & IO                   
         MVC   IOADDR,WORK2+L'IOKEY                                             
                                                                                
         TM    FLTINFLT,FLTCANAD   FILTERING FOR CANADIAN PUBS ONLY?            
         BZ    *+14                                                             
         CLC   PUBSTACD,=C'90'     CANADIAN?                                    
         JE    EXITY                                                            
         TM    FLTINFLT,FLTUSAPB   FILTERING FOR USA PUBS ONLY?                 
         BZ    *+14                                                             
         CLC   PUBSTACD,=C'90'     USA?                                         
         JNE   EXITY                                                            
                                                                                
         J     EXITN               FAILED NATIONALITY FILTER                    
         DROP  RB,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK FOR SPECIAL REP FILTER (CC EQUAL -> MATCHED)                  *         
***********************************************************************         
                                                                                
CKSREP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
         USING PBSREPEL,R3                                                      
         BRAS  RE,MODFYFIL         MODIFY FILTERS                               
         NI    DL_FLAG1,X'FF'-FLTPAYEQ                                          
                                                                                
CKSREP10 CLI   0(R3),0             END OF BUY RECORD?                           
         BE    CKSREP80                                                         
         CLI   0(R3),PBREPEQ       SPECIAL REP ELEM?                            
         BE    CKSREP20                                                         
         SR    R0,R0               BUMP TO NEXT ELEMENT ON RECORD               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CKSREP10                                                         
                                                                                
CKSREP20 CLC   PBSREP,SPACES       HAVE SREP CODE?                              
         BNH   CKSREP80                                                         
         NI    DL_FLAG1,X'FF'-SKIPPYEQ                                          
         CLC   FILTSREP,PBSREP     SREP CODE MATCH FILTER?                      
         JE    EXITY                                                            
         J     EXITN                                                            
                                                                                
CKSREP80 OI    DL_FLAG1,FLTPAYEQ   INSERTION DL WILL FILTER ON PAYEE            
         J     EXITY                                                            
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ESTABLISH CHECK NUMBER                                   *         
***********************************************************************         
                                                                                
GETCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SAVEKEY,IOKEY       SAVE THE CURRENT RECORD KEY                  
         XC    WORK2(L'IOKEY),WORK2                                             
*                                                                               
         LA    R3,IOKEY                                                         
         USING PPCLRST,R3          READ CLEARANCE STATUS RECORDS                
         XC    PPCLKEY,PPCLKEY     TO ESTABLISH CHECK NUMBER                    
         MVC   PPCLAGY,AGY                                                      
         MVC   PPCLMED,PBUYKMED                                                 
         MVI   PPCLTYPE,PPCLTYPQ                                                
         MVC   PPCLCLT,PBUYKCLT                                                 
         MVC   PPCLPUB(L'PBUYKPUB),PBUYKPUB                                     
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO7'                               
         JNE   GETCHK46                                                         
         J     GETCHK04                                                         
*                                                                               
GETCHK02 MVC   WORK2(L'IOKEY),PPCLKEY                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO7'                               
         JNE   GETCHK08                                                         
*                                                                               
GETCHK04 OC    WORK2(L'IOKEY),WORK2                                             
         JNZ   GETCHK06                                                         
         CLC   PPCLKEY(PPCLDATE-PPCLKEY),IOKEYSAV                               
         JNE   GETCHK46                                                         
         J     GETCHK02                                                         
*                                                                               
GETCHK06 CLC   PPCLKEY(PPCLDATE-PPCLKEY),WORK2                                  
         JNE   GETCHK08                                                         
         CLC   WORK(L'PPCLDATE),PPCLDATE                                        
         JH    GETCHK02                                                         
*                                                                               
GETCHK08 MVC   IOKEY,WORK2         RESTORE LAST GOOD KEY                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO7'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO7'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,IOADDR                                                        
         LA    R4,PPCLELEM-PPCLRST(R4)                                          
         USING PPCLELEM,R4         TRY FOR PAYMENT ON THIS RECORD               
*                                                                               
GETCHK10 CLI   PPCLELEM,0          TEST END OF RECORD                           
         JE    GETCHK34                                                         
         CLI   PPCLELEM,PPCLELQ    TEST CLEARANCE ELEMENT                       
         JNE   GETCHK12                                                         
         CLC   PPCLCLRD(L'PPCLCLRD+L'PPCLCLSQ),WORK                             
         JNE   GETCHK12                                                         
         MVI   BYTE4,0             Temp switch to indicate prior ck#            
         CLC   PPCLCHK,SPACES      Have check number?                           
         JNH   *+8                                                              
         MVI   BYTE4,YESQ                                                       
         TM    PPCLSTAT,X'02'      Have invoice elements to follow?             
         JO    GETCHK22                                                         
         J     GETCHK18                                                         
GETCHK12 LLC   R0,PPCLELEM+1       BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         J     GETCHK10                                                         
*                                                                               
GETCHK18 MVC   WORK(L'PPCLCHK),PPCLCHK                                          
         MVC   WORK+L'PPCLCHK(L'PPCLCHDT),PPCLCHDT                              
         J     GETCHKX                                                          
*                                                                               
GETCHK22 XC    WORK(L'PPCLCHK+L'PPCLCHDT),WORK                                  
         LLC   R0,1(R4)            Bump to next element                         
         AR    R4,R0                                                            
         USING PPCLEL05,R4         Invoice elements                             
GETCHK24 CLI   PPCLEL05,0          End of record?                               
         JE    GETCHKX                                                          
         CLI   PPCLEL05,X'01'      Next set of clearance detail elem?           
         JE    GETCHKX                                                          
         CLI   PPCLEL05,X'05'                                                   
         JNE   GETCHK26                                                         
         MVC   WORK(L'PCL5CHK),PCL5CHK                                          
         MVC   WORK+L'PCL5CHK(L'PCL5CHDT),PCL5CHDT                              
         CLC   PCL5CHK,SPACES                                                   
         JH    GETCHK26                                                         
         CLI   BYTE4,YESQ          Have prior check number?                     
         JNE   GETCHK26                                                         
         MVC   WORK(06),=C'<HIST>'                                              
GETCHK26 LLC   R0,PPCLLN05         Bump to next element                         
         AR    R4,R0                                                            
         J     GETCHK24                                                         
*                                                                               
GETCHK34 XC    WORK2(L'IOKEY),WORK2                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO7'                               
         JE    GETCHK04            GO PROCESS NEXT RECORD                       
                                                                                
GETCHK46 XC    WORK(L'PPCLCHK+L'PPCLCHDT),WORK                                  
         J     GETCHKX                                                          
                                                                                
GETCHKX  MVC   IOKEY,SAVEKEY       RESTORE SAVED KEY                            
         J     EXIT                                                             
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO ESTABLISH INVOICES IN CLEARANCE STATUS RECORD            *         
***********************************************************************         
                                                                                
INVCLSTA NTR1  BASE=*,LABEL=*      CK INVOICE IN CLEARANCE STATUS REC           
                                                                                
         MVC   SAVEKEY,IOKEY       SAVE THE CURRENT RECORD KEY                  
                                                                                
         USING IPAD,R4             PAY HISTORY OUTPUT FIELDS                    
         MVC   ELEM(L'IPADATE),IPADATE                                          
         MVC   ELEM+L'IPADATE(L'IPAPAYEE),IPAPAYEE                              
         MVC   ELEM+L'IPADATE+L'IPAPAYEE(L'IPAACODE),IPAACODE                   
                                                                                
         ST    R3,FULL1                                                         
         L     R1,FULL1                                                         
         USING PPAYELEM,R1         PAYMENT ELEMENTS                             
         CLI   PPAYELEM+1,22       BIG PAY ELEM?                                
         BNH   INVCLS_X                                                         
         CLI   PPDSEQNO,0          HAVE SEQ NUMBER?                             
         BE    INVCLS_X                                                         
         MVC   WORK(L'PPDDATE),PPDDATE                                          
         MVC   WORK+L'PPDDATE(L'PPDSEQNO),PPDSEQNO                              
         TM    PPDSTAT,X'01'       HAVE SECONDARY SQN?                          
         BZ    INVCLS14                                                         
         LLC   RF,PPAYELEM+1       GET ELEMENT LENGTH                           
         SHI   RF,2                POINT TO LAST 2 BYTES                        
         LA    RF,PPAYELEM(RF)                                                  
         MVC   WORK+L'PPDDATE+L'PPDSEQNO(L'PPSEQN2),0(RF)                       
         DROP  R1                                                               
                                                                                
INVCLS14 LA    R3,IOKEY                                                         
         USING PPCLRST,R3          READ CLEARANCE STATUS RECORDS                
         XC    PPCLKEY,PPCLKEY     TO ESTABLISH CHECK NUMBER                    
         MVC   PPCLAGY,AGY                                                      
         MVC   PPCLMED,PBUYKMED                                                 
         MVI   PPCLTYPE,PPCLTYPQ                                                
         MVC   PPCLCLT,PBUYKCLT                                                 
         MVC   PPCLPUB(L'PBUYKPUB),PBUYKPUB                                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO7'                               
         BNE   INVCLS_X                                                         
                                                                                
INVCLS24 CLC   IOKEY(11),IOKEYSAV                                               
         BNE   INVCLS_X                                                         
                                                                                
INVCLS30 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO7'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,IOADDR                                                        
         LA    R3,PPCLELEM-PPCLRST(R3)                                          
         USING PPCLELEM,R3         TRY FOR PAYMENT ON THIS RECORD               
         SR    R0,R0                                                            
INVCLS40 CLI   PPCLELEM,0          TEST END OF RECORD                           
         BE    INVCLS46                                                         
         CLI   PPCLELEM,PPCLELQ    TEST CLEARANCE ELEMENT                       
         BNE   INVCLS42                                                         
         L     R1,FULL1                                                         
         USING PPAYELEM,R1         PAYMENT ELEMENTS                             
         CLC   PPCLCLRD,PPDDATE                                                 
         BNE   INVCLS42            NO MATCH ON DATE                             
         CLC   PPCLCLSQ,PPDSEQNO                                                
         BNE   INVCLS42            NO MATCH ON SEQ#                             
         CLI   PPDSEQNO,X'FF'                                                   
         BNE   INVCLS50            OKAY IF NOT 255                              
         TM    PPDSTAT,X'01'                                                    
         BZ    INVCLS50            OKAY IF NO SECONDARY SQN                     
         LA    RF,WORK+L'PPDDATE+L'PPDSEQNO                                     
         CLC   PPCLCLS2,0(RF)      MATCH ON SECONDARY SQN                       
         BE    INVCLS50                                                         
         DROP  R1                                                               
INVCLS42 IC    R0,PPCLELEM+1       BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     INVCLS40                                                         
                                                                                
INVCLS46 XC    IOKEY,IOKEY                                                      
         L     RF,IOADDR                                                        
         MVC   IOKEY(25),0(RF)                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO7'                               
         BNE   INVCLS_X                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO7'                               
         BE    INVCLS24            GO PROCESS NEXT RECORD                       
                                                                                
INVCLS50 MVC   IPASTAT1,PPCLSTAT                                                
         TM    PPCLSTAT,X'02'      CLEARANCE STATUS 03 ELEMS FOLLOW?            
         BZ    INVCLS_X                                                         
                                                                                
         XC    ELEM2,ELEM2                                                      
I        USING INVCTOTD,ELEM2                                                   
         LHI   RE,INVCTLNQ                                                      
         CHI   RE,L'ELEM2                                                       
         BNH   *+6                                                              
         DC    H'0'                WORK AREA OVERFLOW!                          
         XC    I.GCSVEL03,I.GCSVEL03                                            
         XC    I.GCSVEL05,I.GCSVEL05                                            
         ST    R3,I.GCSEL01A       SAVE ELEM POINTER                            
         BRAS  RE,GETTOTS          GET POSITIVE AND NEGATIVE TOTALS             
         BRAS  RE,GCDTOTS          SAME FOR CD AMOUNTS                          
         L     R3,I.GCSEL01A       RESTORE ELEM POINTER                         
                                                                                
         LLC   RF,PPCLEL01+1       GET ELEMENT LENGTH                           
         LA    R5,PPCLEL01(RF)     Bump to next element                         
                                                                                
INVCLS52 CLI   0(R5),0             DONE IF END OF RECORD                        
         BE    INVCLS70                                                         
         CLI   0(R5),X'01'         DONE IF ANOTHER X'01' ELEMENT                
         BE    INVCLS70                                                         
         CLI   0(R5),X'03'         MUST BE INVOICE ELEMENT                      
         BNE   INVCLS68                                                         
                                                                                
         MVI   BYTE3,0             Init payment ratio calculation flag          
         BRAS  RE,TSTBYINV         Test buy invoice                             
         JNE   *+12                                                             
         BRAS  RE,CMPBYINV         Compare buy invoice                          
         JNE   INVCLS68                                                         
                                                                                
         XC    IPAD(IPASVLNQ),IPAD                                              
         MVC   IPADATE,ELEM                                                     
         MVC   IPAPAYEE,ELEM+L'IPADATE                                          
         MVC   IPAACODE,ELEM+L'IPADATE+L'IPAPAYEE                               
         MVC   IPASEQ_#,WORK+L'PPDDATE                                          
                                                                                
         USING PPCLEL03,R5         ESTABLISH INVOICE ELEMENT                    
         MVC   IPAINVNO,PPCLINV    INVOICE NUMBER                               
         TM    PPCLSTAT,X'08'      Script Pay Upload?                           
         JZ    *+10                                                             
         MVC   IPAINVTX,=C' (Script) '                                          
         TM    PPCLSTAT,X'10'      Autopay?                                     
         JZ    *+10                                                             
         MVC   IPAINVTX,=C' (Autopay)'                                          
         CLI   PPCLSRCE,PPCLSPRQ   Prisma?                                      
         JNE   *+10                                                             
         MVC   IPAINVTX,=C' (Prisma) '                                          
         CLI   PPCLSRCE,PPCLSRAQ   Radia?                                       
         JNE   *+10                                                             
         MVC   IPAINVTX,=C' (Radia)  '                                          
                                                                                
         CLC   PPCLINV,PPCLINV-PPCLEL03+I.GCSVEL03     NEW INVOICE?             
         BE    *+10                                                             
         XC    I.GCSVEL05,I.GCSVEL05                                            
         MVC   I.GCSVEL03,PPCLEL03                                              
                                                                                
         LLC   RF,1(R5)            GET ELEMENT LENGTH                           
         AR    R5,RF               BUMP TO NEXT ELEMENT                         
                                                                                
         CLI   0(R5),X'05'         MUST BE A CHECK ELEMENT                      
         BNE   INVCLS68                                                         
                                                                                
         USING PPCLEL05,R5         ESTABLISH CHECK ELEMENT                      
*                                                                               
* CALCULATE PERCENT OF PAYMENT ON THIS INVOICE                                  
*                                                                               
         ZAP   I.GCSRTIOD,=P'0'    INIT DEBIT  RATIO                            
         ZAP   I.GCSRTIOC,=P'0'    INIT CREDIT RATIO                            
                                                                                
         LA    R7,I.GCSRTIOD       POINT TO DEBIT RATIO                         
         LA    RE,I.GCSDEBN        ASSUME NET DEBIT TOTALS                      
         ICM   RF,15,PCL5NET       GET CLEARED NET                              
         TM    PPCLSTAT,X'20'      IF NOT CLEARED NET                           
         BO    *+12                                                             
         LA    RE,I.GCSDEBG        POINT TO TOTAL DEBIT GROSS                   
         ICM   RF,15,PCL5GRS       GET CLEARED GROSS                            
                                                                                
         LTR   RF,RF               IF NEGATIVE AMOUNT                           
         BNM   *+12                                                             
         LA    RE,16(RE)           USE CREDIT TOTALS                            
         LA    R7,16(R7)           USE CREDIT RATIO                             
                                                                                
         CVD   RF,DUB2                                                          
         ZAP   TEMP2(16),DUB2                                                   
                                                                                
         ICM   RF,15,0(RE)         GET TOTAL CLEARED                            
         BNZ   *+14                                                             
         ZAP   0(8,R7),=P'1000000' IF ZERO, RATIO IS 1                          
         B     INVCLS53                                                         
                                                                                
         CVD   RF,DUB2                                                          
                                                                                
         SRP   TEMP2(16),7,0       * 10**7 FOR ROUNDING                         
                                                                                
         DP    TEMP2(16),DUB2      CALCULATE RATIO INVOICE TO TOTAL             
         SRP   TEMP2(8),64-1,5     ROUND TO 6 DECIMALS                          
         ZAP   0(8,R7),TEMP2(8)    SAVE RATIO                                   
                                                                                
INVCLS53 CP    I.GCSRTIOC,PZERO    IF NO CREDIT RATIO                           
         BNZ   *+10                                                             
         ZAP   I.GCSRTIOC,I.GCSRTIOD     DEFAULT TO DEBIT RATIO                 
                                                                                
         CP    I.GCSRTIOD,PZERO    IF NO DEBIT RATIO                            
         BNZ   *+10                                                             
         ZAP   I.GCSRTIOD,I.GCSRTIOC     DEFAULT TO CREDIT RATIO                
                                                                                
         LA    RE,I.GCSRTIOD       ASSUME DEBIT                                 
         L     R7,FULL1            POINT TO PAY ELEM                            
         USING PPAYELEM,R7         PAYMENT ELEMENTS                             
         ICM   RF,15,PPGROSS       GET GROSS CLEARED FOR BUY                    
         BNM   *+8                                                              
         LA    RE,I.GCSRTIOC       USE CREDIT RATIO IF NEGATIVE                 
         DROP  R7                                                               
                                                                                
         CVD   RF,DUB2             GCD                                          
         ZAP   TEMP2(16),DUB2      MOVE TO WORKAREA                             
                                                                                
         MP    TEMP2(16),0(8,RE)   CALCULATE GROSS ON THIS INVOICE              
         SRP   TEMP2(16),64-6,5    ROUND TO PENNIES                             
                                                                                
         MVC   DUB1,TEMP2+8                                                     
         CVB   RF,DUB1                                                          
         STCM  RF,15,IPAGROSS      GROSS                                        
                                                                                
         L     R7,FULL1            POINT TO PAY ELEM                            
         USING PPAYELEM,R7         PAYMENT ELEMENTS                             
         ICM   RF,15,PPGROSS       GET GROSS CLEARED FOR BUY                    
         ICM   R0,15,PPAGYCOM      GET AGY COMM CLEARED FOR BUY                 
         SR    RF,R0               GET NET CLEARED FOR BUY                      
                                                                                
         CVD   RF,DUB2                                                          
         ZAP   TEMP2(16),DUB2                                                   
                                                                                
         MP    TEMP2(16),0(8,RE)   CALCULATE NET ON THIS INVOICE                
         SRP   TEMP2(16),64-6,5    ROUND TO PENNIES                             
                                                                                
         MVC   DUB1,TEMP2+8                                                     
         CVB   RF,DUB1                                                          
         STCM  RF,15,IPANET        NET                                          
                                                                                
*                                                                               
         CLI   BYTE3,C'Y'          Need to calculate ratio?                     
         JNE   INVCLS57                                                         
         MVC   IPAGROSS,PCL5GRS    Cleared gross                                
         MVC   IPANLCDP,PCL5NET    Cleared net less CD paid                     
         OC    PCL5GRS,PCL5GRS     Have cleared gross?                          
         JNZ   INVCLS54                                                         
         OC    PCL5NET,PCL5NET     Have cleared net?                            
         JZ    INVCLS55                                                         
         CLC   PPAGYCOM,PPGROSS    Agency commission is 100%?                   
         JNE   *+14                                                             
         MVC   IPAGROSS,PPGROSS                                                 
         J     INVCLS54                                                         
         OC    PPAGYCOM,PPAGYCOM   Have agency commission?                      
         JNZ   INVCL53H                                                         
         ICM   RF,15,PCL5NET                                                    
         ICM   RE,15,PCL5CD                                                     
         AR    RF,RE                                                            
         STCM  RF,15,IPAGROSS                                                   
         J     INVCLS54                                                         
INVCL53H ICM   RF,15,PPAGYCOM                                                   
         CVD   RF,DUB2                                                          
         MP    DUB2,=P'100000'                                                  
         ZAP   TEMP2(16),DUB2                                                   
         ICM   RF,15,PPGROSS                                                    
         CVD   RF,DUB2                                                          
         DP    TEMP2(16),DUB2      Calculate agency commission %                
         ZAP   DUB2,=P'100000'                                                  
         SP    DUB2,TEMP2(8)       100% - AC% = Net%                            
         ICM   RF,15,PCL5NET                                                    
         ICM   RE,15,PCL5CD                                                     
         AR    RF,RE                                                            
         ZAP   TEMP2(16),=P'0'                                                  
         CVD   RF,DUB                                                           
         MVC   TEMP2+8(8),DUB                                                   
         MP    TEMP2(16),=P'100000'                                             
         DP    TEMP2(16),DUB2      Calculate gross amount                       
         MVC   DUB,TEMP2                                                        
         CVB   RF,DUB                                                           
         STCM  RF,15,IPAGROSS                                                   
                                                                                
INVCLS54 OC    PCL5NET,PCL5NET     Have cleared net?                            
         JNZ   INVCLS55                                                         
         ICM   RF,15,PPAGYCOM                                                   
         CVD   RF,DUB2                                                          
         MP    DUB2,=P'100000'                                                  
         ZAP   TEMP2(16),DUB2                                                   
         ICM   RF,15,PPGROSS                                                    
         CVD   RF,DUB2                                                          
         DP    TEMP2(16),DUB2      Calculate agency commission %                
         ZAP   DUB2,TEMP2(8)                                                    
         ZAP   TEMP2(16),=P'100000'                                             
         SP    TEMP2(16),DUB2      100% - AC% = Net %                           
         ICM   RF,15,PCL5GRS                                                    
         CVD   RF,DUB2                                                          
         MP    TEMP2(16),DUB2      Get net amount                               
         ZAP   DUB2,=P'100000'                                                  
         DP    TEMP2(16),DUB2                                                   
         ZAP   DUB2,TEMP2(8)                                                    
         CVB   RF,DUB2                                                          
         STCM  RF,15,IPANLCDP      Net less CD paid                             
                                                                                
INVCLS55 TM    IPASTAT1,X'20'      Cleared net?                                 
         JZ    INVCLS56                                                         
         ICM   RF,15,IPANLCDP      Net less CD paid                             
         ICM   R1,15,PCL5CD        CD                                           
         AR    RF,R1                                                            
         STCM  RF,15,IPANET        Net paid                                     
         J     INVCLS60                                                         
                                                                                
INVCLS56 MVC   IPANET,IPANLCDP     Net paid                                     
         ICM   RF,15,IPANET                                                     
         ICM   R1,15,PCL5CD        CD                                           
         SR    RF,R1                                                            
         STCM  RF,15,IPANLCDP      Net less CD paid                             
         J     INVCLS60                                                         
         DROP  R7                                                               
*                                                                               
* CALCULATE PERCENT OF PAYMENT OF CD ON THIS INVOICE                            
*                                                                               
INVCLS57 ZAP   I.GCSRTCDD,=P'0'    INIT DEBIT  RATIO                            
         ZAP   I.GCSRTCDC,=P'0'    INIT CREDIT RATIO                            
                                                                                
         LA    RE,I.GCSDEBCD       ASSUME CD DEBIT TOTALS                       
         ICM   RF,15,PCL5CD        GET CLEARED CD                               
         BNM   *+8                 IF NEGATIVE                                  
         LA    RE,I.GCSCRDCD       USE CREDIT TOTALS                            
                                                                                
         CVD   RF,DUB2                                                          
         ZAP   TEMP2(16),DUB2                                                   
                                                                                
         ICM   RF,15,0(RE)         GET TOTAL CLEARED                            
         BNZ   *+14                                                             
         ZAP   4(8,RE),=P'1000000' IF ZERO, RATIO IS 1                          
         B     INVCLS58                                                         
                                                                                
         CVD   RF,DUB2                                                          
                                                                                
         SRP   TEMP2(16),7,0       * 10**7 FOR ROUNDING                         
                                                                                
         DP    TEMP2(16),DUB2      CALCULATE RATIO INVOICE TO TOTAL             
         SRP   TEMP2(8),64-1,5     ROUND TO 6 DECIMALS                          
         ZAP   4(8,RE),TEMP2(8)    SAVE RATIO                                   
                                                                                
INVCLS58 CP    I.GCSRTCDC,PZERO    IF NO CREDIT RATIO                           
         BNZ   *+10                                                             
         ZAP   I.GCSRTCDC,I.GCSRTCDD     DEFAULT TO DEBIT RATIO                 
                                                                                
         CP    I.GCSRTIOD,PZERO    IF NO DEBIT RATIO                            
         BNZ   *+10                                                             
         ZAP   I.GCSRTCDD,I.GCSRTCDC     DEFAULT TO CREDIT RATIO                
                                                                                
         LA    RE,I.GCSRTCDD       ASSUME DEBIT                                 
         L     R7,FULL1            POINT TO PAY ELEM                            
         USING PPAYELEM,R7         PAYMENT ELEMENTS                             
         ICM   RF,15,PPCSHDSC      GET CASH DISC CLEARED FOR BUY                
         BNM   *+8                                                              
         LA    RE,I.GCSRTCDC       USE CREDIT RATIO IF NEGATIVE                 
         DROP  R7                                                               
                                                                                
         CVD   RF,DUB2                                                          
         ZAP   TEMP2(16),DUB2                                                   
                                                                                
         MP    TEMP2(16),0(8,RE)   CALCULATE CSH DISC ON THIS INVOICE           
         SRP   TEMP2(16),64-6,5    ROUND TO PENNIES                             
                                                                                
         MVC   DUB1,TEMP2+8                                                     
         CVB   RF,DUB1             Cash Discount                                
         ICM   R1,15,IPANET        Net                                          
         SR    R1,RF                                                            
         STCM  R1,15,IPANLCDP      Net less CD Paid                             
                                                                                
INVCLS60 OC    PCL5CHK,PCL5CHK     SKIP IF NO CHECK ISSUED                      
         BNZ   INVCLS62                                                         
                                                                                
         OC    PCL5CHK-PPCLEL05+I.GCSVEL05,PCL5CHK-PPCLEL05+I.GCSVEL05          
         BZ    INVCLS66            SKIP IF INV NOT PAID BEFORE                  
                                                                                
         MVC   PCL5CHK,PCL5CHK-PPCLEL05+I.GCSVEL05                              
         MVC   PCL5CHDT,PCL5CHDT-PPCLEL05+I.GCSVEL05                            
                                                                                
INVCLS62 MVC   IPACHECK,PCL5CHK                                                 
         CLC   PCL5CHK,SPACES      IF SPACES CHK WAS VOIDED                     
         BE    *+10                                                             
         CLC   =C'VOID',PCL5CHK    OR VOID, CHK WAS VOIDED                      
         BNE   INVCLS64                                                         
         XC    IPACHECK,IPACHECK                                                
         MVC   IPACHECK,=C'VOIDED'                                              
         B     INVCLS66                                                         
INVCLS64 MVC   IPACKDAT,PCL5CHDT                                                
                                                                                
INVCLS66 TM    PCL5STAT,PCL5STAT_CK                                             
         JZ    *+10                                                             
         MVC   IPACPTYP,=C'CK'                                                  
         TM    PCL5STAT,PCL5STAT_CR                                             
         JZ    *+10                                                             
         MVC   IPACPTYP,=C'CR'                                                  
                                                                                
         SR    R4,R4                                                            
         ICM   R4,3,IPA#                                                        
         LA    RF,1(R4)                                                         
         STCM  RF,3,IPA#                                                        
         MHI   R4,IPAL                                                          
         A     R4,AIO4             POINT TO NEXT OUTPUT ENTRY                   
                                                                                
         MVC   I.GCSVEL05,PPCLEL05                                              
                                                                                
INVCLS68 LLC   RF,PPCLEL05+1       GET ELEMENT LENGTH                           
         LA    R5,PPCLEL05(RF)     BUMP TO NEXT ELEMENT                         
         B     INVCLS52            LOOP FOR MORE                                
                                                                                
INVCLS70 OC    IPAD(IPAL),IPAD     ANY OUTPUT IN CURRENT ENTRY?                 
         BNZ   INVCLS_X                                                         
         SR    RE,RE                                                            
         ICM   RE,3,IPA#                                                        
         SHI   RE,1                                                             
         STCM  RE,3,IPA#           ADJUSTMENT - ONE ENTRY TOO MANY              
                                                                                
INVCLS_X MVC   IOKEY,SAVEKEY       RESTORE SAVED KEY                            
         J     EXIT                                                             
                                                                                
*                                                                               
TSTBYINV CLI   CLTPRBY+15,C'Y'     Pay detail for linked inv only?              
         JNE   BYINVNEQ                                                         
         L     R1,AIO2             Buy record is still in IO2                   
         LA    R1,33(R1)           Test buy has invoices                        
         CLI   0(R1),X'20'         Buy description element present?             
         JE    *+6                                                              
         DC    H'0'                                                             
TBYINV10 CLI   0(R1),0             Invoice# not found?                          
         JE    BYINVNEQ                                                         
         CLI   0(R1),X'CC'         Custom column element?                       
         JE    TBYINV30                                                         
         CLI   0(R1),X'51'         New invoice element?                         
         JE    BYINVEQ                                                          
TBYINV16 LLC   R0,1(R1)                                                         
         AR    R1,R0               Bump to next element                         
         J     TBYINV10                                                         
TBYINV30 CLC   =AL2(8213),(BYCCSQN-BYCCELM)(R1)                                 
         JNE   TBYINV16                                                         
         J     BYINVEQ                                                          
*                                                                               
CMPBYINV L     R1,AIO2             Buy record is still in IO2                   
         LA    R1,33(R1)           Test buy has invoices                        
         CLI   0(R1),X'20'         Buy description element present?             
         JE    *+6                                                              
         DC    H'0'                                                             
CBYINV10 CLI   0(R1),0             No matching invoice#?                        
         JE    BYINVNEQ                                                         
         CLI   0(R1),X'CC'         Custom column element?                       
         JE    CBYINV30                                                         
         CLI   0(R1),X'51'         New invoice element?                         
         JE    CBYINV50                                                         
CBYINV16 LLC   R0,1(R1)                                                         
         AR    R1,R0               Bump to next element                         
         J     CBYINV10                                                         
CBYINV30 CLC   =AL2(8213),(BYCCSQN-BYCCELM)(R1)                                 
         JNE   CBYINV16                                                         
         CLC   (PPCLINV-PPCLEL03)(L'PPCLINV,R5),BYCCHDRL(R1)                    
         JE    CBYINV70                                                         
         J     CBYINV16                                                         
*                                                                               
CBYINV50 CLC   (PPCLINV-PPCLEL03)(L'PPCLINV,R5),(PBNVINV#-PBNVELM)(R1)          
         JE    CBYINV70                                                         
         J     CBYINV16                                                         
*                                                                               
CBYINV70 MVI   BYTE3,C'Y'          Don't need to calc payment ratio             
*                                                                               
BYINVEQ  CR    RE,RE               Equal                                        
         J     *+6                                                              
BYINVNEQ LTR   RE,RE               Not equal                                    
         BR    RE                                                               
*                                                                               
* TO GET POSITIVE AND NEGATIVE TOTALS FOR PAYMENT                               
* DONE BY SUMMING DEBITS AND CREDITS SEPARATELY                                 
*                                                                               
GETTOTS  LR    R0,RE                                                            
         L     R7,FULL1                                                         
         USING PPAYELEM,R7         PAYMENT ELEMENTS                             
*                                                                               
         XC    I.GCSDEBG,I.GCSDEBG INIT TOTAL DEBITS  GROSS                     
         XC    I.GCSDEBN,I.GCSDEBN INIT TOTAL DEBITS  NET                       
         XC    I.GCSCRDG,I.GCSCRDG INIT TOTAL CREDITS GROSS                     
         XC    I.GCSCRDN,I.GCSCRDN INIT TOTAL CREDITS NET                       
*                                                                               
         ICM   R5,15,I.GCSEL01A    POINT TO CURRENT 01 ELEMENT                  
         BZ    GETTOTSX            NONE - GET OUT                               
*                                                                               
         ICM   R1,15,PPGROSS       GET GROSS PAID                               
*                                                                               
         TM    PPCLSTAT,X'20'      IF PAID NET                                  
         BNO   GTT10                                                            
*                                                                               
         ICM   RF,15,PPAGYCOM      GET AGENCY COMMISSION                        
         ICM   RE,15,PPCSHDSC      GET CD                                       
         SR    R1,RE               CALC NET                                     
         SR    R1,RF                                                            
*                                                                               
GTT10    DS    0H                                                               
         LTR   R1,R1               MUST BE NON-NEGATIVE                         
         BZ    GTT20                                                            
*                                                                               
         ICM   RE,15,PPCLGRS       GET CHECK TOTAL GROSS                        
*                                                                               
         TM    PPCLSTAT,X'20'      IF PAID NET                                  
         BNO   *+8                                                              
         ICM   RE,15,PPCLNET       GET CHECK TOTAL NET                          
*                                                                               
         CR    R1,RE               SKIP IF NOT EQUAL - MULTIPLE BUYS            
         BNE   GTT20                                                            
*                                                                               
         STCM  R1,15,I.GCSDEBN     SET NET TOTALS                               
         STCM  R1,15,I.GCSCRDN     SET NET TOTALS                               
*                                                                               
         TM    PPCLSTAT,X'20'      SKIP IF PAID NET                             
         BO    *+12                                                             
         STCM  R1,15,I.GCSDEBG     SET GROSS TOTALS                             
         STCM  R1,15,I.GCSCRDG     SET GROSS TOTALS                             
*                                                                               
         B     GETTOTSX                                                         
         DROP  R7                                                               
*                                                                               
GTT20    DS    0H                                                               
*                                                                               
         LLC   RF,PPCLEL01+1       GET ELEMENT LENGTH                           
         LA    R5,PPCLEL01(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
GTTLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R5),0             DONE IF END OF RECORD                        
         BE    GTTDONE                                                          
*                                                                               
         CLI   0(R5),X'01'         DONE IF ANOTHER X'01' ELEMENT                
         BE    GTTDONE                                                          
*                                                                               
         CLI   0(R5),X'05'         MUST BE 05      ELEMENT                      
         BNE   GTTCONT                                                          
*                                                                               
         USING PPCLEL05,R5         ESTABLISH 05 ELEMENT                         
*                                                                               
         TM    PPCLSTAT,X'20'      SKIP IF NOT CLEARED NET                      
         BNO   GTTNETN                                                          
*                                                                               
         LA    R1,I.GCSDEBN        POINT TO CURRENT DEBIT TOTAL NET             
*                                                                               
         ICM   RF,15,PCL5NET       GET CLEARED NET                              
         BNM   *+8                                                              
         LA    R1,I.GCSCRDN        POINT TO CREDIT TOTAL IF MINUS               
*                                                                               
         ICM   RE,15,0(R1)         UPDATE TOTAL                                 
         AR    RE,RF                                                            
         STCM  RE,15,0(R1)                                                      
*                                                                               
         B     GTTGRSX                                                          
*                                                                               
GTTNETN  DS    0H                  PAID GROSS                                   
*                                                                               
         LA    R1,I.GCSDEBG        POINT TO CURRENT DEBIT TOTAL GRS             
*                                                                               
         ICM   RF,15,PCL5GRS       GET CLEARED GROSS                            
         BNM   *+8                                                              
         LA    R1,I.GCSCRDG        POINT TO CREDIT TOTAL IF MINUS               
*                                                                               
         ICM   RE,15,0(R1)         UPDATE TOTAL                                 
         AR    RE,RF                                                            
         STCM  RE,15,0(R1)                                                      
*                                                                               
GTTGRSX  DS    0H                                                               
*                                                                               
GTTCONT  DS    0H                                                               
*                                                                               
         LLC   RF,PPCLEL05+1       GET ELEMENT LENGTH                           
         LA    R5,PPCLEL05(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
         B     GTTLOOP                                                          
*                                                                               
GTTDONE  DS    0H                                                               
*                                                                               
GETTOTSX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* TO GET POSITIVE AND NEGATIVE TOTALS FOR CD PAYMENT                            
* DONE BY SUMMING DEBITS AND CREDITS SEPARATELY                                 
*                                                                               
GCDTOTS  LR    R0,RE                                                            
         XC    I.GCSDEBCD,I.GCSDEBCD   INIT TOTAL DEBIT  CD                     
         XC    I.GCSCRDCD,I.GCSCRDCD   INIT TOTAL CREDIT CD                     
*                                                                               
         L     R7,FULL1                                                         
         USING PPAYELEM,R7         PAYMENT ELEMENTS                             
*                                                                               
         ICM   R1,15,PPCSHDSC      GET CASH DISCOUNT PAID                       
         BZ    GCT20                  MUST BE NON-NEGATIVE                      
*                                                                               
         ICM   RF,15,PCL5CD        GET CLEARED NET CD                           
*                                                                               
         CR    R1,RF               SKIP IF NOT EQUAL - MULTIPLE BUYS            
         BNE   GCT20                                                            
*                                                                               
         STCM  R1,15,I.GCSDEBCD    SET CD TOTALS                                
         STCM  R1,15,I.GCSCRDCD    SET CD TOTALS                                
*                                                                               
         B     GETTOTSX                                                         
         DROP  R7                                                               
*                                                                               
GCT20    DS    0H                                                               
*                                                                               
         ICM   R5,15,I.GCSEL01A    POINT TO CURRENT 01 ELEMENT                  
         BZ    GETTOTSX            NONE - GET OUT                               
*                                                                               
         LLC   RF,PPCLEL01+1       GET ELEMENT LENGTH                           
         LA    R5,PPCLEL01(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
GCDLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R5),0             DONE IF END OF RECORD                        
         BE    GCDDONE                                                          
*                                                                               
         CLI   0(R5),X'01'         DONE IF ANOTHER X'01' ELEMENT                
         BE    GCDDONE                                                          
*                                                                               
         CLI   0(R5),X'05'         MUST BE 05      ELEMENT                      
         BNE   GCDCONT                                                          
*                                                                               
         USING PPCLEL05,R5         ESTABLISH 05 ELEMENT                         
*                                                                               
         LA    R1,I.GCSDEBCD       POINT TO CURRENT DEBIT TOTAL CD              
*                                                                               
         ICM   RF,15,PCL5CD        GET CLEARED NET CD                           
         BNM   *+8                                                              
         LA    R1,I.GCSCRDCD       POINT TO CREDIT TOTAL CD IF MINUS            
*                                                                               
         ICM   RE,15,0(R1)         UPDATE TOTAL                                 
         AR    RE,RF                                                            
         STCM  RE,15,0(R1)                                                      
*                                                                               
GCDCONT  DS    0H                                                               
*                                                                               
         LLC   RF,PPCLEL05+1       GET ELEMENT LENGTH                           
         LA    R5,PPCLEL05(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
         B     GCDLOOP                                                          
*                                                                               
GCDDONE  DS    0H                                                               
*                                                                               
GCDTOTSX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  I                                                                
         DROP  R3,R4,R5,RB                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ JOB RECORD AND ESTABLISH JOB CAPTION                           *         
***********************************************************************         
                                                                                
GETCAP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   SAVEKEY,IOKEY       SAVE THE CURRENT RECORD KEY                  
                                                                                
         LA    R3,IOKEY                                                         
         USING PJOBRECD,R3         READ JOB RECORD                              
         XC    PJOBKEY,PJOBKEY                                                  
         MVC   PJOBKAGY,AGY                                                     
         MVC   PJOBKMED,PBUYKMED                                                
         MVI   PJOBKRCD,PJOBKRCQ                                                
         MVC   PJOBKCLT,PBUYKCLT                                                
         MVC   PJOBKPRD,PBUYKPRD                                                
         MVC   PJOBKJOB,PBDJOB                                                  
                                                                                
         CLC   LASTJOB,PJOBKEY     TEST SAME JOB AS LAST TIME                   
         BE    GETCAP80                                                         
         MVC   LASTJOB,PJOBKEY     NO - SET LAST JOB KEY                        
         XC    LASTCAP,LASTCAP     CLEAR LAST CAPTION                           
         XC    LASTADID,LASTADID   CLEAR LAST AD ID                             
         XC    LASTSUBA,LASTSUBA   CLEAR LAST LIST OF AD CODE OR AD ID          
                                                                                
         CLC   PBDJOB,SPACES       HAVE AD CODE?                                
         BNH   GETCAP80                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO7'                               
         BNE   GETCAP80                                                         
         CLC   LASTJOB,PJOBKEY     TEST CORRECT RECORD FOUND                    
         BNE   GETCAP80                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO7'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,IOADDR           POINT TO I/O AREA                            
                                                                                
         MVC   LASTCAP(L'PJOBCAP1),PJOBCAP1                                     
         LA    R1,LASTCAP+(L'PJOBCAP1-1)                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C' '                                                       
         MVC   2(L'PJOBCAP2,R1),PJOBCAP2                                        
                                                                                
         MVC   LASTADID,PJOBADID                                                
                                                                                
         LA    R3,(PJOBELEM-PJOBREC)(R3)                                        
         MVI   ELCODE,PJSUBIDQ     SUB AD CODE ELEM CODE                        
         USING PJSUBEL,R3                                                       
         LA    RF,LASTSUBA                                                      
GETCAP30 BRAS  RE,NXTELEM                                                       
         BNE   GETCAP80                                                         
         MVC   0(L'PJSUBCOD,RF),PJSUBCOD                                        
         CLI   PJSUBCOD,X'FF'                                                   
         BNE   GETCAP32                                                         
         XC    0(L'PJSUBCOD,RF),0(RF)                                           
         CLI   PJSUBLEN,PJSUBMAX                                                
         BL    GETCAP30                                                         
         MVC   0(L'PJSUBAID,RF),PJSUBAID                                        
GETCAP32 CLI   0(RF),C' '                                                       
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)            NEXT ENTRY                                   
         B     GETCAP30                                                         
                                                                                
GETCAP80 MVC   IOKEY,SAVEKEY       RESTORE SAVED KEY                            
         J     EXIT                                                             
         DROP  R2,R3,RB                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK IF INVOICE SERIAL# IS ON FILE                                 *         
***********************************************************************         
                                                                                
TSTINVS# NTR1  BASE=*,LABEL=*      INVOICE SERIAL # IS GOOD?                    
                                                                                
         MVC   SAVEKEY,IOKEY       SAVE THE CURRENT RECORD KEY                  
                                                                                
         LA    RE,IOKEY                                                         
         USING PNVKEY,RE           INVOICE SERIAL# KEY                          
         XC    PNVKEY,PNVKEY                                                    
         MVC   PNVKAGY,AGY                                                      
         MVC   PNVKMED,WORK2                                                    
         MVI   PNVKRCD,PNVKRCDQ                                                 
         MVC   PNVKSER#,WORK2+L'PNVKMED                                         
         MVC   IOKEYSAV,IOKEY                                                   
         DROP  RE                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO7'                               
         BNE   T_INSS#N                                                         
         CLC   IOKEY(PNVKELMK-PNVKEY),IOKEYSAV                                  
         BNE   T_INSS#N                                                         
                                                                                
T_INSS#Y MVC   IOKEY,SAVEKEY       RESTORE SAVED KEY                            
         J     EXITY                                                            
                                                                                
T_INSS#N MVC   IOKEY,SAVEKEY       RESTORE SAVED KEY                            
         J     EXITN                                                            
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET PUBLICATION RECORDS & BUILD PUBLISHER RECORDS FOR DOWNLOADING   *         
***********************************************************************         
                                                                                
GETPUB   J     *+12                                                             
         DC    C'*GETPUB*'                                                      
         LR    RB,RF                                                            
         USING GETPUB,RB                                                        
                                                                                
         TM    FLTMATST,FLTMSNOI                                                
         JNZ   NOMORE              DON'T EXECUTE FOR RNO DOWNLOAD               
                                                                                
         TM    GIND1,GIONLINE                                                   
         JNZ   NOMORE              DON'T EXECUTE IF RUNNING ONLINE              
                                                                                
         CLI   DL_TYPE,DL_PRBQ                                                  
         JE    NOMORE              DON'T EXECUTE FOR PROBE DOWNLOAD             
                                                                                
         USING LP_D,R1                                                          
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   GETPUB02                                                         
         XC    PBUKEY(PBUKEYL),PBUKEY                                           
         GOTOR VBUFFRIN,DMCB,('BUFFARDH',PUBBUFF),PBUD,ACOMFACS                 
         B     GETPUB04                                                         
                                                                                
GETPUB02 GOTOR VBUFFRIN,DMCB,('BUFFASEQ',PUBBUFF),PBUD,ACOMFACS                 
                                                                                
GETPUB04 JNE   NOMORE                                                           
         L     R1,ALP                                                           
         LA    R0,PBUD                                                          
         STCM  R0,15,LP_ADATA      SET A(PUBLISHER/REP RECORD)                  
         OC    PBUREP,PBUREP       TEST PUBLISHER/REP CODE ESTABLISHED          
         JZ    EXITY               NO - EXIT                                    
                                                                                
         MVC   RBUMED,PBUMED       READ PUBLISHER/REP BUFFER RECORD             
         MVC   RBUREP,PBUREP                                                    
         GOTOR VBUFFRIN,DMCB,('BUFFAGET',REPBUFF),RBUD,ACOMFACS                 
         JE    EXITY               ALREADY READ IT BEFORE                       
                                                                                
         LA    R2,IOKEY                                                         
         USING PREPRECD,R2         READ PUBLISHER/REP RECORD                    
         XC    PREPKEY,PREPKEY                                                  
         MVC   PREPKAGY,AGY                                                     
         MVC   PREPKMED,PBUMED                                                  
         MVI   PREPKRCD,PREPKRCQ                                                
         MVC   PREPKREP,PBUREP                                                  
                                                                                
         MVC   RBUMED,PBUMED                                                    
         MVC   RBUREP,PBUREP                                                    
         MVI   RBUNAME,C'?'                                                     
         MVC   RBUNAME+1(L'RBUNAME-1),RBUNAME                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETPUB06                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         MVC   RBUNAME,PREPNAME                                                 
                                                                                
GETPUB06 GOTOR VBUFFRIN,DMCB,('BUFFAPUT',REPBUFF),RBUD,ACOMFACS                 
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET PUBLISHER/REP RECORDS FOR DOWNLOADING                           *         
***********************************************************************         
                                                                                
GETREP   J     *+12                                                             
         DC    C'*GETREP*'                                                      
         LR    RB,RF                                                            
         USING GETREP,RB                                                        
                                                                                
         TM    FLTMATST,FLTMSNOI                                                
         JNZ   NOMORE              DON'T EXECUTE FOR RNO DOWNLOAD               
                                                                                
         TM    GIND1,GIONLINE      DON'T EXECUTE IF RUNNING ONLINE              
         JNZ   NOMORE                                                           
                                                                                
         CLI   DL_TYPE,DL_PRBQ                                                  
         JE    NOMORE              DON'T EXECUTE FOR PROBE DOWNLOAD             
                                                                                
         USING LP_D,R1                                                          
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   GETREP02                                                         
         XC    RBUKEY(RBUKEYL),RBUKEY                                           
         GOTOR VBUFFRIN,DMCB,('BUFFARDH',REPBUFF),RBUD,ACOMFACS                 
         B     GETREP04                                                         
                                                                                
GETREP02 GOTOR VBUFFRIN,DMCB,('BUFFASEQ',REPBUFF),RBUD,ACOMFACS                 
                                                                                
GETREP04 JNE   NOMORE                                                           
         L     R1,ALP                                                           
         LA    R0,RBUD                                                          
         STCM  R0,15,LP_ADATA      SET A(PUBLISHER/REP RECORD)                  
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET CLIENT PROFILES                                      *         
***********************************************************************         
                                                                                
GETCPR   NTR1  ,                                                                
         L     R2,AIO1             R2=A(CLIENT RECORD)                          
         USING PCLTRECD,R2                                                      
         XC    CLTPROFS(CLTPROFL),CLTPROFS                                      
         XC    WORK(12),WORK                                                    
         MVC   WORK+00(4),=C'P0B1'                                              
         MVC   WORK+04(L'AGY),AGY                                               
         MVC   WORK+06(L'PCLTKMED),PCLTKMED                                     
         MVC   WORK+07(L'PCLTKCLT),PCLTKCLT                                     
         CLI   PCLTOFF,C' '                                                     
         JNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(L'PCLTOFF),PCLTOFF                                       
         GOTOR VGETPROF,DMCB,(X'C0',WORK),CLTPRB1,VDATAMGR                      
         MVC   WORK+00(4),=C'pB1X'                                              
         GOTOR VGETPROF,DMCB,(X'C0',WORK),CLTPRB1X,VDATAMGR                     
         MVC   WORK+00(4),=C'P0BY'                                              
         GOTOR VGETPROF,DMCB,(X'C0',WORK),CLTPRBY,VDATAMGR                      
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ BUY RECORD USING SERIAL NUMBER - R1=A(SERIAL NUMBER)           *         
***********************************************************************         
                                                                                
GETBUY   NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING PSERRECD,R2         READ SERIAL PASSIVE POINTER                  
         XC    PSERKEY,PSERKEY                                                  
         MVC   PSERKAGY,AGY                                                     
         MVC   PSERKMED,BUYSMED-BUYSER(R1)                                      
         MVI   PSERKTYP,PSERKTYQ                                                
         MVC   PSERKCLT,BUYSCLT-BUYSER(R1)                                      
         PACK  DUB,BUYSSER-BUYSER(,R1)                                          
         ZAP   WORK(L'DUB),=P'1000000000'                                       
         SP    WORK(L'DUB),DUB                                                  
         MVC   PSERKSER,WORK+3                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORDD+IODIR+IO2'                              
         JE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         JZ    EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2+IORDEL'                       
         JE    EXITY                                                            
         TM    IOERR,IOEDEL                                                     
         JNZ   EXITY                                                            
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ INVOICE RECORD USING SERIAL NUMBER - R1=A(SERIAL NUMBER)       *         
***********************************************************************         
                                                                                
GETINV   NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING PNVKEY,R2                                                        
         XC    PNVKEY,PNVKEY                                                    
         MVC   PNVKAGY,AGY                                                      
         MVC   PNVKMED,INVMED-INVKEY(R1)                                        
         MVI   PNVKRCD,PNVKRCDQ                                                 
         XC    WORK,WORK                                                        
         MVC   WORK(L'INVSER#),INVSER#-INVKEY(R1)                               
         MVI   WORK+L'INVSER#,C'0'                                              
         PACK  DUB,WORK(L'INVSER#+1)                                            
         MVC   PNVKSER#,DUB+2                                                   
         MVI   PNVKELMK,FF                                                      
         MVC   PNVKELMK+1(L'PNVKELMK-1),PNVKELMK                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDD+IODIR+IO2'                              
         JE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         JZ    EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2+IORDEL'                       
         JE    EXITY                                                            
         TM    IOERR,IOEDEL                                                     
         JNZ   EXITY                                                            
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT INSERTION ORDER NUMBER FOR DOWNLOAD                          *         
***********************************************************************         
                                                                                
FMTION   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         USING PIOELEM,R2          R2=A(INSERTION ORDER ELEMENT)                
         MVC   0(1,R3),0(R1)                                                    
         MVI   1(R3),C'-'                                                       
         GOTOR VDATCON,DMCB,(3,PIODATE),(0,DUB)                                 
         MVC   2(5,R3),DUB+1                                                    
         MVI   7(R3),C'-'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,PIONUM                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  8(4,R3),DUB                                                      
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* FORMAT WEB INSERTION ORDER NUMBER FOR DOWNLOAD                      *         
***********************************************************************         
                                                                                
FMTWION  NTR1  LABEL=NO                                                         
         LM    R2,R4,0(R1)                                                      
         USING PBUYKEY,R2                                                       
         USING PWIOELEM,R3         R3=A(WEB INSERTION ORDER ELEMENT)            
         USING WIOLKEYD,R4                                                      
         MVI   WIOLGKEY,C'-'                                                    
         MVC   WIOLGKEY+1(WIOLGKYL-1),WIOLGKEY                                  
         MVC   WIOLGMED,PBUYKMED                                                
         MVC   FULL1(L'PWIODATE),PWIODATE                                       
         MVC   FULL1(L'PWIO#YER),PWIO#YER                                       
         GOTOR VDATCON,DMCB,(3,FULL1),(10,DUB1)                                 
         MVC   WIOLGYER,DUB1+6                                                  
         MVC   WIOLGCLT,PBUYKCLT                                                
         CLI   WIOLGCLT+2,C' '                                                  
         JNE   *+8                                                              
         MVI   WIOLGCLT+2,C'-'                                                  
         EDITR PWIO#SQ#,WIOLGRNO,0,FILL=0                                       
         MVI   WIOLGRNO+L'WIOLGRNO,C' '                                         
         MVC   WIOLGRET,SPACES                                                  
         MVC   WIOLGRE#,SPACES                                                  
         CLI   PWIO#REV,0                                                       
         JE    FMTWIO40            NO REVISION #                                
         MVI   WIOLGRNO+L'WIOLGRNO,C'-'                                         
         MVC   WIOLGRET,=C'REV'                                                 
         EDITR PWIO#REV,WIOLGRE#,0,FILL=0                                       
                                                                                
FMTWIO40 SR    RE,RE                                                            
         ICM   RE,7,PWIO#SQ#                                                    
         CHI   RE,9999                                                          
         JH    EXIT                                                             
         MVC   WIOLGKEY+DISPER#Q(LNAERF#Q),WIOLGKEY+DISP_R#Q                    
         MVI   WIOLGKEY+(WIOLGKYL-L'WIOLGER#),C' '                              
         J     EXIT                                                             
         DROP  R2,R3,R4                                                         
                                                                                
***********************************************************************         
* FORMAT ENHANCED SPACE RESERVATION NUMBER FOR DOWNLOAD               *         
***********************************************************************         
                                                                                
FMTESRN  NTR1  LABEL=NO                                                         
         LM    R2,R4,0(R1)                                                      
         USING PBUYKEY,R2                                                       
         USING PESRELEM,R3         R3=A(ESR ELEMENT)                            
         USING ESRLKEYD,R4                                                      
         MVI   ESRLGKEY,C'-'                                                    
         MVC   ESRLGKEY+1(ESRLGKYL-1),ESRLGKEY                                  
         MVC   ESRLGSRT,=C'SR'                                                  
         MVC   ESRLGMED,PBUYKMED                                                
         MVC   FULL1(L'PESRDATE),PESRDATE                                       
         MVC   FULL1(L'PESR#YER),PESR#YER                                       
         GOTOR VDATCON,DMCB,(3,FULL1),(10,DUB1)                                 
         MVC   ESRLGYER,DUB1+6                                                  
         MVC   ESRLGCLT,PBUYKCLT                                                
         CLI   ESRLGCLT+2,C' '                                                  
         JNE   *+8                                                              
         MVI   ESRLGCLT+2,C'-'                                                  
         EDITR PESR#SQ#,ESRLGRNO,0,FILL=0                                       
         MVI   ESRLGRNO+L'ESRLGRNO,C' '                                         
         MVC   ESRLGRET,SPACES                                                  
         MVC   ESRLGRE#,SPACES                                                  
         CLI   PESR#REV,0                                                       
         JE    FMTESR40            NO REVISION #                                
         MVI   ESRLGRNO+L'ESRLGRNO,C'-'                                         
         MVC   ESRLGRET,=C'REV'                                                 
         EDITR PESR#REV,ESRLGRE#,0,FILL=0                                       
                                                                                
FMTESR40 SR    RE,RE                                                            
         ICM   RE,7,PESR#SQ#                                                    
         CHI   RE,9999                                                          
         JH    EXIT                                                             
         MVC   ESRLGKEY+D_SR_E#Q(L_SR_E#Q),ESRLGKEY+D_SR_R#Q                    
         MVI   ESRLGKEY+(ESRLGKYL-L'ESRLGER#),C' '                              
         J     EXIT                                                             
         DROP  R2,R3,R4                                                         
                                                                                
***********************************************************************         
* FORMAT INVOICE NUMBER FOR DOWNLOAD                                  *         
***********************************************************************         
                                                                                
FMTINO   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         USING PBILELEM,R2         R2=A(BILLING ELEMENT)                        
         IC    R0,0(R1)                                                         
         GOTOR VDATCON,DMCB,(3,PBLDATE),(0,DUB)                                 
         GOTOR VFMTINO,DMCB,DUB,(2,PBINVNO),((R0),CLTPRB1),CLTPRB1X             
         L     R1,0(R1)                                                         
         MVC   0(L'BUYBINVN,R3),0(R1)                                           
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* FORMAT PAYEE FOR PRINTING                                           *         
***********************************************************************         
                                                                                
FMTPYE   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         USING PPAYELEM,R2         R2=A(PAYMENT ELEMENT)                        
         MVC   WORK(L'PPREP),PPREP                                              
         NI    WORK,FF-X'C0'                                                    
         OC    WORK(L'PPREP),WORK                                               
         JNZ   *+14                                                             
         MVC   0(L'ACTVTYPE,R3),PP@DIRCK                                        
         J     EXIT                                                             
         SR    R0,R0                                                            
         ICM   R0,3,WORK                                                        
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   0(L'ACTVTYPE,R3),SPACES                                          
         UNPK  0(4,R3),DUB                                                      
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* INTERFACE TO TSAR (ONLINE) OR BUFFERIN (OFFLINE)                    *         
* R1 -> 1 = USING INVOICE BUFFER                                      *         
*       2 = USING CUSTOM COLUMN BUFFER                                *         
***********************************************************************         
                                                                                
BUFFER   NTR1  LABEL=NO                                                         
         OC    VMASTC,VMASTC       TEST ONLINE                                  
         JNZ   BUFFER02                                                         
         GOTOR VTSAR,TSARD         YES - USE TSAR                               
         J     EXIT                                                             
                                                                                
BUFFER02 LHI   R0,BUFFAINI         CONVERT TSAR ACTION CODE TO BUFFERIN         
         CLI   TSACTN,TSAINI                                                    
         JE    BUFFER04                                                         
         LHI   R0,BUFFAPUT                                                      
         CLI   TSACTN,TSAADD                                                    
         JE    BUFFER04                                                         
         CLI   TSACTN,TSAWRT                                                    
         JE    BUFFER04                                                         
         LHI   R0,BUFFASEQ                                                      
         CLI   TSACTN,TSANXT                                                    
         JE    BUFFER04                                                         
                                                                                
         MVC   TKEYSV_I,IBUKEY     BUY INVOICE BUFFER KEY                       
         LHI   R0,BUFFARDH                                                      
         CLI   TSACTN,TSARDH                                                    
         JE    BUFFER04                                                         
         DC    H'0'                                                             
                                                                                
BUFFER04 GOTOR VBUFFRIN,DMCB,((R0),INVBUFF),IBUD,ACOMFACS                       
         CLI   TSACTN,TSARDH                                                    
         JNE   BUFFER06                                                         
         CLC   TKEYSV_I,IBUKEY     EMULATE TSAR READ HIGH                       
         JE    BUFFER06                                                         
         OI    BUFFERRS-BUFFPARM(R1),BUFFERNF                                   
                                                                                
BUFFER06 MVC   TSERRS,BUFFERRS-BUFFPARM(R1)                                     
         CLI   TSERRS,0                                                         
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MINIO INTERFACE ROUTINE (INITIALIZATION)                            *         
***********************************************************************         
                                                                                
MININIT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,AINVVAL                                                       
         USING INVVALSD,R5                                                      
         LA    R0,MINBLOCK         CLEAR MINBLOCK AREA                          
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    ELEM2,ELEM2         CLEAR ELEMENT WORK AREA                      
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         L     RF,ACOMFACS                                                      
         L     RF,(CRECUP-COMFACSD)(RF)                                         
         STCM  RF,15,MINRECUP                                                   
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,PRTFIL       FILE NAME                                    
         MVC   MINDIR,PRTDIR       DIR NAME                                     
         LA    R1,L'PNVKEY                                                      
         STC   R1,MINFKLEN         KEY LENGTH                                   
         LHI   R1,75               SET SPLIT PERCENTAGE TO 75%                  
         STCM  R1,3,MINSPCT                                                     
         MVI   MINNCTL,L'PNVDCNTL  2 CONTROL BYTES                              
         LHI   R1,IODDWQ           L'IOS                                        
         STCM  R1,3,MINFRCLM       MAX RECORD LENGTH IS IOAREA LNGTH            
         MVI   MINEKLEN,L'PNVKELMK      ELEMENT KEY LENGTH                      
         MVI   MINEKDSP,PNVKELMK-PNVKEY ELEMENT KEY DISPLACEMENT                
         MVC   MINBUFF,AIO1        A(FIRST MINIO BUFFER)                        
         LA    R1,ELEM2            A(ELEMENT AREA)                              
         ST    R1,MINELEM                                                       
         LHI   R1,L'ELEM2          MAX ELEMENT/CLUSTER LENGTH                   
         STCM  R1,3,MINMAXEL                                                    
         J     EXITY                                                            
         DROP  RB,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AN ELEMENT IN A MINIO RECORD                         *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE FOUND)                         *         
*               IF LENGTH OF ELEMENT PROVIDED, KEY MATCHING IS DONE   *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
* EXIT - CC    NEQ - ERROR OCCURRED (NOT USED HERE)                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
***********************************************************************         
                                                                                
MGETEL   NTR1  BASE=*,LABEL=*                                                   
         L     R5,AINVVAL                                                       
         USING INVVALSD,R5                                                      
         LR    R3,R1               POINT TO ELEMENT TO BE FOUND                 
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         XC    0(L'ELEM2,RF),0(RF)                                              
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
         MVC   MINEKEY(1),0(R3)    BUILD ELEMENT KEY                            
         LHI   R0,MINRD            DEFAULT TO DIRECT READ                       
         CLI   1(R3),0             USE DEFAULT IF NO LENGTH GIVEN               
         BE    MGETEL02                                                         
         SR    RF,RF                                                            
         IC    RF,1(R3)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
         CLM   RF,1,MINEKLEN       USE READ IF GREATER THAN KEY LENGTH          
         BNL   MGETEL02                                                         
         LHI   R0,MINHI            SET FOR READ HI/EQUAL                        
         STC   RF,MINFILTL         SET FILTER LENGTH                            
                                                                                
MGETEL02 SR    RF,RF                                                            
         IC    RF,MINEKLEN         GET KEY LENGTH                               
         AHI   RF,-2               DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R3)  SETS REST OF ELEMENT KEY                     
         GOTOR VMINIO,MINPARMS,((R0),MINBLKD)                                   
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
         J     EXITY               SET CC                                       
         DROP  RB,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT ELEMENT IN A MINIO RECORD                       *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                    *         
*               IF LENGTH OF ELEMENT PROVIDED, KEY MATCHING IS DONE   *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
* EXIT - CC    NEQ - ERROR OCCURRED (NOT USED HERE)                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
***********************************************************************         
                                                                                
MNXTEL   NTR1  BASE=*,LABEL=*                                                   
         L     R5,AINVVAL                                                       
         USING INVVALSD,R5                                                      
         LR    R3,R1               POINT TO ELEMENT TO BE FOUND                 
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         XC    0(L'ELEM2,RF),0(RF)                                              
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
         MVC   MINEKEY(1),0(R3)    BUILD ELEMENT KEY                            
         CLI   1(R3),0             NO FILTERING IF NO LENGTH GIVEN              
         BE    MNXTEL02                                                         
         SR    RF,RF                                                            
         IC    RF,1(R3)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   MNXTEL02                                                         
         STC   RF,MINFILTL         SET FILTER LENGTH                            
                                                                                
MNXTEL02 SR    RF,RF                                                            
         IC    RF,MINEKLEN         GET KEY LENGTH                               
         AHI   RF,-2               DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R3)  SETS REST OF ELEMENT KEY                     
         GOTOR VMINIO,MINPARMS,('MINSEQ',MINBLKD)                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
         J     EXITY               SET CC                                       
         DROP  RB,R5                                                            
         EJECT                                                                  
***********************************************************************         
* TRANSLATE PID TO A NAME                                             *         
*                                                                     *         
*        P1       A(PID)                                              *         
*        P2+0(1)  L'RETURN AREA                                       *         
*        P2+1(3)  A(RETURN AREA)                                      *         
***********************************************************************         
                                                                                
GETWHO   NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         IC    R2,4(R1)            SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
         LA    R3,0(R3)                                                         
         L     R1,0(R1)            POINT TO PID                                 
         MVC   SAVEKEY,IOKEY       SAVE THE CURRENT RECORD KEY                  
         OC    0(L'CT0KNUM,R1),0(R1)                                            
         BZ    GETWHO08                                                         
                                                                                
         LA    RE,IOKEY                                                         
         USING CT0REC,RE           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KNUM,0(R1)       SET PID                                      
         L     R1,ALP                                                           
         L     R1,LP_ASECD                                                      
         MVC   CT0KAGY,SECAGY-SECD(R1)                                          
                                                                                
         LHI   R1,IORD+IOCTL+IO3                                                
         TM    DL_FLAG1,PIDIHSTQ   PID FOR INSERTION HISTORY DOWNLOAD?          
         BZ    *+8                                                              
         LHI   R1,IORD+IOCTL+IO1                                                
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BNE   GETWHO08                                                         
                                                                                
         L     RE,AIO3             POINT TO FOUND RECORD                        
         TM    DL_FLAG1,PIDIHSTQ   PID FOR INSERTION HISTORY DOWNLOAD?          
         BZ    *+8                                                              
         L     RE,AIO1             POINT TO FOUND RECORD                        
         LA    RF,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         USING SAPALEL,RF                                                       
GETWHO02 CLI   SAPALEL,0           CHECK FOR END OF RECORD                      
         BE    GETWHO08                                                         
         CLI   SAPALEL,SAPALELQ    MATCH ON ELEMENT CODE?                       
         BE    *+14                                                             
         IC    R0,SAPALLN          GET ELEMENT LENGTH                           
         AR    RF,R0               BUMP TO NEXT ELEMENT                         
         B     GETWHO02            GO FIND NEXT ELEMENT                         
                                                                                
         TM    DL_FLAG1,GETPIDOQ   GET PID ONLY?                                
         BZ    *+18                                                             
         MVC   WORK(L'SAPALPID),SAPALPID                                        
         LA    R1,8                TOTAL OUTPUT LENGTH                          
         B     GETWHO09                                                         
                                                                                
         LA    RE,IOKEY            READ PRESON RECORD                           
         USING SAPEREC,RE          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
         L     R1,ALP                                                           
         L     R1,LP_ASECD                                                      
         MVC   SAPEAGY,SECAGY-SECD(R1)                                          
         MVC   SAPEPID,SAPALPID    SET USERID FROM PREVIOUS RECORD              
                                                                                
         LHI   R1,IOHI+IOCTL+IO3                                                
         TM    DL_FLAG1,PIDIHSTQ   PID FOR INSERTION HISTORY DOWNLOAD?          
         BZ    *+8                                                              
         LHI   R1,IOHI+IOCTL+IO1                                                
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,AIO3             POINT TO FOUND RECORD                        
         TM    DL_FLAG1,PIDIHSTQ   PID FOR INSERTION HISTORY DOWNLOAD?          
         BZ    *+8                                                              
         L     RE,AIO1             POINT TO FOUND RECORD                        
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),IOKEYSAV                                
         BNE   GETWHO08                                                         
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
GETWHO04 CLI   SANAMEL,0           LOCATE NAME ELEMENT                          
         BE    GETWHO08                                                         
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    *+14                                                             
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     GETWHO04            GO PROCESS NEXT ELEMENT                      
                                                                                
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   GETWHO08            NO NAME IN ELEMENT                           
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         XC    WORK,WORK                                                        
         LA    R1,WORK             BUILD NAME IN WORK AREA                      
                                                                                
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
GETWHO06 IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   GETWHO10            END OF ELEMENT REACHED                       
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         MVI   0(R1),C' '                                                       
         AHI   R1,1                ADD IN A SPACING CHARACTER                   
         B     GETWHO06                                                         
                                                                                
GETWHO08 MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,7                TOTAL OUTPUT LENGTH                          
GETWHO09 SR    RF,RF                                                            
                                                                                
GETWHO10 BCTR  R1,0                                                             
         SR    R1,RF               R1=OUTPUT LENGTH-1                           
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      INIT OUTPUT AREA                             
         CR    R1,R2               NAME TOO LONG?                               
         BNH   *+6                                                              
         LR    R1,R2               USE MAX FOR RETURN AREA                      
         EX    R1,*+8                                                           
         B     GETWHOX                                                          
         MVC   0(0,R3),WORK        RETURN NAME IN OUTPUT AREA                   
                                                                                
GETWHOX  MVC   IOKEY,SAVEKEY       RESTORE SAVED KEY                            
         NI    DL_FLAG1,X'FF'-PIDIHSTQ                                          
         NI    DL_FLAG1,X'FF'-GETPIDOQ                                          
         J     EXIT                                                             
         DROP  RB,RE,RF                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF MEDIA FOR CFM CLIENT DOWNLOAD                         *         
***********************************************************************         
                                                                                
BLDMED   NTR1  ,                                                                
         L     RE,ALP                                                           
         L     R7,LP_AWMP-LP_D(RE)                                              
         USING LW_D,R7             R7=A(MEDIA ELEMENT IN POOL)                  
         STCM  R7,7,AMED                                                        
         XC    LW_D(LW_DATA2-LW_D),LW_D                                         
         MVI   LW_TYPE,LQ_TLSTQ                                                 
         LA    R3,LW_DATA2                                                      
         SR    R0,R0                                                            
         LHI   R4,C'A'                                                          
         LA    R2,IOKEY                                                         
         USING PAGYRECD,R2         R2=A(MEDIA RECORD KEY)                       
                                                                                
BLDMED02 XC    PAGYKEY,PAGYKEY                                                  
         MVC   PAGYKAGY,AGY                                                     
         STC   R4,PAGYKMED                                                      
         MVI   PAGYKRCD,PAGYKIDQ                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   PAGYKAGY,AGY                                                     
         JNE   BLDMED08                                                         
         CLI   PAGYKRCD,PAGYKIDQ                                                
         JE    BLDMED04                                                         
         IC    R4,PAGYKMED                                                      
         JL    BLDMED02                                                         
         J     BLDMED06                                                         
                                                                                
BLDMED04 MVC   0(L'PAGYKMED,R3),PAGYKMED                                        
         AHI   R3,L'PAGYKMED                                                    
         AHI   R0,1                                                             
         IC    R4,PAGYKMED                                                      
                                                                                
BLDMED06 AHI   R4,1                                                             
         J     BLDMED02                                                         
                                                                                
BLDMED08 STCM  R0,3,LW_NUMN                                                     
         LA    R0,LW_D                                                          
         SR    R3,R0                                                            
         STCM  R3,3,LW_LN                                                       
         AR    R7,R3                                                            
         L     RE,ALP                                                           
         ST    R7,LP_AWMP-LP_D(RE)                                              
         J     EXITY                                                            
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD MEDIA/RECORD/CLIENT TABLE FOR CFM PRODUCT DOWNLOAD            *         
***********************************************************************         
                                                                                
BLDMRC   NTR1  ,                                                                
         SR    R7,R7                                                            
         ICM   R7,7,ASMC                                                        
         JZ    EXITN                                                            
         USING LW_D,R7                                                          
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN                                                     
         LA    R2,LW_DATA2                                                      
         LA    R3,LW_DATA2                                                      
         SR    RF,RF                                                            
                                                                                
BLDMRC02 CLI   0(R2),PRTLETQ       TEST ENTRY FOR ME                            
         JNE   BLDMRC04                                                         
         MVC   0(1,R3),1(R2)       SET MEDIA CODE                               
         MVI   1(R3),X'06'         SET PRODUCT RECORD TYPE                      
         CR    R2,R3                                                            
         JE    *+10                                                             
         MVC   1+L'PPRDKMED(L'PPRDKCLT,R3),1+L'PPRDKMED(R2)                     
         AHI   R3,1+L'PPRDKMED+L'PPRDKCLT                                       
         AHI   RF,1                BUMP N'ENTRIES IN TABLE                      
                                                                                
BLDMRC04 AHI   R2,1+L'PPRDKMED+L'PPRDKCLT                                       
         BRCT  R0,BLDMRC02                                                      
         STCM  RF,3,LW_NUMN                                                     
         J     EXITY                                                            
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD MEDIA TABLE FOR CFM VENDOR DOWNLOAD                           *         
***********************************************************************         
                                                                                
BLDPUM   NTR1  ,                                                                
         SR    R2,R2                                                            
         ICM   R2,7,ASMC                                                        
         JZ    EXITN                                                            
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R2)                                            
         LA    R2,LW_DATA2-LW_D(R2)                                             
         USING SMDTABD,R2          R2=A(SYSTEM/MEDIA LIST)                      
         LA    R4,PUBMED           R4=A(MEDIA TABLE)                            
         SR    RE,RE                                                            
BLDPUM02 CLI   SMDTSYS,PRTLETQ     TEST ENTRY FOR ME                            
         JNE   BLDPUM04                                                         
         MVC   0(L'PUBMED,R4),SMDTMED                                           
         AHI   R4,L'PUBMED                                                      
         AHI   RE,1                                                             
BLDPUM04 AHI   R2,SMDTABL                                                       
         BRCT  R0,BLDPUM02                                                      
         LTR   RE,RE               TEST ANY PRINT MEDIA FOUND                   
         JZ    EXITN                                                            
         STCM  RE,3,PUBMED#        SET NUMBER OF MEDIA IN LIST                  
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
SMDTABD  DSECT                     ** SYSTEM/MEDIA TABLE **                     
SMDTSYS  DS    C                   SYSTEM CODE                                  
SMDTMED  DS    C                   MEDIA CODE                                   
SMDTABL  EQU   *-SMDTABD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR CFMIO READING (STATS DOWNLOAD)                       *         
***********************************************************************         
                                                                                
INICFM   NTR1  ,                                                                
         LA    R0,CFMIOD                                                        
         LHI   R1,CFMIOL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   CFMSYS,PRTLETQ                                                   
         MVC   CFMABFIN,VBUFFRIN                                                
         MVC   CFMATSAR,VTSAR                                                   
         MVC   CFMAGRAT,VGETINS                                                 
         MVC   CFMACOM,ACOMFACS                                                 
         MVC   CFMAIO,AIO1                                                      
         BASR  RE,0                                                             
         AHI   RE,CNVMED-*                                                      
         STCM  RE,15,CFMACNVM                                                   
         BASR  RE,0                                                             
         AHI   RE,VALCFC-*                                                      
         STCM  RE,15,CFMAVALC                                                   
         BASR  RE,0                                                             
         AHI   RE,VALCFV-*                                                      
         STCM  RE,15,CFMAVALV                                                   
         BASR  RE,0                                                             
         AHI   RE,GETSPC-*                                                      
         STCM  RE,15,CFMAGSPC                                                   
         MVC   CFMALP,ALP                                                       
         MVC   CFMAMED,MEDIND                                                   
         MVC   CFMADVN,ADVIND                                                   
         MVC   CFMVENN,SUPNODE                                                  
         MVC   CFMSUMOP,SUMOPTN                                                 
         MVC   CFMCALOP,CALOPTN                                                 
         MVC   CFMSPLOP,SPLOPTN                                                 
         MVC   CFMPNTOP,PNTOPTN                                                 
         MVC   CFMESTOP,ESTOPTN                                                 
         MVC   CFMEDTOP,ESNOPTN                                                 
         MVC   CFMACBA,CLTIND                                                   
                                                                                
         OC    STRDATEE,STRDATEE                                                
         JZ    INICFM02                                                         
         GOTOR VDATCON,DMCB,STRDATEE,(3,CFMSTDTB)                               
         GOTOR (RF),(R1),,(2,CFMSTDTC)                                          
                                                                                
INICFM02 OC    ENDDATEE,ENDDATEE                                                
         JZ    INICFM04                                                         
         GOTOR VDATCON,DMCB,ENDDATEE,(3,CFMENDTB)                               
         GOTOR (RF),(R1),,(2,CFMENDTC)                                          
                                                                                
INICFM04 MVI   CFMACTN,CFMAINI                                                  
         GOTOR VCFMIO,CFMIOD                                                    
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD PUBLICATION TABLE FOR INSERTION DOWNLOAD                      *         
***********************************************************************         
                                                                                
BLDPUB   NTR1  BASE=*,LABEL=*                                                   
         L     R7,ALP              POINT TO END OF WORK POOL                    
         L     R7,LP_AWMP-LP_D(R7)                                              
         USING LW_D,R7             R7=A(PUBLICATION ELEMENT) IN POOL            
         XC    LW_D(LW_DATA2-LW_D),LW_D                                         
                                                                                
         OC    PUBCODE,PUBCODE     TEST PUBLICATION CODE GIVEN                  
         BZ    BLDPUB10                                                         
                                                                                
***********************************************************************         
* HANDLE PUBLICATION CODE WITH OPTIONAL ZONE AND EDITION INPUT        *         
***********************************************************************         
                                                                                
         MVC   WORK,SPACES         CLEAR PUBCODE BUILD AREA                     
         LA    RE,PUBCODE+L'PUBCODE-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    R0,PUBCODE                                                       
         SR    RE,R0                                                            
         LA    R1,WORK                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),PUBCODE                                                  
         LA    RF,1(RE)            RF=LENGTH OF INPUT SO FAR                    
         LA    R1,1(RE,R1)         BUMP OUTPUT POINTER                          
                                                                                
         OC    PUBZONE,PUBZONE     TEST ZONE ENTERED                            
         BZ    BLDPUB02                                                         
         LA    RE,PUBZONE+L'PUBZONE-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    R0,PUBZONE                                                       
         SR    RE,R0                                                            
         MVI   0(R1),COMMA         YES - TACK ON ',ZONE'                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),PUBZONE                                                  
         LA    RF,2(RE,RF)         BUMP INPUT LENGTH                            
         LA    R1,2(RE,R1)         BUMP OUTPUT POINTER                          
                                                                                
BLDPUB02 OC    PUBEDTN,PUBEDTN     TEST EDITION ENTERED                         
         BZ    BLDPUB04                                                         
         LA    RE,PUBEDTN+L'PUBEDTN-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    R0,PUBEDTN                                                       
         SR    RE,R0                                                            
         MVI   0(R1),COMMA         YES - TACK ON ',EDITION'                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),PUBEDTN                                                  
         LA    RF,2(RE,RF)         BUMP INPUT LENGTH                            
                                                                                
BLDPUB04 GOTOR VPUBVAL,DMCB,((RF),WORK),DUB                                     
         CLI   0(R1),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                DIE IF INPUT IS INVALID                      
                                                                                
         OC    PUBZONE,PUBZONE     TEST ZONE GIVEN                              
         BNZ   BLDPUB06                                                         
         MVI   LW_TYPE,LQ_TRNGQ    BUILD RANGE - ALL ZONES/EDITIONS             
         MVC   LW_DATA1(L'PUBKPUB),DUB                                          
         XC    LW_DATA1+L'PUBKPUB(LPUBKZE),LW_DATA1+L'PUBKPUB                   
         MVC   LW_DATA1+LPUBKPZE(L'PUBKPUB),DUB                                 
         MVC   LW_DATA1+LPUBKPZE+L'PUBKPUB(LPUBKZE),EFFS                        
         LHI   R0,LW_LN1Q+(LPUBKPZE*2)                                          
         B     BLDPUB54                                                         
                                                                                
BLDPUB06 OC    PUBEDTN,PUBEDTN     TEST EDITION GIVEN                           
         BNZ   BLDPUB08                                                         
         MVI   LW_TYPE,LQ_TRNGQ    BUILD RANGE - ONE ZONE/ALL EDITIONS          
         MVC   LW_DATA1(LPUBKPZ),DUB                                            
         MVI   LW_DATA1+LPUBKPZ,0                                               
         MVC   LW_DATA1+LPUBKPZE(LPUBKPZ),DUB                                   
         MVI   LW_DATA1+LPUBKPZE+LPUBKPZ,FF                                     
         LHI   R0,LW_LN1Q+(LPUBKPZE*2)                                          
         B     BLDPUB54                                                         
                                                                                
BLDPUB08 MVI   LW_TYPE,LQ_TSINQ    BUILD SINGLE ENTRY                           
         MVC   LW_DATA1(LPUBKPZE),DUB                                           
         LHI   R0,LW_LN1Q+LPUBKPZE                                              
         B     BLDPUB54                                                         
                                                                                
BLDPUB10 OC    PUBPGRP,PUBPGRP     TEST PUBLICATION GROUP GIVEN                 
         BZ    BLDPUB40                                                         
                                                                                
***********************************************************************         
* HANDLE PUBLICATION GROUP INPUT                                      *         
***********************************************************************         
                                                                                
         MVI   LW_TYPE,LQ_TLSTQ    SET LIST OF VALUES                           
         LA    R3,LW_DATA2         R3=A(LIST OF PUBLICATION CODES)              
         LA    R2,IOKEY                                                         
         USING GRPRECD,R2                                                       
         XC    GRPGKEY,GRPGKEY                                                  
         MVC   GRPGAGY,AGY                                                      
         ICM   R1,7,AMED                                                        
         MVC   GRPGMED,LW_DATA1-LW_D(R1)                                        
         MVI   GRPGTYP,GRPGBGQ                                                  
         MVC   GRPGID,PUBPGRP                                                   
                                                                                
         SR    RE,RE                                                            
         LA    R0,L'PUBPGRP-1                                                   
         LA    R1,PUBPGRP+1                                                     
         MVI   BYTE1,0                                                          
BLDPUB12 CLI   0(R1),C'*'          TEST 'WILDCARD'                              
         BNE   BLDPUB14                                                         
         OI    BYTE1,1             SET 'WILDCARD' FOUND                         
         MVI   0(R1),FF                                                         
         SR    R4,R4                                                            
         B     BLDPUB16                                                         
                                                                                
BLDPUB14 NI    0(R1),X'0F'         CONVERT TO PWOS                              
         SR    R4,R4                                                            
         IC    R4,0(R1)                                                         
                                                                                
BLDPUB16 SLL   RE,4                                                             
         OR    RE,R4                                                            
         AHI   R1,1                                                             
         BCT   R0,BLDPUB12                                                      
                                                                                
         STCM  RE,3,DUB            SET GROUP CODE                               
         OC    DUB(2),DUB          TEST FOR ZERO                                
         BNZ   BLDPUB18                                                         
         LHI   R0,1                YES - SET TO LOW VALUE                       
         STCM  R0,3,DUB                                                         
                                                                                
BLDPUB18 MVC   GRPGCODE,DUB                                                     
         LHI   R1,IOHI+IODIR+IO1                                                
                                                                                
BLDPUB20 GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GRPGKEY(GRPGCODE-GRPGKEY),IOKEYSAV                               
         BNE   BLDPUB30                                                         
         TM    BYTE1,1             TEST ANY 'WILDCARDS'                         
         BNZ   BLDPUB22                                                         
         CLC   GRPGCODE,IOKEYSAV+(GRPGCODE-GRPGKEY)                             
         BE    BLDPUB26                                                         
         B     BLDPUB30                                                         
                                                                                
BLDPUB22 ICM   RF,B'1100',GRPGCODE RF=KEY VALUE                                 
         LA    R1,PUBPGRP+1        R1=A(TEST MASK)                              
         LA    R0,4                R0=DIGIT COUNT                               
                                                                                
BLDPUB24 SR    RE,RE                                                            
         SLDL  RE,4                SHIFT NEXT TEST DIGIT INTO RE                
         CLI   0(R1),FF            TEST 'WILCARD'                               
         BE    *+12                                                             
         CLM   RE,1,0(R1)          NO - MATCH DIGIT TO KEY                      
         BNE   BLDPUB28                                                         
         AHI   R1,1                BUMP TO NEXT KEY DIGIT                       
         BCT   R0,BLDPUB24         DO FOR NUMBER OF KEY DIGITS                  
                                                                                
BLDPUB26 MVC   0(LPUBKPZE,R3),GRPGVAL                                           
         AHI   R3,LPUBKPZE                                                      
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        BUMP N'ENTRIES IN LIST                       
         AHI   R0,1                                                             
         STCM  R0,3,LW_NUMN                                                     
                                                                                
BLDPUB28 LHI   R1,IOSQ+IODIR+IO1                                                
         B     BLDPUB20            GO GET NEXT DIRECTORY RECORD                 
                                                                                
BLDPUB30 SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        EXIT IF NO PUBLICATIONS FOUND                
         JZ    EXITN                                                            
         SHI   R0,1                R0=N'ENTRIES-1                               
         BZ    BLDPUB38            (NO SORT IF SINGLE PUBLICATION)              
         LA    R1,LW_DATA2                                                      
                                                                                
BLDPUB32 LA    RF,LPUBKPZE(R1)                                                  
         LR    RE,R0                                                            
                                                                                
BLDPUB34 CLC   0(LPUBKPZE,R1),0(RF) TEST IN SEQUENCE                            
         BNH   BLDPUB36                                                         
         XC    0(LPUBKPZE,R1),0(RF) SWAP ENTRIES IF OUT                         
         XC    0(LPUBKPZE,RF),0(R1)                                             
         XC    0(LPUBKPZE,R1),0(RF)                                             
                                                                                
BLDPUB36 AHI   RF,LPUBKPZE                                                      
         BCT   RE,BLDPUB34                                                      
         AHI   R1,LPUBKPZE                                                      
         BCT   R0,BLDPUB32                                                      
                                                                                
BLDPUB38 ICM   R0,3,LW_NUMN                                                     
         MHI   R0,LPUBKPZE                                                      
         AHI   R0,LW_DATA2-LW_D                                                 
         B     BLDPUB54                                                         
                                                                                
BLDPUB40 OC    PUBPLST,PUBPLST     TEST PUBLICATION LIST GIVEN                  
         BZ    BLDPUB52                                                         
                                                                                
***********************************************************************         
* HANDLE PUBLICATION LIST INPUT                                       *         
***********************************************************************         
                                                                                
         MVI   LW_TYPE,LQ_TLSTQ    BUILD SINGLE ENTRY                           
         LA    R3,LW_DATA2         R3=A(LIST OF PUBLICATION CODES)              
         LA    R2,IOKEY                                                         
         USING PLISRECD,R2                                                      
         XC    PLISKEY,PLISKEY                                                  
         MVC   PLISKAGY,AGY                                                     
         ICM   R1,7,AMED                                                        
         MVC   PLISKMED,LW_DATA1-LW_D(R1)                                       
         MVI   PLISKRCD,PLISKRCQ                                                
         CLI   CLTIND,LQ_TSINQ                                                  
         BE    *+14                                                             
         MVC   PLISKCLT,ZZZ                                                     
         B     *+14                                                             
         ICM   R1,7,ACLT                                                        
         MVC   PLISKCLT,LW_DATA1-LW_D(R1)                                       
         MVC   PLISKCOD,PUBPLST                                                 
                                                                                
BLDPUB42 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   PLISKEY(PLISKLIN-PLISKEY),IOKEYSAV                               
         BE    BLDPUB44                                                         
         CLC   IOKEYSAV+(PLISKCLT-PLISKEY)(L'PLISKCLT),ZZZ                      
         JE    EXITN                                                            
         MVC   PLISKEY,IOKEYSAV                                                 
         MVC   PLISKCLT,ZZZ                                                     
         B     BLDPUB42                                                         
                                                                                
BLDPUB44 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         LA    R4,PLISFRST                                                      
         USING PLISPBD,R4                                                       
         SR    R0,R0                                                            
BLDPUB46 CLI   PLISPBEL,0          TEST END OF RECORD                           
         BE    BLDPUB50                                                         
         CLI   PLISPBEL,PLISBELQ   TEST PUBLICATION LIST ELEMENT                
         BNE   BLDPUB48                                                         
         MVC   0(LPUBKPZE,R3),PLISPUB                                           
         AHI   R3,LPUBKPZE                                                      
         SR    RE,RE                                                            
         ICM   RE,3,LW_NUMN        BUMP N'ENTRIES IN LIST                       
         AHI   RE,1                                                             
         STCM  RE,3,LW_NUMN                                                     
                                                                                
BLDPUB48 IC    R0,PLISPBEL+1       BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     BLDPUB46                                                         
         DROP  R4                                                               
                                                                                
BLDPUB50 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         CLC   PLISKEY(PLISKLIN-PLISKEY),IOKEYSAV                               
         BE    BLDPUB44                                                         
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        EXIT IF NO PUBLICATIONS FOUND                
         JZ    EXITN                                                            
         MHI   R0,LPUBKPZE                                                      
         AHI   R0,LW_DATA2-LW_D                                                 
         B     BLDPUB54                                                         
                                                                                
***********************************************************************         
* HANDLE NO PUBLICATION INPUT AT ALL                                  *         
***********************************************************************         
                                                                                
BLDPUB52 MVI   LW_TYPE,LQ_TRNGQ    NO PUBS SPECIFIED - SET TO ALL               
         XC    LW_DATA1(LPUBKPZE),LW_DATA1                                      
         MVC   LW_DATA1+LPUBKPZE(LPUBKPZE),EFFS                                 
         LHI   R0,LW_LN1Q+(LPUBKPZE*2)                                          
                                                                                
***********************************************************************         
* FINALIZE PUBLICATION REQUEST ELEMENT                                *         
***********************************************************************         
                                                                                
BLDPUB54 STCM  R7,7,APUB                                                        
         MVC   PUBIND,LW_TYPE      SET PUBLICATION TYPE                         
         STCM  R0,3,LW_LN          SET ELEMENT LENGTH                           
         LHI   R1,D#PUBCOD                                                      
         STCM  R1,3,LW_CODE                                                     
         AR    R7,R0                                                            
         L     R1,ALP                                                           
         ST    R7,LP_AWMP          SET A(END OF WORK POOL)                      
                                                                                
BLDPUBX  J     EXIT                                                             
         DROP  R7,RB                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF ADDITIONAL CHARGE CODES                              *         
***********************************************************************         
                                                                                
BLDACC   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO6                                                          
         USING ACHTABD,R3          R3=A(ADDITIONAL CHARGES TABLE)               
         LA    R2,IOKEY                                                         
         USING PSPLRECD,R2                                                      
         LA    RF,X'41'            (TO POSITION TO FIRST MEDIA)                 
         SR    R4,R4               R4=ZERO IF NO MEDIA SPECIFIED                
         CLI   MEDIND,LQ_TALLQ     TEST 'ALL' MEDIA REQUEST                     
         BE    BLDACC02                                                         
         CLI   MEDIND,0            TEST NO MEDIA REQUEST                        
         BE    BLDACC02                                                         
         ICM   R4,7,AMED                                                        
         LHI   RE,LW_DATA1-LW_D                                                 
         LHI   R0,1                                                             
         CLI   MEDIND,LQ_TSINQ                                                  
         BE    *+12                                                             
         LHI   RE,LW_DATA2-LW_D                                                 
         ICM   R0,3,LW_NUMN-LW_D(R4)                                            
         AR    R4,RE               R4=A(LIST OF MEDIAS)                         
                                                                                
BLDACC02 XC    PSPLKEY,PSPLKEY     BUILD KEY OF RECORD                          
         MVC   PSPLKAGY,AGY                                                     
         STC   RF,PSPLKMED                                                      
         LTR   R4,R4               TEST ANY REQUESTED MEDIAS                    
         BZ    *+10                                                             
         MVC   PSPLKMED,0(R4)      YES - SET NEXT MEDIA TO READ FOR             
         MVI   PSPLKRCD,PSPLKIDQ                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BNE   BLDACCX                                                          
         CLC   PSPLKAGY,AGY                                                     
         BNE   BLDACCX                                                          
         CLI   PSPLKRCD,PSPLKIDQ  Additional charge record?                     
         JNE   BLDACC08                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO1                                                          
         LA    R1,PSPLELEM-PSPLRECD(R1)                                         
         USING PSPLELEM,R1                                                      
         SR    RE,RE                                                            
BLDACC04 CLI   PSPLELEM,0          TEST END OF RECORD                           
         BE    BLDACC08                                                         
         CLI   PSPLELEM,PSPELQ     TEST ADDITIONAL CHARGE ELEMENT               
         BNE   BLDACC06                                                         
                                                                                
         MVC   ACHMED,PSPLKMED     BUILD TABLE ENTRY                            
         MVC   ACHCODE,PSPLCODE                                                 
         MVC   ACHTYPE,PSPLTYPE                                                 
         MVC   ACHDESC,PSPLDESC                                                 
         AHI   R3,ACHTABL          BUMP TO NEXT TABLE ENTRY                     
                                                                                
BLDACC06 IC    RE,PSPLELEM+1       BUMP TO NEXT ELEMENT                         
         AR    R1,RE                                                            
         B     BLDACC04                                                         
         DROP  R1                                                               
                                                                                
BLDACC08 LTR   R4,R4               TEST ANY REQUESTED MEDIAS                    
         BZ    *+16                                                             
         AHI   R4,L'PSPLKMED       YES - BUMP TO NEXT MEDIA                     
         BCT   R0,BLDACC02                                                      
         B     BLDACCX                                                          
                                                                                
         LLC   RF,PSPLKMED         BUMP TO NEXT MEDIA CODE                      
         AHI   RF,1                FOR ALL MEDIA REQUEST                        
         B     BLDACC02                                                         
         DROP  R2                                                               
                                                                                
BLDACCX  XC    ACHTABD(ACHTABL),ACHTABD                                         
         J     EXIT                                                             
         DROP  R3,RB                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* INSERTION HISTORY DOWNLOAD                                          *         
***********************************************************************         
                                                                                
BLDHST   NTR1  BASE=*,LABEL=*                                                   
         XC    I#(I#L),I#          CLEAR TABLE COUNTERS                         
         MVI   BUYFOUND,NOQ        SET BUY NOT FOUND                            
         GOTOR GETBUY,BUYSER       READ BUY RECORD USING SERIAL NUMBER          
         BE    *+12                                                             
         MVI   BUYBSTAT,BUYBSNFD   SET STATUS TO 'NOT FOUND'                    
         B     BLDHSTX                                                          
                                                                                
         MVI   BUYFOUND,YESQ       SET BUY FOUND                                
         L     R2,IOADDR                                                        
         USING PBUYRECD,R2         R2=A(BUY RECORD)                             
                                                                                
         GOTOR (#GETCLT,AGETCLT),DMCB,PBUYKAGY,PBUYKMED,PBUYKCLT                
         BNE   BLDHSTX                                                          
         GOTOR GETCPR              GET CLIENT PROFILES                          
*                                                                               
         LA    R3,PBUYFRST         R3=A(FIRST BUY RECORD ELEMENT)               
BLDHST1B CLI   0(R3),0             TEST END OF RECORD                           
         JE    BLDHST01                                                         
         CLI   0(R3),PBILELQ       TEST BILLING ELEMENT                         
         JNE   BLDHST1X                                                         
         USING PBILELEM,R3         BILLING ELEMENTS                             
         OC    PBLDATE,PBLDATE                                                  
         JZ    BLDHST1X                                                         
         DROP  R3                                                               
BLDHST1X LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         J     BLDHST1B                                                         
*                                                                               
BLDHST01 LA    R3,PBUYFRST         R3=A(FIRST BUY RECORD ELEMENT)               
*                                                                               
BLDHST02 CLI   0(R3),0             TEST END OF RECORD                           
         BE    BLDHSTX                                                          
         CLI   0(R3),PIOELQ        TEST INSERTION ORDER ELEMENT                 
         BE    BLDHST06                                                         
         CLI   0(R3),PWIOELCQ      TEST WEB INSERTION ORDER ELEMENT             
         BE    BLDHST07                                                         
         CLI   0(R3),PESRELCQ      TEST ENHANCED SPACE RESERVATION              
         BE    BLDHST08                                                         
         CLI   0(R3),PBILELQ       TEST BILLING ELEMENT                         
         BE    BLDHST09                                                         
*                                                                               
         CLI   0(R3),PORBELQ       Open rate bill elem?                         
         JE    BLDHST09                                                         
*                                                                               
         CLI   0(R3),PPAYELQ       TEST PAYING ELEMENT                          
         BE    BLDHST10                                                         
BLDHST04 SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BLDHST02                                                         
                                                                                
         USING PIOELEM,R3          INSERTION ORDER ELEMENTS                     
BLDHST06 OC    PIODATE,PIODATE                                                  
         BZ    BLDHST04                                                         
         SR    R4,R4                                                            
         ICM   R4,3,IOA#                                                        
         LA    RF,1(R4)                                                         
         STCM  RF,3,IOA#                                                        
         MHI   R4,IOAL                                                          
         A     R4,AIO6                                                          
         USING IOAD,R4                                                          
         MVC   IOADATE,PIODATE                                                  
         GOTOR FMTION,DMCB,(PBUYKMED,PIOELEM),IOANUM                            
         MVC   IOATYPE,PP@IONEW                                                 
         CLI   PIOTYP,C'N'                                                      
         BE    BLDHST04                                                         
         MVC   IOATYPE,PP@IOCHA                                                 
         CLI   PIOTYP,C'C'                                                      
         BE    BLDHST04                                                         
         MVC   IOATYPE,PP@IOCAN                                                 
         CLI   PIOTYP,C'D'                                                      
         BE    BLDHST04                                                         
         MVC   IOATYPE,SPACES                                                   
         B     BLDHST04                                                         
         DROP  R3,R4                                                            
                                                                                
         USING PWIOELEM,R3         WEB INSERTION ORDER ELEMENTS                 
BLDHST07 OC    PWIODATE,PWIODATE                                                
         BZ    BLDHST04                                                         
         SR    R4,R4                                                            
         ICM   R4,3,IOA#                                                        
         LA    RF,1(R4)                                                         
         STCM  RF,3,IOA#                                                        
         MHI   R4,IOAL                                                          
         A     R4,AIO6                                                          
         USING IOAD,R4                                                          
         MVC   IOADATE,PWIODATE                                                 
         GOTOR FMTWION,DMCB,PBUYKEY,PWIOELEM,IOANUM                             
         MVC   IOATYPE,PP@IONEW                                                 
         CLI   PWIOMODC,C'N'                                                    
         BE    BLDHST04                                                         
         MVC   IOATYPE,PP@IOCHA                                                 
         CLI   PWIOMODC,C'C'                                                    
         BE    BLDHST04                                                         
         MVC   IOATYPE,PP@IOCAN                                                 
         CLI   PWIOMODC,C'D'                                                    
         BE    BLDHST04                                                         
         MVC   IOATYPE,=C'NoChange'                                             
         CLI   PWIOMODC,C'U'                                                    
         BE    BLDHST04                                                         
         MVC   IOATYPE,SPACES                                                   
         B     BLDHST04                                                         
         DROP  R3,R4                                                            
                                                                                
                                                                                
         USING PESRELEM,R3         ENHANCED SPACE RESERVATION ELEM              
BLDHST08 OC    PESRDATE,PESRDATE                                                
         BZ    BLDHST04                                                         
         SR    R4,R4                                                            
         ICM   R4,3,IOA#                                                        
         LA    RF,1(R4)                                                         
         STCM  RF,3,IOA#                                                        
         MHI   R4,IOAL                                                          
         A     R4,AIO6                                                          
         USING IOAD,R4                                                          
         MVC   IOADATE,PESRDATE                                                 
         GOTOR FMTESRN,DMCB,PBUYKEY,PESRELEM,IOANUM                             
         MVC   IOATYPE,PP@IONEW                                                 
         CLI   PESRMODC,C'N'                                                    
         BE    BLDHST04                                                         
         MVC   IOATYPE,PP@IOCHA                                                 
         CLI   PESRMODC,C'C'                                                    
         BE    BLDHST04                                                         
         MVC   IOATYPE,PP@IOCAN                                                 
         CLI   PESRMODC,C'D'                                                    
         BE    BLDHST04                                                         
         MVC   IOATYPE,=C'NoChange'                                             
         CLI   PESRMODC,C'U'                                                    
         BE    BLDHST04                                                         
         MVC   IOATYPE,SPACES                                                   
         B     BLDHST04                                                         
         DROP  R3,R4                                                            
                                                                                
         USING PBILELEM,R3         BILLING ELEMENTS                             
BLDHST09 CLI   PBILELEM+1,X'19'    Additional charge billing elem?              
         JE    BLDHST04                                                         
         OC    PBLDATE,PBLDATE                                                  
         BZ    BLDHST04                                                         
         SR    R4,R4                                                            
         ICM   R4,3,IBA#                                                        
         LA    RF,1(R4)                                                         
         STCM  RF,3,IBA#                                                        
         MHI   R4,IBAL                                                          
         A     R4,AIO3                                                          
         USING IBAD,R4                                                          
         MVC   IBADATE,PBLDATE                                                  
         MVC   IBASTATS,PBBILST                                                 
         GOTOR FMTINO,DMCB,(PBUYKMED,PBILELEM),IBAINVNO                         
         USING PPBVALDD,WORKAREA                                                
         GOTOR VPPBVAL,DMCB,(C'E',PBILELEM),PPBVALDD                            
         MVC   IBAGROSS,PPBVEEG                                                 
         MVC   IBANET,PPBVEEN                                                   
         ICM   RF,15,IBANET                                                     
         ICM   R1,15,PPBVEEC                                                    
         SR    RF,R1                                                            
         STCM  RF,15,IBANLCDB                                                   
         CLI   PBILELEM,PORBELQ    Open rate element?                           
         JNE   *+10                                                             
         MVC   IBABTYPE(04),=C'COS2'                                            
         B     BLDHST04                                                         
         DROP  R3,R4                                                            
                                                                                
         USING PPAYELEM,R3         PAYMENT ELEMENTS                             
BLDHST10 L     RE,ALP                                                           
         USING LP_D,RE                                                          
         CLC   LP_VRSN1,=AL1(3,5,0,41)                                          
         BNL   BLDHST12                                                         
         DROP  RE                                                               
         CLI   PPAYELEM+1,PPACCODE-PPAYELEM                                     
         BH    BLDHST04                                                         
BLDHST12 OC    PPDDATE,PPDDATE                                                  
         BZ    BLDHST04                                                         
         SR    R4,R4                                                            
         ICM   R4,3,IPA#                                                        
         LA    RF,1(R4)                                                         
         STCM  RF,3,IPA#                                                        
         MHI   R4,IPAL                                                          
         A     R4,AIO4                                                          
         USING IPAD,R4                                                          
         MVC   IPADATE,PPDDATE                                                  
         CLI   PPAYELEM+1,PPACCODE-PPAYELEM                                     
         BL    *+10                                                             
         MVC   IPAACODE,PPACCODE                                                
         MVC   IPASEQ_#,PPDSEQNO                                                
         MVC   IPAGROSS,PPGROSS                                                 
         ICM   RF,15,PPGROSS                                                    
         ICM   R1,15,PPAGYCOM                                                   
         SR    RF,R1                                                            
         STCM  RF,15,IPANET                                                     
         ICM   R1,15,PPCSHDSC                                                   
         SR    RF,R1                                                            
         STCM  RF,15,IPANLCDP                                                   
         GOTOR FMTPYE,DMCB,PPAYELEM,IPAPAYEE                                    
         MVC   WORK(L'PPDDATE),PPDDATE                                          
         MVC   WORK+L'PPDDATE(L'PPDSEQNO),PPDSEQNO                              
         GOTOR GETCHK                                                           
         MVC   IPACHECK,WORK                                                    
         MVC   IPACKDAT,WORK+L'PPCLCHK                                          
                                                                                
         L     RE,ALP                                                           
         USING LP_D,RE                                                          
         CLC   LP_VRSN1,=AL1(3,6,0,0)                                           
         BNH   BLDHST14                                                         
         DROP  RE                                                               
         BRAS  RE,INVCLSTA         CK INVOICE IN CLEARANCE STATUS REC           
         J     BLDHST04            CR/CK already set, next history elem         
                                                                                
BLDHST14 TM    PPREP,X'80'         PAYMENT IS CK?                               
         BZ    *+10                                                             
         MVC   IPACPTYP,=C'CK'                                                  
         TM    PPREP,X'40'         PAYMENT IS CR?                               
         BZ    *+10                                                             
         MVC   IPACPTYP,=C'CR'                                                  
         B     BLDHST04                                                         
                                                                                
BLDHSTX  J     EXIT                                                             
         DROP  R2,R3,R4,RB                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET INSERTION HISTORY FROM INSERTION ACTIVITY ELEM                  *         
***********************************************************************         
                                                                                
GETINSHS NTR1  BASE=*,LABEL=*                                                   
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R4,LP_BLKS+((B#INSHIS-1)*L'LP_BLKS)                              
         STCM  R4,15,LP_ADATA                                                   
         USING ICAD,R4                                                          
         LA    R0,ICATYPE                                                       
         LHI   R1,ICAL                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR REPLY DATA                             
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GTINH20                                                          
                                                                                
         GOTOR GETBUY,BUYSER       READ BUY RECORD USING SERIAL NUMBER          
         BNE   GTINH_N                                                          
         L     R2,AIO2             R2=A(BUY RECORD)                             
         USING PBUYRECD,R2                                                      
         GOTOR (#GETCLT,AGETCLT),DMCB,PBUYKAGY,PBUYKMED,PBUYKCLT                
         BNE   GTINH_N                                                          
                                                                                
         XC    ICACHGB,ICACHGB     FOR PROCESSING CHANGE BYTES                  
         XC    ICAAELM,ICAAELM     FOR CHG ELEMS NAVIGATION                     
         MVI   ICAFLAG,0           INIT INSERTION HISTORY DL FLAG               
                                                                                
         LA    R3,PBUYFRST                                                      
         SR    R0,R0                                                            
         USING PPIDELD,R3                                                       
GTINH16  CLI   PPIDELM,0           END OF BUY RECORD?                           
         BE    GTINH18                                                          
         CLI   PPIDELM,PPIDELQ     PID ELEM?                                    
         BE    GTINH18                                                          
         IC    R0,PPIDELL                                                       
         AR    R3,R0                                                            
         B     GTINH16                                                          
                                                                                
GTINH18  GOTOR VDATCON,DMCB,(3,PBDBUYDT),(2,ICADATE)                            
         MVC   ICATYPE,PP@ADD                                                   
         CLI   PPIDELM,0           PID ADD/DELETE ELEM FOUND?                   
         BNE   *+14                                                             
         MVC   ICAWHOM(L'PBDBUYER),PBDBUYER                                     
         B     GTINH19                                                          
         OI    DL_FLAG1,PIDIHSTQ                                                
         GOTOR GETWHO,DMCB,PPIDADD,(L'ICAWHOM,ICAWHOM)                          
GTINH19  LA    R3,PBUYFRST         PID ELEM NOT IN CHRONOLOGICAL ORDER          
         ST    R3,ICAAELM          FOR NEXT ROUND                               
         B     GTINH_Y                                                          
                                                                                
GTINH20  OC    ICACHGB,ICACHGB     ANY CHANGE BYTES TO PROCESS?                 
         BNZ   GTINH30                                                          
         OC    ICAAELM,ICAAELM     ANY CHG ELEM TO PROCESS?                     
         BZ    GTINH90                                                          
         L     R3,ICAAELM          CONTINUE FROM PREVIOUS CHG ELEM              
         SR    R0,R0                                                            
         USING PCHGELEM,R3         BUY CHANGE ACTIVITY ELEMENTS                 
GTINH24  CLI   PCHGELCD,0          END OF BUY RECORD?                           
         BE    GTINH90                                                          
         IC    R0,PCHGLEN                                                       
         AR    R3,R0                                                            
         CLI   PCHGELCD,PCHGELEQ   BUY ACTIVITY ELEM?                           
         BNE   GTINH24                                                          
         MVC   ICACDAT,PCHGDAT                                                  
         XC    ICACHGB,ICACHGB                                                  
         XC    ICABPID,ICABPID                                                  
         XC    ICATEMP,ICATEMP                                                  
         XC    ICAPVRT,ICAPVRT     Previous buy rate if changed                 
                                                                                
         TM    PCHGIND1,X'40'      Rate is changed?                             
         JZ    GTINH26                                                          
         CLI   PCHGLEN,PCHGOLDL    Old format and cost changed elem?            
         JE    *+12                                                             
         CLI   PCHGLEN,PCHGNEWL    New format and cost changed elem?            
         JNE   *+14                                                             
         OC    PCHGGRS,PCHGGRS     Free buy previously?                         
         JNZ   *+14                                                             
         MVC   ICAPVRT(04),=C'FREE'                                             
         J     GTINH26                                                          
         EDITR PCHGGRS,ICAPVRT,2,COMMAS=YES,ALIGN=LEFT,FLOAT=-                  
                                                                                
GTINH26  LA    RE,ICATEMP          IN CASE NO PID IN ACTIVITY ELEM              
         CLI   PCHGLEN,PCHGNEWS                                                 
         BNE   *+8                                                              
         LA    RE,PCHG_XSS                                                      
         CLI   PCHGLEN,PCHGNEWL                                                 
         BNE   *+8                                                              
         LA    RE,PCHG_XLS                                                      
         ST    R3,ICAAELM                                                       
         MVC   ICABPID,0(RE)                                                    
         MVC   ICACHGB+01(03),PCHGIND1                                          
         MVC   ICACHGB+04(01),PCHGIND4                                          
         MVC   ICACHGB+05(01),2(RE)                                             
                                                                                
GTINH30  CLI   ICACHGB+01,0        ANY CHANGES IN CHG BYTE 1?                   
         BE    GTINH34                                                          
         MVC   ICATYPE,PP@CHA                                                   
                                                                                
         TM    ICACHGB+01,X'80'    ALLOCATION CHANGE?                           
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@ALLO                                     
         NI    ICACHGB+01,X'FF'-X'80'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+01,X'40'    RATE CHANGE?                                 
         BZ    GTINH32                                                          
         MVC   ICAWHAT(L'ACTVWHAT),PP@@RATE                                     
         MVC   ICAWHAT+5(7),=C'(Gross)'                                         
         NI    ICACHGB+01,X'FF'-X'40'                                           
         B     GTINH80                                                          
                                                                                
GTINH32  TM    ICACHGB+01,X'20'    UNITS CHANGE?                                
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@UNIT                                     
         NI    ICACHGB+01,X'FF'-X'20'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+01,X'10'    DESCRIPTION CHANGE?                          
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@SPAC                                     
         NI    ICACHGB+01,X'FF'-X'10'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+01,X'08'    INSERTION DATE CHANGE?                       
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@IDAT                                     
         NI    ICACHGB+01,X'FF'-X'08'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+01,X'04'    PREMIUM CHANGE?                              
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@PREM                                     
         NI    ICACHGB+01,X'FF'-X'04'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+01,X'02'    REGULAR COMMENT CHANGE?                      
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@COMM                                     
         NI    ICACHGB+01,X'FF'-X'02'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+01,X'01'    INSERTION ORDER COMMENT CHANGE?              
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@IOCM                                     
         NI    ICACHGB+01,X'FF'-X'01'                                           
         B     GTINH80                                                          
                                                                                
         MVI   ICACHGB+01,0        CLEAR CHANGE BITS                            
         B     GTINH70                                                          
                                                                                
GTINH34  CLI   ICACHGB+02,0        ANY CHANGES IN CHG BYTE 2?                   
         BE    GTINH38                                                          
         MVC   ICATYPE,PP@CHA                                                   
                                                                                
         TM    ICACHGB+02,X'80'    CLOSE DATE?                                  
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@CLDT                                     
         NI    ICACHGB+02,X'FF'-X'80'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+02,X'40'    SALE DATE?                                   
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@SADT                                     
         NI    ICACHGB+02,X'FF'-X'40'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+02,X'20'    BILLABLE DATE?                               
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@BBDT                                     
         NI    ICACHGB+02,X'FF'-X'20'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+02,X'10'    PAYABLE DATE?                                
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@PBDT                                     
         NI    ICACHGB+02,X'FF'-X'10'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+02,X'08'    AD CODE?                                     
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@ACCH                                     
         NI    ICACHGB+02,X'FF'-X'08'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+02,X'04'    AGY COMM?                                    
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@ACOM                                     
         NI    ICACHGB+02,X'FF'-X'04'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+02,X'02'    CASH DISCOUNT?                               
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@CDSC                                     
         NI    ICACHGB+02,X'FF'-X'02'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+02,X'01'    IO DATE CHANGE?                              
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@IODT                                     
         NI    ICACHGB+02,X'FF'-X'01'                                           
         B     GTINH80                                                          
                                                                                
         MVI   ICACHGB+02,0        CLEAR CHANGE BITS                            
         B     GTINH70                                                          
                                                                                
GTINH38  CLI   ICACHGB+03,0        ANY CHANGES IN CHG BYTE 3?                   
         BE    GTINH42                                                          
         MVC   ICATYPE,PP@CHA                                                   
                                                                                
         TM    ICACHGB+03,X'80'    2ND INS DATE CHG?                            
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@SIDT                                     
         NI    ICACHGB+03,X'FF'-X'80'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+03,X'40'    AD CODE ADDED?                               
         BZ    *+24                                                             
         MVC   ICATYPE,PP@ADD                                                   
         MVC   ICAWHAT(L'ACTVWHAT),PP@@ACAD                                     
         NI    ICACHGB+03,X'FF'-X'40'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+03,X'20'    SPECIAL REP CHG?                             
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@SREP                                     
         NI    ICACHGB+03,X'FF'-X'20'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+03,X'10'    PLANNED COST CHG?                            
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@PCST                                     
         NI    ICACHGB+03,X'FF'-X'10'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+03,X'08'    TAX CHG?                                     
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@TAX                                      
         NI    ICACHGB+03,X'FF'-X'08'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+03,X'04'    MADE LIVE?                                   
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@LIVE                                     
         NI    ICACHGB+03,X'FF'-X'04'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+03,X'02'    MATERIALS CLOSING DATE?                      
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@MCDT                                     
         NI    ICACHGB+03,X'FF'-X'02'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+03,X'01'    POSITION INSTRUCTIONS CHG?                   
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@POSI                                     
         NI    ICACHGB+03,X'FF'-X'01'                                           
         B     GTINH80                                                          
                                                                                
         MVI   ICACHGB+03,0        CLEAR CHANGE BITS                            
         B     GTINH70                                                          
                                                                                
GTINH42  CLI   ICACHGB+04,0        ANY CHANGES IN CHG BYTE 4?                   
         BE    GTINH46                                                          
         MVC   ICATYPE,PP@CHA                                                   
                                                                                
         TM    ICACHGB+04,X'80'    SFH STATUS CHANGE?                           
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@SFHS                                     
         NI    ICACHGB+04,X'FF'-X'80'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+04,X'40'    COST2 FACTOR CHANGE?                         
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@GRP9                                      
         NI    ICACHGB+04,X'FF'-X'40'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+04,X'20'    CT, PV, IMPRESSION CHANGES?                  
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@IMPS                                     
         NI    ICACHGB+04,X'FF'-X'20'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+04,X'10'    ADDITIONAL CHARGES?                          
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@ACHG                                     
         NI    ICACHGB+04,X'FF'-X'10'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+04,X'08'    LEGAL WARNINGS?                              
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@@LWAR                                     
         NI    ICACHGB+04,X'FF'-X'08'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+04,X'04'    SRC COMMENTS?                                
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@SRCCM                                     
         NI    ICACHGB+04,X'FF'-X'04'                                           
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+04,X'02'    ENHANCED IO GENERATED?                       
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@EIOGN                                     
         NI    ICACHGB+04,X'FF'-X'02'                                           
         B     GTINH80                                                          
                                                                                
         MVI   ICACHGB+04,0        CLEAR CHANGE BITS                            
         B     GTINH70                                                          
                                                                                
GTINH46  CLI   ICACHGB+05,0        ANY CHANGES IN CHG BYTE 5?                   
         BE    GTINH50                                                          
         MVC   ICATYPE,PP@CHA                                                   
                                                                                
         TM    ICACHGB+05,PCHGESRG ESR GENERATED                                
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@ESRGN                                     
         NI    ICACHGB+05,X'FF'-PCHGESRG                                        
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+05,PCHGTSAQ TEARSHEET APPROVED                           
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@TSHAP                                     
         NI    ICACHGB+05,X'FF'-PCHGTSAQ                                        
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+05,PCHGPO#Q PURCHASE ORDER # CHANGE                      
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@PONUM                                     
         NI    ICACHGB+05,X'FF'-PCHGPO#Q                                        
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+05,PCHGQIVQ REQUEST INVOICE                              
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@QINVO                                     
         NI    ICACHGB+05,X'FF'-PCHGQIVQ                                        
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+05,PCHGRIVQ RECEIVE INVOICE                              
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@RINVO                                     
         NI    ICACHGB+05,X'FF'-PCHGRIVQ                                        
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+05,PCHFXRTQ FX RATE                                      
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@EXCRT                                     
         NI    ICACHGB+05,X'FF'-PCHFXRTQ                                        
         B     GTINH80                                                          
                                                                                
         TM    ICACHGB+05,PCHGTRKQ TRACKED STANDARD COLUMN CHANGED?             
         BZ    *+18                                                             
         MVC   ICAWHAT(L'ACTVWHAT),PP@CCFDA                                     
         NI    ICACHGB+05,X'FF'-PCHGTRKQ                                        
         B     GTINH80                                                          
                                                                                
         MVI   ICACHGB+05,0        CLEAR CHANGE BITS                            
         B     GTINH70                                                          
                                                                                
GTINH50  DC    H'0'                NEED TO DEFINE NEW CHG BYTE                  
                                                                                
GTINH70  MVC   ICAWHAT(7),=CL7'UNKNOWN'                                         
                                                                                
GTINH80  MVC   ICADATE,ICACDAT                                                  
         OC    ICABPID,ICABPID     HAVE PID TO LOOK UP NAME?                    
         JNZ   GTINH82                                                          
         MVC   ICAWHOM(05),=C'<P16>'                                            
         J     GTINH_Y                                                          
GTINH82  OI    DL_FLAG1,PIDIHSTQ                                                
         GOTOR GETWHO,DMCB,ICABPID,(L'ICAWHOM,ICAWHOM)                          
         B     GTINH_Y                                                          
                                                                                
GTINH90  TM    ICAFLAG,ICAFINQ     ALL DATA REPLIED?                            
         BNZ   GTINH_N                                                          
         L     R2,AIO2             R2=A(BUY RECORD)                             
         TM    PBUYCNTL,X'80'      BUY IS DELETED?                              
         BZ    GTINH_N                                                          
         MVC   ICAWHOM(3),PBDBUYER                                              
         LA    R3,PBUYFRST                                                      
         SR    R0,R0                                                            
         USING PPIDELD,R3                                                       
GTINH90C CLI   PPIDELM,0           END OF BUY RECORD?                           
         BE    GTINH90F                                                         
         CLI   PPIDELM,PPIDELQ     PID ELEM?                                    
         BE    GTINH90F                                                         
         IC    R0,PPIDELL                                                       
         AR    R3,R0                                                            
         B     GTINH90C                                                         
                                                                                
GTINH90F GOTOR VDATCON,DMCB,(3,PBDDATE),(2,ICADATE)                             
         MVC   ICATYPE,PP@DEL                                                   
         CLI   PPIDELM,PPIDELQ                                                  
         BE    *+14                                                             
         MVC   ICAWHOM(3),PBDBUYER                                              
         B     GTINH90X                                                         
         OI    DL_FLAG1,PIDIHSTQ                                                
         GOTOR GETWHO,DMCB,PPIDDEL,(L'ICAWHOM,ICAWHOM)                          
                                                                                
GTINH90X OI    ICAFLAG,ICAFINQ                                                  
         B     GTINH_Y                                                          
                                                                                
GTINH_N  J     NOMORE                                                           
                                                                                
GTINH_Y  J     EXITY                                                            
         DROP  R2,R3,R4,RB                                                      
         EJECT                                                                  
                                                                                
NOMORE   L     R1,ALP                                                           
         MVI   LP_RMODE-LP_D(R1),LP_RLAST                                       
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
                                                                                
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
ONLYQ    EQU   C'O'                                                             
COMMA    EQU   C','                                                             
                                                                                
NEWSMEDQ EQU   C'N'                NEWSPAPER MEDIA LETTER                       
ODORMEDQ EQU   C'O'                OUTDOOR MEDIA LETTER                         
                                                                                
LPUBKPZ  EQU   L'PUBKPUB+L'PUBKZON                                              
LPUBKPZE EQU   L'PUBKPUB+L'PUBKZON+L'PUBKED                                     
LPUBKZE  EQU   L'PUBKZON+L'PUBKED                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
LINETAB  DC    C'ABCDEFGHIJKLMNOP'                                              
BITLIST  DC    X'8040201008040201'                                              
EFFS     DC    X'FFFFFFFF'                                                      
PZERO    DC    P'0'                                                             
P100     DC    P'100'                                                           
                                                                                
FREE     DC    C'FREE'                                                          
ZZZ      DC    C'ZZZ'                                                           
ALL      DC    C'ALL'                                                           
                                                                                
M@DLHST  DC    AL2(M#DLHST)        HISTORY DOWNLOAD MAP CODE                    
M@DLREF  DC    AL2(M#DLREF)        REFRESH INSERTIONS                           
M@CFMCLT DC    AL2(M#CFMCLT)       CFM CLIENT DOWNLOAD                          
M@CFMPRD DC    AL2(M#CFMPRD)       CFM PRODUCT DOWNLOAD                         
M@CFMPUB DC    AL2(M#CFMPUB)       CFM VENDOR DOWNLOAD                          
M@STATSD DC    AL2(M#STATSD)       STATS DOWNLOAD                               
                                                                                
FILES    DS    0X                  ** SYSTEM/FILE LIST **                       
         DC    C'PRINT  '          SYSTEM NAME FOR OPEN                         
         DC    C'N'                                                             
PRTDIR   DC    C'PRTDIR '                                                       
         DC    C'N'                                                             
PRTFIL   DC    C'PRTFIL '                                                       
         DC    C'N'                                                             
PUBDIR   DC    C'PUBDIR '                                                       
         DC    C'N'                                                             
PUBFIL   DC    C'PUBFIL '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
                                                                                
ACTVDICT DS    0X                  ** DICTIONARY LOOKUP LIST **                 
         DCDDL PP#@ALLO,L'ACTVWHAT,L                                            
         DCDDL PP#@RATE,L'ACTVWHAT,L                                            
         DCDDL PP#@UNIT,L'ACTVWHAT,L                                            
         DCDDL PP#@SPAC,L'ACTVWHAT,L                                            
         DCDDL PP#@IDAT,L'ACTVWHAT,L                                            
         DCDDL PP#@PREM,L'ACTVWHAT,L                                            
         DCDDL PP#@COMM,L'ACTVWHAT,L                                            
         DCDDL PP#@IOCM,L'ACTVWHAT,L                                            
         DCDDL PP#@CLDT,L'ACTVWHAT,L                                            
         DCDDL PP#@SADT,L'ACTVWHAT,L                                            
         DCDDL PP#@BBDT,L'ACTVWHAT,L                                            
         DCDDL PP#@PBDT,L'ACTVWHAT,L                                            
         DCDDL PP#@ACCH,L'ACTVWHAT,L                                            
         DCDDL PP#@ACOM,L'ACTVWHAT,L                                            
         DCDDL PP#@CDSC,L'ACTVWHAT,L                                            
         DCDDL PP#@IODT,L'ACTVWHAT,L                                            
         DCDDL PP#@SIDT,L'ACTVWHAT,L                                            
         DCDDL PP#@ACAD,L'ACTVWHAT,L                                            
         DCDDL PP#@SREP,L'ACTVWHAT,L                                            
         DCDDL PP#@PCST,L'ACTVWHAT,L                                            
         DCDDL PP#@TAX,L'ACTVWHAT,L                                             
         DCDDL PP#@LIVE,L'ACTVWHAT,L                                            
         DCDDL PP#@MCDT,L'ACTVWHAT,L                                            
         DCDDL PP#@POSI,L'ACTVWHAT,L                                            
         DCDDL PP#@SFHS,L'ACTVWHAT,L                                            
         DCDDL PP#GRP9,L'ACTVWHAT,L                                             
         DCDDL PP#@IMPS,L'ACTVWHAT,L                                            
         DCDDL PP#@ACHG,L'ACTVWHAT,L                                            
         DCDDL PP#@LWAR,L'ACTVWHAT,L                                            
         DCDDL PP#SRCCM,L'ACTVWHAT,L                                            
         DCDDL PP#EIOGN,L'ACTVWHAT,L                                            
         DCDDL PP#ESRGN,L'ACTVWHAT,L                                            
         DCDDL PP#TSHAP,L'ACTVWHAT,L                                            
         DCDDL PP#PONUM,L'ACTVWHAT,L                                            
         DCDDL PP#QINVO,L'ACTVWHAT,L                                            
         DCDDL PP#RINVO,L'ACTVWHAT,L                                            
         DCDDL PP#ADD,L'ACTVTYPE,L                                              
         DCDDL PP#CHA,L'ACTVTYPE,L                                              
         DCDDL PP#DEL,L'ACTVTYPE,L                                              
         DCDDL PP#IONEW,L'ACTVTYPE,L                                            
         DCDDL PP#IOCHA,L'ACTVTYPE,L                                            
         DCDDL PP#IOCAN,L'ACTVTYPE,L                                            
         DCDDL PP#DIRCK,L'ACTVTYPE,L                                            
         DCDDL PP#EXCRT,L'ACTVWHAT,L                                            
         DCDDL PP#CCFDA,L'ACTVWHAT,L                                            
DDLISTX  DC    AL1(0)                                                           
                                                                                
         DS    0D                  PUBLICATION BUFFERIN FILE                    
         DC    C'*PUBBUFF*PUBBUFF*PUBBUFF*PUBBUFF'                              
PUBBUFF  BUFFD TYPE=D,KEYLEN=PBUKEYL,COMLEN=PBUDATAL,BUFFERS=10                 
                                                                                
         DS    0D                  PUBLISHER/REP BUFFERIN FILE                  
         DC    C'*REPBUFF*REPBUFF*REPBUFF*REPBUFF'                              
REPBUFF  BUFFD TYPE=D,KEYLEN=RBUKEYL,COMLEN=RBUDATAL,BUFFERS=10                 
                                                                                
         DS    0D                  INVOICE SERIAL NUMBER BUFFERIN FILE          
         DC    C'*INVBUFF*INVBUFF*INVBUFF*INVBUFF'                              
INVBUFF  BUFFD TYPE=D,KEYLEN=IBUKEYL,COMLEN=IBUDATAL,BUFFERS=10                 
         EJECT                                                                  
                                                                                
BUYPPTB  LKKEY H,PBYPKEY,SAVED                                                  
         LKKEY SIN,PBYPAGY,AGY                                                  
         LKKEY WMP,PBYPMED,AMED                                                 
         LKKEY LIT,PBYPRCD,PBYPRIDQ                                             
         LKKEY SIN,PBYPDTYP,FLTDLDTT                                            
         LKKEY WMP,PBYPCLT,ACLT                                                 
         LKKEY WMP,PBYPPRD,APRD                                                 
         LKKEY WMP,PBYPEST,AEST                                                 
         LKKEY RNG,PBYPDTE,FLTDLDTR                                             
         LKKEY ALL,PBYPDADR                                                     
         LKKEY E                                                                
                                                                                
BUYTAB   DS    0X                  ** BUY KEY DRIVER TABLE **                   
         DC    AL2(L'PBUYKEY)                                                   
                                                                                
         DC    AL1(PBUYKAGY-PBUYKEY,L'PBUYKAGY-1)                               
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LK_ISINQ)                                                    
                                                                                
         DC    AL1(PBUYKMED-PBUYKEY,L'PBUYKMED-1)                               
         DC    AL2(AMED-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PBUYKRCD-PBUYKEY,L'PBUYKRCD-1)                               
         DC    AL1(PBUYKRCQ),AL1(0)                                             
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PBUYKCLT-PBUYKEY,L'PBUYKCLT-1)                               
         DC    AL2(ACLT-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PBUYKPRD-PBUYKEY,L'PBUYKPRD-1)                               
         DC    AL2(APRD-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PBUYKPUB-PBUYKEY,LPUBKPZE-1)                                 
         DC    AL2(APUB-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PBUYKDAT-PBUYKEY,L'PBUYKDAT-1)                               
         DC    AL2(STRDATE-SAVED)                                               
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(PBUYKEST-PBUYKEY,L'PBUYKEST-1)                               
         DC    AL2(AEST-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PBUYKACT-PBUYKEY,L'PBUYKACT-1)                               
         DC    X'00',AL1(0)                                                     
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PBUYKLIN-PBUYKEY,L'PBUYKLIN-1)                               
         DC    AL2(LINRNG-SAVED)                                                
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
BUYTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
CLTTAB   DS    0X                  ** CLIENT KEY DRIVER TABLE **                
         DC    AL2(L'PCLTKEY)                                                   
                                                                                
         DC    AL1(PCLTKAGY-PCLTKEY,L'PCLTKAGY-1)                               
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LK_ISINQ)                                                    
                                                                                
         DC    AL1(PCLTKMED-PCLTKEY,L'PCLTKMED-1)                               
         DC    AL2(AMED-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PCLTKRCD-PCLTKEY,L'PCLTKRCD-1)                               
         DC    X'02',AL1(0)                                                     
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PCLTKCLT-PCLTKEY,L'PCLTKCLT-1)                               
         DC    AL2(CLTRNG-SAVED)                                                
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(PCLTKCLT+L'PCLTKCLT-PCLTKEY)                                 
         DC    AL1(L'PCLTKEY-(PCLTKCLT+L'PCLTKCLT-PCLTKEY)-1)                   
         DC    AL1(0,0)                                                         
         DC    AL1(LK_ILITQ)                                                    
                                                                                
CLTTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
PRDTAB   DS    0X                  ** PRODUCT KEY DRIVER TABLE **               
         DC    AL2(L'PPRDKEY)                                                   
                                                                                
         DC    AL1(PPRDKAGY-PPRDKEY,L'PPRDKAGY-1)                               
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LK_ISINQ)                                                    
                                                                                
         DC    AL1(PPRDKMED-PPRDKEY)                                            
         DC    AL1(L'PPRDKMED+L'PPRDKRCD+L'PPRDKCLT-1)                          
         DC    AL2(ASMC-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PPRDKPRD-PPRDKEY,L'PPRDKPRD-1)                               
         DC    AL2(PRDRNG-SAVED)                                                
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(PPRDKPRD+L'PPRDKPRD-PPRDKEY)                                 
         DC    AL1(L'PPRDKEY-(PPRDKPRD+L'PPRDKPRD-PPRDKEY)-1)                   
         DC    AL1(0,0)                                                         
         DC    AL1(LK_ILITQ)                                                    
                                                                                
PRDTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
PUBTAB   DS    0X                  ** PUBLICATION KEY DRIVER TABLE **           
         DC    AL2(L'PUBKEY)                                                    
                                                                                
         DC    AL1(PUBKMED-PUBKEY,L'PUBKMED-1)                                  
         DC    AL2(PUBMED#-SAVED)                                               
         DC    AL1(LK_ILSTQ)                                                    
                                                                                
         DC    AL1(PUBKPUB-PUBKEY,L'PUBKPUB+L'PUBKZON+L'PUBKED-1)               
         DC    AL2(PUBRNG-SAVED)                                                
         DC    AL1(LK_IRNGQ)                                                    
                                                                                
         DC    AL1(PUBKAGY-PUBKEY,L'PUBKAGY-1)                                  
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LK_ISINQ)                                                    
                                                                                
         DC    AL1(PUBKCOD-PUBKEY,L'PUBKCOD-1)                                  
         DC    AL1(PUBKCODQ,0)                                                  
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PUBKCOD+L'PUBKCOD-PUBKEY)                                    
         DC    AL1(L'PUBKEY-(PUBKCOD+L'PUBKCOD-PUBKEY))                         
         DC    AL1(0,0)                                                         
         DC    AL1(LK_ILITQ)                                                    
                                                                                
PUBTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
LVALUES  DS    0D                  ** LITERAL VALUES **                         
         DC    V(PPBYOUT)                                                       
         DC    V(PPFMTINO)                                                      
         DC    V(PPBVAL)                                                        
         DC    X'00',X'FF'                                                      
         DC    X'000001',X'FFFFFF'                                              
         DC    X'000000000001',X'FFFFFFFFFFFF'                                  
                                                                                
ABENDS   DC    C'KWAN,BOBY:'                                                    
SLOWS    DC    C'KWAN:'                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP FOR BUY DOWNLOAD                                        *         
***********************************************************************         
                                                                                
REQINS   LKREQ H,M#DLINS,OUTBUY                                                 
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,LIST=NOD,DEFAULT=Y,TEXT=PP#MEDCS,COL=*           
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,LIST=NOD,DEFAULT=Y,TEXT=PP#CLTCS,COL=*           
PrdCd    LKREQ F,D#PRDCOD,(I,B#SAVED,PRDIND),CHAR,OLEN=L'PPRDKPRD,     *        
               LIST=NOD,DEFAULT=Y,TEXT=PP#PRDCS,COL=*                           
EstNo    LKREQ F,D#ESTNUM,(I,B#SAVED,ESTIND),UBIN,OLEN=L'PESTKEST,     *        
               LIST=NOD,RANGE=Y,DEFAULT=Y,TEXT=PP#ESTNS,COL=*                   
StrDt    LKREQ F,D#STRDAT,(D,B#SAVED,STRDATE),BDAT,TEXT=PP#STDAT,COL=*          
EndDt    LKREQ F,D#ENDDAT,(D,B#SAVED,ENDDATE),BDAT,TEXT=PP#ENDAT,COL=*          
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,TEXT=PP#PUBCD,COL=*          
PZone    LKREQ F,D#PUBZON,(D,B#SAVED,PUBZONE),CHAR,TEXT=PP#PZONE,COL=*          
PEdtn    LKREQ F,D#PUBEDT,(D,B#SAVED,PUBEDTN),CHAR,TEXT=PP#PEDTN,COL=*          
PubGp    LKREQ F,D#PUBGRP,(D,B#SAVED,PUBPGRP),CHAR,TEXT=PP#PGRPC,COL=*          
PubLs    LKREQ F,D#PUBLST,(D,B#SAVED,PUBPLST),CHAR,TEXT=PP#PLSTC,COL=*          
SDFlt    LKREQ F,D#SPCDSC,(D,B#SAVED,FILTDESC),CHAR,TEXT=PP#SDESC,COL=*         
AdCod    LKREQ F,D#ADCODE,(D,B#SAVED,FILTADNO),CHAR,TEXT=PP#ADNO,COL=*          
Ad_ID    LKREQ F,D#AD_ID,(D,B#SAVED,FILTADID),CHAR,TEXT=PP#AD_ID,COL=*          
IStat    LKREQ F,D#INSSTA,(D,B#SAVED,FILTSTAT),LBIN,TEXT=PP#INSTA,COL=*         
ICNum    LKREQ F,D#ICNUM,(D,B#SAVED,FILTICNO),CHAR,TEXT=PP#ICCOD,COL=*          
DLInv    LKREQ F,D#DLIDAT,(D,B#SAVED,INVDLSW),CHAR,TEXT=PP#DLINV,COL=*          
ClrSt    LKREQ F,D#CLRSTA,(D,B#SAVED,FLTCLRST),LBIN,TEXT=PP#CSTAT,COL=*         
MatSt    LKREQ F,D#MATSTA,(D,B#SAVED,FLTMATST),LBIN,TEXT=PP#IMSTA,COL=*         
IvSrc    LKREQ F,D#IVBSRC,(D,B#SAVED,INVSURCE),CHAR,TEXT=PP#FROM,COL=*          
PbOpt    LKREQ F,D#DLPOPT,(D,B#SAVED,FLTPBOPT),CHAR,TEXT=PP#OPTNS,COL=*         
DLTyp    LKREQ F,D#DLTYPE,(D,B#SAVED,DL_TYPE),CHAR,TEXT=PP#DLTYP,COL=*          
SpRep    LKREQ F,D#SPREP,(D,B#SAVED,FILTSREP),CHAR,TEXT=PP#@SREP,COL=*          
CmIH#    LKREQ F,D#IVHCM#,(D,B#SAVED,FILT#IHC),UBIN,TEXT=PP##IHCM,COL=*         
CmID#    LKREQ F,D#IVDCM#,(D,B#SAVED,FILT#IDC),UBIN,TEXT=PP##IDCM,COL=*         
DLDat    LKREQ F,D#DLBDTT,(D,B#SAVED,FLTDLDTT),LBIN,TEXT=PP#TYPE,COL=*          
DLDtS    LKREQ F,D#DLBDTS,(D,B#SAVED,FLTDLDTS),BDAT,TEXT=PP#STDAT,COL=*         
DLDtE    LKREQ F,D#DLBDTE,(D,B#SAVED,FLTDLDTE),BDAT,TEXT=PP#ENDAT,COL=*         
InFlt    LKREQ F,D#INFLTS,(D,B#SAVED,FLTINFLT),LBIN,TEXT=PP#FLTRS,COL=*         
ByOrg    LKREQ F,D#BUYORG,(D,B#SAVED,FLTBYORG),LBIN,TEXT=PP#GRP6,COL=*          
DlVI#    LKREQ F,D#VINV#_,(D,B#SAVED,DL_VINV#),CHAR,TEXT=PP#VINVN,COL=*         
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR INSERTION HISTORY DOWNLOAD                          *         
***********************************************************************         
                                                                                
REQHST   LKREQ H,M#DLHST,OUTHST                                                 
InsKy    LKREQ F,D#INSKEY,(D,B#SAVED,BUYSER),CHAR,OLEN=BUYSERSL,       *        
               TEXT=PP#KEY,COL=*                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR REFRESH INSERTIONS                                  *         
***********************************************************************         
                                                                                
REQREF   LKREQ H,M#DLREF,OUTREF                                                 
InsKy    LKREQ F,D#INSKEY,(I,B#SAVED,SERIND),CHAR,OLEN=BUYSERSL,       *        
               LIST=F,SORT=N,TEXT=PP#KEY,COL=*                                  
DlVI#    LKREQ F,D#VINV#_,(D,B#SAVED,DL_VINV#),CHAR,TEXT=PP#VINVN,COL=*         
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR INVOICE DOWNLOAD (BY INVOICE NUMBER)                *         
***********************************************************************         
                                                                                
REQIVN   LKREQ H,M#DLINVN,OUTIV#                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,LIST=NOD,DEFAULT=Y,TEXT=PP#MEDCS,COL=*           
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),CHAR,                     *        
               OLEN=L'PCLTKCLT,LIST=NOD,DEFAULT=Y,TEXT=PP#CLTCS,COL=*           
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,BUYPUB),CHAR,TEXT=PP#PUBCD,COL=*           
InvNo    LKREQ F,D#VINVNO,(D,B#INVVAL,INVVNDR#),CHAR,TEXT=PP#VINVN,    *        
               COL=*                                                            
StrDt    LKREQ F,D#STRDAT,(D,B#SAVED,STRDATE),BDAT,TEXT=PP#STDAT,COL=*          
EndDt    LKREQ F,D#ENDDAT,(D,B#SAVED,ENDDATE),BDAT,TEXT=PP#ENDAT,COL=*          
CStDt    LKREQ F,D#DLBDTS,(D,B#SAVED,STRDAT2),BDAT,TEXT=PP#STDAT,COL=*          
CEnDt    LKREQ F,D#DLBDTE,(D,B#SAVED,ENDDAT2),BDAT,TEXT=PP#ENDAT,COL=*          
MatSt    LKREQ F,D#IMATST,(D,B#SAVED,INVMATST),CHAR,TEXT=PP#IMSTA,COL=*         
IvSrc    LKREQ F,D#INVSRC,(D,B#SAVED,INVSURCE),CHAR,TEXT=PP#FROM,COL=*          
DLTyp    LKREQ F,D#DLTYPE,(D,B#SAVED,DL_TYPE),CHAR,TEXT=PP#DLTYP,COL=*          
SpRep    LKREQ F,D#SPREP,(D,B#SAVED,FILTSREP),CHAR,TEXT=PP#@SREP,COL=*          
CmIH#    LKREQ F,D#IVHCM#,(D,B#SAVED,FILT#IHC),UBIN,TEXT=PP##IHCM,COL=*         
CmID#    LKREQ F,D#IVDCM#,(D,B#SAVED,FILT#IDC),UBIN,TEXT=PP##IDCM,COL=*         
DlVI#    LKREQ F,D#VINV#_,(D,B#SAVED,DL_VINV#),CHAR,TEXT=PP#VINVN,COL=*         
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR INVOICE DOWNLOAD (BY INVOICE KEY)                   *         
***********************************************************************         
                                                                                
REQIVK   LKREQ H,M#DLINVK,OUTIVK                                                
DlBuy    LKREQ F,D#DLBDAT,(D,B#SAVED,INSDLSW),CHAR,TEXT=PP#DLINS,COL=*          
InvKy    LKREQ F,D#INVKEY,(I,B#SAVED,SERIND),CHAR,OLEN=INVKEYSL,       *        
               LIST=F,TEXT=PP#INVKY,COL=*                                       
DlVI#    LKREQ F,D#VINV#_,(D,B#SAVED,DL_VINV#),CHAR,TEXT=PP#VINVN,COL=*         
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR RUN-NOT-ORDERED DOWNLOAD                            *         
***********************************************************************         
                                                                                
REQRNO   LKREQ H,M#DL_RNO,OUTRNO                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,LIST=NOD,DEFAULT=Y,TEXT=PP#MEDCS,COL=*           
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),CHAR,                     *        
               OLEN=L'PCLTKCLT,LIST=NOD,DEFAULT=Y,TEXT=PP#CLTCS,COL=*           
PrdCd    LKREQ F,D#PRDCOD,(I,B#SAVED,PRDIND),CHAR,OLEN=L'PPRDKPRD,     *        
               LIST=NOD,DEFAULT=Y,TEXT=PP#PRDCS,COL=*                           
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,BUYPUB),CHAR,TEXT=PP#PUBCD,COL=*           
PbOpt    LKREQ F,D#DLPOPT,(D,B#SAVED,FLTPBOPT),CHAR,TEXT=PP#OPTNS,COL=*         
StrDt    LKREQ F,D#STRDAT,(D,B#SAVED,STRDATE),BDAT,TEXT=PP#STDAT,COL=*          
EndDt    LKREQ F,D#ENDDAT,(D,B#SAVED,ENDDATE),BDAT,TEXT=PP#ENDAT,COL=*          
CmID#    LKREQ F,D#IVDCM#,(D,B#SAVED,FILT#IDC),UBIN,TEXT=PP##IDCM,COL=*         
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR PROFILE DOWNLOAD                                    *         
***********************************************************************         
                                                                                
REQPRFD  LKREQ H,M#DLPROF,OUTPRFV                                               
ProfN    LKREQ F,D#PROFNM,(D,B#SAVED,PROFNAME),CHAR,TEXT=PP#PRFNM,     *        
               COL=*                                                            
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,LIST=NOD,DEFAULT=Y,TEXT=PP#MEDCS,COL=*           
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,LIST=NOD,DEFAULT=Y,TEXT=PP#CLTCS,COL=*           
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR EIO SETUP RECORD DOWNLOAD                           *         
***********************************************************************         
                                                                                
REQESRD  LKREQ H,M#SETRDL,OUTESRV                                               
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,LIST=NOD,DEFAULT=Y,TEXT=PP#MEDCS,COL=*           
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,LIST=NOD,DEFAULT=Y,TEXT=PP#CLTCS,COL=*           
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAPS FOR CFM DOWNLOADS                                      *         
***********************************************************************         
                                                                                
REQCFMC  LKREQ *,M#CFMCLT,OUTCFMC                                               
                                                                                
REQCFMP  LKREQ H,M#CFMPRD,OUTCFMP                                               
SysCd    LKREQ F,4,(I,B#SAVED,SMCIND),CHAR,OLEN=1,ARRAY=S,             *        
               TEXT=PP#SCODE,COL=*                                              
MedCd    LKREQ F,5,(I,B#SAVED,SMCIND),CHAR,OLEN=1,TEXT=PP#MED,COL=*             
CltCd    LKREQ F,6,(I,B#SAVED,SMCIND),CHAR,OLEN=3,ARRAY=E,             *        
               TEXT=PP#CLTC,COL=*                                               
         LKREQ E                                                                
                                                                                
REQCFMV  LKREQ H,M#CFMPUB,OUTCFMV                                               
SysCd    LKREQ F,1,(I,B#SAVED,SMCIND),CHAR,OLEN=1,ARRAY=S,             *        
               TEXT=PP#SCODE,COL=*                                              
MedCd    LKREQ F,2,(I,B#SAVED,SMCIND),CHAR,OLEN=1,ARRAY=E,             *        
               TEXT=PP#MED,COL=*                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR STATS DOWNLOAD                                      *         
***********************************************************************         
                                                                                
REQSTAT  LKREQ H,M#STATSD,OUTSTAT                                               
MNode    LKREQ F,21,(I,B#SAVED,MEDIND),LBIN,OLEN=4,LIST=F,             *        
               TEXT=(2,SP#MCONT),COL=*                                          
ANode    LKREQ F,22,(I,B#SAVED,ADVIND),LBIN,OLEN=4,LIST=F,             *        
               TEXT=(2,SP#ACONT),COL=*                                          
SNode    LKREQ F,23,(I,B#SAVED,SUPNODE),LBIN,TEXT=(2,SP#SCONT),COL=*,  *        
               LIST=F,OLEN=4                                                    
SDate    LKREQ F,24,(D,B#SAVED,STRDATEE),EDAT,TEXT=(2,SP#STDT),COL=*            
EDate    LKREQ F,25,(D,B#SAVED,ENDDATEE),EDAT,TEXT=(2,SP#ENDT),COL=*            
CalOp    LKREQ F,26,(D,B#SAVED,CALOPTN),CHAR,TEXT=(2,SP#CALOP),COL=*            
Split    LKREQ F,27,(D,B#SAVED,SPLOPTN),CHAR,TEXT=(2,SP#SPLOP),COL=*            
Sumry    LKREQ F,28,(D,B#SAVED,SUMOPTN),CHAR,TEXT=(2,SP#SUMRZ),COL=*            
PrgNm    LKREQ F,29,(D,B#SAVED,PNTOPTN),CHAR,TEXT=(2,SP#PRGNM),COL=*            
BrdNd    LKREQ F,30,(I,B#SAVED,CLTIND),LBIN,OLEN=4,ARRAY=S,            *        
               TEXT=(2,SP#BCONT),COL=*                                          
CliCd    LKREQ F,31,(I,B#SAVED,CLTIND),CHAR,OLEN=L'PCLTKCLT,           *        
               TEXT=(2,SP#CLI),COL=*                                            
BrdCd    LKREQ F,32,(I,B#SAVED,CLTIND),CHAR,OLEN=L'PPRDKPRD,ARRAY=E,   *        
               TEXT=(2,SP#PRO),COL=*                                            
EstOp    LKREQ F,33,(D,B#SAVED,ESTOPTN),CHAR,TEXT=(2,SP#ESTOP),COL=*            
EstNm    LKREQ F,34,(D,B#SAVED,ESNOPTN),CHAR,TEXT=(*,ESNLIT),COL=*              
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
                                                                                
ESNLIT   DC    C'Estimate names required?'                                      
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR BUY DOWNLOAD                                        *         
***********************************************************************         
                                                                                
OUTBUY   LKOUT H                                                                
                                                                                
BUYC     LKOUT R,E#BUY                                                          
Array    LKOUT C,E#BUY,(A,ARYBUY)                                               
         LKOUT E                                                                
                                                                                
INVC     LKOUT R,E#INVHDR                                                       
Array    LKOUT C,E#INVHDR,(A,ARYINV)                                            
         LKOUT E                                                                
                                                                                
PUBC1    LKOUT R,E#PUB                                                          
Array    LKOUT C,E#PUB,(A,ARYPUB)                                               
         LKOUT E                                                                
                                                                                
PUBC2    LKOUT R,E#PUB                                                          
Array    LKOUT C,E#PUB,(A,ARYPUB2),FILTROUT=TSTTDRR,PCVERSION=3.4.0.31          
         LKOUT E                                                                
                                                                                
REPC     LKOUT R,E#REP                                                          
Array    LKOUT C,E#REP,(A,ARYREP)                                               
         LKOUT E                                                                
                                                                                
AGYREC1  LKOUT R,E#AGENCY                                                       
Array    LKOUT C,E#AGENCY,(A,ARYAGYR),FILTROUT=TSTTDRR                          
         LKOUT E                                                                
                                                                                
PROB1    LKOUT R,E#BUY                                                          
TotIns   LKOUT C,D#INSCNT,(D,B#SAVED,T_INSCNT),UBIN,ND=Y,              +        
               FILTROUT=TSTTPRB                                                 
TotInv   LKOUT C,D#INVCNT,(D,B#SAVED,T_INVCNT),UBIN,ND=Y,              +        
               FILTROUT=TSTTPRB                                                 
TotIvd   LKOUT C,D#IVDCNT,(D,B#SAVED,T_IVDCNT),UBIN,ND=Y,              +        
               FILTROUT=TSTTPRB                                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR INSERTION HISTORY DOWNLOAD                          *         
***********************************************************************         
                                                                                
OUTHST   LKOUT H                                                                
                                                                                
IHINS    LKOUT R,E#BUY                                                          
Array    LKOUT C,E#BUY,(A,ARYIHINS)                                             
         LKOUT E                                                                
                                                                                
IOAC     LKOUT R,E#HSTINS                                                       
Array    LKOUT C,E#HSTINS,(A,ARYIOA)                                            
         LKOUT E                                                                
                                                                                
IBAC     LKOUT R,E#HSTBIL                                                       
Array    LKOUT C,E#HSTBIL,(A,ARYIBA)                                            
         LKOUT E                                                                
                                                                                
IPAC     LKOUT R,E#HSTPAY                                                       
Array    LKOUT C,E#HSTPAY,(A,ARYIPA)                                            
         LKOUT E                                                                
                                                                                
ICAC     LKOUT R,E#HSTACT                                                       
Array    LKOUT C,E#HSTACT,(A,ARYINSHS)                                          
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR BUY REFRESH DOWNLOAD                                *         
***********************************************************************         
                                                                                
OUTREF   LKOUT H                                                                
                                                                                
REFC     LKOUT R,E#BUY                                                          
Array    LKOUT C,E#BUY,(A,ARYREF)                                               
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR INVOICE DOWNLOAD (BY INVOICE NUMBER)                *         
***********************************************************************         
                                                                                
OUTIV#   LKOUT H                                                                
                                                                                
IV#C     LKOUT R,E#INVHDR                                                       
Array    LKOUT C,E#INVHDR,(A,ARYDIV#)                                           
         LKOUT E                                                                
                                                                                
SREPC    LKOUT R,E#SPREP                                                        
Array    LKOUT C,E#SPREP,(A,ARYSREP),FILTROUT=TSTTDRR                           
         LKOUT E                                                                
                                                                                
IV#BC    LKOUT R,E#BUY                                                          
Array    LKOUT C,E#BUY,(A,ARYIVB)                                               
         LKOUT E                                                                
                                                                                
PUBD1    LKOUT R,E#PUB                                                          
Array    LKOUT C,E#PUB,(A,ARYPUB),FILTROUT=TSTTDRR                              
         LKOUT E                                                                
                                                                                
PUBD2    LKOUT R,E#PUB                                                          
Array    LKOUT C,E#PUB,(A,ARYPUB2),FILTROUT=TSTTDRR,PCVERSION=3.4.0.31          
         LKOUT E                                                                
                                                                                
INSDET   LKOUT R,E#INSDET                                                       
Array    LKOUT C,E#INSDET,(A,ARYINSD),FILTROUT=TSTTDRR                          
         LKOUT E                                                                
                                                                                
AGYREC2  LKOUT R,E#AGENCY                                                       
Array    LKOUT C,E#AGENCY,(A,ARYAGYR),FILTROUT=TSTTDRR                          
         LKOUT E                                                                
                                                                                
MSGRPY   LKOUT R,E#ERRRPY                                                       
Array    LKOUT C,E#ERRRPY,(A,ARYMSGR),FILTROUT=TSTTMSG,                *        
               PCVERSION=3.2.0.3                                                
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR INVOICE DOWNLOAD (BY INVOICE KEY)                   *         
***********************************************************************         
                                                                                
OUTIVK   LKOUT H                                                                
                                                                                
IVKC     LKOUT R,E#INVHDR                                                       
Array    LKOUT C,E#INVHDR,(A,ARYDIVK)                                           
         LKOUT E                                                                
                                                                                
IVKBC    LKOUT R,E#BUY                                                          
Array    LKOUT C,E#BUY,(A,ARYIVB),FILTROUT=TSTINSDL                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR RUN-NOT-ORDERED DOWNLOAD                            *         
***********************************************************************         
                                                                                
OUTRNO   LKOUT H                                                                
                                                                                
INVRNO   LKOUT R,E#RNODET                                                       
Array    LKOUT C,E#RNODET,(A,ARYRNO)                                            
         LKOUT E                                                                
                                                                                
RNOMSG   LKOUT R,E#ERRRPY                                                       
Array    LKOUT C,E#ERRRPY,(A,ARYMSGR),FILTROUT=TSTTMSG                          
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR PROFILE DOWNLOAD                                    *         
***********************************************************************         
                                                                                
OUTPRFV  LKOUT H                                                                
                                                                                
PRFV     LKOUT R,E#PROFVL                                                       
Array    LKOUT C,E#PROFVL,(A,ARYPROF)                                           
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR EIO SETUP RECORD DOWNLOAD                           *         
***********************************************************************         
                                                                                
OUTESRV  LKOUT H                                                                
                                                                                
ESRV     LKOUT R,E#SETRRP                                                       
Array    LKOUT C,E#SETRRP,(A,ARYESREC)                                          
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR CFM CLIENT DOWNLOAD                                 *         
***********************************************************************         
                                                                                
OUTCFMC  LKOUT H                                                                
                                                                                
CFMC     LKOUT R,3                                                              
Array    LKOUT C,3,(A,ARYCLT)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR CFM PRODUCT DOWNLOAD                                *         
***********************************************************************         
                                                                                
OUTCFMP  LKOUT H                                                                
                                                                                
CFMP     LKOUT R,3                                                              
Array    LKOUT C,3,(A,ARYPRD)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR CFM VENDOR DOWNLOAD                                 *         
***********************************************************************         
                                                                                
OUTCFMV  LKOUT H                                                                
                                                                                
CFMV     LKOUT R,3                                                              
Array    LKOUT C,3,(A,ARYVEN)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* OUTPUT MAP FOR STATS DOWNLOAD                                       *         
***********************************************************************         
                                                                                
OUTSTAT  LKOUT H                                                                
                                                                                
         LKOUT R,3                                                              
Array    LKOUT C,3,(A,ARYSTAT)                                                  
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUY DOWNLOAD                                   *         
***********************************************************************         
                                                                                
ARYBUY   LKOUT A,(R,NXTBUY),MULTIROW=Y                                          
Array    LKOUT C,E#BUY,(A,ARYBUYV1),FILTROUT=TSTTNUL                            
Array    LKOUT C,E#BUY,(A,ARYBUYV2),FILTROUT=TSTTDRR                            
         LKOUT E                                                                
                                                                                
TSTTNUL  CLI   DL_TYPE,DL_NULQ     DOWNLOAD TYPE IS NULL?                       
         BR    RE                                                               
                                                                                
TSTTDRR  CLI   DL_TYPE,DL_DRRQ     DISCREPANCY RESOLUTION REPORT DL?            
         BR    RE                                                               
                                                                                
TSTTMSG  CLI   DL_TYPE,DL_MSGQ     MESSAGE REPLY?                               
         BR    RE                                                               
                                                                                
TSTTPRB  CLI   DL_TYPE,DL_PRBQ     PROBE DOWNLOAD?                              
         BR    RE                                                               
                                                                                
TSTINSDL L     R1,ALP                                                           
         USING LP_D,R1                                                          
         LAY   RF,=AL1(3,6,0,0)                                                 
         CLC   LP_VRSN1,0(RF)      VERSION 3.6.0.0?                             
         JH    *+8                                                              
         CR    RB,RB                                                            
         BR    RE                                                               
         CLI   INSDLSW,YESQ        DOWNLOAD INSERTION DATA?                     
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
TSTBS#   TP    BUYS#_TO            MAKE SURE THIS IS PACKED                     
         BNZR  RE                  OTHERWISE A DUMP WILL OCCUR                  
         LAY   RF,=PL3'32767'                                                   
         CP    BUYS#_TO,0(3,RF)    MAX SIZE                                     
         JH    *+8                                                              
         CR    RB,RB               FORCE EQ CC IF NOT HIGH                      
         BR    RE                                                               
         LTR   RB,RB               FORCE NE CC IF GREATER                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE DOWNLOAD                               *         
***********************************************************************         
*                                                                               
ARYINV   LKOUT A,(R,NXTINV),MULTIROW=Y                                          
Array    LKOUT C,E#INVHDR,(A,ARYIHV)                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE DOWNLOAD (BY INVOICE NUMBER)           *         
***********************************************************************         
                                                                                
ARYDIV#  LKOUT A,(R,NXTIV#),MULTIROW=Y                                          
Array    LKOUT C,E#INVHDR,(A,ARYIHV)                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE DOWNLOAD (BY INVOICE KEY)              *         
***********************************************************************         
                                                                                
ARYDIVK  LKOUT A,(R,NXTIVK),MULTIROW=Y                                          
Array    LKOUT C,E#INVHDR,(A,ARYIHV)                                            
         LKOUT E                                                                
                                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE ITEMS LINKED TO INSERTIONS (DRR)       *         
***********************************************************************         
                                                                                
ARYINSD  LKOUT A,(R,NXTIVI),MULTIROW=Y                                          
Array    LKOUT C,E#INSDET,(A,ARYIVI)                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR RUN-NOT-ORDERED INVOICE LINE ITEMS             *         
***********************************************************************         
                                                                                
ARYRNO   LKOUT A,(R,NXTRNO),MULTIROW=Y                                          
Array    LKOUT C,E#RNODET,(A,RNODET)                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUY REFRESH DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYREF   LKOUT A,(R,NXTSER),MULTIROW=Y                                          
Array    LKOUT C,E#BUY,(A,ARYBUYV1)                                             
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUY VALUES DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYBUYV1 LKOUT A,(D,B#SAVED,BUYVALS),NEWEL=Y,NROWS=1                            
InsKy    LKOUT C,D#INSKEY,(D,B#SAVED,BUYSER),CHAR,LEN=BUYSERLL,ND=Y             
PrdCd    LKOUT C,D#PRDCOD,(D,B#BUY,PBUYKPRD),CHAR,ND=Y                          
EstNo    LKOUT C,D#ESTNUM,(D,B#BUY,PBUYKEST),UBIN,ND=Y                          
PubCd    LKOUT C,D#PUBCOD,(D,B#SAVED,BUYPUB),CHAR,ND=Y                          
RepCd    LKOUT C,D#PUBREP,(D,B#SAVED,PBUREC+(PBUREP-PBUD)),CHAR,       *        
               LEN=L'PBUREP,ND=Y                                                
Space    LKOUT C,D#SPCDSC,(D,B#SAVED,BUYSPACE),CHAR,ND=Y                        
RegDs    LKOUT C,D#REGDSP,(D,B#SAVED,BUYREGDS),SPAK,ND=Y                        
IllPa    LKOUT C,D#ILLPAN,(D,B#SAVED,BUYILLPA),SPAK,ND=Y                        
Shows    LKOUT C,D#SHOWGS,(D,B#SAVED,BUYSHOWS),CHAR,ND=Y                        
UntRt    LKOUT C,D#UNTRAT,(D,B#SAVED,BUYRATE),CHAR,ND=Y                         
Prem.    LKOUT C,D#PREMUM,(D,B#SAVED,BUYPREM),CHAR,ND=Y                         
Alloc    LKOUT C,D#ALLOCS,(D,B#SAVED,BUYALLOC),CHAR,ND=Y                        
AdCod    LKOUT C,D#ADCODE,(D,B#BUY,PBDJOB),CHAR,ND=Y                            
AdCap    LKOUT C,D#ADCAP,(D,B#SAVED,LASTCAP),CHAR,ND=Y                          
AdID     LKOUT C,D#AD_ID,(D,B#SAVED,LASTADID),CHAR,ND=Y,               *        
               PCVERSION=3.2.0.3                                                
SubAd    LKOUT C,D#SUBADC,(D,B#SAVED,LASTSUBA),CHAR,ND=Y,              *        
               PCVERSION=3.5.0.41                                               
RtCod    LKOUT C,D#RATECD,(D,B#SAVED,BUYRTCOD),CHAR,ND=Y,              *        
               PCVERSION=3.2.0.3                                                
BStat    LKOUT C,D#INSSTA,(D,B#SAVED,BUYBSTAT),UBIN,ND=Y                        
BSta2    LKOUT C,D#BYSTA2,(D,B#SAVED,BUYSTAT2),LBIN,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
BSta3    LKOUT C,D#BYSTA3,(D,B#SAVED,BUYSTAT3),LBIN,ND=Y,              *        
               PCVERSION=3.5.0.6                                                
IONum    LKOUT C,D#INSIOR,(D,B#SAVED,BUYORDNO),CHAR,ND=Y                        
IOSta    LKOUT C,D#LASIOS,(D,B#SAVED,BUYIOSTA),UBIN,ND=Y,              *        
               PCVERSION=3.5.0.4                                                
SRNum    LKOUT C,D#LESRNO,(D,B#SAVED,BUYESRNO),CHAR,ND=Y,              *        
               PCVERSION=3.3.0.2                                                
SRDat    LKOUT C,D#LESRDT,(D,B#SAVED,BUYESRDT),BDAT,ND=Y,              *        
               PCVERSION=3.3.0.2                                                
SRSta    LKOUT C,D#LASSRS,(D,B#SAVED,BUYSRSTA),UBIN,ND=Y,              *        
               PCVERSION=3.5.0.4                                                
Repro    LKOUT C,D#REPROQ,(D,B#SAVED,BUYREPRO),UBIN,ND=Y                        
Buyer    LKOUT C,D#BUYERC,(D,B#BUY,PBDBUYER),CHAR,ND=Y                          
GSTax    LKOUT C,D#GST,(D,B#BUY,PBDGST),CHAR,ND=Y                               
PSTax    LKOUT C,D#PST,(D,B#SAVED,BUYPST),CHAR,ND=Y                             
IDate    LKOUT C,D#INSDAT,(D,B#BUY,PBUYKDAT),BDAT,ND=Y                          
ILine    LKOUT C,D#INSBYL,(D,B#SAVED,BUYLINE),CHAR,ND=Y                         
IType    LKOUT C,D#INSFRQ,(D,B#SAVED,BUYTYPE),CHAR,ND=Y                         
IDat2    LKOUT C,D#INSDA2,(D,B#BUY,PBDIDAT2),BDAT,ND=Y                          
SCDat    LKOUT C,D#SPCDAT,(D,B#BUY,PBDCDATE),BDAT,ND=Y                          
OSDat    LKOUT C,D#ONSDAT,(D,B#BUY,PBDSDATE),BDAT,ND=Y                          
BLDat    LKOUT C,D#BBLDAT,(D,B#SAVED,BLBLDT),BDAT,ND=Y                          
PLDat    LKOUT C,D#PBLDAT,(D,B#BUY,PBDPDATE),BDAT,ND=Y                          
ShpDt    LKOUT C,D#SHPDAT,(D,B#SAVED,BUYSHPDT),BDAT,ND=Y                        
MCXDt    LKOUT C,D#MCLXDT,(D,B#SAVED,BUYMCLXD),BDAT,ND=Y                        
MCXDy    LKOUT C,D#MCLXDY,(D,B#SAVED,BUYMCXDY),SPAK,ND=Y                        
IODat    LKOUT C,D#IORDAT,(D,B#SAVED,BUYORDDT),BDAT,ND=Y                        
MCDat    LKOUT C,D#MCLDAT,(D,B#BUY,PBDMDATE),BDAT,ND=Y                          
RCom1    LKOUT C,D#REGCO1,(D,B#SAVED,BUYREGC1),CHAR,ND=Y                        
RCom2    LKOUT C,D#REGCO2,(D,B#SAVED,BUYREGC2),CHAR,ND=Y                        
RCom3    LKOUT C,D#REGCO3,(D,B#SAVED,BUYREGC3),CHAR,ND=Y                        
RCom4    LKOUT C,D#REGCO4,(D,B#SAVED,BUYREGC4),CHAR,ND=Y                        
RCom5    LKOUT C,D#REGCO5,(D,B#SAVED,BUYREGC5),CHAR,ND=Y                        
ICom1    LKOUT C,D#INSCO1,(D,B#SAVED,BUYINSC1),CHAR,ND=Y                        
ICom2    LKOUT C,D#INSCO2,(D,B#SAVED,BUYINSC2),CHAR,ND=Y                        
ICom3    LKOUT C,D#INSCO3,(D,B#SAVED,BUYINSC3),CHAR,ND=Y                        
ICom4    LKOUT C,D#INSCO4,(D,B#SAVED,BUYINSC4),CHAR,ND=Y                        
ICom5    LKOUT C,D#INSCO5,(D,B#SAVED,BUYINSC5),CHAR,ND=Y                        
PIns1    LKOUT C,D#POSIN1,(D,B#SAVED,BUYPOSI1),CHAR,ND=Y                        
PIns2    LKOUT C,D#POSIN2,(D,B#SAVED,BUYPOSI2),CHAR,ND=Y                        
PIns3    LKOUT C,D#POSIN3,(D,B#SAVED,BUYPOSI3),CHAR,ND=Y                        
PIns4    LKOUT C,D#POSIN4,(D,B#SAVED,BUYPOSI4),CHAR,ND=Y                        
PIns5    LKOUT C,D#POSIN5,(D,B#SAVED,BUYPOSI5),CHAR,ND=Y                        
SRCm1    LKOUT C,D#SRCOM1,(D,B#SAVED,BUYSRCM1),CHAR,ND=Y,              +        
               PCVERSION=3.6.0.7                                                
SRCm2    LKOUT C,D#SRCOM2,(D,B#SAVED,BUYSRCM2),CHAR,ND=Y,              +        
               PCVERSION=3.6.0.7                                                
SRCm3    LKOUT C,D#SRCOM3,(D,B#SAVED,BUYSRCM3),CHAR,ND=Y,              +        
               PCVERSION=3.6.0.7                                                
SRCm4    LKOUT C,D#SRCOM4,(D,B#SAVED,BUYSRCM4),CHAR,ND=Y,              +        
               PCVERSION=3.6.0.7                                                
SRCm5    LKOUT C,D#SRCOM5,(D,B#SAVED,BUYSRCM5),CHAR,ND=Y,              +        
               PCVERSION=3.6.0.7                                                
GrsOr    LKOUT C,D#GRSORD,(D,B#SAVED,GROSS),CBIN,ND=Y                           
NetOr    LKOUT C,D#NETORD,(D,B#SAVED,BUYNETOR),CBIN,ND=Y                        
GLCDO    LKOUT C,D#GLCDO,(D,B#SAVED,BLABLE),CBIN,ND=Y                           
NLCDO    LKOUT C,D#NLCDO,(D,B#SAVED,PYABLE),CBIN,ND=Y                           
ACPct    LKOUT C,D#COMPCT,(D,B#SAVED,BUYACP),SPAK,ND=Y                          
ACAmt    LKOUT C,D#COMAMT,(D,B#SAVED,AGYCOM),CBIN,ND=Y                          
CDPct    LKOUT C,D#DSCPCT,(D,B#SAVED,BUYCDPCT),SPAK,PZERO=S                     
CDAmt    LKOUT C,D#DSCAMT,(D,B#SAVED,CSHDSC),CBIN,ND=Y                          
TaxRt    LKOUT C,D#TAXPCT,(D,B#BUY,PBDTAX),CBIN,ND=Y                            
TaxAm    LKOUT C,D#TAXAMT,(D,B#SAVED,TAX),CBIN,ND=Y                             
BDDat    LKOUT C,D#BLDDAT,(D,B#SAVED,BUYBINVD),BDAT,ND=Y                        
                                                                                
GrsBd    LKOUT C,D#GRSBD,(D,B#SAVED,BGROSS),CBIN,ND=Y                           
NetBd    LKOUT C,D#NETBD,(D,B#SAVED,BUYNETBD),CBIN,ND=Y                         
GLCBD    LKOUT C,D#GLCDB,(D,B#SAVED,BILLED),CBIN,ND=Y                           
NLCDB    LKOUT C,D#NLCDB,(D,B#SAVED,BUYBLNCD),CBIN,ND=Y                         
                                                                                
C2GBd    LKOUT C,D#CGRSBD,(D,B#SAVED,BYC2GRSB),CBIN,ND=Y,              +        
               PCVERSION=4.2.0.22                                               
C2NBd    LKOUT C,D#CNETBD,(D,B#SAVED,BYC2NETB),CBIN,ND=Y,              +        
               PCVERSION=4.2.0.22                                               
C2GCB    LKOUT C,D#CGLCDB,(D,B#SAVED,BYC2GCDB),CBIN,ND=Y,              +        
               PCVERSION=4.2.0.22                                               
C2NCB    LKOUT C,D#CNLCDB,(D,B#SAVED,BYC2NCDB),CBIN,ND=Y,              +        
               PCVERSION=4.2.0.22                                               
                                                                                
BInv#    LKOUT C,D#INVNUM,(D,B#SAVED,BUYBINVN),CHAR,ND=Y                        
PayDt    LKOUT C,D#PAYDAT,(D,B#SAVED,BUYPAYDT),BDAT,ND=Y                        
GrsPd    LKOUT C,D#GRSPD,(D,B#SAVED,PGROSS),CBIN,ND=Y                           
NetPd    LKOUT C,D#NETPD,(D,B#SAVED,BUYNETPD),CBIN,ND=Y                         
GLCDP    LKOUT C,D#GLCDP,(D,B#SAVED,PAID),CBIN,ND=Y                             
NLCDP    LKOUT C,D#NLCDP,(D,B#SAVED,BUYPLNCD),CBIN,ND=Y                         
PayRp    LKOUT C,D#PAYREP,(D,B#SAVED,BUYPREP),CHAR,ND=Y                         
ChkNo    LKOUT C,D#CHECKN,(D,B#SAVED,BUYCHKNO),CHAR,ND=Y                        
ChkDt    LKOUT C,D#CKDATE,(D,B#SAVED,BUYCHKDT),CDAT,ND=Y                        
PLCst    LKOUT C,D#PLCOST,(D,B#BUY,PBDPLCOS),CBIN,ND=Y                          
PLCst    LKOUT C,D#PLCOST,(D,B#SAVED,BUYPLCOS),CHAR,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
PCnet    LKOUT C,D#PC_NET,(D,B#SAVED,BUYPCNET),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
PCgrs    LKOUT C,D#PC_GRO,(D,B#SAVED,BUYPCGRS),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
SpRep    LKOUT C,D#SPREP,(D,B#SAVED,BUYSREP),CHAR,ND=Y                          
SpFiH    LKOUT C,D#SPECFH,(D,B#SAVED,BUYSFH),CHAR,ND=Y                          
GrsAc    LKOUT C,D#GRSAC,(D,B#SAVED,BUYGRSAC),CBIN,ND=Y                         
NetAc    LKOUT C,D#NETAC,(D,B#SAVED,BUYNETAC),CBIN,ND=Y                         
GLCAC    LKOUT C,D#GLCAC,(D,B#SAVED,BUYGLCAC),CBIN,ND=Y                         
NLCAC    LKOUT C,D#NLCAC,(D,B#SAVED,BUYNLCAC),CBIN,ND=Y                         
MchDt    LKOUT C,D#MCHDAT,(D,B#SAVED,BUYMCHDT),BDAT,ND=Y                        
MchIn    LKOUT C,D#MCHINV,(D,B#SAVED,BUYMCHIN),CHAR,ND=Y                        
TAppr    LKOUT C,D#TSHAPR,(D,B#SAVED,BUYTAPPR),CHAR,ND=Y                        
TStat    LKOUT C,D#TSHSTA,(D,B#SAVED,BUYTSTAT),CHAR,ND=Y                        
ByOrg    LKOUT C,D#BUYORG,(D,B#SAVED,BUYORGIN),UBIN,ND=Y,              *        
               PCVERSION=3.5.0.41                                               
Page#    LKOUT C,D#TSHNOT,(D,B#SAVED,BUYPAGEN),CHAR,ND=Y                        
TCom1    LKOUT C,D#TSHCO1,(D,B#SAVED,BUYTSHC1),CHAR,ND=Y                        
TCom2    LKOUT C,D#TSHCO2,(D,B#SAVED,BUYTSHC2),CHAR,ND=Y                        
TCom3    LKOUT C,D#TSHCO3,(D,B#SAVED,BUYTSHC3),CHAR,ND=Y                        
TCom4    LKOUT C,D#TSHCO4,(D,B#SAVED,BUYTSHC4),CHAR,ND=Y                        
DECir    LKOUT C,D#DEFCIR,(D,B#SAVED,BUYDECIR),SPAK,ND=Y                        
Rpnts    LKOUT C,D#REPNTS,(D,B#SAVED,BUYREPTS),SPAK,ND=Y                        
EImps    LKOUT C,D#ESTIMP,(D,B#SAVED,BUYEIMPS),SPAK,ND=Y                        
AImps    LKOUT C,D#ACTIMP,(D,B#SAVED,BUYAIMPS),SPAK,ND=Y                        
EsCPM    LKOUT C,D#ECPM,(D,B#SAVED,BUYECPM),SPAK,ND=Y                           
AcCPM    LKOUT C,D#ACPM,(D,B#SAVED,BUYACPM),SPAK,ND=Y                           
WSJBy    LKOUT C,D#WSJBUY,(D,B#SAVED,BUYWSJ),CHAR,ND=Y                          
ISite    LKOUT C,D#SITELO,(D,B#SAVED,BUYISITE),CHAR,ND=Y                        
ICNum    LKOUT C,D#ICNUM,(D,B#SAVED,BUYICNUM),CHAR,ND=Y                         
ICImp    LKOUT C,D#TOTIMP,(D,B#SAVED,BUYICIMP),SPAK,ND=Y                        
ICCPM    LKOUT C,D#TOTCPM,(D,B#SAVED,BUYICCPM),SPAK,ND=Y                        
ICRat    LKOUT C,D#TOTRAT,(D,B#SAVED,BUYICRAT),CHAR,ND=Y                        
ICIns    LKOUT C,D#ICINUM,(D,B#SAVED,BUYICINS),SPAK,ND=Y                        
Click    LKOUT C,D#CLICK,(D,B#SAVED,BUYCLICK),SPAK,ND=Y                         
Views    LKOUT C,D#VIEWS,(D,B#SAVED,BUYVIEWS),SPAK,ND=Y                         
FSIns    LKOUT C,D#FSINS,(D,B#SAVED,BUYFSINS),CHAR,ND=Y                         
CUVal    LKOUT C,D#CONUVL,(D,B#BUY,PBDCU),UBIN,ND=Y                             
ConLe    LKOUT C,D#CONLIE,(D,B#SAVED,BUYCLE),CHAR,ND=Y                          
RefNo    LKOUT C,D#REFNUM,(D,B#SAVED,BUYREFNO),CHAR,ND=Y                        
ISSNm    LKOUT C,D#ISSNM,(D,B#SAVED,BUYISSNM),CHAR,ND=Y                         
UplID    LKOUT C,D#UPLDID,(D,B#SAVED,BUYUPLID),CHAR,ND=Y                        
IClrS    LKOUT C,D#CLRSTA,(D,B#SAVED,BUYCLRST),CHAR,ND=Y,              *        
               PCVERSION=2.0.0.0                                                
ITSRS    LKOUT C,D#TEAREC,(D,B#SAVED,BUYTSRST),CHAR,ND=Y,              *        
               PCVERSION=2.0.0.0                                                
IMatS    LKOUT C,D#MATSTA,(D,B#SAVED,BUYMATST),CHAR,ND=Y,              *        
               PCVERSION=2.0.0.0                                                
IDisS    LKOUT C,D#DISSTA,(D,B#SAVED,BUYDISST),CHAR,ND=Y,              *        
               PCVERSION=2.0.0.0                                                
NPybl    LKOUT C,D#NPYBLE,(D,B#SAVED,BUYNPYBL),CBIN,ND=Y,              *        
               PCVERSION=2.0.0.0                                                
GPybl    LKOUT C,D#GPYBLE,(D,B#SAVED,BUYGPYBL),CBIN,ND=Y,              *        
               PCVERSION=2.0.0.0                                                
C2Fac    LKOUT C,D#C2FACT,(D,B#SAVED,BUYCOS2F),CHAR,ND=Y,              *        
               PCVERSION=3.3.0.8                                                
C2Grs    LKOUT C,D#C2FGRS,(D,B#SAVED,BUYC2GRS),CBIN,ND=Y,              *        
               PCVERSION=3.3.0.8                                                
C2Net    LKOUT C,D#C2FNET,(D,B#SAVED,BUYC2NET),CBIN,ND=Y,              *        
               PCVERSION=3.3.0.8                                                
S#Fro    LKOUT C,D#BS#FRO,(D,B#SAVED,BUYS#FRO),SPAK,ND=Y,              *        
               PCVERSION=3.4.0.5                                                
S#_To    LKOUT C,D#BS#_TO,(D,B#SAVED,BUYS#_TO),SPAK,ND=Y,              *        
               PCVERSION=3.4.0.5,FILTROUT=TSTBS#                                
QIPID    LKOUT C,D#RQIPID,(D,B#SAVED,BUYQIPID),CHAR,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
QIDAT    LKOUT C,D#RQIDAT,(D,B#SAVED,BUYQIDAT),CDAT,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
RIPID    LKOUT C,D#RCIPID,(D,B#SAVED,BUYRIPID),CHAR,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
RIDAT    LKOUT C,D#RCIDAT,(D,B#SAVED,BUYRIDAT),CDAT,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
POSQ#    LKOUT C,D#PO#SQN,(D,B#SAVED,BUYPOSQ#),UBIN,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
InADt    LKOUT C,D#INSADT,(D,B#BUY,PBDBUYDT),BDAT,ND=Y,                *        
               PCVERSION=3.5.0.41                                               
                                                                                
FXRat    LKOUT C,D#FXRATE,(D,B#SAVED,FX_RATE_),CHAR,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXACA    LKOUT C,D#FXACAM,(D,B#SAVED,FXAC_AMT),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXCDA    LKOUT C,D#FXCDAM,(D,B#SAVED,FXCD_AMT),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXNet    LKOUT C,D#FXNETO,(D,B#SAVED,FXNETORD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXGrs    LKOUT C,D#FXGRSO,(D,B#SAVED,FXGRSORD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXNPb    LKOUT C,D#FXNPYB,(D,B#SAVED,FXNETPYB),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXGPb    LKOUT C,D#FXGPYB,(D,B#SAVED,FXGRSPYB),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXNPd    LKOUT C,D#FXNETP,(D,B#SAVED,FXNET_PD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXGPd    LKOUT C,D#FXGRSP,(D,B#SAVED,FXGRS_PD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXNLP    LKOUT C,D#FXNLCP,(D,B#SAVED,FXNLCDPD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXGLP    LKOUT C,D#FXGLCP,(D,B#SAVED,FXGLCDPD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXNLO    LKOUT C,D#FXNLCO,(D,B#SAVED,FXNLCDOR),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
FXGLO    LKOUT C,D#FXGLCO,(D,B#SAVED,FXGLCDOR),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_Net    LKOUT C,D#S_NETO,(D,B#SAVED,S_NETORD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_Grs    LKOUT C,D#S_GRSO,(D,B#SAVED,S_GRSORD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_NPb    LKOUT C,D#S_NPYB,(D,B#SAVED,S_NETPYB),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_GPb    LKOUT C,D#S_GPYB,(D,B#SAVED,S_GRSPYB),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_NPd    LKOUT C,D#S_NETP,(D,B#SAVED,S_NET_PD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_GPd    LKOUT C,D#S_GRSP,(D,B#SAVED,S_GRS_PD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_NLP    LKOUT C,D#S_NLCP,(D,B#SAVED,S_NLCDPD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_GLP    LKOUT C,D#S_GLCP,(D,B#SAVED,S_GLCDPD),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_NLO    LKOUT C,D#S_NLCO,(D,B#SAVED,S_NLCDOR),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
S_GLO    LKOUT C,D#S_GLCO,(D,B#SAVED,S_GLCDOR),CBIN,ND=Y,              *        
               PCVERSION=3.4.0.41                                               
COS2$    LKOUT C,D#COS2$$,(D,B#SAVED,BUYCOS2$),CHAR,ND=Y,              *        
               PCVERSION=4.0.0.4                                                
                                                                                
Array    LKOUT C,E#ACR,(A,ARYACR)                                               
Array    LKOUT C,E#INV,(A,ARYBINV),PCVERSION=2.0.0.0                            
Array    LKOUT C,E#BCC,(A,ARYBCC),PCVERSION=2.0.0.0                             
Array    LKOUT C,E#ZZZPO#,(A,ARYBPO#),PCVERSION=3.4.0.31                        
Array    LKOUT C,E#VIV#PY,(A,ARYPIV#),PCVERSION=4.0.0.0,FILTROUT=TSTPIV         
         LKOUT E                                                                
                                                                                
TSTPIV   CLI   DL_VINV#,YESQ       Test downloading Pay Invoice#                
         BR    RE                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DISCREPANCY RESOLUTION REPORT DOWNLOAD         *         
***********************************************************************         
                                                                                
ARYBUYV2 LKOUT A,(D,B#SAVED,BUYVALS),NEWEL=Y,NROWS=1                            
InsKy    LKOUT C,D#INSKEY,(D,B#SAVED,BUYSER),CHAR,LEN=BUYSERLL,ND=Y             
PrdCd    LKOUT C,D#PRDCOD,(D,B#BUY,PBUYKPRD),CHAR,ND=Y                          
EstNo    LKOUT C,D#ESTNUM,(D,B#BUY,PBUYKEST),UBIN,ND=Y                          
PubCd    LKOUT C,D#PUBCOD,(D,B#SAVED,BUYPUB),CHAR,ND=Y                          
Space    LKOUT C,D#SPCDSC,(D,B#SAVED,BUYSPACE),CHAR,ND=Y                        
RegDs    LKOUT C,D#REGDSP,(D,B#SAVED,BUYREGDS),SPAK,ND=Y                        
IllPa    LKOUT C,D#ILLPAN,(D,B#SAVED,BUYILLPA),SPAK,ND=Y                        
Shows    LKOUT C,D#SHOWGS,(D,B#SAVED,BUYSHOWS),CHAR,ND=Y                        
UntRt    LKOUT C,D#UNTRAT,(D,B#SAVED,BUYRATE),CHAR,ND=Y                         
Prem.    LKOUT C,D#PREMUM,(D,B#SAVED,BUYPREM),CHAR,ND=Y                         
IONum    LKOUT C,D#INSIOR,(D,B#SAVED,BUYORDNO),CHAR,ND=Y                        
IDate    LKOUT C,D#INSDAT,(D,B#BUY,PBUYKDAT),BDAT,ND=Y                          
IDat2    LKOUT C,D#INSDA2,(D,B#BUY,PBDIDAT2),BDAT,ND=Y                          
GrsOr    LKOUT C,D#GRSORD,(D,B#SAVED,GROSS),CBIN,ND=Y                           
NetOr    LKOUT C,D#NETORD,(D,B#SAVED,BUYNETOR),CBIN,ND=Y                        
GLCDO    LKOUT C,D#GLCDO,(D,B#SAVED,BLABLE),CBIN,ND=Y                           
NLCDO    LKOUT C,D#NLCDO,(D,B#SAVED,PYABLE),CBIN,ND=Y                           
ACPct    LKOUT C,D#COMPCT,(D,B#SAVED,BUYACP),SPAK,ND=Y                          
ACAmt    LKOUT C,D#COMAMT,(D,B#SAVED,AGYCOM),CBIN,ND=Y                          
CDPct    LKOUT C,D#DSCPCT,(D,B#SAVED,BUYCDPCT),SPAK,PZERO=S                     
CDAmt    LKOUT C,D#DSCAMT,(D,B#SAVED,CSHDSC),CBIN,ND=Y                          
TaxRt    LKOUT C,D#TAXPCT,(D,B#BUY,PBDTAX),CBIN,ND=Y                            
TaxAm    LKOUT C,D#TAXAMT,(D,B#SAVED,TAX),CBIN,ND=Y                             
BDDat    LKOUT C,D#BLDDAT,(D,B#SAVED,BUYBINVD),BDAT,ND=Y                        
SpRep    LKOUT C,D#SPREP,(D,B#SAVED,BUYSREP),CHAR,ND=Y                          
ILine    LKOUT C,D#INSBYL,(D,B#SAVED,BUYLINE),CHAR,ND=Y                         
TAppr    LKOUT C,D#TSHAPR,(D,B#SAVED,BUYTAPPR),CHAR,ND=Y                        
GSTax    LKOUT C,D#GST,(D,B#BUY,PBDGST),CHAR,ND=Y                               
PSTax    LKOUT C,D#PST,(D,B#SAVED,BUYPST),CHAR,ND=Y                             
AdCap    LKOUT C,D#ADCAP,(D,B#SAVED,LASTCAP),CHAR,ND=Y                          
ISSNm    LKOUT C,D#ISSNM,(D,B#SAVED,BUYISSNM),CHAR,ND=Y                         
ITSRS    LKOUT C,D#TEAREC,(D,B#SAVED,BUYTSRST),CHAR,ND=Y                        
NPybl    LKOUT C,D#NPYBLE,(D,B#SAVED,BUYNPYBL),CBIN,ND=Y                        
GPybl    LKOUT C,D#GPYBLE,(D,B#SAVED,BUYGPYBL),CBIN,ND=Y                        
IDisS    LKOUT C,D#DISSTA,(D,B#SAVED,BUYDISST),CHAR,ND=Y                        
IMatS    LKOUT C,D#MATSTA,(D,B#SAVED,BUYMATST),CHAR,ND=Y                        
RCom1    LKOUT C,D#REGCO1,(D,B#SAVED,BUYREGC1),CHAR,ND=Y                        
IClrS    LKOUT C,D#CLRSTA,(D,B#SAVED,BUYCLRST),CHAR,ND=Y,              *        
               PCVERSION=3.4.0.31                                               
                                                                                
Array    LKOUT C,E#INV,(A,ARYBINV)                                              
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE HEADER VALUE DOWNLOAD                  *         
***********************************************************************         
                                                                                
ARYIHV   LKOUT A,(D,B#INVVAL,INVVALSD),NEWEL=Y,NROWS=1,ROWWIDTH=0               
InvKy    LKOUT C,D#INVKEY,(D,,INVKEY),CHAR,LEN=INVKEYSL,ND=Y                    
HGSta    LKOUT C,D#GENSTA,(D,,INVHGSTA),CHAR,ND=Y                               
ICltC    LKOUT C,D#CLTCOD,(D,,INVCLTC),CHAR,ND=Y                                
IPubC    LKOUT C,D#PUBCOD,(D,,INVHPUBC),CHAR,ND=Y                               
VInv#    LKOUT C,D#VINVNO,(D,,INVVNDR#),CHAR,ND=Y                               
IvSta    LKOUT C,D#INVSTA,(D,,INVSTAT),CHAR,ND=Y                                
SPStr    LKOUT C,D#IPRSTR,(D,,INVPSTR),BDAT,ND=Y                                
SPEnd    LKOUT C,D#IPREND,(D,,INVPEND),BDAT,ND=Y                                
IvDat    LKOUT C,D#INVDAT,(D,,INVDATE),BDAT,ND=Y                                
IvTot    LKOUT C,D#INVTOT,(D,,INVTOTL),SPAK,ND=Y                                
IvTyp    LKOUT C,D#ITOTYP,(D,,INVTTYP),CHAR,ND=Y                                
IvSRp    LKOUT C,D#SPREP,(D,,INVSREP),CHAR,ND=Y                                 
IvTax    LKOUT C,D#INVTAX,(D,,INVHTAX),CBIN,ND=Y,PCVERSION=4.2.0.13             
IvSrc    LKOUT C,D#INVSRC,(D,,INVHSRC),CHAR,ND=Y,PCVERSION=4.0.0.1              
IvCDT    LKOUT C,D#INVCDT,(D,,INVHCDT),BDAT,ND=Y,PCVERSION=4.0.0.1              
Array    LKOUT C,E#IHDRCM,(A,ARYINVC)                                           
Array    LKOUT C,E#INVDET,(A,ARYIDV)                                            
Array    LKOUT C,E#INVSMY,(A,ARYISMY),PCVERSION=4.2.0.16                        
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE DETAIL VALUE DOWNLOAD                  *         
***********************************************************************         
                                                                                
ARYIDV   LKOUT A,(R,GETIDV),MULTIROW=Y,ROWNAME=INVVALSD                         
ItmSN    LKOUT C,D#ITMSQN,(D,,INVISQN),UBIN,ND=Y                                
DGSta    LKOUT C,D#GENSTA,(D,,INVDGSTA),CHAR,ND=Y                               
InsKy    LKOUT C,D#INSKEY,(D,,INVINSKY),CHAR,ND=Y                               
IRate    LKOUT C,D#UNTRAT,(D,,INVRATE),CHAR,ND=Y                                
IPrem    LKOUT C,D#PREMUM,(D,,INVPREM),CHAR,ND=Y                                
NetOr    LKOUT C,D#NETORD,(D,,INVNET),SPAK,ND=Y                                 
GrsOr    LKOUT C,D#GRSORD,(D,,INVGROSS),SPAK,ND=Y                               
I#Lin    LKOUT C,D#NUMLIN,(D,,INV#LINE),UBIN,ND=Y                               
IInsD    LKOUT C,D#INSDAT,(D,,INVINSDT),BDAT,ND=Y                               
ISpDs    LKOUT C,D#SPCDSC,(D,,INVSPDSP),CHAR,ND=Y                               
IAdCp    LKOUT C,D#ADCAP,(D,,INVADCAP),CHAR,ND=Y                                
ICltC    LKOUT C,D#CLTCOD,(D,,INVCLTCD),CHAR,ND=Y                               
IPrdC    LKOUT C,D#PRDCOD,(D,,INVPRDCD),CHAR,ND=Y                               
IPubC    LKOUT C,D#PUBCOD,(D,,INVDPUBC),CHAR,ND=Y                               
IDCMC    LKOUT C,D#DCOMCD,(D,,INVDDCMC),CHAR,ND=Y,PCVERSION=3.6.0.1             
IDCMP    LKOUT C,D#DCOMPI,(D,,INVDDCPI),CHAR,ND=Y,PCVERSION=3.6.0.7             
IDCMD    LKOUT C,D#DCOMDT,(D,,INVDDCDT),BDAT,ND=Y,PCVERSION=3.6.0.7             
IvSrc    LKOUT C,D#INVSRC,(D,,INVDSRC),CHAR,ND=Y,PCVERSION=4.0.0.1              
Array    LKOUT C,E#IDETCM,(A,ARYINVC)                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR RUN-NOT-ORDERED INVOICE LINE ITEMS             *         
***********************************************************************         
                                                                                
RNODET   LKOUT A,(R,GETRNO),MULTIROW=Y                                          
Array    LKOUT C,E#RNODET,(A,ARYIVI)                                            
Array    LKOUT C,E#IDETCM,(A,ARYINVC),PCVERSION=3.4.0.31                        
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE ITEMS FROM INSERTION (DRR)             *         
***********************************************************************         
                                                                                
ARYIVI   LKOUT A,(D,B#INVVAL,INVVALSD),NEWEL=Y,NROWS=1,ROWWIDTH=0               
InvKy    LKOUT C,D#INVKEY,(D,,IVNDKEY),CHAR,LEN=INVKEYSL,ND=Y                   
VInv#    LKOUT C,D#VINVNO,(D,,INVDVI#),CHAR,ND=Y                                
IvSta    LKOUT C,D#INVSTA,(D,,INVDSTA),CHAR,ND=Y                                
IvTyp    LKOUT C,D#ITOTYP,(D,,INVDTYP),CHAR,ND=Y                                
ItmSN    LKOUT C,D#ITMSQN,(D,,INVISQN),UBIN,ND=Y                                
DGSta    LKOUT C,D#GENSTA,(D,,INVDGSTA),CHAR,ND=Y                               
InsKy    LKOUT C,D#INSKEY,(D,,INVINSKY),CHAR,ND=Y                               
IRate    LKOUT C,D#UNTRAT,(D,,INVRATE),CHAR,ND=Y                                
IPrem    LKOUT C,D#PREMUM,(D,,INVPREM),CHAR,ND=Y                                
NetOr    LKOUT C,D#NETORD,(D,,INVNET),SPAK,ND=Y                                 
GrsOr    LKOUT C,D#GRSORD,(D,,INVGROSS),SPAK,ND=Y                               
I#Lin    LKOUT C,D#NUMLIN,(D,,INV#LINE),UBIN,ND=Y                               
IInsD    LKOUT C,D#INSDAT,(D,,INVINSDT),BDAT,ND=Y                               
ISpDs    LKOUT C,D#SPCDSC,(D,,INVSPDSP),CHAR,ND=Y                               
IAdCp    LKOUT C,D#ADCAP,(D,,INVADCAP),CHAR,ND=Y                                
ICltC    LKOUT C,D#CLTCOD,(D,,INVCLTCD),CHAR,ND=Y                               
IPrdC    LKOUT C,D#PRDCOD,(D,,INVPRDCD),CHAR,ND=Y                               
IPubC    LKOUT C,D#PUBCOD,(D,,INVDPUBC),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE COMMENT DOWNLOAD                       *         
***********************************************************************         
                                                                                
ARYINVC  LKOUT A,(R,GETIVC),MULTIROW=Y,ROWNAME=INVVALSD                         
DGSta    LKOUT C,D#GENSTA,(D,,INVCGSTA),CHAR,ND=Y                               
Buyrc    LKOUT C,D#BUYERC,(D,,INVPIDNM),CHAR,ND=Y                               
ComDt    LKOUT C,D#COMMDT,(D,,INVCOMDT),BDAT,ND=Y                               
Array    LKOUT C,D#COMMNT,(A,ARYIVCC)                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY OF ARRAY DEFINITION FOR INVOICE COMMENTS                      *         
***********************************************************************         
                                                                                
ARYIVCC  LKOUT A,(D,B#INVCOM,IVCOMNTD),ROWWIDTH=L'IVCOMMNT,            *        
               NROWS=IVCOMMMX                                                   
Comnt    LKOUT C,D#COMMNT,(D,,IVCOMMNT),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUY DOWNLOAD (DRIVEN BY INVOICES)              *         
***********************************************************************         
                                                                                
ARYIVB   LKOUT A,(R,NXTIVB),MULTIROW=Y                                          
Array    LKOUT C,E#BUY,(A,ARYBUYV1),FILTROUT=TSTTNUL                            
Array    LKOUT C,E#BUY,(A,ARYBUYV2),FILTROUT=TSTTDRR                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for Invoice Summary reply record                   *         
***********************************************************************         
                                                                                
ARYISMY  LKOUT A,(R,NXISMY),MULTIROW=Y,ROWNAME=INVVALSD                         
IvTwTax  LKOUT C,001,(D,,INVTWTAX),CBIN,ND=Y                                    
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PROFILE VALUES                                 *         
***********************************************************************         
                                                                                
ARYPROF  LKOUT A,(R,GETPRF),MULTIROW=Y                                          
ProfV    LKOUT C,D#PROFVL,(D,B#SAVED,PROFILEV),CHAR,ND=Y                        
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ADDITIONAL CHARGE RATES DOWNLOAD               *         
***********************************************************************         
                                                                                
ARYACR   LKOUT A,(D,B#SAVED,BUYATAB),NEWEL=Y,NROWS=(B#SAVED,BUYATAB#), *        
               ROWWIDTH=BUYATABL                                                
ACCod    LKOUT C,D#ACHCOD,(D,,BUYATCOD),CHAR                                    
ACGrs    LKOUT C,D#ACHGRS,(D,,BUYATCRG),CHAR                                    
ACSac    LKOUT C,D#ACHSAC,(D,,BUYATCOM),CHAR,ND=Y                               
ACCC%    LKOUT C,D#ACHCPT,(D,,BUYATCPC),SPAK,ND=Y                               
ACSCD    LKOUT C,D#ACHCDA,(D,,BUYATSCD),CHAR,ND=Y                               
ACSTA    LKOUT C,D#ACHSTA,(D,,BUYATSTA),LBIN,ND=Y,PCVERSION=3.5.0.17            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY OF ARRAY DEFINITION FOR INVOICE DATA IN BUY RECORD            *         
***********************************************************************         
                                                                                
ARYBINV  LKOUT A,(D,B#INSARY,BINVTAB),ROWNAME=BINVTAB,                 *        
               NROWS=(B#INSARY,BINVTAB#),ROWWIDTH=BINVTABL,NEWEL=Y              
BIvKy    LKOUT C,D#INVKEY,(D,,BINVKEY),CHAR,ND=Y                                
BIvSq    LKOUT C,D#ITMSQN,(D,,BINVSEQ#),UBIN                                    
BIvNo    LKOUT C,D#VINVNO,(D,,BINVVIV#),CHAR                                    
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY OF ARRAY DEFINITION FOR ZZZ BUY PURCHASE ORDER NUMBER         *         
***********************************************************************         
                                                                                
ARYBPO#  LKOUT A,(D,B#INSARY,BPO#TAB),ROWNAME=BPO#TAB,                 *        
               NROWS=(B#INSARY,BPO#TAB#),ROWWIDTH=BPO#TABL,NEWEL=Y              
BPrdC    LKOUT C,D#PO#PRD,(D,,BPO#PRDC),CHAR                                    
BPOSq    LKOUT C,D#PO#SQN,(D,,BPO#SEQ#),UBIN                                    
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY OF ARRAY DEFINITION FOR CUSTOM COLUMN DATA IN BUY RECORD      *         
***********************************************************************         
                                                                                
ARYBCC   LKOUT A,(R,GETBCC),MULTIROW=Y,ROWNAME=BCCVARY,                *        
               PCVERSION=2.2.0.0                                                
BCCS#    LKOUT C,D#CCSEQN,(D,,BUYCCSQ#),UBIN,ND=Y                               
BCCFD    LKOUT C,D#CCFDAT,(D,,BUYCCFDA),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array OF array definition for manual Vendor Invoice# from Pay       *         
***********************************************************************         
                                                                                
ARYPIV#  LKOUT A,(R,GETPIV),MULTIROW=Y,ROWNAME=IPAD,PCVERSION=4.0.0.0           
VIv#P    LKOUT C,D#INVNUM,(D,,IPAINVNO),CHAR                                    
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PUBLICATION RECORDS                            *         
***********************************************************************         
                                                                                
ARYPUB   LKOUT A,(R,GETPUB),MULTIROW=Y,ROWNAME=PBUD                             
Media    LKOUT C,D#MEDCOD,(D,,PBUMED),CHAR                                      
PCode    LKOUT C,D#PUBCOD,(D,,PBUCODE),CHAR                                     
PName    LKOUT C,D#PUBNAM,(D,,PBUNAME),CHAR                                     
PAdL1    LKOUT C,D#ADDRL1,(D,,PBUADDL1),CHAR,PCVERSION=3.2.0.2                  
PAdL2    LKOUT C,D#ADDRL2,(D,,PBUADDL2),CHAR,PCVERSION=3.2.0.2                  
PAdL3    LKOUT C,D#ADDRL3,(D,,PBUADDL3),CHAR,PCVERSION=3.2.0.2                  
PZNam    LKOUT C,D#PZNAME,(D,,PBUZNAME),CHAR PCVERSION=3.2.0.2                  
PCity    LKOUT C,D#PCITY,(D,,PBUCITY),CHAR                                      
PStat    LKOUT C,D#PSTATE,(D,,PBUSTATE),CHAR                                    
PZipC    LKOUT C,D#ZIPCOD,(D,,PBUZIPCD),CHAR,PCVERSION=3.2.0.2                  
PAYRP    LKOUT C,D#PAYREP,(D,,PBUPAYCD),CHAR,ND=Y,PCVERSION=3.5.0.6             
PNStC    LKOUT C,D#PNSTCD,(D,,PBUNSTCD),CHAR,ND=Y,PCVERSION=3.5.0.41            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR 2ND PUBLICATION ADDRESS                        *         
***********************************************************************         
                                                                                
ARYPUB2  LKOUT A,(R,GETPUB),MULTIROW=Y,ROWNAME=PBUD                             
Media    LKOUT C,D#MEDCOD,(D,,PBUMED),CHAR                                      
PCode    LKOUT C,D#PUBCOD,(D,,PBUCODE),CHAR                                     
PNam2    LKOUT C,D#PUBNAM,(D,,PBUNAME2),CHAR                                    
PAL12    LKOUT C,D#ADDRL1,(D,,PBUAL1_2),CHAR                                    
PAL22    LKOUT C,D#ADDRL2,(D,,PBUAL2_3),CHAR                                    
PAL32    LKOUT C,D#ADDRL3,(D,,PBUAL3_2),CHAR                                    
PZNm2    LKOUT C,D#PZNAME,(D,,PBUZNAM2),CHAR                                    
PCty2    LKOUT C,D#PCITY,(D,,PBUCITY2),CHAR                                     
PSta2    LKOUT C,D#PSTATE,(D,,PBUSTAT2),CHAR                                    
PZip2    LKOUT C,D#ZIPCOD,(D,,PBUZIPC2),CHAR                                    
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PUBLISHER/REP RECORDS                          *         
***********************************************************************         
                                                                                
ARYREP   LKOUT A,(R,GETREP),MULTIROW=Y,ROWNAME=RBUD                             
Media    LKOUT C,D#MEDCOD,(D,,RBUMED),CHAR                                      
RCode    LKOUT C,D#PUBREP,(D,,RBUREP),CHAR                                      
RName    LKOUT C,D#REPNAM,(D,,RBUNAME),CHAR                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION HISTORY DOWNLOAD (SERIAL# NOT FOUND) *         
***********************************************************************         
                                                                                
ARYIHINS LKOUT A,(D,B#SAVED,BUYVALS),NEWEL=Y,NROWS=1,                  *        
               ROWID=(BUYFOUND,NOQ)                                             
InsKy    LKOUT C,D#INSKEY,(D,B#SAVED,BUYSER),CHAR                               
BStat    LKOUT C,D#INSSTA,(D,B#SAVED,BUYBSTAT),UBIN                             
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION ORDER HISTORY                        *         
***********************************************************************         
                                                                                
ARYIOA   LKOUT A,(I,B#WORKD,AIO6),NROWS=(B#SAVED,IOA#),ROWWIDTH=IOAL,  *        
               ROWNAME=IOAD,NEWEL=B                                             
IODat    LKOUT C,D#INSDTP,(D,,IOADATE),BDAT                                     
IONum    LKOUT C,D#INSNUM,(D,,IOANUM),CHAR                                      
IOTyp    LKOUT C,D#INSTYP,(D,,IOATYPE),CHAR                                     
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION BILLING HISTORY                      *         
***********************************************************************         
                                                                                
ARYIBA   LKOUT A,(I,B#WORKD,AIO3),NROWS=(B#SAVED,IBA#),ROWWIDTH=IBAL,  *        
               ROWNAME=IBAD,NEWEL=B                                             
BlTyp    LKOUT C,00000141,(D,,IBABTYPE),CHAR,ND=Y,PCVERSION=4.2.0.22            
BlDat    LKOUT C,D#BLDDAT,(D,,IBADATE),BDAT                                     
BlGrs    LKOUT C,D#GRSBD,(D,,IBAGROSS),CBIN                                     
BlNet    LKOUT C,D#NETBD,(D,,IBANET),CBIN                                       
BlN-C    LKOUT C,D#NLCDB,(D,,IBANLCDB),CBIN                                     
BlIn#    LKOUT C,D#INVNUM,(D,,IBAINVNO),CHAR                                    
BlSta    LKOUT C,D#BILSTA,(D,,IBASTATS),LBIN,ND=Y,PCVERSION=4.0.0.0             
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION PAYING HISTORY                       *         
***********************************************************************         
                                                                                
ARYIPA   LKOUT A,(I,B#WORKD,AIO4),NROWS=(B#SAVED,IPA#),ROWWIDTH=IPAL,  *        
               ROWNAME=IPAD,NEWEL=B                                             
PdDat    LKOUT C,D#PAYDAT,(D,,IPADATE),BDAT                                     
PdIv#    LKOUT C,D#INVNUM,(D,,IPAINVX_),CHAR,PCVERSION=3.6.0.1                  
PdGrs    LKOUT C,D#GRSPD,(D,,IPAGROSS),CBIN                                     
PdNet    LKOUT C,D#NETPD,(D,,IPANET),CBIN                                       
PdN-C    LKOUT C,D#NLCDP,(D,,IPANLCDP),CBIN                                     
PdRep    LKOUT C,D#PAYREP,(D,,IPAPAYEE),CHAR                                    
PdCk#    LKOUT C,D#CHECKN,(D,,IPACHECK),CHAR                                    
PdACd    LKOUT C,D#ACHCOD,(D,,IPAACODE),CHAR,ND=Y                               
PdSq#    LKOUT C,D#CHKSQN,(D,,IPASEQ_#),UBIN                                    
PdCkD    LKOUT C,D#CKDATE,(D,,IPACKDAT),CDAT,PCVERSION=3.6.0.1                  
PayTy    LKOUT C,D#PAYTYP,(D,,IPACPTYP),CHAR,PCVERSION=3.6.0.7                  
PaySt    LKOUT C,D#PAYSTA,(D,,IPASTAT1),LBIN,ND=Y,PCVERSION=4.0.0.0             
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SPECIAL REP (PRINTPAK REP RECORDS)             *         
***********************************************************************         
                                                                                
ARYSREP  LKOUT A,(R,GETSREP),MULTIROW=Y,ROWNAME=INVVALSD                        
Media    LKOUT C,D#MEDCOD,(D,,SREP_MED),CHAR                                    
RCode    LKOUT C,D#SPREP,(D,,SREP_REP),CHAR                                     
RName    LKOUT C,D#REPNAM,(D,,SREP_NAM),CHAR                                    
Atten    LKOUT C,D#ATTENT,(D,,SREP_ATT),CHAR,ND=Y                               
Alin1    LKOUT C,D#ADDRL1,(D,,SREP_AD1),CHAR,ND=Y                               
Alin2    LKOUT C,D#ADDRL2,(D,,SREP_AD2),CHAR,ND=Y                               
Alin3    LKOUT C,D#ADDRL3,(D,,SREP_AD3),CHAR,ND=Y                               
StaCd    LKOUT C,D#STATE,(D,,SREP_STC),CHAR,ND=Y                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AGENCY INFORMATION                             *         
***********************************************************************         
                                                                                
ARYAGYR  LKOUT A,(R,GETAGYR),MULTIROW=Y,ROWNAME=AGYRARYD                        
Media    LKOUT C,D#MEDCOD,(D,,AGY_MEDC),CHAR                                    
AgyName  LKOUT C,D#AGYNAM,(D,,AGY_NAME),CHAR                                    
AgyAddr  LKOUT C,D#AGYADR,(D,,AGY_ADDR),CHAR                                    
                                                                                
         LKOUT E                                                                
                                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MESSAGE REPLY                                  *         
***********************************************************************         
                                                                                
ARYMSGR  LKOUT A,(R,MSG_RPY),MULTIROW=Y,ROWNAME=MSGREPYD                        
MapCode  LKOUT C,D#ERRNUM,(D,,MSG_MAPC),UBIN                                    
MsgText  LKOUT C,D#ERRDSC,(D,,MSG_TEXT),CHAR                                    
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR EIO SETUP RECORD REPLY                         *         
***********************************************************************         
                                                                                
ARYESREC LKOUT A,(R,EIOSRPY),MULTIROW=Y,ROWNAME=EIOSRD                          
MediaCd  LKOUT C,D#MEDCOD,(D,,EIO_MEDC),CHAR                                    
ClientCd LKOUT C,D#CLTCOD,(D,,EIO_CLTC),CHAR                                    
UseEIO   LKOUT C,D#WEBIOS,(D,,EIO_UEIO),CHAR                                    
USEESR   LKOUT C,D#USEESR,(D,,EIO_UESR),CHAR,PCVERSION=3.2.0.3                  
EIOEst   LKOUT C,D#EIOEST,(D,,EIO_EIOE),CHAR,PCVERSION=3.5.0.4,ND=Y             
EIODAT   LKOUT C,D#EIOEDT,(D,,EIO_EIOD),BDAT,PCVERSION=3.5.0.4,ND=Y             
ESREst   LKOUT C,D#ESREST,(D,,EIO_ESRE),CHAR,PCVERSION=3.5.0.4,ND=Y             
ESRDAT   LKOUT C,D#ESREDT,(D,,EIO_ESRD),BDAT,PCVERSION=3.5.0.4,ND=Y             
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CFM CLIENT DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYCLT   LKOUT A,(R,NXTCLT),MULTIROW=Y,ROWNAME=PCLTKEY                          
Media    LKOUT C,1,(D,,PCLTKMED),CHAR,ND=Y                                      
CltCd    LKOUT C,2,(D,,PCLTKCLT),CHAR                                           
CltNm    LKOUT C,3,(D,,PCLTNAME),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CFM PRODUCT DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=PPRDKEY                          
Media    LKOUT C,1,(D,,PPRDKMED),CHAR,ND=Y                                      
CltCd    LKOUT C,2,(D,,PPRDKCLT),CHAR,ND=Y                                      
PrdCd    LKOUT C,3,(D,,PPRDKPRD),CHAR                                           
PrdNm    LKOUT C,4,(D,,PPRDNAME),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CFM VENDOR DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYVEN   LKOUT A,(R,NXTVEN),MULTIROW=Y,ROWNAME=PUBRECD                          
Array    LKOUT C,13,(A,ARYPUR)                                                  
Media    LKOUT C,1,(D,,PUBKMED),CHAR,ND=Y                                       
PubCd    LKOUT C,2,(D,B#SAVED,BUYPUB),CHAR                                      
PubNm    LKOUT C,3,(D,,PUBNAME),CHAR                                            
PubRC    LKOUT C,4,(D,,PUBPLSH),CHAR,ND=Y                                       
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CFM PUBLICATION REP DOWNLOAD                   *         
***********************************************************************         
                                                                                
ARYPUR   LKOUT A,(R,NXTPUR),MULTIROW=Y,ROWNAME=PREPRECD                         
Media    LKOUT C,1,(D,,PREPKMED),CHAR,ND=Y                                      
RepCd    LKOUT C,2,(D,,PREPKREP),CHAR                                           
RepNm    LKOUT C,3,(D,,PREPNAME),CHAR                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATS DOWNLOAD                                 *         
***********************************************************************         
                                                                                
ARYSTAT  LKOUT A,(R,NXTSTAT),MULTIROW=Y,ROWNAME=PPVALUES                        
MedCd    LKOUT C,1,(D,,PPMEDCDE),CHAR,ND=Y                                      
CltCd    LKOUT C,2,(D,,PPCLTCDE),CHAR,ND=Y                                      
CltNd    LKOUT C,3,(D,,PPCLTNDE),UBIN,ND=Y                                      
PubCd    LKOUT C,4,(D,,PPPUBCOD),(R,EDTPUB),LEN=20,ND=Y                         
VenNd    LKOUT C,5,(D,,PPVENNDE),UBIN,ND=Y                                      
PrdCd    LKOUT C,6,(D,,PPPRDCDE),CHAR,ND=Y                                      
PrdNd    LKOUT C,7,(D,,PPPRDNDE),UBIN,ND=Y                                      
InsDt    LKOUT C,8,(D,,PPDATPER),BDAT,ND=Y                                      
PubNm    LKOUT C,9,(D,,PPPUBNAM),CHAR,ND=Y,PCVERSION=1.1.0.27                   
SpcDs    LKOUT C,11,(D,,PPSPACE),CHAR,ND=Y                                      
EstNo    LKOUT C,17,(D,,PPESTNUM),UBIN,ND=Y                                     
EstNm    LKOUT C,18,(D,,PPESTNAM),CHAR,ND=Y                                     
InsNo    LKOUT C,13,(D,,PPBUYS),UBIN,ND=Y                                       
ZInsN    LKOUT C,14,(D,,PPZBUYS),UBIN,ND=Y                                      
NetAm    LKOUT C,15,(D,,PPNETC),SPAK                                            
GrsAm    LKOUT C,16,(D,,PPGROSSC),SPAK                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION HISTORY DOWNLOAD                     *         
***********************************************************************         
                                                                                
ARYINSHS LKOUT A,(R,GETINSHS),MULTIROW=Y                                        
                                                                                
AcTyp    LKOUT C,D#ACTTYP,(D,B#INSHIS,ICATYPE),CHAR                             
AcDat    LKOUT C,D#ACTDAT,(D,B#INSHIS,ICADATE),CDAT,ND=Y                        
AcWho    LKOUT C,D#ACTUSR,(D,B#INSHIS,ICAWHOM),CHAR,ND=Y                        
AcAct    LKOUT C,D#ACTACT,(D,B#INSHIS,ICAWHAT),CHAR,ND=Y                        
AcAct    LKOUT C,D#ACTRAT,(D,B#INSHIS,ICAPVRT),CHAR,ND=Y,              +        
               PCVERSION=4.2.0.22                                               
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
                                                                                
         LKARY T                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Open Rate (COS2 $) element                                          *         
***********************************************************************         
                                                                                
BUYORC2$ NTR1  BASE=*,LABEL=*                                                   
         USING PBUYRECD,R2                                                      
         USING PORELEM,R3                                                       
         CLI   PORELMCD,PORELMEQ   Open Rate element?                           
         JNE   EXITN                                                            
         XC    BUYCOS2$,BUYCOS2$                                                
         TM    PORCOSS1,PORCOS$Q   COS2 $ non-financial?                        
         JZ    EXITY                                                            
         CLI   PORC$TYP,PORC$NEQ   Entered as Net?                              
         JE    B_C2$30                                                          
         EDITR PORCOS,BUYCOS2$,2,ALIGN=LEFT,FLOAT=-,IZERO=Y                     
         J     B_C2$50                                                          
                                                                                
B_C2$30  MVC   BUYCOS2$(L'PORC$TYP),PORC$TYP                                    
         ZAP   DUB,PORCOS                                                       
         CVB   R1,DUB                                                           
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         JNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDITR (R1),(10,BUYCOS2$+1),2,ALIGN=LEFT,FLOAT=-,IZERO=Y                
                                                                                
B_C2$50  LA    RF,PVALUESX-PVALUES                                              
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ELEM2(0),PVALUES                                                 
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR VGETINS,DMCB,PBUYRECD,(C'O',PVALUES),PBUYKPRD,0,0,0              
         L     R1,GROSS                                                         
         STCM  R1,15,BUYC2GRS      SET COS2 FACTOR GROSS AMOUNT                 
         S     R1,AGYCOM                                                        
         STCM  R1,15,BUYC2NET      SET COS2 FACTOR NET AMOUNT                   
                                                                                
         LA    RF,PVALUESX-PVALUES                                              
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   PVALUES(0),ELEM2    RESTORE PVALUES                              
         EX    RF,0(RE)                                                         
                                                                                
         J     EXITY                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R3,R2                                                         
                                                                                
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
                                                                                
ACTVWHAT DS    0CL30               ** ACTIVITY PHRASES **                       
         DSDDL                                                                  
ACTV#    EQU   (PP@ADD-ACTVWHAT)/L'ACTVWHAT                                     
ACTVTYPE DS    0CL8                DEFINITION FOR ACTION WORDS ETC.             
                                                                                
VMASTC   DS    A                                                                
VSYSFACS DS    A                                                                
VBUFFRIN DS    A                                                                
VMINIO   DS    A                                                                
VPUBVAL  DS    A                                                                
VPUBEDIT DS    A                                                                
VGETINS  DS    A                                                                
VPSTVAL  DS    A                                                                
VPPGETAD DS    A                                                                
                                                                                
WVALUES  DS    0D                  ** LITERAL VALUES **                         
                                                                                
OADCONS  DS    0A                  ** RELOCATED ADCONS **                       
VPPBYOUT DS    V                                                                
VFMTINO  DS    V                                                                
VPPBVAL  DS    V                                                                
OADCONSN EQU   (*-OADCONS)/L'OADCONS                                            
                                                                                
LINRNG   DS    XL2                                                              
CLTRNG   DS    0XL6                                                             
PRDRNG   DS    XL6                                                              
PUBRNG   DS    XL12                                                             
SAVEKEY  DS    XL(L'IOKEY)                                                      
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
REQVALS  DS    0X                  ** REQUEST VALUES **                         
                                                                                
AGY      DS    CL2                 AGENCY CODE                                  
                                                                                
MEDIND   DS    X                   MEDIA REQUEST VALUES                         
AMED     DS    AL3                                                              
                                                                                
CLTIND   DS    X                   CLIENT REQUEST VALUES                        
ACLT     DS    AL3                                                              
                                                                                
PRDIND   DS    X                   PRODUCT REQUEST VALUES                       
APRD     DS    AL3                                                              
                                                                                
ESTIND   DS    X                   ESTIMATE REQUEST VALUES                      
AEST     DS    AL3                                                              
                                                                                
PUBIND   DS    X                   PUBLICATION REQUEST VALUES                   
APUB     DS    AL3                                                              
                                                                                
SERIND   DS    X                   SERIAL KEY REQUEST VALUES                    
ASER     DS    AL3                                                              
                                                                                
SMCIND   DS    X                   SYSTEM/MEDIA/CLIENT VALUES                   
ASMC     DS    AL3                                                              
                                                                                
WRKMEDCD DS    CL1                                                              
WRKCLTCD DS    CL3                                                              
WRKPUBCD DS    XL6                                                              
                                                                                
STRDATE  DS    XL(L'PBUYKDAT)      REQUEST START DATE                           
ENDDATE  DS    XL(L'PBUYKDAT)      REQUEST END DATE                             
STRDAT2  DS    XL(L'STRDATE)       2nd set of start and end date                
ENDDAT2  DS    XL(L'ENDDATE)                                                    
                                                                                
STRDATEE DS    CL6                 START DATE (EBCDIC)                          
ENDDATEE DS    CL6                 END DATE (EBCDIC)                            
ADVIND   DS    XL4                 ADVERTISER NODE                              
SUPNODE  DS    XL(L'CFMVENN)       SUPPLIER NODE                                
CALOPTN  DS    C                   CALENDAR OPTION                              
SPLOPTN  DS    C                   SPLIT OPTION                                 
SUMOPTN  DS    C                   SUMMARIZE BY... OPTION                       
PNTOPTN  DS    C                   PROGRAM NAME/TIME OPTION                     
ESTOPTN  DS    C                   ESTIMATE OPTION                              
ESNOPTN  DS    C                   ESTIMATE NAME OPTION                         
                                                                                
PUBCODE  DS    CL8                 PUBLICATION CODE                             
PUBZONE  DS    CL2                 ........... ZONE                             
PUBEDTN  DS    CL3                 ........... EDITION                          
                                                                                
PUBPGRP  DS    CL5                 PUBLICATION GROUP CODE                       
                                                                                
PUBPLST  DS    CL3                 PUBLICATION LIST CODE                        
                                                                                
FILTADNO DS    CL(L'PBDJOB)        AD NUMBER FILTER                             
FILTADID DS    CL(L'PJOBADID)      AD-ID FILTER                                 
FILTICNO DS    CL(L'PICCONN)       INTERNET CONTRACT NUMBER FILTER              
FILTDESC DS    CL(L'PBYOSPC1)      SPACE/DESCRIPTION FILTER                     
FILTSREP DS    CL(L'PBSREP)        SPECIAL REP FILTER                           
FILT#IHC DS    X                   # OF INVOICE HEADER COMMENTS                 
FILT#IDC DS    X                   # OF INVOICE DETAIL COMMENTS                 
                                                                                
FILTSTAT DS    X                   ** INSERTION STATUS FILTER **                
FILTSLUD EQU   X'01'               LIVE UNDELETED BUYS                          
FILTSTUD EQU   X'02'               TEST UNDELETED BUYS                          
FILTSLDE EQU   X'04'               LIVE DELETED BUYS                            
FILTSTDE EQU   X'08'               TEST DELETED BUYS                            
FILTSSUD EQU   X'10'               STEWARD UNDELETED BUYS                       
FILTSSDE EQU   X'20'               STEWARD DELETED BUYS                         
FILTLIVQ EQU   FILTSLUD+FILTSLDE                                                
FILTTSTQ EQU   FILTSTUD+FILTSTDE                                                
FILTSTWQ EQU   FILTSSUD+FILTSSDE                                                
FILTSALL EQU   FILTLIVQ+FILTTSTQ+FILTSTWQ                                       
                                                                                
FLTCLRST DS    X                   INSERTION CLEARANCE STATUS FILTER            
FLTCSCLR EQU   X'01'               CLEARED                                      
FLTCSNCL EQU   X'02'               NOT CLEARED                                  
FLTPAYBL EQU   X'04'               PAYABLE INSERTIONS                           
FLTNPYBL EQU   X'08'               NON-PAYABLE INSERTIONS                       
                                                                                
FLTMATST DS    X                   INSERTION MATCHING STATUS FILTER             
FLTMSPEN EQU   X'01'               PENDING                                      
FLTMSMAT EQU   X'02'               MATCHED                                      
FLTMSDIS EQU   X'04'               DISCREPANT                                   
FLTMSNOI EQU   X'08'               NO INVOICE ONLY                              
FLTMSINV EQU   X'10'               WITH INVOICE                                 
FLTMSPSI EQU   X'20'               PLUS NO INVOICE                              
                                                                                
FLTPBOPT DS    C                   BUY DOWNLOAD - PUB FILTERING OPTION          
FLTPOBAS EQU   C'B'                BASE PUB ONLY (NO ZONE AND EDITIONS)         
FLTPOZON EQU   C'Z'                PUB WITH ZONE AND/OR EDITIONS                
                                                                                
FLTDLDTT DS    X                   DOWNLOAD BY DATE - DATE TYPE                 
FLTMCLDQ EQU   PBYPMCLQ            MATERIAL CLOSING DATE                        
FLTSCLDQ EQU   PBYPCLSQ            SPACE CLOSING DATE                           
FLTPYBDQ EQU   PBYPBLBQ            BILLABLE DATE                                
FLTBILDQ EQU   PBYPPAYQ            PAYABLE DATE                                 
FLTDLDTR DS    0XL(L'PBYPDTE*2)                                                 
FLTDLDTS DS    XL(L'PBYPDTE)       DOWNLOAD BY DATE - START DATE                
FLTDLDTE DS    XL(L'PBYPDTE)       DOWNLOAD BY DATE - END DATE                  
                                                                                
FLTINFLT DS    X                   INSERTION FILTERING STATUS                   
FLTNOMSD EQU   X'01'               NO MAT CLOSING & SPACE CLOSING DATE          
FLTCANAD EQU   X'02'               CANADIAN PUBS ONLY                           
FLTUSAPB EQU   X'04'               USA PUBS ONLY                                
                                                                                
FLTBYORG DS    X                   BUY ORIGIN FILTER                            
FLTPPAKQ EQU   X'01'               PRINTPAK                                     
FLTSFMCQ EQU   X'02'               SFM BUY COPY                                 
FLTSFMMQ EQU   X'04'               SFM BUY MOVE                                 
FLTADBYQ EQU   X'08'               ADBUYER                                      
FLTIDSKQ EQU   X'10'               IDESK                                        
FLT_PBUQ EQU   X'20'               PBU                                          
FLT_IMPQ EQU   X'40'               AdBuyer imports                              
                                                                                
PUBMED#  DS    AL2                 N'ENTRIES IN PUBMED LIST                     
PUBMED   DS    16CL(L'PUBKMED)     LIST OF VENDOR MEDIA CODES                   
                                                                                
INSDLSW  DS    C                   INSERTION DOWNLOAD SWITCH                    
INVDLSW  DS    C                   INVOICE DOWNLOAD SWITCH                      
                                                                                
DL_TYPE  DS    C                   DOWNLOAD TYPE                                
DL_NULQ  EQU   0                                                                
DL_DRRQ  EQU   C'D'                DISCREPANCY RESOLUTION REPORT                
DL_PRBQ  EQU   C'P'                PROBE DOWNLOAD                               
DL_IMRQ  EQU   C'R'                INVOICE MATCH REPORT (DRR2)                  
DL_MSGQ  EQU   C'!'                MESSAGE REPLY                                
                                                                                
DL_FLAG1 DS    X                   DOWNLOAD FLAG                                
PIDIHSTQ EQU   X'80'               PID FOR INSERTION HISTORY DOWNLOAD           
IVIRPLYQ EQU   X'40'               INVOICE ITEMS FROM INSERTIONS (DRR)          
GETPIDOQ EQU   X'20'               GET PID ONLY                                 
FLTPAYEQ EQU   X'10'               FILTERING ON PAYEE CODE                      
SKIPPYEQ EQU   X'08'               FAILED PAYEE FILTER, SKIP RECORD             
                                                                                
INVDLMOD DS    X                   INVOICE DOWNLOAD MODE SWITCH                 
IVDLMD_# EQU   1                   DOWNLOAD BY INVOICE NUMBER                   
IVDLMD_D EQU   2                   DOWNLOAD BY START AND END DATES              
IVDLMD_B EQU   3                   DOWNLOAD BY BUY (INSERTION) RECORDS          
IVDLMD_K EQU   4                   DOWNLOAD BY INVOICE KEY(S)                   
IVDLMD_C EQU   5                   Download by invoice creation dates           
                                                                                
INVCMSW  DS    X                   INVOICE COMMENT SWITCH (HDR OR DET)          
*                                  PNVCKDTQ = DETAIL COMMENT                    
                                                                                
INVMATST DS    CL3                 INVOICE MATCHING STATUS FILTER               
INVSURCE DS    CL5                 Invoice source filter                        
                                                                                
DL_VINV# DS    C                   Download vendor invoice# from Pay?           
*                                  Y=Yes, N=No (default)                        
                                                                                
PROFNAME DS    CL3                 NAME OF PROFILE                              
                                                                                
T_INSCNT DS    XL4                 TOTAL # OF INSERTIONS COUNTER                
T_INVCNT DS    XL4                 TOTAL # OF INVOICES COUNTER                  
T_IVDCNT DS    XL4                 TOTAL # OF INVOICE DETAILS COUNTER           
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
TKEYSV_I DS    XL(IBUKEYL)         TSAR RECORD KEY SAVE AREA (INVOICE)          
                                                                                
AINVVAL  DS    A                   A(INVOICE VALUES)                            
ANXTSER  DS    A                   A(NEXT SERIAL NUMBER IN POOL)                
NSER     DS    H                   N'SERIAL NUMBERS LEFT TO PROCESS             
                                                                                
ELCODE   DS    X                   GENERAL ELELMENT CODE                        
FLAG     DS    X                   GENERAL FLAG BYTE                            
READDELS DS    X                   READ DELETED BUYS FLAG                       
SVPUBKEY DS    0XL(L'PUBKEY)       SAVED PUBLICATION KEY                        
SVCLTKEY DS    XL(L'PCLTKEY)       SAVED CLIENT KEY                             
                                                                                
SVMSGMAP DS    XL2                 MESSAGE MAP CODE                             
SVMSGNUM DS    XL2                 MESSAGE TEXT                                 
                                                                                
PROFILEV DS    CL16                PROFILE VALUE                                
                                                                                
LASTS    DS    0F                  ** LAST TIME VALUES **                       
LASTREP  DS    H                   LAST REP# PROCESSED                          
LASTMED  DS    CL(L'PBUYKMED)                                                   
LASTCLT  DS    CL(L'PBUYKCLT)                                                   
LASTJOB  DS    XL(PJOBKPUB-PJOBKEY)                                             
LASTCAP  DS    CL(L'PJOBCAP1+L'PJOBCAP2+1)                                      
LASTADID DS    CL(L'PJOBADID)                                                   
LASTSUBA DS    CL(L'PJOBADID*10+10)                                             
LASTL    EQU   *-LASTS                                                          
                                                                                
CLTPROFS DS    0C                  ** CLIENT BILLING PROFILES **                
CLTPRB1  DS    CL16                B1  PROFILE                                  
CLTPRB1X DS    CL16                B1X PROFILE                                  
CLTPRBY  DS    CL16                BY  PROFILE                                  
CLTPROFL EQU   *-CLTPROFS                                                       
                                                                                
BUYVALS  DS    0X                  ** EXTRACTED BUY RECORD VALUES **            
                                                                                
BUYABILE DS    AL4                 A(LAST BILL ELEMENT)                         
BUYAPAYE DS    AL4                 A(LAST PAY ELEMENT)                          
BUYAIORE DS    AL4                 A(LAST INSERTION ORDER ELEMENT)              
BUYAWIOE DS    AL4                 A(LAST WEB INSERTION ORDER ELEM)             
BUYAESRE DS    AL4                 A(LAST ESR ELEM)                             
                                                                                
BUYPUB   DS    CL15                PUBLICATION CODE                             
                                                                                
BUYSER   DS    0C                  ** BUY SERIAL NUMBER **                      
BUYSMED  DS    CL(L'PBUYKMED)      MEDIA                                        
BUYSCLT  DS    CL(L'PBUYKCLT)      CLIENT                                       
BUYSSER  DS    CL9                 SERIAL NUMBER                                
BUYSERSL EQU   *-BUYSER            L'SHORT BUY SERIAL                           
         ORG   BUYSSER                                                          
BUYSPRD  DS    CL(L'PBUYKPRD)      OR KEY                                       
BUYSPUB  DS    CL((PBUYKACT-PBUYKPUB)*2)                                        
BUYSLIN  DS    XL((L'PBUYKLIN)*2)                                               
BUYSERLL EQU   *-BUYSER            L'LONG BUY SERIAL                            
                                                                                
BUYLINE  DS    CL11                INSERTION DATE WITH LINE NUMBER              
BUYTYPE  DS    CL13                INSERTION TYPE                               
BUYALLOC DS    CL94                ALLOCATION LINE                              
                                                                                
BUYRTCOD DS    CL(L'PBDRCODE)                                                   
BUYCDPCT DS    XL(L'PBDCD)         CASH DISCOUNT PERCENT                        
BUYACP   DS    PL(L'PBDACP+1)      AGENCY COMMISSION PERCENT                    
BUYREGDS DS    PL(L'PBDREG)        NUMBER OF REGULAR DISPLAYS                   
BUYILLPA DS    PL(L'PBDILLUM)      NUMBER OF ILLUMINATED DISPLAYS               
BUYSHOWS DS    CL5                 NUMBER OF SHOWINGS                           
BUYSFH   DS    CL4                 SPECIAL FINANCIAL HANDLING                   
BUYSHPDT DS    XL(L'PBSHDATE)      SHIP DATE                                    
BUYMCLXD DS    XL(L'PEXDATE)       MATERIALS CLOSING EXTENSION DATE             
BUYMCXDY DS    XL(L'PEXDAYS)       MATERIALS CLOSING EXTENSION DAYS             
BUYSREP  DS    CL(L'PBSREP)        SPECIAL REP                                  
BUYMCHDT DS    XL(L'PBINVMDT)      MATCHED DATE                                 
BUYMCHIN DS    CL(L'PBINVNUM)      MATCHED INVOICE NUMBER                       
BUYPREP  DS    CL(L'ACTVTYPE)      PAYEE                                        
BUYPAYDT DS    XL(L'PPDDATE)       PAID DATE                                    
BUYPAYSQ DS    XL(L'PPDSEQNO)      PAYMENT SEQUENCE NUMBER                      
BUYBINVN DS    CL10                BILLING INVOICE NUMBER                       
BUYBINVD DS    XL(L'PBLDATE)       BILLING DATE                                 
BUYREFNO DS    CL(L'PBREFNO)       REFERENCE NUMBER                             
BUYCHKNO DS    CL(L'PPCLCHK)       CHECK NUMBER                                 
BUYCHKDT DS    CL(L'PPCLCHDT)      CHECK DATE                                   
BUYCLE   DS    CL12                CONTRACT LINEAGE EQUIVALENCY                 
BUYSPACE DS    CL(L'PBDSPACE)      SPACE DESCRIPTION                            
BUYRATE  DS    CL11                RATE                                         
BUYPREM  DS    CL11                PREMIUM                                      
BUYPST   DS    CL4                 PST (EG - PQ=S)                              
BUYPLCOS DS    CL12                PLANNED COST                                 
BUYPCNET DS    XL4                 PLANNED COST NET AMOUNT                      
BUYPCGRS DS    XL4                 PLANNED COST GROSS AMOUNT                    
                                                                                
BUYBSTAT DS    X                   ** BUY STATUS **                             
BUYBSLIV EQU   X'01'               LIVE BUY                                     
BUYBSTST EQU   X'02'               TEST BUY                                     
BUYBSDEL EQU   X'04'               DELETED BUY                                  
BUYBSNFD EQU   X'08'               BUY WAS NOT FOUND (REFRESH ONLY)             
BUYBSTEW EQU   X'10'               STEWARDSHIP INSERTION                        
                                                                                
BUYSTAT2 DS    X                   ** BUY STATUS 2 **                           
BYS2PYBL EQU   FLTPAYBL            PAYABLE INSERTIONS                           
BYS2NPYB EQU   FLTNPYBL            NON-PAYABLE INSERTION                        
                                                                                
BUYSTAT3 DS    X                   ** BUY STATUS 3 **                           
BYS3ACHQ EQU   X'01'               INSERTION HAS ADDITIONAL CHARGES             
                                                                                
BUYORGIN DS    X                   BUY ORIGIN (SEE PPIDPRG)                     
                                                                                
BUYTAPPR DS    C                   TEARSHEET APPROVED                           
BUYPAGEN DS    CL(L'PTSHPAGE)      PAGE NOTATION                                
BUYTSTAT DS    CL5                 TEARSHEET STATUS                             
BUYFSINS DS    CL10                NUMBER OF FREE STANDING INSERTS              
BUYREPRO DS    XL(L'PTSHREPO)      REPRODUCTION QUALITY                         
                                                                                
BUYORDDT DS    XL(L'PIODATE)       INSERTION ORDER DATE                         
BUYORDNO DS    CL21                INSERTION ORDER NUMBER (WITH TYPE)           
BUYIOSTA DS    XL(L'PWIOSTAT)      INSERTION ORDER STATUS                       
BIOSESTQ EQU   X'01'               EIO BY ESTIMATE                              
BIOSEPRQ EQU   X'02'               EIO BY ESTIMATE PERIOD                       
                                                                                
BUYESRDT DS    XL(L'PESRDATE)      LAST ENHANCED SPACE RESERVATION DATE         
BUYESRNO DS    CL(ESRLGKYL)        LAST EHHANCED SPACE RESERVATION #            
BUYSRSTA DS    XL(L'PESRSTAT)      SPACE RESERVATION STATUS                     
BSRSESTQ EQU   X'01'               ESR BY ESTIMATE                              
BSRSEPRQ EQU   X'02'               ESR BY ESTIMATE PERIOD                       
                                                                                
BUYVIEWS DS    PL(L'PPAGEVS)       NUMBER OF PAGE VIEWS                         
BUYCLICK DS    PL(L'PCLCKTS)       CLICK THRUS                                  
BUYDECIR DS    PL(L'PBDEC)         DAILY EFFECTIVE CIRCULATION                  
BUYREPTS DS    PL(L'PBRPTNO)       NUMBER OF FREE STANDING INSERTS              
BUYEIMPS DS    PL(L'PIMPRS)        NUMBER OF ESTIMATED IMPRESSIONS              
BUYAIMPS DS    PL(L'PAIMPRS)       NUMBER OF ACTUAL IMPRESSIONS                 
BUYECPM  DS    PL(L'PECPM)         ESTIMATED CPM                                
BUYACPM  DS    PL(L'PACPM)         ACTUAL CPM                                   
BUYS#FRO DS    PL(L'PBYMSER#)      BUY SERIAL# - "FROM" INSERTION               
BUYS#_TO DS    PL(L'PBYMSER#)      BUY SERIAL# - "TO" INSERTION                 
                                                                                
BUYQIPID DS    CL(L'SAPALPID)      LATEST REQUEST INVOICE PID                   
BUYQIDAT DS    XL(L'PCHGDAT)       LATEST RECEIVE INVOICE DATE                  
BUYRIPID DS    CL(L'SAPALPID)      LATEST REQUEST INVOICE PID                   
BUYRIDAT DS    XL(L'PCHGDAT)       LATEST RECEIVE INVOICE DATE                  
BUYPOSQ# DS    XL(L'PBYPOSQ#)      PUCHASE ORDER SEQUENCE NUMBER                
                                                                                
BUYWSJ   DS    C                   C'Y' IF WSJ BUY                              
                                                                                
BUYISITE DS    XL(L'PISITE)        INTERNET SITE LOCATION                       
BUYICNUM DS    XL(L'PICCONN)       INTERNET CONTRACT NUMBER                     
BUYICIMP DS    PL(L'PICIMPS)       TOTAL IMPRESSIONS                            
BUYICCPM DS    PL(L'PICCPM)        TOTAL CPM                                    
BUYICRAT DS    CL11                TOTAL RATE (N (NET) MAY PRECEDE)             
BUYICINS DS    PL(L'PICINS)        NUMBER OF INSERTIONS                         
                                                                                
BUYGRSAC DS    XL(L'GROSS)         GROSS ADDITIONAL CHARGES                     
BUYGLCAC DS    XL(L'BLABLE)        BILLABLE ADDITIONAL CHARGES                  
BUYNLCAC DS    XL(L'PYABLE)        PAYABLE ADDITIONAL CHARGES                   
BUYNETAC DS    XL4                 NET ADDITIONAL CHARGES                       
                                                                                
BUYNETOR DS    XL4                 NET ORDERED                                  
BUYNETPD DS    XL4                 NET PAID                                     
BUYPLNCD DS    XL4                 PAID NET OF CASH DISCOUNT                    
BUYNETBD DS    XL4                 NET BILLED                                   
BUYBLNCD DS    XL4                 BILLED NET OF CASH DISCOUNT                  
                                                                                
BUYNPYBL DS    XL4                 NET PAYABLE (CALC'D FROM PAY ELEMS)          
BUYGPYBL DS    XL4                 GRS PAYABLE (CALC'D FROM PAY ELEMS)          
                                                                                
BUYCOS2F DS    XL(2*L'PCOS2FAC)    COS2 FACTOR                                  
BUYC2GRS DS    XL4                 COS2 FACTOR GROSS AMOUNT                     
BUYC2NET DS    XL4                 COS2 FACTOR NET AMOUNT                       
BUYCOS2$ DS    CL11                COS2 $ Rate                                  
                                                                                
BYC2GRSB DS    XL4                 COS2 gross billed                            
BYC2NETB DS    XL4                 COS2 net billed                              
BYC2GCDB DS    XL4                 COS2 gross less CD billed                    
BYC2NCDB DS    XL4                 COS2 net less CD billed                      
                                                                                
REGCLNQ  EQU   47                  MAX W'COMMENTS-REGULAR                       
IOCMLNQ  EQU   44                  MAX W'COMMENTS-INSERTION ORDER               
PICMLNQ  EQU   44                  MAX W'COMMENTS-POSITION INSTRUCTION          
TSCMLNQ  EQU   66                  MAX W'COMMENTS-TEARSHEET                     
SRCMLNQ  EQU   40                  MAX W'COMMENTS-SPECIAL REMITTANCE            
COMMAXQ  EQU   5                   MAX N'COMMENTS                               
TSCMAXQ  EQU   4                   MAX N'COMMENTS-TEARSHEET                     
                                                                                
BUYREGC# DS    AL1                 N'REGULAR COMMENT LINES                      
BUYREGC1 DS    CL(REGCLNQ)         REGULAR COMMENT LINE 1                       
BUYREGC2 DS    CL(REGCLNQ)         .....................2                       
BUYREGC3 DS    CL(REGCLNQ)         .....................3                       
BUYREGC4 DS    CL(REGCLNQ)         .....................4                       
BUYREGC5 DS    CL(REGCLNQ)         .....................5                       
                                                                                
BUYINSC# DS    AL1                 N'INSERTION ORDER COMMENT LINES              
BUYINSC1 DS    CL(IOCMLNQ)         INSERTION ORDER COMMENT LINE 1               
BUYINSC2 DS    CL(IOCMLNQ)         .............................2               
BUYINSC3 DS    CL(IOCMLNQ)         .............................3               
BUYINSC4 DS    CL(IOCMLNQ)         .............................4               
BUYINSC5 DS    CL(IOCMLNQ)         .............................5               
                                                                                
BUYPOSI# DS    AL1                 N'POSITION INSTRUCTION LINES                 
BUYPOSI1 DS    CL(PICMLNQ)         POSITION INSTRUCTION LINE 1                  
BUYPOSI2 DS    CL(PICMLNQ)         ..........................2                  
BUYPOSI3 DS    CL(PICMLNQ)         ..........................3                  
BUYPOSI4 DS    CL(PICMLNQ)         ..........................4                  
BUYPOSI5 DS    CL(PICMLNQ)         ..........................5                  
                                                                                
BUYTSHC# DS    AL1                 N'TEARSHEET COMMENT LINES                    
BUYTSHC1 DS    CL(TSCMLNQ)         TEARSHEET COMMENT LINE 1                     
BUYTSHC2 DS    CL(TSCMLNQ)         .......................2                     
BUYTSHC3 DS    CL(TSCMLNQ)         .......................3                     
BUYTSHC4 DS    CL(TSCMLNQ)         .......................4                     
                                                                                
BUYSRCM# DS    AL1                 N'SPECIAL REMITTANCE COMMENT LINES           
BUYSRCM1 DS    CL(SRCMLNQ)         SPECIAL REMITTANCE COMMENT LINE 1            
BUYSRCM2 DS    CL(SRCMLNQ)         ................................2            
BUYSRCM3 DS    CL(SRCMLNQ)         ................................3            
BUYSRCM4 DS    CL(SRCMLNQ)         ................................4            
BUYSRCM5 DS    CL(SRCMLNQ)         ................................5            
                                                                                
BUYISSNM DS    CL(L'PISNAME)       ISSUE NAME                                   
BUYUPLID DS    CL15                UPLOAD ID FROM PBU UPLOADS                   
                                                                                
BUYCLRST DS    CL1                 CLEARANCE STATUS                             
BUYTSRST DS    CL1                 TEARSHEET RECEIVED                           
BUYMATST DS    CL(L'PBMTSTAT)      INVOICE MATCHING STATUS                      
BUYDISST DS    CL(L'PBMTDSTA)      INVOICE MATCHING DISCREPANCY STATUS          
                                                                                
BUYATAB# DS    AL2                 NUMBER OF ADDITIONAL CHARGES                 
                                                                                
BUYATAB  DS    0X                  ** ADDITIONAL CHARGE TABLE **                
BUYATABM EQU   10                  MAXIMUM N'ADDITIONAL CHARGES                 
BUYATCOD DS    CL(L'PACCODE)       ADDITIONAL CHARGE CODE                       
BUYATCRG DS    CL12                ADDITIONAL CHARGE                            
BUYATCOM DS    CL(L'PACAC)         SUBJECT TO COMMISSION?                       
BUYATCPC DS    XL(L'PACACOM)       COMMISSION%                                  
BUYATSCD DS    CL(L'PACCD)         SUBJECT TO CASH DISCOUNT?                    
BUYATSTA DS    X                   ADDITIONAL CHARGE STATUS                     
BUYATBLQ EQU   X'01'               CHARGE HAS BEEN BILLED                       
BUYATPYQ EQU   X'02'               CHARGE HAS BEEN PAID                         
BUYATABL EQU   *-BUYATAB                                                        
         DS    (BUYATABM-1)XL(BUYATABL)                                         
                                                                                
BUYFOUND DS    C                   BUY FOUND FLAG                               
                                                                                
I#       DS    0X                  ** INSERTION HISTORY COUNTERS **             
IOA#     DS    AL2                 NUMBER OF INSERTION ORDERS                   
IBA#     DS    AL2                 NUMBER OF INSERTION BILLINGS                 
IPA#     DS    AL2                 NUMBER OF INSERTION PAYINGS                  
I#L      EQU   *-I#                                                             
                                                                                
BUYADDDT DS    XL(L'PBDBUYDT)      DATE BUY ADDED                               
                                                                                
FX_VAL_S DS    0X                                                               
FX_RATE_ DS    CL8                 FOREIGN EXCHANGE RATE                        
                                                                                
FXAC_AMT DS    XL4                 FX AGENCY AMOUNT                             
FXCD_AMT DS    XL4                 FX CASH DISCOUNT AMOUNT                      
FXNETORD DS    XL4                 FX NET ORDERED                               
FXGRSORD DS    XL4                 FX GROSS ORDERED                             
FXNETPYB DS    XL4                 FX NET PAYABLE                               
FXGRSPYB DS    XL4                 FX GROSS PAYABLE                             
FXNET_PD DS    XL4                 FX NET PAID                                  
FXGRS_PD DS    XL4                 FX GROSS PAID                                
FXNLCDPD DS    XL4                 FX NET LESS CD PAID                          
FXGLCDPD DS    XL4                 FX GROSS LESS CD PAID                        
FXNLCDOR DS    XL4                 FX NET LESS CD ORDERED                       
FXGLCDOR DS    XL4                 FX GROSS LESS CD ORDERED                     
                                                                                
S_NETORD DS    XL4                 NET ORDERED + FX NET ORDERED                 
S_GRSORD DS    XL4                 GROSS ORDERED + FX GROSS ORDERED             
S_NETPYB DS    XL4                 NET PAYABLE + FX NET PAYABLE                 
S_GRSPYB DS    XL4                 GROSS PAYABLE + FX GROSS PAYABLE             
S_NET_PD DS    XL4                 NET PAID + FX NET PAID                       
S_GRS_PD DS    XL4                 GROSS PAID + FX GROSS PAID                   
S_NLCDPD DS    XL4                 NET LESS CD PAID + FX NLCD PAID              
S_GLCDPD DS    XL4                 GROSS LESS CD PAID + FX GLCD PAID            
S_NLCDOR DS    XL4                 NET LESS CD ORDERED + FX NLCD ORD'D          
S_GLCDOR DS    XL4                 GROSS LESS CD ORD'D + FX GLCD ORD'D          
FX_VAL_E DS    0X                                                               
FX_VALNQ EQU   FX_VAL_E-FX_VAL_S                                                
                                                                                
* PVALUES                                                                       
         PRINT OFF                                                              
         DS    0D                                                               
       ++INCLUDE PVALUES           PARAMETER BLOCK FOR GETINS                   
         PRINT ON                                                               
                                                                                
BUYVALSL EQU   *-BUYVALS                                                        
                                                                                
PBUREC   DS    XL(PBUL)            PUBLICATION RECORD                           
RBUREC   DS    XL(RBUL)            PUBLISHER/REP RECORD                         
IBUREC   DS    XL(IBUL)            INVOICE RECORD                               
                                                                                
         DS    0D                                                               
WORKAREA DS    XL650                                                            
                                                                                
INVWKKEY DS    XL(L'PNVKEY)        INVOICE WORKING KEY                          
INVWKSEQ DS    XL(L'PNVDKSQN)      INVOICE DETAIL  WORKING SEQ NUMBER           
INVELMKY DS    XL(L'PNVHKEY)       INVOICE ELEMENT NAVIGATION KEY               
MINPARMS DS    6F                  PARAMETER LIST                               
MINMAST  DS    XL32                CURRENT MINIO MASTER KEY                     
MINBLOCK DS    XL(MINBLKL)         MINIO CONTROL BLOCK                          
                                                                                
         ORG   BUYVALS                                                          
CFMIOC   DS    XL(CFMIOL)          CFMIO CONTROL BLOCK                          
       ++INCLUDE GESTATPRT                                                      
         ORG                                                                    
                                                                                
SAVEL    EQU   *-SAVED                                                          
SAVEFREE EQU   (8*ONEK)-SAVEL                                                   
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DDPSTBLK                                                       
                                                                                
PPBVALDD DSECT                                                                  
       ++INCLUDE PPBVALD                                                        
                                                                                
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
                                                                                
       ++INCLUDE PPLNKWRK                                                       
       ++INCLUDE DDBUFFD                                                        
                                                                                
PUBRECD  DSECT                                                                  
PUBKCODQ EQU   X'81'                                                            
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
       ++INCLUDE PUBREPEL                                                       
                                                                                
PREPRECD DSECT                                                                  
PREPKRCQ EQU   X'11'               PUBLISHER/REP RECORD CODE                    
       ++INCLUDE PREPREC                                                        
PPCLTYPQ EQU   X'25'               CLEARANCE STATUS RECORD CODE                 
PPCLELQ  EQU   X'01'               CLEARANCE STATUS ELEMENT CODE                
       ++INCLUDE PPCLRST                                                        
       ++INCLUDE PGENGRP                                                        
PLISRECD DSECT                                                                  
PLISKRCQ EQU   X'17'               PUBLICATION LIST RECORD CODE                 
       ++INCLUDE PLISREC                                                        
PLISFRST DS    0X                                                               
PLISPBD  DSECT                                                                  
PLISBELQ EQU   X'20'               PUBLICATION LIST ELEMENT                     
       ++INCLUDE PLISPBEL                                                       
PSPLRECD DSECT                                                                  
       ++INCLUDE PSPCGREC                                                       
PSPELQ   EQU   X'10'               ADDITIONAL CHARGE DEFINITION ELEMENT         
       ++INCLUDE PPGENPNV                                                       
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
PBUYKRCQ EQU   X'20'               BUY RECORD TYPE                              
PBUYFRST DS 0X                                                                  
       ++INCLUDE PBDELEM                                                        
PALCEL   DS    X                   ELEMENT CODE                                 
PALCELQ  EQU   X'21'               PRODUCT ALLOCATION ELEMENT                   
PALCLEN  DS    X                   ELEMENT LENGTH                               
PALCPRD  DS    CL3                 PRODUCT CODE                                 
PALCCSHR DS    X                   COST SHARE                                   
PALCSSHR DS    X                   SPACE SHARE                                  
PCHGEQ   EQU   X'24'               INSERTION CHANGE ELEMENT                     
       ++INCLUDE PCHGELEM                                                       
PPAYELQ  EQU   X'25'               PAY ELEMENT                                  
       ++INCLUDE PPAYELEM                                                       
PBILELQ  EQU   X'26'               BILL ELEMENT                                 
PORBELQ  EQU   X'28'               Open rate bill                               
       ++INCLUDE PBILELEM                                                       
PORCELQ  EQU   PORELMEQ            Open Rate element (COS2 $)                   
       ++INCLUDE PORELEM                                                        
PWSJEQ   EQU   X'35'               WSJ ELEMENT                                  
PWSJELD  DSECT                                                                  
       ++INCLUDE PWSJELEM                                                       
PACELQ   EQU   X'44'               ADDITIONAL CHARGE ELEMENT                    
       ++INCLUDE PACELEM                                                        
PBINVEQ  EQU   X'50'               INVOICE MATCHING ELEMENT                     
       ++INCLUDE PBINVELM                                                       
       ++INCLUDE PPGENPBNV         NEW INVOICE ELEM                             
PICONEQ  EQU   X'97'               INTERNET CONTRACT ELEMENT                    
       ++INCLUDE PICONEL                                                        
PRGCELQ  EQU   X'66'               REGULAR COMMENTS ELEMENT                     
PIOCELQ  EQU   X'67'               INSERTION ORDER COMMENTS ELEMENT             
PPOCELQ  EQU   X'68'               POSITION ORDER COMMENTS ELEMENT              
PTSHELQ  EQU   X'69'               TEARSHEET ORDER COMMENTS ELEMENT             
PSRCELQ  EQU   X'6A'               SPECIAL REMITTANCE COMMENTS ELEMENT          
PCOMEL   DS    X                   ** COMMENT ELEMENTS **                       
PCOMLEN  DS    X                   COMMENT ELEMENT LENGTH                       
PCOMLN1Q EQU   *-PCOMEL                                                         
PCOMMENT DS    0C                  COMMENT (VARIABLE LENGTH)                    
PIOELQ   EQU   X'70'               INSERTION ORDER ELEMENT                      
       ++INCLUDE PIOELEM                                                        
       ++INCLUDE PPGENBYIO         WEB INSERTION ORDER ELEM                     
PBREPEQ  EQU   X'80'               SPECIAL REP ELEMENT                          
       ++INCLUDE PBSREPEL                                                       
PBDECELQ EQU   X'81'               DAILY EFFECTIVE CIRULATION ELEMENT           
       ++INCLUDE PBDECEL                                                        
       ++INCLUDE PBFSIEL           FREE STANDING INSERTS ELEMENT                
PBREFELQ EQU   X'83'               REFERENCE NUMBER ELEMENT                     
       ++INCLUDE PBREFEL                                                        
PBYPSTEQ EQU   X'84'               PST ELEMENT                                  
       ++INCLUDE PBYPSTEL                                                       
PBRPTELQ EQU   X'85'               NUMBER OF REPAINTS ELEMENT                   
       ++INCLUDE PBRPTEL                                                        
PBSHPDEQ EQU   X'86'               SHIP DATE ELEMENT                            
       ++INCLUDE PBSHPDEL                                                       
PPAGEVQ  EQU   X'87'               PAGE VIEWS ELEMENT                           
       ++INCLUDE PPAGEVEL                                                       
PCLCKTEQ EQU   X'88'               CLICK THRUS ELEMENT                          
       ++INCLUDE PCLICKTEL                                                      
PEXDAYEQ EQU   X'89'               MATERIALS CLOSING EXTENSION DAYS             
       ++INCLUDE PEXDAYEL                                                       
PIUPELEQ EQU   X'90'               PBU INSERTION UPLOAD ELEM                    
       ++INCLUDE PIUPLELEM                                                      
PC2FACTQ EQU   X'91'               INSERTION COS2 FACTOR ELEM                   
       ++INCLUDE PCOS2FACEL                                                     
PIMPRSEQ EQU   X'92'               ESTIMATED IMPRESSIONS ELEMENT                
       ++INCLUDE PIMPRSEL                                                       
PAIMSPEQ EQU   X'93'               ACTUAL IMPRESSIONS ELEMENT                   
       ++INCLUDE PAIMPRSEL                                                      
       ++INCLUDE PTSHTEL           TEARSHEET ELEMENT                            
PEXDTELQ EQU   X'96'               MATERIALS CLOSING EXTENSION DATE             
       ++INCLUDE PEXDATEL                                                       
PISITELQ EQU   X'98'               INTERNET SITE LOCATION                       
       ++INCLUDE PISITEEL                                                       
PSEREQ   EQU   X'99'               SERIAL NUMBER ELEMENT                        
       ++INCLUDE PSERELEM                                                       
PECPMEQ  EQU   X'A0'               ESTIMATED CPM                                
       ++INCLUDE PECPMEL                                                        
PACPMEQ  EQU   X'A1'               ACTUAL CPM                                   
       ++INCLUDE PACPMEL                                                        
       ++INCLUDE PPPISNMEL         ISSUE NAME (EQUATE DEFINED IN BOOK)          
       ++INCLUDE PPGENPBMAT        INVOICE MATCHING STATUSES                    
       ++INCLUDE PPGENBYCC         INSERTION CUSTOM COLUMN ELEM                 
       ++INCLUDE PPPPIDEL          INSERTION PERSONAL ID ELEM                   
       ++INCLUDE PPGENBYSR         INSERTION EHHANCED SPACE RESERVATION         
       ++INCLUDE PPGENBYMV         INSERTION BUY MOVE ELEM                      
       ++INCLUDE PPGENBYPO         INSERTION PURCHASE ORDER ELEM                
       ++INCLUDE PPGENBYPC         INSERTION PLANNED COST ELEM                  
                                                                                
PJOBRECD DSECT                                                                  
PJOBKRCQ EQU   X'15'               JOB RECORD CODE                              
       ++INCLUDE PJOBREC                                                        
       ++INCLUDE PCOLREC           CUSTOM COLUMN RECORD                         
       ++INCLUDE PPGENSCH          EIO SETUP RECORD                             
       ++INCLUDE PPBYOUTD                                                       
PPBYOUTL EQU   *-PPBYOUTD                                                       
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE GECFMIOD                                                       
         PRINT ON                                                               
                                                                                
PSERRECD DSECT                     ** DSECT TO COVER SERIAL PASSIVE **          
PSERKEY  DS    0XL25                                                            
PSERKAGY DS    CL2                 AGENCY CODE                                  
PSERKMED DS    C                   MEDIA CODE                                   
PSERKTYP DS    X                   RECORD TYPE                                  
PSERKTYQ EQU   X'99'                                                            
PSERKCLT DS    CL3                 CLIENT CODE                                  
PSERKSER DS    CL5                 SERIAL NUMBER                                
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPGENBYPP         BUY PASSIVE KEYS                             
         PRINT ON                                                               
                                                                                
ACHTABD  DSECT                     ** ADDITIONAL CHARGES TABLE **               
ACHMED   DS    CL(L'PSPLKMED)      MEDIA CODE                                   
ACHCODE  DS    CL(L'PSPLCODE)      ADDITIONAL CHARGE RATE CODE                  
ACHTYPE  DS    CL(L'PSPLTYPE)      ADDITIONAL CHARGE TYPE INDICATOR             
ACHDESC  DS    CL(L'PSPLDESC)      ADDITIONAL CHARGE DESCRIPTION                
ACHTABL  EQU   *-ACHTABD                                                        
                                                                                
PBUD     DSECT                     ** PUBLICATION BUFFER RECORDS **             
PBUKEY   DS    0X                  ** RECORD KEY **                             
PBUMED   DS    CL(L'PUBKMED)       MEDIA CODE                                   
PBUPZE   DS    XL(LPUBKPZE)        PUBLICATION/ZONE/EDITION                     
PBUKEYL  EQU   *-PBUKEY                                                         
PBUDATA  DS    0X                  ** RECORD DATA **                            
PBUCODE  DS    CL15                PUBLICATION CODE                             
PBUREP   DS    CL(L'PUBPLSH)       PUBLISHER/REP                                
PBUNAME  DS    CL(L'PGADNAME)      1ST PUBLICATION NAME                         
PBUZNAME DS    CL(L'PUBZNAME)      1ST PUBLICATION ZONE NAME                    
PBUCITY  DS    CL(L'PUBCITY)       1ST PUBLICATION CITY                         
PBUSTATE DS    CL(L'PUBSTATE)      1ST PUBLICATION STATE                        
PBUADDL1 DS    CL(L'PUBLINE1)      1ST ADDRESS LINE 1                           
PBUADDL2 DS    CL(L'PUBLINE2)      1ST ADDRESS LINE 2                           
PBUADDL3 DS    CL(L'PUBLINE3)      1ST ADDRESS LINE 3                           
PBUZIPCD DS    CL(L'PUBNWZIP)      1ST ZIP CODE                                 
PBUPAYCD DS    CL(L'PUBPAREP)      PAYEE CODE (PAYING REP)                      
PBUNSTCD DS    CL(L'PUBSTACD)      NUMERIC STATE CODE                           
                                                                                
PBUNAME2 DS    CL(L'PGADNAME)      2ND PUBLICATION NAME                         
PBUZNAM2 DS    CL(L'PUBZNAME)      2ND PUBLICATION ZONE NAME                    
PBUCITY2 DS    CL(L'PUBCITY)       2ND PUBLICATION CITY                         
PBUSTAT2 DS    CL(L'PUBSTATE)      2ND PUBLICATION STATE                        
PBUAL1_2 DS    CL(L'PUBLINE1)      2ND ADDRESS LINE 1                           
PBUAL2_3 DS    CL(L'PUBLINE2)      2ND ADDRESS LINE 2                           
PBUAL3_2 DS    CL(L'PUBLINE3)      2ND ADDRESS LINE 3                           
PBUZIPC2 DS    CL(L'PUBNWZIP)      2ND ZIP CODE                                 
PBUDATAL EQU   *-PBUDATA                                                        
PBUL     EQU   *-PBUD                                                           
                                                                                
RBUD     DSECT                     ** PUBLISHER/REP BUFFER RECORDS **           
RBUKEY   DS    0X                  ** RECORD KEY **                             
RBUMED   DS    CL(L'PREPKMED)      MEDIA CODE                                   
RBUREP   DS    XL(L'PREPKREP)      PUBLISHER/REP CODE                           
RBUKEYL  EQU   *-RBUKEY                                                         
RBUDATA  DS    0X                  ** RECORD DATA **                            
RBUNAME  DS    CL(L'PREPNAME)      PUBLISHER/REP NAME                           
RBUDATAL EQU   *-RBUDATA                                                        
RBUL     EQU   *-RBUD                                                           
                                                                                
IBUD     DSECT                     ** INVOICE BUFFER RECORD **                  
IBUKEY   DS    0X                  ** RECORD KEY **                             
IBUMEDCD DS    CL(L'PNVKMED)       MEDIA                                        
IBUINVS# DS    CL(L'PNVKSER#)      INVOICE SERIAL#                              
IBUIVDS# DS    CL(L'PNVDKSQN)      INVOICE DETAIL SEQUENCE#                     
IBUINSKY DS    CL(BUYSERSL)        INSERTION KEY (MEDIA/CLIENT/SERIAL#)         
IBUKEYL  EQU   *-IBUKEY                                                         
IBUDATA  DS    0X                  ** RECORD DATA **                            
IBUDATAL EQU   *-IBUDATA                                                        
IBUL     EQU   *-IBUD                                                           
                                                                                
IOAD     DSECT                     ** INSERTION ORDER ACTIVITY **               
IOADATE  DS    XL(L'PBDIODAT)      INSERTION ORDER DATE                         
IOANUM   DS    CL(L'BUYORDNO)      INSERTION ORDER NUMBER                       
IOATYPE  DS    CL(L'ACTVTYPE)      INSERTION ORDER TYPE                         
IOAL     EQU   *-IOAD                                                           
                                                                                
ICAD     DSECT                     ** INSERTION CHANGE ACTIVITY **              
ICATYPE  DS    CL(L'ACTVTYPE)      INSERTION ACTIVITY TYPE                      
ICADATE  DS    XL(L'PCHGDAT)       INSERTION ACTIVITY DATE                      
ICAWHOM  DS    CL(3*L'SANAME+1)    PID NAME (FIRST, MIDDLE, LAST)               
ICAWHAT  DS    CL(L'ACTVWHAT)      INSERTION ACTIVITY TAKEN                     
ICAPVRT  DS    CL15                Previous buy rate char format                
ICAL     EQU   *-ICAD                                                           
                                                                                
ICAFLAG  DS    X                   INSERTION HISTORY DOWNLOAD FLAG              
ICAFINQ  EQU   X'80'               ALL DATA REPLIED                             
                                                                                
ICAAELM  DS    F                   ADDRESS OF CURRENT ACTIVITY ELEM             
ICACDAT  DS    XL(L'PCHGDAT)       COMPRESSED CHANGE DATE                       
ICABPID  DS    XL(L'PCHGPID)       BINARY PID                                   
ICACHGB  DS    XL10                # CHANGE BYTES (1ST BYTE NOT USED)           
ICATEMP  DS    XL256                                                            
                                                                                
IBAD     DSECT                     ** INSERTION BILLING ACTIVITY **             
IBADATE  DS    XL(L'PBLDATE)       INSERTION BILLING DATE                       
IBAGROSS DS    XL(L'PBGROSS)       INSERTION BILLING GROSS AMOUNT               
IBANET   DS    XL4                 INSERTION BILLING NET AMOUNT                 
IBANLCDB DS    XL4                 INSERTION BILLING NET LESS CD BILLED         
IBAINVNO DS    CL(L'BUYBINVN)      INSERTION BILLING INVOICE NUMBER             
IBASTATS DS    XL(L'PBBILST)       Insertion Billing Status                     
IBABTYPE DS    CL10                Insertion Billing Type                       
IBAL     EQU   *-IBAD                                                           
                                                                                
IPAD     DSECT                     ** INSERTION PAYMENT ACTIVITY **             
IPADATE  DS    XL(L'PPDDATE)       INSERTION PAYMENT DATE                       
IPAGROSS DS    XL(L'PPGROSS)       INSERTION PAYMENT GROSS AMOUNT               
IPANET   DS    XL4                 INSERTION PAYMENT NET AMOUNT                 
IPANLCDP DS    XL4                 INSERTION PAYMENT NET LESS CD PAID           
IPAPAYEE DS    CL(L'ACTVTYPE)      INSERTION PAYMENT REP CODE                   
IPACHECK DS    CL(L'PPCLCHK)       INSERTION PAYMENT CHECK NUMBER               
IPAACODE DS    CL(L'PPACCODE)      ADDITIONAL CHARGE CODE                       
IPASEQ_# DS    CL(L'PPDSEQNO)      SEQ # WITHIN DATE (CLEARANCE STATUS)         
IPACKDAT DS    XL(L'PPCLCHDT)      CHECK DATE                                   
IPAINVX_ DS    0XL(L'IPAINVTX+L'IPAINVNO)                                       
IPAINVTX DS    XL10                Text field before invoice number             
IPAINVNO DS    XL(L'PPCLINV)       CLEARANCE STATUS INVOICE NUMBER              
IPACPTYP DS    CL2                 PAYMENT TYPE (CR/CK)                         
IPASVLNQ EQU   *-IPAD              Fields afterward can be saved                
IPASTAT1 DS    XL(L'PPCLSTAT)      Clearance status                             
IPAL     EQU   *-IPAD                                                           
                                                                                
INVVALSD DSECT                     ** EXTRACTED INVOICE VALUES **               
                                                                                
INVHDRVS DS    0X                  ** INVOICE HEADER VALUES **                  
INVKEY   DS    0C                  INVOICE KEY                                  
INVMED   DS    CL(L'PNVKMED)       MEDIA                                        
INVSER#  DS    CL(2*L'PNVKSER#)    SERIAL NUMBER (10 DIGITS)                    
INVKEYSL EQU   *-INVKEY            L'SHORT INVOICE KEY                          
INVHGSTA DS    C                   HEADER STATUS                                
INVCLTC  DS    CL(L'PNV1CLT)       INVOICE CLIENT CODE                          
INVHPUBC DS    CL(L'BUYPUB)        INVOICE PUBLICATION CODE                     
INVVNDR# DS    CL(L'PNVHINV#)      INVOICE NUMBER                               
INVSTAT  DS    XL(L'PNVHSTAT)      INVOICE STATUS                               
INVPSTR  DS    XL(L'PNVHSTRT)      INVOICE PERIOD START DATE                    
INVPEND  DS    XL(L'PNVHEND)       INVOICE PERIOD END DATE                      
INVDATE  DS    XL(L'PNVHDATE)      INVOICE DATE                                 
INVTOTL  DS    XL(L'PNVHTOT)       INVOICE TOTAL                                
INVTTYP  DS    XL(L'PNVH$TYP)      INVOICE TOTAL TYPE                           
INVSREP  DS    XL(L'PNVHSREP)      SPECIAL REP                                  
INVHTAX  DS    XL(L'PNVHTAX)       Invoice tax amount                           
INVHSRC  DS    CL3                 Invoice source - Header level                
INVHCDT  DS    XL(L'PNVAHDTE)      Inv creation date from activity              
INVHVLQ  EQU   *-INVHDRVS          L'HEADER VALUES                              
                                                                                
INVCOMVS DS    0X                  ** INVOICE COMMENT CHANGE VALUES **          
INVCGSTA DS    C                   COMMENT STATUS                               
INVPIDNM DS    CL(3*L'SANAME+1)    PID NAME (FIRST, MIDDLE, LAST)               
INVCOMDT DS    XL(L'PNVCDATE)      COMMENT DATE                                 
INVCVLQ  EQU   *-INVCOMVS          L'COMMENT CHANGE VALUES                      
                                                                                
INVDETVS DS    0X                  ** INVOICE DETAIL VALUES **                  
                                                                                
IVNDKEY  DS    XL(INVKEYSL)        SAVED HEADER VALUE                           
INVDVI#  DS    XL(L'INVVNDR#)      "                                            
INVDSTA  DS    XL(L'INVSTAT)       "                                            
INVDTYP  DS    XL(L'INVTTYP)       "                                            
                                                                                
INVISQN  DS    XL(L'PNVDKSQN)      DETAIL SEQUENCE NUMBER                       
INVDGSTA DS    C                   DETAIL STATUS                                
INVINSKY DS    XL(BUYSERSL)        SHORT INSERTION KEY                          
INVRATE  DS    CL20                CHARACTER RATE                               
INVPREM  DS    CL20                CHARACTER PREMIUM                            
INVNET   DS    PL(L'PNVDNET)       NET                                          
INVGROSS DS    PL(L'PNVDGRS)       GROSS                                        
INV#LINE DS    XL(L'PNVD#LIN)      NUMBER OF LINE ITEMS                         
INVINSDT DS    XL(L'PNVDDTE)       LINE ITEM DATE (INSERTION DATE)              
INVSPDSP DS    XL(L'PNVDSPC)       SPACE DESCRIPTION                            
INVADCAP DS    XL(L'PNVDACAP+L'PNVDACP2)                                        
INVCLTCD DS    XL(L'PNVDCLT)       CLIENT CODE                                  
INVPRDCD DS    XL(L'PNVDPRD)       PRODUCT CODE                                 
INVDPUBC DS    XL(L'BUYPUB)        PUBLICATION CODE                             
INVDDCMC DS    XL(L'PNVDDCM)       DISCREPANCY COMMENT CODE                     
INVDDCPI DS    CL(L'SAPALPID)      DISCREPANCY COMMENT PID                      
INVDDCDT DS    XL(L'PNVAHDTE)      DISCREPANCY COMMENT DATE                     
INVDSRC  DS    CL(L'INVHSRC)       Invoice source - Detail level                
INVDVLQ  EQU   *-INVDETVS          L'DETAIL VALUES                              
                                                                                
INVSUMYS DS    0X                  ** Invoice summary values **                 
INVTWTAX DS    XL4                 Invoice total with taxes                     
INVSYLQ  EQU   *-INVSUMYS          L'Summary values                             
                                                                                
INVVALSX EQU   *-INVVALSD          END OF EXTRACTED INVOICE VALUES              
                                                                                
GST_DELQ EQU   C'D'                GENERAL DELETED STATUS                       
GST_CLRQ EQU   C'C'                GENERAL CLEARED STATUS                       
                                                                                
SREP_STR DS    0X                  START OF SPECIAL REP DATA                    
SREP_MED DS    CL(L'PREPKMED)      MEDIA CODE                                   
SREP_REP DS    CL(L'PREPKREP)      REP CODE                                     
SREP_NAM DS    CL(L'PREPNAME)      NAME                                         
SREP_ATT DS    CL(L'PREPATTN)      ATTENTION OF                                 
SREP_AD1 DS    CL(L'PREPLIN1)      ADDRESS LINE 1                               
SREP_AD2 DS    CL(L'PREPLIN2)      ADDRESS LINE 2                               
SREP_AD3 DS    CL(L'PREPLIN3)      ADDRESS LINE 3                               
SREP_STC DS    CL(L'PREPSTAC)      STATE CODE                                   
SREP_LNQ EQU   *-SREP_STR          LENGTH OF REPLY DATA                         
                                                                                
SREPTAB  DS    0X                  TABLE OF REP CODES TO LOOK UP                
SREPKMED DS    CL(L'PREPKMED)      MEDIA CODE                                   
SREPKREP DS    CL(L'PREPKREP)      REP CODE                                     
SREPTABL EQU   *-SREPTAB                                                        
         DS    (SREPTMXQ)XL(SREPTABL)                                           
SREPTMXQ EQU   (IOLENQ-INVVALSX-SREP_LNQ)/SREPTABL                              
SREPTLNQ EQU   *-SREP_STR                                                       
                                                                                
IVCOMNTD DSECT                     ** INVOICE COMMENT **                        
IVCOMMMX EQU   IOLENQ/L'IVCOMMNT   MAXIMUM N'COMMENTS                           
IVCOMMNT DS    (IVCOMMMX)CL220     COMMENT LINE                                 
IVCOMMLN EQU   *-IVCOMMNT                                                       
                                                                                
INSVARY  DSECT                     ** ARRAY FOR INSERTION VALUES **             
                                                                                
BINVARYQ EQU   001                 ARRAY #1 - BUY INVOICE DATA                  
BINVTAB# DS    AL2                 NUMBER OF INV ELEM FOUND IN BUY REC          
BINVTAB  DS    0X                  BUY INVOICE DATA START                       
BINVTABM EQU   60                  MAX N'INVOICES IN BUY REC                    
BINVKEY  DS    XL(INVKEYSL)        INVOICE KEY                                  
BINVSEQ# DS    XL(L'PBNVDSQN)      SEQUENCE NUMBER                              
BINVVIV# DS    CL(L'PBNVINV#)      VENDOR INVOICE NUMBER                        
BINVTABL EQU   *-BINVTAB                                                        
         DS    (BINVTABM-1)XL(BINVTABL)                                         
                                                                                
BPO#ARYQ EQU   002                 ARRAY #2 - BUY PURCHASE ORDER DATA           
BPO#TAB# DS    AL2                 NUMBER OF PURCHASE ORDER FOR ZZZ BUY         
BPO#TAB  DS    0X                  ZZZ BUY PURCHASE ORDER DATA START            
BPO#TABM EQU   10                  MAX N'PURCHASE ORDER # IN BUY REC            
BPO#PRDC DS    CL(L'PBYPOPRD)      PRD CODE                                     
BPO#SEQ# DS    XL(L'PBYPOSQ#)      PURCHASE ORDER SEQUENCE NUMBER               
BPO#TABL EQU   *-BPO#TAB                                                        
         DS    (BPO#TABM-1)XL(BPO#TABL)                                         
                                                                                
INSVARYX DS    X                   End of array marker                          
                                                                                
BCCVARY  DSECT                     ** BUY CUSTOM COLUMN VALUES **               
BUYCCSQ# DS    XL(L'BYCCSQN)       BUY CUSTOM COLUMN SEQ NUMBER                 
BUYCCFDA DS    XL(255-BYCCHDRL)    BUY CUSTOM COLUMN FIELD DATA                 
                                                                                
IV#PARY  DSECT                     ** Vendor Invoice # from Pay **              
VINV#PAY DS    XL(L'PPCLINV)       Clearance status invoice number              
                                                                                
WIOLKEYD DSECT                     ** WEB INSERTION ORDER LONG KEY **           
WIOLGKEY DS    0C                  INSERTION ORDER LONG KEY                     
WIOLGMED DS    C                   MEDIA CODE                                   
         DS    C                                                                
WIOLGYER DS    CL2                 YEAR (YY)                                    
WIOLGCLT DS    CL(L'PCLTKCLT)      CLIENT CODE                                  
DISPER#Q EQU   *-WIOLGKEY          DISPLACEMENT TO EXTENED REF #                
WIOLGRNO DS    0CL(L'WIOLGER#+L'WIOLG_R#)                                       
WIOLGER# DS    CL1                 EXTENDED REFERENCE # FOR 9,999+              
DISP_R#Q EQU   *-WIOLGKEY          DISPLACEMENT TO REF #                        
WIOLG_R# DS    CL4                 REFERENCE NUMBER                             
         DS    C                                                                
WIOLGRET DS    CL3                 REVISION TEXT (REV)                          
WIOLGRE# DS    CL3                 REVISION NUMBER (000-999)                    
LNAERF#Q EQU   *-WIOLGER#          LENGTH OF KEY AFTER EXTENDED REF #           
WIOLGKYL EQU   *-WIOLGKEY                                                       
                                                                                
ESRLKEYD DSECT                     ** ENHANCED SPACE RES LONG KEY **            
ESRLGKEY DS    0C                  ESR LONG KEY                                 
ESRLGSRT DS    CL2                 ESR TEXT (SR)                                
         DS    C                                                                
ESRLGMED DS    C                   MEDIA CODE                                   
         DS    C                                                                
ESRLGYER DS    CL2                 YEAR (YY)                                    
ESRLGCLT DS    CL(L'PCLTKCLT)      CLIENT CODE                                  
                                                                                
D_SR_E#Q EQU   *-ESRLGKEY          DISPLACEMENT TO EXTENED REF #                
ESRLGRNO DS    0CL(L'ESRLGER#+L'ESRLG_R#)                                       
ESRLGER# DS    CL1                 EXTENDED REFERENCE # FOR 9,999+              
D_SR_R#Q EQU   *-ESRLGKEY          DISPLACEMENT TO REF #                        
ESRLG_R# DS    CL4                 REFERENCE NUMBER                             
         DS    C                                                                
ESRLGRET DS    CL3                 REVISION TEXT (REV)                          
ESRLGRE# DS    CL3                 REVISION NUMBER (000-999)                    
L_SR_E#Q EQU   *-ESRLGER#          LENGTH OF KEY AFTER EXTENDED REF #           
ESRLGKYL EQU   *-ESRLGKEY                                                       
                                                                                
DRRIVKYD DSECT                     ** DRR INVOICE KEY TABLE **                  
                                                                                
IVKEYTAB DS    0X                  TABLE OF INVOICE KEYS TO LOOK UP             
IVKEYMED DS    CL(L'PNVKMED)       MEDIA CODE                                   
IVKEYSER DS    CL(L'PNVKSER#)      SERIAL #                                     
IVKEYSQ# DS    CL(L'PNVDKSQN)      DETAIL SEQUENCE #                            
IVKEYTBL EQU   *-IVKEYTAB          LENGTH OF ONE ENTRY                          
IVKEYMXQ EQU   IOLENQ/IVKEYTBL     MAXIMUM N'INVOICE KEYS                       
                                                                                
AGYRARYD DSECT                     ** AGENCY RECORD **                          
                                                                                
AGY_MEDC DS    CL(L'PAGYKMED)      MEDIA CODE                                   
AGY_NAME DS    CL(L'PAGYNAME)      AGENCY NAME                                  
AGY_ADDR DS    CL(L'PAGYADDR)      AGENCY ADDRESS                               
                                                                                
MSGREPYD DSECT                     ** MESSAGE REPLY RECORD **                   
                                                                                
MSG_MAPC DS    XL(L'SVMSGMAP)      MESSAGE MAP CODE                             
MSG_TEXT DS    CL60                MESSAGE TEXT                                 
                                                                                
EIOSRD   DSECT                     ** EIO SETUP REPLY RECORD **                 
EIO_MEDC DS    CL(L'SCHKMED)       MEDIA CODE                                   
EIO_CLTC DS    CL(L'SCHKCLT)       CLIENT CODE                                  
EIO_UEIO DS    XL(L'SCHEIO)        USE EIO                                      
EIO_UESR DS    XL(L'SCHESR)        USE ESR                                      
EIO_EIOE DS    XL(L'SCHEIOE)       EIO BY ESTIMATE                              
EIO_EIOD DS    XL(L'SCHEACD)       EIO BY ESTIMATE DATE                         
EIO_ESRE DS    XL(L'SCHESRE)       ESR BY ESTIMATE                              
EIO_ESRD DS    XL(L'SCHESRD)       ESR BY ESTIMATE DATE                         
                                                                                
GETADRPD DSECT                     PPGETADR PARAMETER BLOCK                     
GADR_AGY DS    CL(L'PCLTKAGY)                                                   
GADR_MED DS    CL(L'PCLTKMED)                                                   
GADR_CLT DS    CL(L'PCLTKCLT)                                                   
GADR_OFC DS    CL(L'PCLTOFF)                                                    
                                                                                
F_COST_D DSECT                     FOR FORMATTING COST                          
FC_COST_ DS    PL(L'BYPCCST)       COST                                         
FC_NETSW DS    CL(L'PBDCTYP)       NET COST SWITCH (ENTERED AS NET)             
FC_COTYP DS    CL(L'PBDCOSTY)      COST TYPE (U=UNIT COST)                      
FC_COIND DS    CL(L'PBDCOSIN)      COST INDICATOR                               
FC_RTIND DS    XL(L'PBDRLIND)      RATE LOOK-UP INDICATOR                       
FC_ACPCT DS    PL(L'PBDACP)        AGENCY COMMISSION PERCENT (3 DEC)            
FC_OCOST DS    CL20                FORMATTED COST (OUTPUT)                      
FC_TMPWK DS    CL20                TEMPORARY WORK AREA                          
FC_BLKLQ EQU   *-F_COST_D                                                       
                                                                                
INVCTOTD DSECT                     INVOICE CLEARANCE STATUS TOTAL               
GCSEL01A DS    A                                                                
GCSDEBN  DS    F                   CURRENT CLEARED DEBITS  NET                  
GCSDEBG  DS    F                   CURRENT CLEARED DEBITS  GROSS                
GCSRTIOD DS    PL8                 DEBIT  RATIO                                 
GCSCRDN  DS    F                   CURRENT CLEARED CREDITS NET                  
GCSCRDG  DS    F                   CURRENT CLEARED CREDITS GROSS                
GCSRTIOC DS    PL8                 CREDIT RATIO                                 
GCSDEBCD DS    F                   CURRENT CLEARED DEBITS  CD                   
GCSRTCDD DS    PL8                 DEBIT  RATIO                                 
GCSCRDCD DS    F                   CURRENT CLEARED CREDITS CD                   
GCSRTCDC DS    PL8                 CREDIT RATIO                                 
                                                                                
GCSVEL03 DS    XL32                SAVEAREA FOR 03 ELMEMENT                     
GCSVEL05 DS    XL64                SAVEAREA FOR 05 ELEMENT                      
                                                                                
INVCTLNQ EQU   *-INVCTOTD                                                       
*                                                                               
GSTPST_D DSECT                     Canadian taxes                               
       ++INCLUDE GVALUES                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072PPLNK11   10/14/20'                                      
         END                                                                    
