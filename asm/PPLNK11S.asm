*          DATA SET PPLNK11S   AT LEVEL 074 AS OF 04/30/04                      
*PHASE T41411B                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE PPBYOUT                                                                
*INCLUDE PPFMTINO                                                               
*INCLUDE PUBVAL                                                                 
*INCLUDE PUBEDIT                                                                
*                                                                               
PPLNK11  TITLE '- PRINT SYSTEM INSERTION, INVOICE && CFM DOWNLOADS'             
*                                                                               
SVRDEF   CSECT                                                                  
         DC    (RSVRDEFL)X'00'                                                  
                                                                                
         ORG   SVRDEF                                                           
         DC    C'*SERVER**SERVER**SERVER**SERVER*'                              
         DC    AL2(CODE-SVRDEF)    SERVER ENTRY POINT                           
         DC    AL2(FILES-SVRDEF)   SYSTEM/FILE LIST                             
         DC    AL2(0)              FACILITIES LIST                              
         DC    AL2(REQUEST-SVRDEF) REQUEST MAP                                  
                                                                                
         ORG   SVRDEF+(RSVRNOTE-RSVRDEFD)                                       
         DC    AL2(ABENDS-SVRDEF)  NOTIFICATION LIST FOR ABENDS                 
         DC    AL2(SLOWS-SVRDEF)   NOTIFICATION LIST FOR SLOWS                  
                                                                                
         ORG   SVRDEF+(RSVRTYPE-RSVRDEFD)                                       
         DC    AL1(TSTADBY)        SERVER TYPE                                  
         DC    AL1(PRTSYSQ)        PRINT SYSTEM                                 
         DC    C'PP'               SYSTEM                                       
         DC    C'ID'               PROGRAM                                      
         DC    AL1(WRKIFTWF)       WORKER FILE IN                               
         DC    AL1(WRKIAAPP)       OPEN APPEND                                  
         DC    AL2(SYSPHASE)       SYSPHASE                                     
         DC    AL1(RSVRILNK+RSVRILCO)                                           
         ORG                                                                    
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**PL11*,RR=RE                                                  
         USING LP_D,R1                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         OC    RMASTC,RMASTC       TEST RUNNING ONLINE                          
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK 1              
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK 2              
         L     RA,ATWA             RA=A(TWA)                                    
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,=AL2(WORKL)                                                 
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
         USING SAVED,R8            R8=A(SAVE W/S)                               
         USING PBUD,PBUREC         PUBLICATION RECORD                           
         USING RBUD,RBUREC         PUBLISHER/REP RECORD                         
         USING IBUD,IBUREC         INVOICE RECORD                               
         USING TSARD,TSARBLK       TSAR BLOCK                                   
         L     RA,LP_ATWA                                                       
         ST    RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
INIT04   ST    R1,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
         TM    GIND1,GIONLINE      TEST RUNNING OFFLINE                         
         BNZ   RUNSTR02                                                         
                                                                                
         L     RF,RCOMFACS         YES - LOAD FACILITIES OVERLAYS               
         ST    RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)                                                     
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
         L     RF,RSYSFACS         COPY FACILITIES ADCONS                       
         MVC   SYSADDR(SYSADDRL),0(RF)                                          
                                                                                
         L     R1,ALP                                                           
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
RUNSTR02 MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         LA    R1,OADCONS          RELOCATE ADCONS                              
         LA    R0,OADCONSN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         A     RF,SRVRRELO                                                      
         ST    RF,0(R1)                                                         
         AHI   R1,L'OADCONS                                                     
         BCTR  R0,RE                                                            
                                                                                
         LA    R0,QGETINS          LOAD GETINS                                  
         ICM   R0,B'1110',T00A                                                  
         GOTOR VCALLOV,DMCB,0,(R0),0                                            
         MVC   VGETINS,0(R1)                                                    
                                                                                
         LA    R0,QPSTVAL          LOAD PSTVAL                                  
         ICM   R0,B'1110',T00A                                                  
         GOTOR VCALLOV,DMCB,0,(R0),0                                            
         MVC   VPSTVAL,0(R1)                                                    
                                                                                
         L     R1,ALP                                                           
         LA    R0,SAVED                                                         
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
B#CLT    EQU   3                   CLIENT RECORD                                
B#PRD    EQU   3                   PRODUCT RECORD                               
B#BUY    EQU   3                   BUY RECORD                                   
B#PUB    EQU   3                   PUBLICATION RECORD                           
         MVC   LP_BLKS+((B#BUY-1)*L'LP_BLKS),AIO2                               
B#ACH    EQU   4                   ADDITIONAL CHARGES TABLE                     
         MVC   LP_BLKS+((B#ACH-1)*L'LP_BLKS),AIO6                               
*                                                                               
B#INVVAL EQU   5                   INVOICE VALUES                               
B#PROFVL EQU   5                   PROFILE VALUES                               
         MVC   LP_BLKS+((B#INVVAL-1)*L'LP_BLKS),AIO4                            
B#INVCOM EQU   6                   INVOICE COMMENTS                             
B#INSARY EQU   6                   ARRAYS FOR BUY/INVOICE DATA                  
B#BCCBLK EQU   6                   BLOCK USED FOR BUY CUSTOM COLUMN             
         MVC   LP_BLKS+((B#INVCOM-1)*L'LP_BLKS),AIO5                            
*                                                                               
         TM    GIND1,GIONLINE      TEST RUNNING ONLINE                          
         JZ    EXITY                                                            
         L     RF,ACOMFACS         YES - NEED DICTIONARY WORDS TOO              
         L     RF,CDICTATE-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,C'LL  ',ACTVDICT,ACTVWHAT                              
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNPMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    REQVALS(REQVALL),REQVALS                                         
         XC    LASTS(LASTL),LASTS                                               
         MVC   ENDDATE,EFFS                                                     
         XC    PBUREC,PBUREC       CLEAR PUBLICATION RECORD                     
         XC    RBUREC,RBUREC       CLEAR PUBLISHER/REP RECORD                   
         L     R0,AIO1             CLEAR THE I/O AREAS                          
         SR    R1,R1                                                            
         LHI   R1,IOAREAL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   TSACTN,TSAINI       INITIALIZE TSAR BUFFER                       
         MVC   TSACOM,RCOMFACS                                                  
         MVI   TSKEYL,IBUKEYL                                                   
         MVI   TSPAGN,8                                                         
         OI    TSINDS,TSINODSK+TSIXTTWA                                         
         LHI   R0,IBUL                                                          
         STCM  R0,3,TSRECL                                                      
         LA    R0,IBUREC                                                        
         STCM  R0,15,TSAREC                                                     
         GOTOR BUFFER                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSAADD       SET TSAR ACTION TO 'ADD'                     
*                                                                               
         TM    GIND1,GIONLINE      TEST RUNNING OFFLINE                         
         BNZ   PRCWRKX             NO                                           
                                                                                
         GOTOR RBUFFRIN,DMCB,('BUFFAINI',APBUFFD),PBUD,ACOMFACS                 
         GOTOR RBUFFRIN,DMCB,('BUFFAINI',ARBUFFD),RBUD,ACOMFACS                 
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,PRTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PRTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PUBDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PUBFIL,(4,0),0                               
                                                                                
PRCWRKX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         MVC   AGY,LP_AGY          SET AGENCY CODE                              
         MVC   TWAAGY,AGY                                                       
         ICM   RF,15,RMASTC        SET TRACE OPTION IF OFFLINE                  
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
                                                                                
         GOTOR BLDPUB              BUILD PUBLICATION LIST                       
                                                                                
RUNREQ02 GOTOR BLDACC              BUILD ADDITIONAL CHARGE CODES                
         B     RUNREQGO                                                         
                                                                                
RUNREQ04 GOTOR BLDHST              BUILD INSERTION HISTORY DOWNLOAD             
         B     RUNREQGO                                                         
                                                                                
RUNREQ06 GOTOR BLDMED              BUILD MEDIA LIST                             
         B     RUNREQGO                                                         
                                                                                
RUNREQ08 GOTOR BLDMRC              BUILD MEDIA/RECORD/CLIENT TABLE              
         B     RUNREQGO                                                         
                                                                                
RUNREQ10 GOTOR BLDPUM              BUILD PUBLICATION MEDIA LIST                 
         JNE   EXITY                                                            
                                                                                
RUNREQGO L     R1,ALP                                                           
         USING LP_D,R1                                                          
         GOTOR LP_APUTO            CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
M@DLHST  DC    AL2(M#DLHST)        HISTORY DOWNLOAD MAP CODE                    
M@DLREF  DC    AL2(M#DLREF)        REFRESH INSERTIONS                           
M@CFMCLT DC    AL2(M#CFMCLT)       CFM CLIENT DOWNLOAD                          
M@CFMPRD DC    AL2(M#CFMPRD)       CFM PRODUCT DOWNLOAD                         
M@CFMPUB DC    AL2(M#CFMPUB)       CFM VENDOR DOWNLOAD                          
                                                                                
LVALUES  DS    0D                  ** LITERAL VALUES **                         
         DC    A(BUYTAB)                                                        
         DC    A(CLTTAB)                                                        
         DC    A(PRDTAB)                                                        
         DC    A(PUBTAB)                                                        
         DC    V(PUBVAL)                                                        
         DC    V(PUBEDIT)                                                       
         DC    V(PPBYOUT)                                                       
         DC    V(PPFMTINO)                                                      
         DC    V(PPBVAL)                                                        
         DC    A(PUBBUFF)                                                       
         DC    A(REPBUFF)                                                       
         DC    A(INVBUFF)                                                       
         DC    F'100000'                                                        
         DC    C'DMKEY   '                                                      
         DC    C'PRTDIR  '                                                      
         DC    C'PRTFIL  '                                                      
         DC    C'PUBDIR  '                                                      
         DC    C'PUBFIL  '                                                      
         DC    C'ZZZ'                                                           
         DC    C'ALL'                                                           
         DC    C'FREE'                                                          
         DC    X'FFFFFFFF'                                                      
         DC    X'00FF'                                                          
         DC    P'0'                                                             
         DC    P'100'                                                           
         DC    X'000001FFFFFF'                                                  
         DC    X'000000000001FFFFFFFFFFFF'                                      
         DC    X'8040201008040201'                                              
                                                                                
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
         DCDDL PP#@COS2,L'ACTVWHAT,L                                            
         DCDDL PP#@IMPS,L'ACTVWHAT,L                                            
         DCDDL PP#@ACHG,L'ACTVWHAT,L                                            
         DCDDL PP#@LWAR,L'ACTVWHAT,L                                            
         DCDDL PP#ADD,L'ACTVTYPE,L                                              
         DCDDL PP#CHA,L'ACTVTYPE,L                                              
         DCDDL PP#DEL,L'ACTVTYPE,L                                              
         DCDDL PP#IONEW,L'ACTVTYPE,L                                            
         DCDDL PP#IOCHA,L'ACTVTYPE,L                                            
         DCDDL PP#IOCAN,L'ACTVTYPE,L                                            
         DCDDL PP#DIRCK,L'ACTVTYPE,L                                            
DDLISTX  DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* GET CLIENT RECORDS FOR CFM CLIENT DOWNLOAD                          *         
***********************************************************************         
                                                                                
NXTCLT   J     *+12                                                             
         DC    C'*NXTCLT*'                                                      
         LR    RB,RF                                                            
         USING NXTCLT,RB                                                        
                                                                                
NXTCLT02 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',ACLTTAB),                *        
               ('B#CLT',0),SAVED,('#LIMACC',ALIMACC)                            
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING PCLTRECD,R2         R2=A(CLIENT RECORD)                          
         XC    IOADDR,IOADDR                                                    
         MVC   SVCLTKEY,IOKEY                                                   
         USING PPRDRECD,IOKEY                                                   
         MVI   PPRDKRCD,X'06'                                                   
         GOTOR (#IOEXEC,AIOEXEC),IOHI+IOPRTDIR                                  
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
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET PRODUCT RECORD FOR CFM PRODUCT DOWNLOAD                         *         
***********************************************************************         
*                                                                               
NXTPRD   J     *+12                                                             
         DC    C'*NXTPRD*'                                                      
         LR    RB,RF                                                            
         USING NXTPRD,RB                                                        
                                                                                
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',APRDTAB),                *        
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
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET PUBLICATION RECORD FOR CFM VENDOR DOWNLOAD                      *         
***********************************************************************         
*                                                                               
NXTVEN   J     *+12                                                             
         DC    C'*NXTVEN*'                                                      
         LR    RB,RF                                                            
         USING NXTVEN,RB                                                        
                                                                                
         L     R2,AIO2                                                          
         USING PUBRECD,R2          R2=A(PUBLICATION RECORD)                     
         CLI   PUBKMED,FF                                                       
         BE    NXTVEN04                                                         
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',APUBTAB),                *        
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
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET PUBLICATION REP RECORDS FOR CFM VENDOR DOWNLOAD                 *         
***********************************************************************         
*                                                                               
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
                                                                                
NXTPUR08 L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST                                                
         MVC   IOKEY,SVPUBKEY                                                   
         J     EXITY                                                            
                                                                                
NXTPUR10 L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RQUIT                                                
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET BUY RECORDS, APPLY FILTERS AND FORMAT FIELDS FOR DOWNLOADING    *         
***********************************************************************         
*                                                                               
NXTBUY   J     *+12                                                             
         DC    C'*NXTBUY*'                                                      
         LR    RB,RF                                                            
         USING NXTBUY,RB                                                        
*                                                                               
         MVI   READDELS,YESQ       SET TO INCLUDE DELETED BUYS                  
         TM    FILTSTAT,FILTSLDE+FILTSLUD+FILTSTDE+FILTSTUD                     
         BNM   NXTBUY02                                                         
         MVI   READDELS,0          SET NOT TO GET DELETED BUYS                  
         TM    FILTSTAT,FILTSLDE+FILTSTDE                                       
         BZ    NXTBUY02                                                         
         MVI   READDELS,YESQ       SET TO INCLUDE DELETED BUYS                  
         TM    FILTSTAT,FILTSLUD+FILTSTUD                                       
         BNZ   NXTBUY02                                                         
         MVI   READDELS,ONLYQ      SET TO ONLY GET DELETED RECORDS              
                                                                                
NXTBUY02 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',ABUYTAB),('B#BUY',0),    *        
               (READDELS,SAVED),0                                               
         JNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING PBUYRECD,R2         R2=A(BUY KEY)                                
                                                                                
         CLC   PBUYKMED,LASTMED    TEST CHANGE OF MEDIA/CLIENT                  
         BNE   *+14                                                             
         CLC   PBUYKCLT,LASTCLT                                                 
         BE    NXTBUY06                                                         
         MVC   LASTMED,PBUYKMED    SAVE NEW MEDIA/CLIENT                        
         MVC   LASTCLT,PBUYKCLT                                                 
                                                                                
         GOTOR (#GETCLT,AGETCLT),DMCB,PBUYKAGY,PBUYKMED,PBUYKCLT                
         BE    NXTBUY04                                                         
         LA    R2,IOKEY                                                         
         ICM   R0,7,PBUYKCLT       BUMP TO NEXT CLIENT RECORD                   
         AHI   R0,1                                                             
         STCM  R0,7,PBUYKCLT                                                    
         XC    PBUYKPRD(PBUYLEN-PBUYKPRD),PBUYKPRD                              
         B     NXTBUY02                                                         
                                                                                
NXTBUY04 GOTOR GETCPR              GET CLIENT PROFILES                          
                                                                                
NXTBUY06 GOTOR FMTBUY,1            FILTER AND FORMAT BUY RECORD                 
         BNE   NXTBUY02            DIDN'T PASS THE FILTER TESTS                 
*                                                                               
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET NEXT BUY RECORD FOR INSERTION REFRESH DOWNLOAD                  *         
***********************************************************************         
*                                                                               
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
         BZ    NXTSERN                                                          
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
         BZ    NXTSERN                                                          
         STH   R0,NSER                                                          
         L     R1,ANXTSER                                                       
         AHI   R1,BUYSERSL                                                      
         ST    R1,ANXTSER                                                       
                                                                                
NXTSER04 GOTOR GETBUY,(R1)         READ THE BUY RECORD                          
         BE    NXTSER06                                                         
*                                                                               
         GOTOR XC_BLCK#,'B#INSARY' CLR BUY INVOICE DATA                         
*                                                                               
         LA    R0,BUYVALS          CLEAR EXTRACTED BUY VALUES                   
         LHI   R1,BUYVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   BUYCDPCT,PZERO                                                   
         L     R1,AIO2             CLEAR BUY RECORD VALUES                      
         XC    0(256,R1),0(R1)                                                  
         L     R1,ANXTSER          SET SERIAL NUMBER                            
         MVC   BUYSER(BUYSERSL),0(R1)                                           
         MVI   BUYBSTAT,BUYBSNFD   SET STATUS TO 'NOT FOUND'                    
         J     EXITY                                                            
                                                                                
NXTSER06 L     R2,IOADDR           R2=A(BUY RECORD)                             
*                                                                               
         CLC   PBUYKMED,LASTMED    TEST CHANGE OF MEDIA/CLIENT                  
         BNE   *+14                                                             
         CLC   PBUYKCLT,LASTCLT                                                 
         BE    NXTSER08                                                         
         MVC   LASTMED,PBUYKMED    SAVE NEW MEDIA/CLIENT                        
         MVC   LASTCLT,PBUYKCLT                                                 
*                                                                               
         GOTOR (#GETCLT,AGETCLT),DMCB,PBUYKAGY,PBUYKMED,PBUYKCLT                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR GETCPR              GET CLIENT PROFILES                          
*                                                                               
NXTSER08 GOTOR FMTBUY,0            FORMAT BUY RECORD FOR DOWNLOAD               
*                                                                               
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
NXTSERN  L     R1,ALP              NO MORE RECORDS TO GO                        
         MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R1                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET INVOICE RECORDS FROM VENDOR INVOICE NUMBERS                     *         
***********************************************************************         
*                                                                               
NXTIV#   J     *+12                                                             
         DC    C'*NXTIV#*'                                                      
         LR    RB,RF                                                            
         USING NXTIV#,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                        
*                                                                               
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXIV#50                                                          
         CLI   INVDLSW,0           DOWNLOADING INVOICES SW NOT YET SET?         
         BNE   *+8                                                              
         MVI   INVDLSW,YESQ                                                     
         XC    IOKEY,IOKEY         SET UP INVOICE PASSIVE KEY                   
I        USING PNV3KEY,IOKEY                                                    
         MVC   I.PNV3AGY,AGY                                                    
         ICM   R1,7,AMED                                                        
         MVC   I.PNV3MED,LW_DATA1-LW_D(R1)                                      
         MVI   I.PNV3RCD,PNV3RCDQ                                               
         ICM   R1,7,ACLT                                                        
         MVC   I.PNV3CLT,LW_DATA1-LW_D(R1)                                      
         OC    BUYPUB,SPACES                                                    
         CLC   BUYPUB,SPACES                                                    
         BE    NXIV#92             NO PUB, CANNOT LOOK INVOICE RECS             
         LA    RF,L'BUYPUB                                                      
         LA    RE,BUYPUB+L'BUYPUB-1                                             
         CLI   0(RE),C' '                                                       
         BNE   *+12                                                             
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     *-12                                                             
         GOTOR VPUBVAL,DMCB,((RF),BUYPUB),DUB                                   
         CLI   0(R1),X'FF'                                                      
         BE    NXIV#92             INVALID PUB, NO NEED TO CONTINUE             
         OC    INVVNDR#,INVVNDR#                                                
         BZ    NXIV#14             NO VENDOR INVOICE #, TRY DATES               
         MVI   INVDLMOD,IVDLMD_#   DOWNLOAD BY INOICE NUMBER MODE               
         MVC   I.PNV3PBCD,DUB                                                   
         MVC   I.PNV3INV#,INVVNDR#                                              
         OC    I.PNV3INV#,SPACES                                                
NXIV#12  GOTOR (#IOEXEC,AIOEXEC),'IORDEL+IOHI+IOPRTDIR+IO1'                     
*                                                                               
         CLC   I.PNV3KEY,IOKEYSAV  RECORD FOUND?                                
         BNE   NXIV#92             RECORD NOT FOUND, NOTHING TO RETURN          
*                                                                               
         TM    I.PNV3CNTL,PNVCDELQ RECORD IS DELETED?                           
         BZ    NXIV#20                                                          
         GOTOR XC_IVALS,0          CLR ALL INVOICE REPLY VALUES                 
         MVC   INVKEY(07),=C'DELETED'                                           
         MVI   INVDLSW,0           RESET INVOICE DOWNLOAD SWITCH                
         J     EXITY                                                            
*                                                                               
NXIV#14  OC    STRDATE,STRDATE                                                  
         BZ    NXIV#92             NO START DATE, NOTHING TO LOOK UP            
         MVI   INVDLMOD,IVDLMD_D   DOWNLOAD BY START AND END DATES MODE         
*                                                                               
I        USING PNV2KEY,IOKEY                                                    
         MVI   I.PNV2RCD,PNV2RCDQ                                               
         MVC   I.PNV2PUB,DUB                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
NXIV#16  CLC   I.PNV2KEY(PNV2SDTE-PNV2KEY),IOKEYSAV                             
         BNE   NXIV#92                                                          
         CLC   I.PNV2SDTE,STRDATE                                               
         BL    NXIV#56                                                          
         CLC   I.PNV2EDTE,ENDDATE                                               
         BH    NXIV#56                                                          
         MVC   INVWKKEY,IOKEY      SAVE IT FOR NEXT ROUND                       
*                                                                               
NXIV#20  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         L     RE,AIO1                                                          
         MVC   QINVKEY,0(RE)       SAVE MINIO MASTER KEY                        
         MVC   MINMKEY(L'PNVKEY),QINVKEY                                        
         GOTOR XC_IVALS,4          PREPARE TO REPLY INVOICE HDR VALUES          
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXIV#22             ONLY NEED TO INIT ONCE                       
         BRAS  RE,MININIT          INIT MINIO                                   
         MVC   MINMKEY(L'PNVKEY),QINVKEY                                        
         L     RF,ACOMFACS                                                      
         L     RF,(CMINIO-COMFACSD)(RF)                                         
         GOTOR (RF),PNVPARMS,('MINOPN',MINBLKD)                                 
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NXIV#22  LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVHDRD,RE                                                       
         MVI   PNVHKCDE,PNVHKIDQ   INVOICE HEADER ELEM ID                       
         GOTOR MGETEL,DMCB,ELEM    GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         BNZ   *+6                                                              
         DC    H'0'                THERE MUST BE AN INVOICE HDR ELEM!           
*                                                                               
         TM    PNVHSTUS,PNVHDLQ    DELETED?                                     
         BNZ   NXIV#52             DELETED, PROC NEXT ONE                       
*                                                                               
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY DATES MODE?                      
         BNE   NXIV#28                                                          
         OC    INVMATST,SPACES                                                  
         CLC   INVMATST,SPACES                                                  
         BE    NXIV#28             NO INVOICE STATUS TO FILTER ON               
         CLC   PNVHSTAT,INVMATST+0 PENDING?                                     
         BE    NXIV#28                                                          
         CLC   PNVHSTAT,INVMATST+1 MATCHED?                                     
         BE    NXIV#28                                                          
         CLC   PNVHSTAT,INVMATST+2 DISCREPANT?                                  
         BE    NXIV#28                                                          
         B     NXIV#52             FAILED FILTER, PROC NEXT ONE                 
*                                                                               
NXIV#28  BRAS  RE,FMTIVHDR         FORMAT INVOICE HEADER VALUES                 
         MVI   INVCMSW,0           INIT INV COMMENT SW FOR LATER                
         J     EXITY                                                            
*                                                                               
NXIV#50  CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   NXIV#96                                                          
*                                                                               
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY DATES MODE?                      
         BNE   NXIV#90                                                          
NXIV#52  MVC   IOKEY(L'INVWKKEY),INVWKKEY                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
NXIV#56  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOPRTDIR+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     NXIV#16                                                          
*                                                                               
NXIV#90  CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENTS DONE?                        
         BNE   NXIV#96                                                          
         MVI   INVCMSW,0           SET TO DO NEXT HEADER COMMENT                
         B     NXIV#96                                                          
*                                                                               
NXIV#92  GOTOR XC_IVALS,0          CLR ALL INVOICE REPLY VALUES                 
*                                                                               
NXIV#96  L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST   NO MORE TO PROCESS                           
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R5,R7,I,RE                                                 
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET INVOICE RECORDS FROM INVOICE KEY(S)                             *         
***********************************************************************         
*                                                                               
NXTIVK   J     *+12                                                             
         DC    C'*NXTIVK*'                                                      
         LR    RB,RF                                                            
         USING NXTIVK,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                        
*                                                                               
         GOTOR XC_IVALS,4          PREPARE TO REPLY INVOICE HDR VALUES          
*                                                                               
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
NXIVK10  CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXIVK50                                                          
         CLI   INVDLSW,0           DOWNLOADING INVOICES SW NOT YET SET?         
         BNE   *+8                                                              
         MVI   INVDLSW,YESQ                                                     
         MVI   INVCMSW,0           INIT INV COMMENT SW FOR LATER                
         MVI   INVDLMOD,IVDLMD_K   DOWNLOAD IS DRIVEN BY INVOICE KEY(S)         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,ASER           POINT TO WORK MAP POOL ENTRY                 
         BZ    NXIVK92                                                          
         USING LW_D,RE                                                          
         LHI   R0,1                                                             
         LA    R1,LW_DATA1         SET N'ENTRIES TO 1                           
         CLI   LW_TYPE,LQ_TSINQ    TEST SINGLE ENTRY                            
         BE    *+12                                                             
         ICM   R0,3,LW_NUMN        NO - PICK UP N'ENTRIES FROM POOL             
         LA    R1,LW_DATA2                                                      
         STH   R0,NSER             SET N'ENTRIES TO BE PROCESSED                
         ST    R1,ANXTSER          SET A(NEXT ONE TO BE PROCESSED)              
         B     NXIVK60                                                          
*                                                                               
NXIVK50  LH    R0,NSER             BUMP TO NEXT ENTRY IN POOL                   
         SHI   R0,1                                                             
         BZ    NXIVK96             NO MORE TO PROC, DONE                        
         STH   R0,NSER                                                          
         L     R1,ANXTSER                                                       
         AHI   R1,INVKEYSL                                                      
         ST    R1,ANXTSER                                                       
*                                                                               
NXIVK60  GOTOR GETINV,(R1)         READ INVOICE RECORD                          
         CLI   IOERR,0             ANY ERROR DETECTED?                          
         BE    NXIVK70             TREAT ALL ERROR AS DELETED                   
         MVC   INVKEY(INVKEYSL),0(R1)                                           
         MVI   INVHGSTA,GST_DELQ                                                
         MVI   INVDLSW,0           RESET INVOICE DOWNLOAD SWITCH                
         J     EXITY               TRY NEXT INVOICE KEY                         
*                                                                               
NXIVK70  L     R2,IOADDR           R2=A(INVOICE RECORD)                         
         MVC   QINVKEY,0(R2)       SAVE MINIO MASTER KEY                        
         MVC   MINMKEY(L'PNVKEY),QINVKEY                                        
         BRAS  RE,MININIT          INIT MINIO                                   
         MVC   MINMKEY(L'PNVKEY),QINVKEY                                        
         L     RF,ACOMFACS                                                      
         L     RF,(CMINIO-COMFACSD)(RF)                                         
         GOTOR (RF),PNVPARMS,('MINOPN',MINBLKD)                                 
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NXIVK72  LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVHDRD,RE                                                       
         MVI   PNVHKCDE,PNVHKIDQ   INVOICE HEADER ELEM ID                       
         GOTOR MGETEL,DMCB,ELEM    GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         BNZ   *+6                                                              
         DC    H'0'                THERE MUST BE AN INVOICE HDR ELEM!           
*                                                                               
         BRAS  RE,FMTIVHDR         FORMAT INVOICE HEADER VALUES                 
         MVI   INVCMSW,0           INIT INV COMMENT SW FOR LATER                
         J     EXITY                                                            
*                                                                               
NXIVK90  CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENTS DONE?                        
         BNE   NXIVK96                                                          
         MVI   INVCMSW,0           SET TO DO NEXT HEADER COMMENT                
         B     NXIVK96                                                          
*                                                                               
NXIVK92  GOTOR XC_IVALS,0          CLR ALL INVOICE REPLY VALUES                 
         MVI   INSDLSW,0           RESET INSERTION DOWNLOAD SWITCH              
         MVI   INVDLSW,0           RESET INVOICE DOWNLOAD SWITCH                
*                                                                               
NXIVK96  L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST   NO MORE TO PROCESS                           
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R5,R7,RE                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET INVOICE RECORDS FOR INSERTIONS TO BE DOWNLOADED                 *         
***********************************************************************         
*                                                                               
NXTINV   J     *+12                                                             
         DC    C'*NXTINV*'                                                      
         LR    RB,RF                                                            
         USING NXTINV,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                        
*                                                                               
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         GOTOR XC_IVALS,4          PREPARE TO REPLY INVOICE HDR VALUES          
*                                                                               
         CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   NXINV96                                                          
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXINV50                                                          
         MVI   INVCMSW,0           INIT INV COMMENT SW FOR LATER                
         MVI   INVDLMOD,IVDLMD_B   DL MODE IS DRIVEN BY BUY (INSERTION)         
         XC    IBUREC(IBUL),IBUREC                                              
         MVI   TSACTN,TSARDH       SET TO READ HIGH                             
NXINV18  GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BNZ   NXINV92                                                          
         TM    TSERRS,TSEALF+TSEINIF                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSANXT       SET TO GET NEXT RECORD                       
         OC    IBUREC(L'IBUMEDCD),IBUREC                                        
         BZ    NXINV18                                                          
         OC    IBUREC+L'IBUMEDCD(L'IBUINVS),IBUREC+L'IBUMEDCD                   
         BZ    NXINV18                                                          
*                                                                               
NXINV20  BRAS  RE,MININIT          INIT MINIO                                   
         LA    RE,MINMKEY                                                       
         USING PNVKEY,RE                                                        
         MVC   PNVKAGY,AGY                                                      
         MVC   PNVKMED,IBUMEDCD    MEDIA                                        
         MVI   PNVKRCD,PNVKRCDQ                                                 
         MVC   PNVKSER#,IBUINVS                                                 
         L     RF,ACOMFACS                                                      
         L     RF,(CMINIO-COMFACSD)(RF)                                         
         GOTOR (RF),PNVPARMS,('MINOPN',MINBLKD)                                 
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NXINV22  LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVHDRD,RE                                                       
         MVI   PNVHKCDE,PNVHKIDQ   INVOICE HEADER ELEM ID                       
         GOTOR MGETEL,DMCB,ELEM    GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVHDRLQ,RE),0(RE)                                             
         BNZ   *+6                                                              
         DC    H'0'                THERE MUST BE AN INVOICE HDR ELEM!           
*                                                                               
         BRAS  RE,FMTIVHDR         FORMAT INVOICE HEADER VALUES                 
         B     NXINV98                                                          
*                                                                               
NXINV50  GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BZ    NXINV20             NOT YET, DO NEXT INV KEY IN BUFFER           
*                                                                               
NXINV92  MVI   INVDLSW,0           DONE WITH INVOICE DOWNLOAD                   
*                                                                               
NXINV96  MVI   LP_RMODE,LP_RLAST   NO MORE TO PROCESS                           
*                                                                               
NXINV98  CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENTS DONE?                        
         BNE   *+8                                                              
         MVI   INVCMSW,0           SET TO DO NEXT HEADER COMMENT                
         L     R1,ALP                                                           
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R5,R7,RE                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET INVOICE COMMENTS                                                *         
***********************************************************************         
*                                                                               
GETIVC   J     *+12                                                             
         DC    C'*GETIVC*'                                                      
         LR    RB,RF                                                            
         USING GETIVC,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                        
*                                                                               
         L     R4,LP_BLKS+((B#INVCOM-1)*L'LP_BLKS)                              
*                                                                               
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         GOTOR XC_IVALS,2          PREPARE TO REPLY COMMENT(S)                  
*                                                                               
         CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   GTIVC90                                                          
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY DATES MODE?                      
         BNE   GTIVC10                                                          
         GOTOR XC_IVALS,9          CLEAR ALL, EXCEPT HEADER                     
         B     GTIVC90                                                          
*                                                                               
GTIVC10  CLI   MINMKEY+L'PNVKAGY+L'PNVKMED,PNVKRCDQ                             
         BE    *+6                                                              
         DC    H'0'                MINIO MASTER KEY IS SET AT HDR LV            
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BE    GTIVC20                                                          
         ICM   RE,15,MINELEM       POINT TO ELEMENT FROM PREVIOUS LOOP          
         OC    0(PNVCOMLQ,RE),0(RE)                                             
         BZ    GTIVC90             NOT FOUND, NO NEED TO REPLY                  
         GOTOR MNXTEL,DMCB,ELEM                                                 
         B     GTIVC50                                                          
*                                                                               
GTIVC20  SR    RE,RE                                                            
         ICM   RE,15,MINELEM       POINT TO ELEMENT FROM PREVIOUS LOOP          
         OC    0(08,RE),0(RE)                                                   
         BZ    GTIVC90             NOT FOUND, NO NEED TO REPLY                  
*                                                                               
         GOTOR MNXTEL,DMCB,ELEM                                                 
*                                                                               
GTIVC50  ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVCOMLQ,RE),0(RE)                                             
         BZ    GTIVC90             NOT FOUND, NO NEED TO REPLY                  
*                                                                               
         USING PNVCOMD,RE                                                       
*                                                                               
         CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENT?                              
         BNE   GTIVC54                                                          
         CLI   PNVCKCDE,PNVCKDTQ   INVOICE DETAIL COMMENT CODE?                 
         BE    GTIVC60                                                          
         B     GTIVC90             DETAIL COMMENT ELEM NOT FOUND                
*                                                                               
GTIVC54  CLI   PNVCKCDE,PNVCKHDQ   INVOICE HEADER COMMENT CODE?                 
         BE    GTIVC60                                                          
         B     GTIVC90             HEADER COMMENT ELEM NOT FOUND                
*                                                                               
GTIVC60  CLI   PNVCKTYP,PNVCKCMQ   COMMENT IS GENERAL COMMENTS TYPE?            
         BE    GTIVC64                                                          
         CLI   PNVCKTYP,PNVCKPBQ   COMMENT IS BUYER/PAYER DIALOGUE?             
         BE    GTIVC64                                                          
*                                                                               
         B     GTIVC20             TRY NEXT COMMENT ELEMENT                     
*                                                                               
GTIVC64  CLI   INVCMSW,PNVCKDTQ    DETAIL COMMENT?                              
         BNE   GTIVC66                                                          
         CLC   PNVCKDSQ,INVELMKY+2 DETAIL SEQ NUMBER MATCH?                     
         BNE   GTIVC90             NO, DONE WITH THIS COMMENT                   
*                                                                               
GTIVC66  TM    PNVCSTAT,PNVCDLQ    DELETED?                                     
         BZ    GTIVC68                                                          
*                                                                               
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY?                     
         BNE   GTIVC20                                                          
         MVI   INVCGSTA,GST_DELQ   COMMENT IS DELETED                           
         B     GTIVC92             REPLY DELETED STATUS                         
*                                                                               
GTIVC68  ST    RE,FULL1                                                         
         GOTOR TRANSPID,DMCB,PNVCPID,(L'INVPIDNM,INVPIDNM)                      
         L     RE,FULL1                                                         
         MVC   INVCOMDT,PNVCDATE                                                
*                                                                               
GTIVC80  MVC   FULL2(L'PNVCKCGP),PNVCKCGP                                       
         SR    RF,RF                                                            
         IC    RF,PNVCKLEN                                                      
         SHI   RF,PNVCOMLQ         MINUS COMMENT ELEM OVERHEAD                  
         CHI   RF,0                ANY COMMENT TO BE REPLIED?                   
         BL    GTIVC84             NO, TRY NEXT COMMENT ELEM                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PNVCCOM                                                  
         LA    R4,L'IVCOMMNT(R4)   POINT TO NEXT ENTRY IN ARRAY                 
*                                                                               
GTIVC84  GOTOR MNXTEL,DMCB,ELEM                                                 
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVCOMLQ,RE),0(RE)                                             
         BZ    GTIVC92             NOT FOUND, REPLY CURRENT COMMENT             
*                                                                               
         CLI   PNVCKCDE,PNVCKHDQ   INVOICE HEADER COMMENT CODE?                 
         BE    *+12                                                             
         CLI   PNVCKCDE,PNVCKDTQ   INVOICE DETAIL COMMENT CODE?                 
         BNE   GTIVC92             DO NEXT COMMENT                              
*                                                                               
         CLI   PNVCKTYP,PNVCKCMQ   COMMENT IS GENERAL COMMENTS TYPE?            
         BE    *+12                                                             
         CLI   PNVCKTYP,PNVCKPBQ   COMMENT IS BUYER/PAYER DIALOGUE?             
         BNE   GTIVC92             DO NEXT COMMENT                              
*                                                                               
         CLC   PNVCKCGP,FULL2      STILL IN SAME GROUP?                         
         BE    GTIVC80             YES, CONTINUE COMMENT ARRAY                  
*                                                                               
         B     GTIVC92             DO NEXT COMMENT                              
*                                                                               
GTIVC90  L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST   NO MORE TO PROCESS                           
GTIVC92  J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R5,R7,RE                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET INVOICE DETAIL VALUES                                           *         
***********************************************************************         
*                                                                               
GETIDV   J     *+12                                                             
         DC    C'*GETIDV*'                                                      
         LR    RB,RF                                                            
         USING GETIDV,RB                                                        
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                        
*                                                                               
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         GOTOR XC_IVALS,1          PREPARE TO REPLY DETAIL VALUES               
*                                                                               
         CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   GTIDV96                                                          
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY DATES MODE?                      
         BNE   GTIDV10                                                          
         GOTOR XC_IVALS,9          CLEAR ALL, EXCEPT HEADER                     
         B     GTIDV96                                                          
*                                                                               
GTIDV10  CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GTIDV50                                                          
         MVI   INVCMSW,PNVCKDTQ    SW SET TO DO DETAIL COMMENT LATER            
         CLI   MINMKEY+L'PNVKAGY+L'PNVKMED,PNVKRCDQ                             
         BE    *+6                                                              
         DC    H'0'                MINIO MASTER KEY IS SET AT HDR LV            
*                                                                               
         LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PNVDTLD,RE                                                       
         MVI   PNVDKCDE,PNVDKIDQ   INVOICE DETAIL ELEM ID                       
         MVI   1(RE),1             MATCH ONLY FIRST BYTE (FOR MGETEL)           
         GOTOR MGETEL,DMCB,ELEM    GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    GTIDV96             NOT FOUND, NO NEED TO REPLY                  
                                                                                
*                                                                               
GTIDV20  MVC   INVELMKY,0(RE)      SAVE ELEM KEY FOR NEXT ROUND                 
         CLI   PNVDKTYP,PNVDKDSQ   DETAIL DESCRIPTION ELEMENT?                  
         BNE   GTIDV24             NO, TRY NEXT ELEM                            
         TM    PNVDSTAT,PNVDDLQ    DETAIL ELEM IS DELETED?                      
         BZ    GTIDV30             NO, DO FURTHER CHECKINGS                     
*                                                                               
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY?                     
         BE    GTIDV30             NEED TO REPLY DELETED STATUS                 
*                                                                               
GTIDV24  GOTOR MNXTEL,DMCB,ELEM                                                 
GTIDV25  ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    GTIDV96             NOT FOUND, NO NEED TO REPLY                  
         B     GTIDV20                                                          
*                                                                               
GTIDV30  BRAS  RE,FMTIVDET         FORMAT INVOICE DETAIL VALUES                 
         J     EXITY                                                            
*                                                                               
GTIDV50  CLI   MINMKEY+L'PNVKAGY+L'PNVKMED,PNVKRCDQ                             
         BE    *+6                                                              
         DC    H'0'                MUST HAVE CORRECT MASTER MINIO KEY           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(L'INVELMKY),INVELMKY                                        
         SR    RF,RF                                                            
         ICM   RF,3,INVELMKY+2                                                  
         AHI   RF,1                BUMP UP DETAIL SEQ NUMBER                    
         STCM  RF,3,ELEM+2                                                      
         MVI   ELEM+1,L'INVELMKY                                                
         GOTOR MGETEL,DMCB,ELEM    GET ELEMENT TO RECORD                        
         ICM   RE,15,MINELEM       POINT TO ELEMENT                             
         OC    0(PNVDTLLQ,RE),0(RE)                                             
         BZ    GTIDV96             NOT FOUND, NO NEED TO REPLY                  
         B     GTIDV25                                                          
*                                                                               
GTIDV96  L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST   NO MORE TO PROCESS                           
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R5,R7,RE                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET INSERTION DATA FROM INVOICE CRITERIAS                           *         
***********************************************************************         
*                                                                               
NXTIVB   J     *+12                                                             
         DC    C'*NXTIVB*'                                                      
         LR    RB,RF                                                            
         USING NXTIVB,RB                                                        
*                                                                               
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                        
*                                                                               
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         CLI   INVDLSW,YESQ        DOWNLOADING INVOICES?                        
         BNE   NXIVB94                                                          
         CLI   INVDLMOD,IVDLMD_#   DOWNLOAD BY INVOICE NUMBER?                  
         BE    *+12                                                             
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY(S)?                  
         BNE   NXIVB94                                                          
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXIVB50                                                          
         XC    IBUREC(IBUL),IBUREC                                              
         MVI   TSACTN,TSARDH       SET TO READ HIGH                             
         GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BNZ   NXIVB96                                                          
         MVI   TSACTN,TSANXT       SET TO GET NEXT RECORD                       
NXIVB30  GOTOR GETBUY,IBUINSKY     READ BUY RECORD USING SERIAL NUMBER          
         BNE   NXIVB92             TRY NEXT INSERTION IN BUFFER                 
         L     R2,IOADDR           R2=A(BUY RECORD)                             
         GOTOR (#GETCLT,AGETCLT),DMCB,PBUYKAGY,PBUYKMED,PBUYKCLT                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GETCPR              GET CLIENT PROFILES                          
         GOTOR FMTBUY,0            FORMAT BUY RECORD FOR DOWNLOAD               
*                                                                               
         GOTOR XC_BLCK#,'B#INSARY'                                              
         L     R1,ALP                                                           
         L     R4,LP_BLKS+((B#INSARY-1)*L'LP_BLKS)                              
         USING INSVARY,R4                                                       
*                                                                               
         LA    R1,PBUYFRST         POINT TO FIRST BUY ELEMENT                   
NXIVB36  CLI   0(R1),00            END OF RECORD?                               
         BE    NXIVB46                                                          
         CLI   0(R1),PBNVELQ       INVOICE ELEMENT FOUND?                       
         BE    NXIVB40                                                          
NXIVB38  SR    RE,RE                                                            
         IC    RE,1(R1)                                                         
         AR    R1,RE               BUMP TO NEXT ELEMENT                         
         B     NXIVB36                                                          
         USING PBNVELM,R1                                                       
*                                                                               
NXIVB40  SR    RE,RE                                                            
         ICM   RE,3,BINVTAB#                                                    
         AHI   RE,1                                                             
         CHI   RE,BINVTABM                                                      
         BNH   *+6                                                              
         DC    H'0'                TOO MANY INVOICE ELEM!                       
         STCM  RE,3,BINVTAB#                                                    
         BCTR  RE,0                                                             
         MHI   RE,BINVTABL                                                      
         LA    RF,BINVTAB                                                       
         AR    RF,RE               POINT TO BLANK ENTRY                         
         MVC   0(L'BUYSMED,RF),BUYSMED                                          
         XC    WORK,WORK                                                        
         UNPK  WORK(2*L'PBNVSER#+1),PBNVSER#(L'PBNVSER#+1)                      
         MVI   WORK+2*L'PBNVSER#,C' '                                           
         MVC   L'BUYSMED(2*L'PBNVSER#,RF),WORK                                  
         MVC   L'BINVKEY(L'BINVSEQ#,RF),PBNVDSQN                                
         MVC   L'BINVKEY+L'BINVSEQ#(L'PBNVINV#,RF),PBNVINV#                     
         B     NXIVB38                                                          
*                                                                               
NXIVB46  J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
NXIVB50  GOTOR BUFFER                                                           
         TM    TSERRS,TSEEOF       TEST END OF FILE REACHED                     
         BNZ   NXIVB96                                                          
         B     NXIVB30                                                          
*                                                                               
NXIVB92  GOTOR XC_IVALS,3          CLEAR INSERTION VALUES                       
         L     R1,ALP                                                           
         J     EXITY                                                            
*                                                                               
NXIVB94  GOTOR XC_IVALS,3          CLEAR INSERTION VALUES                       
*                                                                               
NXIVB96  L     R1,ALP              NO MORE RECORDS TO GO                        
         MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
         LTORG                                                                  
         DROP  RB,R7,R5,R4,R3,R1                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET PROFILE VALUES                                                  *         
***********************************************************************         
*                                                                               
GETPRFV  J     *+12                                                             
         DC    C'*GETPRF*'                                                      
         LR    RB,RF                                                            
         USING GETPRFV,RB                                                       
*                                                                               
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#PROFVL-1)*L'LP_BLKS)                        
*                                                                               
         L     R5,LP_BLKS+((B#PROFVL-1)*L'LP_BLKS)                              
         USING PROFVALD,R5                                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GTPRF50                                                          
*                                                                               
         XC    PROFILEV,PROFILEV   CLEAR PROFILE VALUES TO BE REPLIED           
*                                                                               
         CLC   =C'AB',PROFNAME     LOOKING UP "AB" PROFILE?                     
         BE    GTPRF20                                                          
         CLC   =C'BY',PROFNAME     LOOKING UP "BY" PROFILE?                     
         BE    GTPRF20                                                          
*                                                                               
         B     GTPRF50             OTHER PROFILES ARE NOT ALLOWED YET           
*                                                                               
GTPRF20  XC    WORK(12),WORK                                                    
         MVC   WORK+00(2),=C'P0'                                                
         MVC   WORK+02(2),PROFNAME                                              
         MVC   WORK+04(L'AGY),AGY                                               
*                                                                               
         ICM   R1,7,AMED                                                        
         MVC   WORK+06(1),LW_DATA1-LW_D(R1)                                     
         ICM   R1,7,ACLT                                                        
         MVC   WORK+07(3),LW_DATA1-LW_D(R1)                                     
*                                                                               
******** CLI   PCLTOFF,C' '                                                     
******** BNH   *+14                                                             
******** MVI   WORK+10,C'*'                                                     
******** MVC   WORK+11(L'PCLTOFF),PCLTOFF                                       
*                                                                               
         GOTOR VGETPROF,DMCB,WORK,PROFILEV,VDATAMGR                             
         B     GTPRF96                                                          
*                                                                               
GTPRF50  DS    0H                                                               
         MVI   LP_RMODE,LP_RLAST                                                
*                                                                               
GTPRF96  L     R1,ALP              NO MORE RECORDS TO GO                        
         J     EXITY               EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
         LTORG                                                                  
         DROP  RB,R5,R3                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET BUY CUSTOM COLUMN ELEMENT(S)                                    *         
***********************************************************************         
*                                                                               
GETBCC   J     *+12                                                             
         DC    C'*GETBCC*'                                                      
         LR    RB,RF                                                            
         USING GETBCC,RB                                                        
*                                                                               
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         MVC   LP_ADATA,LP_BLKS+((B#BCCBLK-1)*L'LP_BLKS)                        
*                                                                               
         L     R4,LP_BLKS+((B#BCCBLK-1)*L'LP_BLKS)                              
         USING BCCVARY,R4                                                       
*                                                                               
         XC    BUYCCSQ#,BUYCCSQ#   INIT DATA TO BE REPLIED                      
         XC    BUYCCFDA,BUYCCFDA                                                
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   GTBCC50                                                          
         L     R5,AIO2             BUY RECORD IS STILL IN IO2                   
         LA    R5,33(R5)           POINT TO FIRST BUY ELEM                      
         CLI   0(R5),X'20'                                                      
         BNE   GTBCC92             NO BUY RECORD DETECTED                       
*                                                                               
         MVC   TEMP(L'IOKEY),IOKEY                                              
*                                                                               
GTBCC20  CLI   0(R5),0             END OF BUY RECORD?                           
         BE    GTBCC90             YES, DONE WITH BUY CUSTOM COLUMN             
         CLI   0(R5),BYCCIDQ                                                    
         BE    GTBCC30                                                          
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0               BUMP TO NEXT BUY ELEM                        
         B     GTBCC20                                                          
*                                                                               
         USING BYCCELD,R5                                                       
*                                                                               
GTBCC30  MVC   BUYCCSQ#,BYCCSQN    CUSTOM COLUMN SEQ # TO BE REPLIED            
*                                                                               
C        USING PCOLPKEY,IOKEY                                                   
         XC    IOKEY,IOKEY                                                      
         MVC   C.PCOLPAGY,AGY                                                   
         MVI   C.PCOLPMED,C'A'     ALWAYS "A" (FOR CUSTOM COLUMN)               
         MVI   C.PCOLPRCD,X'D1'                                                 
         MVC   C.PCOLPSQN,BYCCSQN                                               
         XC    C.PCOLPSQN,EFFS                                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR+IO1'                            
         CLC   C.PCOLPKEY,IOKEYSAV                                              
         BE    *+6                                                              
         DC    H'0'                CC RECORD MUST BE FOUND!                     
         DROP  C                                                                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         L     R7,AIO1                                                          
         LA    R7,33(R7)           POINT TO FIRST ELEM                          
         USING PCOLELEM,R7                                                      
         CLI   PCOLELEM,X'61'      ELEM PRESENT?                                
         BE    *+6                                                              
         DC    H'0'                INVALID CC RECORD                            
*                                                                               
         CLI   PCOLTYP,C'T'        TEXT?                                        
         BNE   GTBCC34                                                          
         SR    RF,RF                                                            
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL         MINUS ELEM OVERHEAD                          
         BCTR  RF,0                FOR EX                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUYCCFDA(0),BYCCDATA                                             
         B     GTBCC40                                                          
*                                                                               
GTBCC34  CLI   PCOLTYP,C'D'        DATE?                                        
         BE    *+12                                                             
         CLI   PCOLTYP,C'P'        PERIOD (DATE RANGE)?                         
         BNE   GTBCC36                                                          
         MVC   DUB(3),BYCCDATA                                                  
         BRAS  RE,GTBCC_GD         GET DATE (YYYY-MM-DD IN TEMP2)               
         BRAS  RE,GTBCC_FD         FMT DATE (MM-DD-YYYY IN WORK)                
         MVC   BUYCCFDA(10),WORK   REPLY DATE IN MM-DD-YYYY FORMAT              
         SR    RF,RF                                                            
         IC    RF,BYCCLEN                                                       
         SHI   RF,BYCCHDRL         MINUS ELEM OVERHEAD                          
         CHI   RF,3                                                             
         BE    GTBCC40             ONLY ONE DATE                                
         CHI   RF,6                                                             
         BE    *+6                                                              
         DC    H'0'                BAD DATE OR PERIOD CC ELEM!                  
         MVC   DUB(3),BYCCDATA+3                                                
         BRAS  RE,GTBCC_GD         GET DATE (YYYY-MM-DD IN TEMP2)               
         BRAS  RE,GTBCC_FD         FMT DATE (MM-DD-YYYY IN WORK)                
         MVI   BUYCCFDA+10,C'-'    REPLY 2ND DATE AFTER DASH                    
         MVC   BUYCCFDA+11(10),WORK                                             
         B     GTBCC40                                                          
*                                                                               
GTBCC36  CLI   PCOLTYP,C'N'        NUMERIC?                                     
         BE    *+8                                                              
         CLI   PCOLTYP,C'$'        DOLLAR?                                      
         BE    *+12                                                             
         CLI   PCOLTYP,C'%'        PERCENT?                                     
         BNE   GTBCC38                                                          
*                                                                               
         XC    DUB,DUB             DUMMY CURRENY DEFINITION FLD                 
         MVC   DUB+3(1),PCOLDECS                                                
         LA    RF,DUB                                                           
         CURED (P8,BYCCDATA),(20,BUYCCFDA),(RF),ALIGN=LEFT,FLOAT=-              
*                                                                               
         B     GTBCC40                                                          
*                                                                               
GTBCC38  DC    H'0'                INVALID TYPE                                 
*                                                                               
GTBCC40  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0               BUMP TO NEXT BUY ELEM                        
         ST    R5,FULL1            FOR NEXT ROUND                               
         B     GTBCC96                                                          
*                                                                               
GTBCC50  L     R5,FULL1            PICK UP WHERE PREVIOUSLY LEFT                
         B     GTBCC20                                                          
*                                                                               
GTBCC90  MVC   IOKEY,TEMP          RESTORE ORIGINAL KEY                         
GTBCC92  MVI   LP_RMODE,LP_RLAST                                                
*                                                                               
GTBCC96  L     R1,ALP                                                           
         J     EXITY               EXIT TO SEND FORMATTED CC DATA               
*                                                                               
GTBCC_GD ST    RE,FULL2                                                         
         GOTOR VDATCON,DMCB,(3,DUB),(23,TEMP2)                                  
         L     RE,FULL2                                                         
         BR    RE                                                               
*                                                                               
GTBCC_FD MVC   WORK+0(2),TEMP2+5                                                
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),TEMP2+8                                                
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(4),TEMP2+0                                                
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R7,R5,R3                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CLEAR INVOICE VALUES, SO NO VALUES WILL BE REPLIED                  *         
* R1 -> 0 = CLEAR ALL INVOICE VALUES (HDR,DET,COMMENTS,INS)           *         
*       1 = CLEAR DETAIL VALUES ONLY                                  *         
*       2 = CLEAR HEADER/DETAIL COMMENT(S)                            *         
*       3 = CLEAR INSERTION DATA                                      *         
*       4 = CLEAR INVOICE HEADER VALUES ONLY                          *         
*       9 = CLEAR ALL, EXCEPT HEADER                                  *         
***********************************************************************         
*                                                                               
XC_IVALS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         LR    R5,R1                                                            
*                                                                               
         CHI   R5,0                CLEAR ALL INVOICE VALUES?                    
         BNE   XC_IV20                                                          
XC_IV12  L     R1,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R1                                                      
         LA    R1,INVHDRVS         BEGINNING OF INV HDR VALUES                  
         LR    R0,R1                                                            
         LHI   R1,INVHVLQ          L'HEADER VALUES                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CHI   R5,4                CLEAR INVOICE HEADER VALUES ONLY?            
         BE    XC_IV90                                                          
*                                                                               
XC_IV14  L     R1,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
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
         B     XC_IV24             CLEAR DETAIL VALUES                          
*                                                                               
XC_IV20  CHI   R5,1                CLEAR ONLY DETAIL VALUES?                    
         BNE   XC_IV40                                                          
XC_IV24  L     R1,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R1                                                      
         LA    R1,INVDETVS         BEGINNING OFR INV DET VALUES                 
         LR    R0,R1                                                            
         LHI   R1,INVDVLQ          L'DETAIL VALUES                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         CHI   R5,9                CLEAR ALL, EXCEPT HEADER?                    
         BE    XC_IV52                                                          
         B     XC_IV90                                                          
*                                                                               
XC_IV40  CHI   R5,2                CLEAR COMMENT(S)?                            
         BE    XC_IV14                                                          
*                                                                               
XC_IV50  CHI   R5,3                CLEAR INSERTION DATA?                        
         BNE   XC_IV60                                                          
XC_IV52  LA    R0,BUYVALS          BEGINNING OF BUY VALUES                      
         LHI   R1,BUYVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,LP_BLKS+((B#BUY-1)*L'LP_BLKS)                                 
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   BUYCDPCT,PZERO      PACK ZERO WILL NOT BE REPLIED                
*                                                                               
         B     XC_IV90                                                          
*                                                                               
XC_IV60  CHI   R5,4                CLEAR INVOICE HEADER VALUES ONLY?            
         BE    XC_IV12                                                          
*                                                                               
XC_IV70  CHI   R5,9                CLEAR ALL, EXCEPT HEADER?                    
         BE    XC_IV14                                                          
*                                                                               
XC_IV90  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R1                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CLEAR ENTIRE BLOCK, R1 HAS BLOCK NUMBER TO BE CLEARED               *         
***********************************************************************         
*                                                                               
XC_BLCK# NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
*                                                                               
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                BAD PARAMETER                                
         SHI   R1,1                                                             
         MHI   R1,L'LP_BLKS        DISPLACEMENT TO TARGET BLOCK                 
*                                                                               
         LA    RE,LP_BLKS          POINT TO FIRST BLOCK                         
         AR    RE,R1               POINT TO ADDRESS OF TARGET BLOCK #           
         ICM   R0,15,0(RE)         POINT TO START OF BLOCK                      
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* LOOK UP INVOICE ELEMENTS IN INSERTION RECORD AND FORMAT THEM        *         
***********************************************************************         
*                                                                               
F_BIVDAT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR XC_BLCK#,'B#INSARY'                                              
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     R4,LP_BLKS+((B#INSARY-1)*L'LP_BLKS)                              
         USING INSVARY,R4                                                       
*                                                                               
         L     R2,IOADDR           POINT TO BUY RECORD                          
         LA    R3,PBUYFRST         POINT TO FIRST BUY ELEMENT                   
*                                                                               
F_BIV10  CLI   0(R3),00            END OF RECORD?                               
         BE    F_BIV90                                                          
         CLI   0(R3),PBNVELQ       INVOICE ELEMENT FOUND?                       
         BE    F_BIV14                                                          
F_BIV12  SR    RE,RE                                                            
         IC    RE,1(R3)                                                         
         AR    R3,RE               BUMP TO NEXT ELEMENT                         
         B     F_BIV10                                                          
         USING PBNVELM,R3                                                       
*                                                                               
F_BIV14  SR    RE,RE                                                            
         ICM   RE,3,BINVTAB#                                                    
         AHI   RE,1                                                             
         CHI   RE,BINVTABM                                                      
         BNH   *+6                                                              
         DC    H'0'                TOO MANY INVOICE ELEM!                       
         STCM  RE,3,BINVTAB#                                                    
         BCTR  RE,0                                                             
         MHI   RE,BINVTABL                                                      
         LA    RF,BINVTAB                                                       
         AR    RF,RE               POINT TO BLANK ENTRY                         
         MVC   0(L'PBUYKMED,RF),PBUYKMED                                        
         XC    WORK,WORK                                                        
         UNPK  WORK(2*L'PBNVSER#+1),PBNVSER#(L'PBNVSER#+1)                      
         MVI   WORK+2*L'PBNVSER#,C' '                                           
         MVC   L'PBUYKMED(2*L'PBNVSER#,RF),WORK                                 
         MVC   L'BINVKEY(L'BINVSEQ#,RF),PBNVDSQN                                
         MVC   L'BINVKEY+L'BINVSEQ#(L'PBNVINV#,RF),PBNVINV#                     
*                                                                               
* FOR INVOICE DOWNLOAD MODES, NO NEED TO ADD INVOICE KEY TO BUFFER              
*                                                                               
         CLI   INVDLMOD,IVDLMD_#   DOWNLOAD BY INVOICE NUMBER?                  
         BE    F_BIV12                                                          
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY(S)?                  
         BE    F_BIV12                                                          
         CLI   INVDLMOD,IVDLMD_D   DOWNLOAD BY INV START/END DATES?             
         BE    F_BIV12                                                          
*                                                                               
         CLI   INVDLSW,YESQ        DOWNLOAD INVOICE DATA?                       
         BNE   F_BIV12                                                          
         XC    IBUREC(IBUL),IBUREC                                              
         MVC   IBUMEDCD,PBUYKMED   MEDIA CODE                                   
         MVC   IBUINVS,PBNVSER#    INVOICE SERIAL NUMBER                        
         MVI   TSACTN,TSAADD       ADD INVOICE KEY TO BUFFER                    
         GOTOR BUFFER                                                           
         CLI   TSERRS,0            NO ERROR?                                    
         BE    F_BIV12                                                          
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         BE    F_BIV12                                                          
         DC    H'0'                OTHER ERROS ARE NO GOOD                      
*                                                                               
F_BIV90  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R1,R3,R4                                                      
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT INVOICE HEADER DATA                                          *         
***********************************************************************         
*                                                                               
FMTIVHDR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
*                                                                               
         LA    R7,MNELEM           POINT TO ELEMENT                             
         USING PNVHDRD,R7                                                       
*                                                                               
         CLI   PNVHKCDE,PNVHKIDQ   HEADER ELEMENT CODE?                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,MNBLKCB                                                       
         USING MINBLKD,RF                                                       
         LA    RF,MINMKEY                                                       
         USING PNVKEY,RF                                                        
         MVC   INVKEY(L'PNVKMED),PNVKMED                                        
         XC    WORK,WORK                                                        
         UNPK  WORK(2*L'PNVKSER#+1),PNVKSER#(L'PNVKSER#+1)                      
         MVI   WORK+2*L'PNVKSER#,C' '                                           
         MVC   INVKEY+L'PNVKMED(2*L'PNVKSER#),WORK                              
*                                                                               
         MVC   INVCLTC,PNVHCLT                                                  
*                                                                               
         GOTOR VPUBEDIT,DMCB,(X'08',PNVHPUB),(C'S',INVHPUBC)                    
*                                                                               
         MVC   INVVNDR#,PNVHINV#                                                
*                                                                               
         MVC   INVSTAT,PNVHSTAT                                                 
*                                                                               
         MVC   INVPSTR,PNVHSTRT                                                 
*                                                                               
         MVC   INVPEND,PNVHEND                                                  
*                                                                               
         MVC   INVDATE,PNVHDATE                                                 
*                                                                               
         MVC   INVTOTL,PNVHTOT                                                  
*                                                                               
         MVC   INVTTYP,PNVH$TYP                                                 
*                                                                               
         MVC   INVSREP,PNVHSREP                                                 
*                                                                               
         J     EXIT                EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
         LTORG                                                                  
         DROP  RB,R7,R5,R3,RF                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT INVOICE DETAIL DATA                                          *         
***********************************************************************         
*                                                                               
FMTIVDET NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ALP                                                           
         USING LP_D,R3                                                          
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LA    R7,MNELEM           POINT TO ELEMENT                             
         USING PNVDTLD,R7                                                       
*                                                                               
         CLI   PNVDKCDE,PNVDKIDQ   DETAIL ELEMENT CODE?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PNVDKTYP,PNVDKDSQ   TYPE IS DETAIL DESCRIPTION?                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    INVINSKY,INVINSKY                                                
*                                                                               
         TM    PNVDSTAT,PNVDDLQ    DETAIL ELEM IS DELETED?                      
         BZ    FTIVD05             BZ=NOT DELETED                               
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY?                     
         BNE   FTIVD80             DO NOT REPLY ANY DETAIL DATA                 
         MVC   INVISQN,PNVDKSQN                                                 
         MVI   INVDGSTA,GST_DELQ   DETAIL IS DELETED                            
         BRAS  RE,FVD_INSK                                                      
         B     FTIVD80             DONE WITH DETAIL DATA REPLY                  
*                                                                               
FTIVD05  MVC   INVISQN,PNVDKSQN                                                 
*                                                                               
         BRAS  RE,FVD_INSK                                                      
*                                                                               
         CLI   INVDLMOD,IVDLMD_#   DOWNLOAD BY INVOICE NUMBER?                  
         BE    FTIVD10                                                          
         CLI   INVDLMOD,IVDLMD_K   DOWNLOAD BY INVOICE KEY(S)?                  
         BNE   FTIVD20                                                          
         CLI   INSDLSW,NOQ         NEED TO DOWNLOAD INSERTION DATA?             
         BE    FTIVD20                                                          
FTIVD10  XC    IBUREC(IBUL),IBUREC                                              
         MVC   IBUINSKY,INVINSKY   ONLY INSERTION KEY NEEDED                    
         OC    IBUREC(IBUL),IBUREC                                              
         BZ    FTIVD20             NOTHING TO ADD TO BUFFER                     
         MVI   TSACTN,TSAADD       ADD INVOICE DATA TO BUFFER                   
         GOTOR BUFFER              NO NEED TO CHECK FOR ERROR                   
         CLI   TSERRS,0            NO ERROR?                                    
         BE    FTIVD20                                                          
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         BE    FTIVD20                                                          
         DC    H'0'                OTHER ERROS ARE NO GOOD                      
*                                                                               
FTIVD20  OC    PNVDRATE,PNVDRATE   NULLS?                                       
         BZ    FTIVD40             YES, NO RATE TO BE REPLIED                   
         CP    PNVDRATE,PZERO      PACKED ZERO?                                 
         BNE   *+14                                                             
         MVC   INVRATE(L'FREE),FREE                                             
         B     FTIVD40                                                          
*                                                                               
         LA    R4,INVRATE                                                       
         CLI   PNVDCSTP,C'N'       NET AMOUNT                                   
         BNE   FTIVD20D                                                         
         MVI   0(R4),C'N'                                                       
         B     FTIVD20H                                                         
FTIVD20D CLI   PNVDCSIN,0                                                       
         BE    FTIVD20M                                                         
         CLI   PNVDCSIN,C' '                                                    
         BE    FTIVD20M                                                         
         MVC   0(L'PNVDCSIN,R4),PNVDCSIN                                        
FTIVD20H LA    R4,1(R4)                                                         
FTIVD20M EDIT  (P6,PNVDRATE),(15,(R4)),2,ALIGN=LEFT,FLOAT=-                     
         OC    INVRATE,SPACES                                                   
*                                                                               
FTIVD40  OC    PNVDPREM,PNVDPREM                                                
         BNZ   *+12                SKIP IF NO PREM                              
         CLI   PNVDNCL,0                                                        
         BE    FTIVD60             AND NO COLORS                                
*                                                                               
         OC    PNVDPREM,PNVDPREM                                                
         BZ    FTIVD40F            NO PREIMUM, DO COLOR                         
*                                                                               
         CP    PNVDPREM,PZERO      ZERO COST?                                   
         BNZ   *+14                                                             
         MVC   INVPREM(L'FREE),FREE                                             
         B     FTIVD60             SET PREMIUM TO 'FREE'                        
*                                                                               
FTIVD40F LA    R4,INVPREM                                                       
*                                                                               
         CLI   PNVDNCL,0           COLORS PRESENT?                              
         BE    FTIVD40H                                                         
         MVC   0(1,R4),PNVDNCL     NUMBER OF COLORS                             
         OI    0(R4),X'F0'         FILL IN ZONE                                 
         MVI   1(R4),C'C'          INDICATE COLORS                              
         LA    R4,2(R4)            BUMP OUTPUT POINTER                          
         OC    PNVDPREM,PNVDPREM                                                
         BZ    FTIVD60             DONE IF NO PREMIUM COST                      
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
FTIVD40H CLI   PNVDPRTP,C' '       COST TYPE PRESENT?                           
         BNH   *+14                                                             
         MVC   0(1,R4),PNVDPRTP                                                 
         LA    R4,1(R4)                                                         
*                                                                               
         CLI   PNVDPRIN,C' '       COST INDICATOR PRESENT?                      
         BNH   *+14                                                             
         MVC   0(1,R4),PNVDPRIN                                                 
         LA    R4,1(R4)                                                         
*                                                                               
         EDITR (P6,PNVDPREM),(15,(R4)),2,ALIGN=LEFT,FLOAT=-,           +        
               ZERO=NOBLANK,IZERO=Y                                             
         OC    INVPREM,SPACES                                                   
*                                                                               
FTIVD60  MVC   INVNET,PNVDNET                                                   
*                                                                               
         MVC   INVGROSS,PNVDGRS                                                 
*                                                                               
         MVC   INV#LINE,PNVD#LIN                                                
*                                                                               
         MVC   INVINSDT,PNVDDTE                                                 
*                                                                               
         MVC   INVSPDSP,PNVDSPC                                                 
*                                                                               
         MVC   INVADCAP(L'PNVDACAP),PNVDACAP                                    
         MVC   INVADCAP+L'PNVDACAP(L'PNVDACP2),PNVDACP2                         
*                                                                               
         MVC   INVCLTCD,PNVDCLT                                                 
*                                                                               
         MVC   INVPRDCD,PNVDPRD                                                 
*                                                                               
         OC    PNVDPUB,PNVDPUB                                                  
         BZ    FTIVD80                                                          
         GOTOR VPUBEDIT,DMCB,(0,PNVDPUB),(C'S',INVDPUBC)                        
*                                                                               
FTIVD80  DS    0H                                                               
         J     EXIT                EXIT TO SEND FORMATTED BUY RECORD            
*                                                                               
FVD_INSK DS    0H                  FORMAT INSERTION KEY                         
         OC    PNVDSER#,PNVDSER#                                                
         BZ    FVD_IKX                                                          
         CP    PNVDSER#,PZERO                                                   
         BE    FVD_IKX                                                          
         LA    RF,MNBLKCB                                                       
         USING MINBLKD,RF                                                       
         LA    RF,MINMKEY                                                       
         USING PNVKEY,RF                                                        
         MVC   INVINSKY(L'PNVKMED),PNVKMED                                      
         MVC   INVINSKY+L'PNVKMED(L'PNVDCLT),PNVDCLT                            
         XC    WORK,WORK                                                        
         UNPK  WORK(2*L'PNVDSER#-1),PNVDSER#                                    
         OI    WORK+(2*L'PNVDSER#-1),X'F0'                                      
         MVC   INVINSKY+L'PNVKMED+L'PNVDCLT(2*L'PNVDSER#-1),WORK                
FVD_IKX  BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R7,R5,R3,RF                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* OPTIONALLY FILTER AND FORMAT BUY FOR DOWNLOADING                    *         
*                                                                     *         
* NTRY - R1=ZERO TO FORMAT ONLY, 1 TO FILTER AND FORMAT               *         
* EXIT - CC=NOT EQUAL IF BUY DIDN'T PASS A FILTER TEST                *         
***********************************************************************         
                                                                                
FMTBUY   NTR1  BASE=*,LABEL=*                                                   
         STC   R1,FLAG             SAVE CALL TYPE                               
                                                                                
         LA    R0,BUYVALS          CLEAR EXTRACTED BUY VALUES                   
         LHI   R1,BUYVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   BUYBSTAT,BUYBSLIV   SET BUY STATUS                               
         CLI   PBDBFD,C'T'         IS THIS A TEST BUY                           
         BNE   *+8                                                              
         MVI   BUYBSTAT,BUYBSTST                                                
         TM    PBUYCNTL,X'80'      IS THIS BUY DELETED                          
         BZ    *+8                                                              
         OI    BUYBSTAT,BUYBSDEL                                                
                                                                                
         CLI   FLAG,0              TEST APPLYING FILTERS                        
         BE    FMTBUY06            NO                                           
         TM    FILTSTAT,FILTSLUD+FILTSTUD+FILTSLDE+FILTSTDE                     
         BNM   FMTBUY04                                                         
                                                                                
         TM    BUYBSTAT,BUYBSLIV   TEST LIVE BUY                                
         BZ    FMTBUY02                                                         
         TM    BUYBSTAT,BUYBSDEL   TEST LIVE DELETE                             
         BNZ   *+16                                                             
         TM    FILTSTAT,FILTSLUD   TEST WANT LIVE UNDELETED BUYS                
         JZ    EXITN                                                            
         B     FMTBUY04                                                         
         TM    FILTSTAT,FILTSLDE   TEST WANT LIVE DELETED BUYS                  
         JZ    EXITN                                                            
         B     FMTBUY04                                                         
                                                                                
FMTBUY02 TM    BUYBSTAT,BUYBSDEL   TEST TEST DELETE                             
         BNZ   *+16                                                             
         TM    FILTSTAT,FILTSTUD   TEST WANT TEST UNDELETED BUYS                
         JZ    EXITN                                                            
         B     FMTBUY04                                                         
         TM    FILTSTAT,FILTSTDE   TEST WANT TEST DELETED BUYS                  
         JZ    EXITN                                                            
                                                                                
FMTBUY04 OC    PBDJOB,SPACES                                                    
         OC    FILTADNO,FILTADNO   TEST AD NUMBER FILTER SET                    
         BZ    FMTB_F10                                                         
         CLC   PBDJOB,FILTADNO     YES - APPLY FILTER                           
         JNE   EXITN                                                            
*                                                                               
FMTB_F10 TM    FLTCLRST,FLTCSCLR+FLTCSNCL                                       
         BNM   FMTB_F20                                                         
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
         MVI   BYTE2,0             INIT TEMP CLERANCE STATUS SWITCH             
FMTB_F11 CLI   0(R3),0             END OF BUY RECORD?                           
         BE    FMTB_F14                                                         
         USING PPAYELEM,R3                                                      
         CLI   PPAYELEM,PPAYELQ    TEST PAYMENT ELEMENT                         
         BNE   FMTB_F12                                                         
         OC    PPDDATE,PPDDATE                                                  
         BZ    FMTB_F12                                                         
         MVI   BYTE2,C'C'          STATUS IS CLEARED                            
         B     FMTB_F14                                                         
FMTB_F12 SR    R0,R0               BUMP TO NEXT ELEMENT ON RECORD               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTB_F11                                                         
FMTB_F14 CLI   BYTE2,C'C'          CLEARED?                                     
         BNE   FMTB_F16                                                         
         TM    FLTCLRST,FLTCSCLR   FILTER WANTS CLEARED?                        
         JZ    EXITN                                                            
         B     FMTB_F20                                                         
FMTB_F16 TM    FLTCLRST,FLTCSNCL   FILTER WANTS NOT CLEARED?                    
         JZ    EXITN                                                            
*                                                                               
FMTB_F20 TM    FLTMATST,FLTMSPEN+FLTMSMAT+FLTMSDIS                              
         BNM   FMTB_F30                                                         
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
FMTB_F22 CLI   0(R3),0             END OF BUY RECORD?                           
         JE    EXITN               INVOICE MATCHING ELEM NOT FOUND              
         USING PBMATELD,R3                                                      
         CLI   PBMATELM,PBMATELQ   INVOICE MATCHING STATUSES ELEM?              
         BE    FMTB_F24                                                         
         SR    R0,R0               BUMP TO NEXT ELEMENT ON RECORD               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTB_F22                                                         
FMTB_F24 CLI   PBMTSTAT,PBMTSPDQ   PENDING?                                     
         BNE   FMTB_F25                                                         
         TM    FLTMATST,FLTMSPEN   FILTER WANTS PENDING?                        
         JZ    EXITN                                                            
         B     FMTB_F30                                                         
FMTB_F25 CLI   PBMTSTAT,PBMTSMTQ   MATCHED?                                     
         BNE   FMTB_F26                                                         
         TM    FLTMATST,FLTMSMAT   FILTER WANTS MATCHED?                        
         JZ    EXITN                                                            
         B     FMTB_F30                                                         
FMTB_F26 CLI   PBMTSTAT,PBMTSDSQ   DISCREPANT?                                  
         JNE   EXITN               NO OTHER INVOICE MATCHING STATUSES           
         TM    FLTMATST,FLTMSDIS   FILTER WANTS DISCREPANT?                     
         JZ    EXITN                                                            
*                                                                               
FMTB_F30 DS    0X                  FOR FUTURE FILTERS                           
*                                                                               
FMTBUY06 GOTOR VPUBEDIT,DMCB,(X'08',PBUYKPUB),(C'S',BUYPUB)                     
                                                                                
         GOTOR VGETINS,DMCB,PBUYRECD,PVALUES,PBUYKPRD,(C'A',0),,ALL             
                                                                                
         MVC   BUYGRSAC,GROSS      EXTRACT GROSS ADDITIONAL CHARGES             
         L     R1,GROSS                                                         
         S     R1,AGYCOM                                                        
         STCM  R1,15,BUYNETAC      SET NET ADDITIONAL CHARGES                   
         MVC   BUYGLCAC,BLABLE     EXTRACT BILLABLE ADDITIONAL CHARGES          
         MVC   BUYNLCAC,PYABLE     EXTRACT PAYABLE ADDITIONAL CHARGES           
                                                                                
         GOTOR VGETINS,DMCB,PBUYRECD,PVALUES,PBUYKPRD,(C'X',0)                  
                                                                                
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
*                                                                               
         ICM   RE,15,GROSS                                                      
         ICM   RF,15,PGROSS                                                     
         SR    RE,RF                                                            
         STCM  RE,15,BUYGPYBL      GROSS-SUM(PPGROSS)                           
*                                                                               
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
*                                                                               
         USING PPBYOUTD,WORKAREA                                                
         LA    R0,PBUYRECD                                                      
         ST    R0,PBYOINPT                                                      
         LA    R0,PVALUES                                                       
         ST    R0,PBYOVALS                                                      
         MVC   PBYODTCN,VDATCON                                                 
         GOTOR VPPBYOUT,DMCB,PPBYOUTD                                           
         MVC   BUYSPACE,PBYOSPC1   SET SPACE/DESCRIPTION                        
*                                                                               
         CLI   PBUYKMED,ODORMEDQ   TEST IF OUTDOOR                              
         BNE   FMTBUY08                                                         
         CLI   PBDSPACE,FF         YES - TEST FOR SHOWING                       
         BNE   *+10                                                             
         CP    PBDSHOW,PZERO                                                    
         BNE   *+10                                                             
         CP    PBDREG,PZERO                                                     
         BNE   *+10                                                             
         CP    PBDILLUM,PZERO                                                   
         BE    FMTBUY08                                                         
         MVC   BUYSPACE,SPACES     YES - CLEAR SPACE/DESCRIPTION                
*                                                                               
FMTBUY08 CLI   PBUYKMED,NEWSMEDQ                                                
         BNE   *+8                                                              
         GOTOR FMTNSD              FORMAT NEWSPAPER SPACE/DESCRIPTION           
*                                                                               
         OC    BUYSPACE,SPACES     ENSURE NO TRAILING BINARY ZEROES             
*                                                                               
         CLI   FLAG,0              TEST APPLYING FILTERS                        
         BE    FMTBUY10            NO                                           
         OC    FILTDESC,FILTDESC   TEST SPACE/DESCRIPTION FILTER SET            
         BZ    FMTBUY10                                                         
         CLC   BUYSPACE,FILTDESC   YES - APPLY FILTER                           
         JNE   EXITN                                                            
*                                                                               
FMTBUY10 BRAS  RE,F_BIVDAT         FORMAT BUY INVOICE DATA                      
*                                                                               
         LA    R3,PBUYFRST         R3=A(BUY RECORD ELEMENT)                     
*                                                                               
         GOTOR FMTLIN              FORMAT INSERTION DATE/LINE NUMBER            
*                                                                               
         CLC   PBUYKPRD,ZZZ        SEE IF ZZZ INSERTION                         
         BNE   *+8                                                              
         GOTOR FMTPRD              FORMAT ALLOCATIONS                           
*                                                                               
         MVC   BUYCDPCT,PBDCD      SET CASH DISCOUNT PERCENTAGE                 
         ZAP   BUYACP,PBDACP       SET AGENCY COMMISSION PERCENT                
         CP    BUYACP,=P'-1'       TEST SPECIAL VALUE FOR 100%                  
         BNE   *+10                                                             
         ZAP   BUYACP,=P'100000'                                                
                                                                                
*                                                                               
         TM    PBDSTAT,X'10'       TEARSHEET RECEIVED?                          
         BZ    *+8                                                              
         MVI   BUYTSRST,YESQ       YES                                          
*                                                                               
         MVC   BUYSFH,SPACES                                                    
         TM    PBDSTAT,X'0C'       TEST SPECIAL FINANCIAL HANDLING              
         BZ    FMTBUY12                                                         
         MVC   BUYSFH,=C'HOLD'                                                  
         TM    PBDSTAT,X'08'                                                    
         BNZ   FMTBUY12                                                         
         MVC   BUYSFH,=C'REL '                                                  
                                                                                
FMTBUY12 CLI   PBDSPACE,FF         TEST OUTDOOR                                 
         BNE   FMTBUY14                                                         
         ZAP   BUYREGDS,PBDREG     SET NUMBER OF REGULAR DISPLAYS               
         ZAP   BUYILLPA,PBDILLUM   SET NUMBER OF ILLUMINATED DISPLAYS           
         MVC   BUYSHOWS,SPACES                                                  
         CP    PBDSHOW,=P'99999'   SPECIAL VALUE FOR SPC (SPECIALS)             
         BNE   *+14                                                             
         MVC   BUYSHOWS(3),=C'SPC'                                              
         B     FMTBUY16                                                         
         EDIT  PBDSHOW,(5,BUYSHOWS),ALIGN=LEFT                                  
         B     FMTBUY16            STILL DO RATES                               
                                                                                
FMTBUY14 CLI   PBUYKMED,NEWSMEDQ   TEST NEWSPAPERS                              
         BNE   FMTBUY16                                                         
         GOTOR FMTNRT              FORMAT NEWSPAPER RATES                       
         GOTOR FMTNPR              FORMAT NEWSPAPER PREMIUMS                    
         GOTOR FMTNCL              FORMAT NEWSPAPER CONTRACT LINEAGE            
         B     FMTBUY18                                                         
                                                                                
FMTBUY16 GOTOR FMTORF              NON-NEWSPAPER RATE FORMAT                    
                                                                                
FMTBUY18 CLI   0(R3),0             TEST END OF RECORD                           
         BE    FMTBUY62                                                         
                                                                                
         USING PSERELEM,R3                                                      
         CLI   PSERELEM,PSEREQ     TEST SERIAL NUMBER ELEMENT                   
         BNE   FMTBUY20                                                         
         MVC   BUYSMED,PBUYKMED    YES - BUILD SERIAL NUMBER                    
         MVC   BUYSCLT,PBUYKCLT                                                 
         UNPK  BUYSSER,PSERNUM                                                  
         OI    BUYSSER+L'BUYSSER-1,X'F0'                                        
         B     FMTBUY60                                                         
                                                                                
         USING PBINVELM,R3                                                      
FMTBUY20 CLI   PBINVELM,PBINVEQ    TEST INVOICE ELEMENT                         
         BNE   FMTBUY22                                                         
         MVC   BUYMCHIN,PBINVNUM   SET PAID INVOICE NUMBER                      
         MVC   BUYMCHDT,PBINVMDT   SET MATCHED DATE                             
         B     FMTBUY60                                                         
                                                                                
         USING PPAYELEM,R3                                                      
FMTBUY22 CLI   PPAYELEM,PPAYELQ    TEST PAYMENT ELEMENT                         
         BNE   FMTBUY24                                                         
         OC    PPDDATE,PPDDATE                                                  
         BZ    FMTBUY60                                                         
         STCM  R3,15,BUYAPAYE      SAVE A(PAYMENT ELEMENT)                      
         MVI   BUYCLRST,C'C'       CLEARED IF PAY ELEM W/ DATE IS FOUND         
         B     FMTBUY60                                                         
                                                                                
         USING PBILELEM,R3                                                      
FMTBUY24 CLI   PBILELEM,PBILELQ    TEST BILLING ELEMENT                         
         BNE   FMTBUY26                                                         
         OC    PBLDATE,PBLDATE     TEST REAL BILLING ELEMENT                    
         BZ    FMTBUY60                                                         
         STCM  R3,15,BUYABILE      SAVE A(BILLING ELEMENT)                      
         B     FMTBUY60                                                         
                                                                                
         USING PACELEM,R3                                                       
FMTBUY26 CLI   PACELEM,PACELQ      TEST ADDITIONAL CHARGE ELEMENT               
         BNE   FMTBUY40                                                         
         L     R4,AIO6                                                          
         USING ACHTABD,R4                                                       
FMTBUY28 CLI   ACHMED,0            TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACHMED,PBUYKMED     MATCH MEDIA CODE TO CHARGE TABLE             
         BNE   *+14                                                             
         CLC   ACHCODE,PACCODE     AND ADDITIONAL CHARGE CODE                   
         BE    *+12                                                             
         AHI   R4,ACHTABL          NO MATCH - BUMP TO NEXT TABLE ENTRY          
         B     FMTBUY28                                                         
                                                                                
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
         BZ    FMTBUY36            NO AMT PRESENT                               
                                                                                
         MVC   BUYATCRG(1),PACGN                                                
         CLI   PACGN,C'G'          GROSS AMOUNT?                                
         BE    FMTBUY30                                                         
         CLI   PACGN,C'N'          NET AMOUNT?                                  
         BE    FMTBUY32                                                         
         DC    H'0'                NO OTHER TYPE OF AMT FOR NOW                 
                                                                                
FMTBUY30 ZAP   DUB2,PACAMT         AMT IN GROSS, NO CALCULATIONS NEEDED         
         B     FMTBUY34                                                         
                                                                                
FMTBUY32 CLI   PACAC,C'N'          SUBJECT TO COMMISSION?                       
         BE    FMTBUY30            NO, NET AND GROSS ARE SAME                   
                                                                                
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
                                                                                
FMTBUY34 CP    DUB2,PZERO                                                       
         BNE   *+14                                                             
         MVC   BUYATCRG+1(4),=C'0.00'                                           
         B     FMTBUY36                                                         
         EDIT  (P8,DUB2),(11,BUYATCRG+1),2,ALIGN=LEFT,FLOAT=-                   
                                                                                
FMTBUY36 MVC   BUYATCPC,PACACOM    SET COMMISSION %                             
         CLI   PACAC,YESQ          TEST SUBJECT TO COMMISSION                   
         BNE   *+8                                                              
         MVI   BUYATCOM,YESQ       SET SUBJECT TO COMMISSION                    
         CLI   PACCD,YESQ          TEST SUBJECT TO CASH DISCOUNT                
         BNE   *+8                                                              
         MVI   BUYATSCD,YESQ       SET SUBJECT TO CASH DISCOUNT                 
         B     FMTBUY60                                                         
         DROP  R5,R4                                                            
                                                                                
         USING PCOMEL,R3                                                        
FMTBUY40 LA    R1,BUYREGC#                                                      
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
         BNE   FMTBUY42                                                         
                                                                                
         SR    RE,RE                                                            
         IC    RE,0(R1)            RE=N'COMMENT LINES SO FAR                    
         CHI   RE,COMMAXQ                                                       
         BE    FMTBUY60                                                         
         LA    RF,1(RE)                                                         
         STC   RF,0(R1)            SET N'COMMENT LINES NOW                      
         MHI   RE,COMLENQ                                                       
         LA    RE,1(RE,R1)         RE=A(COMMENT LINE)                           
         MVC   0(COMLENQ,RE),SPACES                                             
         IC    RF,PCOMLEN                                                       
         SHI   RF,PCOMLN1Q+1                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),PCOMMENT    MOVE COMMENT TO OUTPUT AREA                  
         B     FMTBUY60                                                         
                                                                                
         USING PIOELEM,R3                                                       
FMTBUY42 CLI   PIOELEM,PIOELQ      TEST INSERTION ORDER ELEMENT                 
         BNE   FMTBUY44                                                         
         OC    PIODATE,PIODATE                                                  
         BZ    FMTBUY60                                                         
         STCM  R3,15,BUYAIORE      SAVE A(INSERTION ORDER ELEMENT)              
         B     FMTBUY60                                                         
                                                                                
         USING PICONELD,R3                                                      
FMTBUY44 CLI   PICONEL,PICONEQ     TEST INTERNET CONTRACT ELEMENT               
         BNE   FMTBUY50                                                         
         MVC   BUYICNUM,PICCONN    CONTRACT NUMBER                              
         MVC   BUYICIMP,PICIMPS    TOTAL IMPRESSIONS                            
         MVC   BUYICCPM,PICCPM     TOTAL CPM                                    
         MVC   WORK2(L'BUYICRAT),SPACES                                         
         EDIT  PICRATE,(10,WORK2),2,ALIGN=LEFT                                  
         CLI   PICRIND,C'N'        NET INDICATOR                                
         BNE   FMTBUY46                                                         
         MVC   BUYICRAT(L'PICRIND),PICRIND                                      
         MVC   BUYICRAT+L'PICRIND(L'BUYICRAT-L'PICRIND),WORK2                   
         B     FMTBUY48                                                         
FMTBUY46 MVC   BUYICRAT,WORK2                                                   
FMTBUY48 MVC   BUYICINS,PICINS     NUMBER OF INSERTIONS                         
         B     FMTBUY60                                                         
                                                                                
         USING PPAGEVEL,R3                                                      
FMTBUY50 CLI   PPAGEVEL,PPAGEVQ    TEST PAGE VIEWS ELEMENT                      
         BNE   *+14                                                             
         MVC   BUYVIEWS,PPAGEVS    SET NUMBER OF PAGE VIEWS                     
         B     FMTBUY60                                                         
                                                                                
         USING PBSHPDEL,R3                                                      
         CLI   PBSHPDEL,PBSHPDEQ   TEST SHIP DATE ELEMENT                       
         BNE   *+14                                                             
         MVC   BUYSHPDT,PBSHDATE   SET SHIP DATE                                
         B     FMTBUY60                                                         
                                                                                
         USING PBSREPEL,R3                                                      
         CLI   PBSREPEL,PBREPEQ    TEST SPECIAL REP ELEMENT                     
         BNE   *+14                                                             
         MVC   BUYSREP,PBSREP      SET SPECIAL REP                              
         B     FMTBUY60                                                         
                                                                                
         USING PCLCKTEL,R3                                                      
         CLI   PCLCKTEL,PCLCKTEQ   TEST CLICK THRUS ELEMENT                     
         BNE   *+14                                                             
         MVC   BUYCLICK,PCLCKTS                                                 
         B     FMTBUY60                                                         
                                                                                
         USING PBREFEL,R3                                                       
         CLI   PBREFEL,PBREFELQ    TEST REFERENCE NUMBER ELEMENT                
         BNE   *+14                                                             
         MVC   BUYREFNO,PBREFNO    SET REFERENCE NUMBER                         
         B     FMTBUY60                                                         
                                                                                
         USING PBDECEL,R3                                                       
         CLI   PBDECEL,PBDECELQ    TEST DAILY EFF. CIRCULATION ELEMENT          
         BNE   *+14                                                             
         MVC   BUYDECIR,PBDEC      SET NUMBER OF FREE STANDING INSERTS          
         B     FMTBUY60                                                         
                                                                                
         USING PBRPTEL,R3                                                       
         CLI   PBRPTEL,PBRPTELQ    TEST NUMBER OF REPAINTS ELEMENT              
         BNE   *+14                                                             
         MVC   BUYREPTS,PBRPTNO    SET NUMBER OF FREE STANDING INSERTS          
         B     FMTBUY60                                                         
                                                                                
         USING PIMPRSEL,R3                                                      
         CLI   PIMPRSEL,PIMPRSEQ   TEST ESTIMATED IMPRESSIONS ELEMENT           
         BNE   *+14                                                             
         MVC   BUYEIMPS,PIMPRS     SET NUMBER OF ESTIMATED IMPRESSIONS          
         B     FMTBUY60                                                         
                                                                                
         USING PAIMSPEL,R3                                                      
         CLI   PAIMSPEL,PAIMSPEQ   TEST ACTUAL IMPRESSIONS ELEMENT              
         BNE   *+14                                                             
         MVC   BUYAIMPS,PAIMPRS    SET NUMBER OF ACTUAL IMPRESSIONS             
         B     FMTBUY60                                                         
                                                                                
         USING PECPMELD,R3                                                      
         CLI   PECPMEL,PECPMEQ     ESTIMATED CPM                                
         BNE   *+14                                                             
         MVC   BUYECPM,PECPM                                                    
         B     FMTBUY60                                                         
                                                                                
         USING PACPMELD,R3                                                      
         CLI   PACPMEL,PACPMEQ     ACTUAL CPM                                   
         BNE   *+14                                                             
         MVC   BUYACPM,PACPM                                                    
         B     FMTBUY60                                                         
                                                                                
         USING PWSJELD,R3                                                       
         CLI   PWSJELEM,PWSJEQ     WSJ INSERTION                                
         BNE   *+12                                                             
         MVI   BUYWSJ,YESQ                                                      
         B     FMTBUY60                                                         
                                                                                
         USING PEXDAYEL,R3                                                      
         CLI   PEXDAYEL,PEXDAYEQ   MATERIALS CLOSING EXTENSION DAYS             
         BNE   *+14                                                             
         MVC   BUYMCXDY,PEXDAYS                                                 
         B     FMTBUY60                                                         
                                                                                
         USING PEXDATEL,R3                                                      
         CLI   PEXDATEL,PEXDTELQ   MATERIALS CLOSING EXTENSION DATE             
         BNE   *+14                                                             
         MVC   BUYMCLXD,PEXDATE                                                 
         B     FMTBUY60                                                         
                                                                                
         USING PISITEEL,R3                                                      
         CLI   PISITEEL,PISITELQ   INTERNET SITE LOCATION                       
         BNE   *+14                                                             
         MVC   BUYISITE,PISITE                                                  
         B     FMTBUY60                                                         
                                                                                
         USING PISNMELM,R3                                                      
         CLI   PISNMELM,PISNMELQ   ISSUE NAME?                                  
         BNE   *+14                                                             
         MVC   BUYISSNM,PISNAME                                                 
         B     FMTBUY60                                                         
                                                                                
         USING PIUPEL,R3                                                        
         CLI   PIUPEL90,PIUPELEQ   PBU UPLOAD ELEM?                             
         BNE   *+14                                                             
         MVC   BUYUPLID,PIUPUSEQ                                                
         B     FMTBUY60                                                         
                                                                                
         USING PBMATELD,R3                                                      
         CLI   PBMATELM,PBMATELQ   INVOICE MATCHING STATUSES ELEM?              
         BNE   *+20                                                             
         MVC   BUYMATST,PBMTSTAT   INVOICE MATCHING STATUS                      
         MVC   BUYDISST,PBMTDSTA   INVOICE MATCHING DISCREPANCY STATUS          
         B     FMTBUY60                                                         
                                                                                
         USING PTSHTEL,R3                                                       
         CLI   PTSHTEL,PTSHTELQ    TEST TEARSHEET ELEMENT                       
         BNE   FMTBUY52                                                         
         MVC   BUYPAGEN,PTSHPAGE   SET PAGE NOTATION                            
         MVC   BUYTAPPR,PTSHSTAT   SET TEARSHEET APPROVAL                       
         MVC   BUYREPRO,PTSHREPO   SET REPRODUCTION QUALITY                     
         MVC   BUYTSTAT,PTSHIND1                                                
         B     FMTBUY60                                                         
                                                                                
         USING PBYPSTEL,R3                                                      
FMTBUY52 CLI   PBYPSTEL,PBYPSTEQ                                                
         BNE   FMTBUY54                                                         
         USING PSTBLKD,WORKAREA                                                 
         XC    PSTBLKD(PSTLNQ),PSTBLKD                                          
         MVI   PSTACT,PSTFMTQ                                                   
         LA    R1,PBYPSTC                                                       
         ST    R1,PSTADIN          ADDRESS OF DATA                              
         LA    R1,WORK                                                          
         ST    R1,PSTADOUT                                                      
         GOTOR VPSTVAL,DMCB,PSTBLKD                                             
         MVC   BUYPST,WORK                                                      
         B     FMTBUY60                                                         
                                                                                
         USING PBFSIEL,R3                                                       
FMTBUY54 CLI   PBFSIEL,PBFSIELQ    FREE STANDING INSERTS                        
         BNE   FMTBUY60                                                         
         MVC   BUYFSINS,SPACES                                                  
         LA    R5,BUYFSINS                                                      
         EDIT  PBFSI,(9,0(R5)),ALIGN=LEFT                                       
         B     FMTBUY60                                                         
                                                                                
FMTBUY60 SR    R0,R0               BUMP TO NEXT ELEMENT ON RECORD               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTBUY18                                                         
*                                                                               
FMTBUY62 OC    FILTICNO,FILTICNO   TEST INTERNET CONTRACT# FILTER SET           
         BZ    *+14                                                             
         CLC   BUYICNUM,FILTICNO   YES - APPLY FILTER                           
         JNE   EXITN                                                            
                                                                                
         ICM   R3,15,BUYABILE      FORMAT BILLING VALUES                        
         BZ    FMTBUY64                                                         
         USING PBILELEM,R3                                                      
         MVC   BUYBINVD,PBLDATE    SET BILLED DATE                              
         GOTOR FMTINO,DMCB,(PBUYKMED,PBILELEM),BUYBINVN                         
                                                                                
FMTBUY64 ICM   R3,15,BUYAPAYE      FORMAT PAYMENT VALUES                        
         BZ    FMTBUY66                                                         
         USING PPAYELEM,R3                                                      
         GOTOR FMTPYE,DMCB,PPAYELEM,BUYPREP                                     
         MVC   BUYPAYDT,PPDDATE    SET PAID DATE                                
         MVC   BUYPAYSQ,PPDSEQNO   SET PAYMENT SEQUENCE NUMBER                  
         MVC   WORK(L'BUYPAYDT),BUYPAYDT                                        
         MVC   WORK+L'BUYPAYDT(L'BUYPAYSQ),BUYPAYSQ                             
         GOTOR GETCHK              RESOLVE CHECK NUMBER                         
         MVC   BUYCHKNO,WORK                                                    
         MVC   BUYCHKDT,WORK+L'PPCLCHK                                          
                                                                                
FMTBUY66 ICM   R3,15,BUYAIORE      FORMAT INSERTION ORDER VALUES                
         BZ    FMTBUY68                                                         
         USING PIOELEM,R3                                                       
         MVC   BUYORDDT,PIODATE    SET INSERTION ORDER DATE                     
         GOTOR FMTION,DMCB,(PBUYKMED,PIOELEM),BUYORDNO                          
         MVI   BUYORDNO+12,C' '                                                 
         MVC   BUYORDNO+13(3),PP@IONEW                                          
         CLI   PIOTYP,C'N'                                                      
         BE    FMTBUY68                                                         
         MVC   BUYORDNO+13(3),PP@IOCHA                                          
         CLI   PIOTYP,C'C'                                                      
         BE    FMTBUY68                                                         
         MVC   BUYORDNO+13(3),PP@IOCAN                                          
         CLI   PIOTYP,C'D'                                                      
         BE    FMTBUY68                                                         
         MVC   BUYORDNO+13(3),SPACES                                            
         DROP  R3                                                               
                                                                                
FMTBUY68 CLC   PBDJOB,SPACES       TEST JOB RECORD ATTACHED TO BUY              
         BNH   *+8                                                              
         GOTOR GETCAP              YES - READ JOB RECORD FOR CAPTION            
                                                                                
         CLC   PBUYKMED,PBUMED     TEST CHANGE OF MEDIA/PUBLICATION             
         BNE   *+14                                                             
         CLC   PBUYKPUB(L'PBUPZE),PBUPZE                                        
         BE    FMTBUY74                                                         
                                                                                
         TM    GIND1,GIONLINE      TEST RUNNING ONLINE                          
         BNZ   FMTBUY70            YES                                          
         XC    PBUREC,PBUREC       NO - SEE IF ALREADY HAVE PUB DETAILS         
         MVC   PBUMED,PBUYKMED     SET MEDIA/PUBLICATION CODE                   
         MVC   PBUPZE,PBUYKPUB                                                  
         GOTOR RBUFFRIN,DMCB,('BUFFAGET',APBUFFD),PBUD,ACOMFACS                 
         BE    FMTBUY74                                                         
                                                                                
FMTBUY70 XC    PBUREC,PBUREC       RESOLVE PUBLICATION DETAILS                  
         MVC   PBUMED,PBUYKMED                                                  
         MVC   PBUPZE,PBUYKPUB                                                  
         MVI   PBUNAME,C'?'                                                     
         MVC   PBUNAME+1(L'PBUNAME-1),PBUNAME                                   
                                                                                
         MVC   WORK(L'IOKEY),IOKEY SAVE THE CURRENT BUY KEY                     
         LA    R3,IOKEY                                                         
         USING PUBRECD,R3          READ PUBLICATION RECORD                      
         XC    PUBKEY,PUBKEY                                                    
         MVC   PUBKMED,PBUYKMED                                                 
         MVC   PUBKPUB(LPUBKPZE),PBUYKPUB                                       
         MVC   PUBKAGY,AGY                                                      
         MVI   PUBKCOD,PUBKCODQ                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPUBDIR+IO3'                            
         BNE   FMTBUY72                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPUBFIL+IO3'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,IOADDR                                                        
         MVC   PBUNAME,PUBNAME     SET PUBLICATION NAME                         
         MVC   PBUZNAME,PUBZNAME   ZONE NAME                                    
         MVC   PBUCITY,PUBCITY     CITY                                         
         MVC   PBUSTATE,PUBSTATE   STATE                                        
         MVC   PBUCODE,BUYPUB      SET PUBLICATION CODE (PRINTABLE)             
         MVC   PBUREP,PUBPLSH      SET PUBLISHER/REP CODE                       
         MVC   IOKEY,WORK          RESTORE SAVED BUY KEY                        
                                                                                
FMTBUY72 TM    GIND1,GIONLINE      TEST RUNNING ONLINE                          
         BNZ   FMTBUY74            YES                                          
         GOTOR RBUFFRIN,DMCB,('BUFFAPUT',APBUFFD),PBUD,ACOMFACS                 
         BE    FMTBUY74                                                         
         DC    H'0'                                                             
                                                                                
FMTBUY74 OC    BUYSER,BUYSER       TEST SERIAL NUMBER SET                       
         JNZ   FMTBUYX                                                          
         MVC   BUYSMED,PBUYKMED    NO - SEND FULL BUY KEY                       
         MVC   BUYSCLT,PBUYKCLT                                                 
         MVC   BUYSPRD,PBUYKPRD                                                 
         GOTOR VHEXOUT,DMCB,PBUYKPUB,BUYSPUB,L'BUYSPUB/2,=C'N'                  
         GOTOR VHEXOUT,DMCB,PBUYKLIN,BUYSLIN,L'BUYSLIN/2,=C'N'                  
                                                                                
FMTBUYX  J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  R3,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT NEWSPAPER SPACE/DESCRIPTION                                  *         
***********************************************************************         
*                                                                               
FMTNSD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   BUYSPACE,SPACES                                                  
*                                                                               
         OC    PBDSPACE,PBDSPACE   ANYTHING IN SPACE?                           
         BZ    FMTNSD02                                                         
         CLC   PBDSPACE,SPACES     ANYTHING IN SPACE?                           
         BE    FMTNSD02                                                         
*                                                                               
         CLC   =X'7B00',PBDSPACE   # AND ZERO - TREAT AS NONE-SPACE BUY         
         BE    FMTNSD02                                                         
         CLC   =C'# ',PBDSPACE     # AND SPACE- TREAT AS NONE-SPACE BUY         
         BE    FMTNSD02                                                         
*                                                                               
         CLI   PBDSPACE,C'*'       OTHER SPECIAL CHARS LOWER THAN C'*'?         
         BL    *+14                                                             
*                                                                               
         CLC   =C'* ',PBDSPACE     TEST SPACE BUY                               
         BNL   FMTNSD02                                                         
*                                                                               
         MVC   BUYSPACE(8),PBDSPACE                                             
         B     FMTNSDX                                                          
*                                                                               
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
         EDIT  (P8,DUB),(6,0(R5)),2,ALIGN=LEFT                                  
         BR    RE                                                               
                                                                                
FMTNSDE2 EDIT  (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT NEWSPAPER CONTRACT LINEAGE EQUIVALENCY                       *         
***********************************************************************         
*                                                                               
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
         EDIT  PBDUNITS,(6,0(R5)),2,ALIGN=LEFT                                  
         AR    R5,R0                                                            
         MVI   0(R5),C'I'                                                       
         B     FMTNCLX                                                          
                                                                                
FMTNCL02 EDIT  PBDUNITS,(5,0(R5)),ALIGN=LEFT                                    
         AR    R5,R0                                                            
         CLI   PBDUIND,C'I'                                                     
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         AHI   R5,1                                                             
         CP    PBDCLMS,PZERO                                                    
         BE    FMTNCLX                                                          
         MVI   0(R5),C'/'                                                       
         AHI   R5,1                                                             
         EDIT  PBDCLMS,(5,0(R5)),ALIGN=LEFT                                     
                                                                                
FMTNCLX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SUBROUTINE TO FORMAT INSERTION DATE/SUBLINE                         *         
***********************************************************************         
*                                                                               
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
         BE    FMTLIN10                                                         
         MVI   0(R5),C'-'                                                       
         SR    R0,R0                                                            
         IC    R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         CHI   R0,100                                                           
         BL    FMTLIN04                                                         
                                                                                
         DP    DUB,=P'10'          DISPLAY 100 - 239 AS A0 - N9                 
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  2(1,R5),DUB+7                                                    
         ZAP   DUB,DUB(6)                                                       
         CVB   RF,DUB                                                           
         SHI   RF,10                                                            
         LA    RF,LINETAB(RF)                                                   
         MVC   1(1,R5),0(RF)                                                    
         B     FMTLIN10                                                         
                                                                                
FMTLIN04 OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(2,R5),DUB                                                      
         CLI   1(R5),C'0'                                                       
         BNE   FMTLIN10                                                         
         MVC   1(2,R5),2(R5)                                                    
                                                                                
FMTLIN10 L     R1,ALP              DDLINK CONTROL BLOCK                         
         USING LP_D,R1                                                          
         CLC   LP_VRSN1,=AL1(1,2,0,0)                                           
         BNL   FMTLINX                                                          
         CLC   LP_VRSN1,=AL1(1,1,0,0)                                           
         BL    FMTLINX                                                          
         CLI   BUYLINE,C'T'        TEST INSERTION INDICATOR?                    
         BNE   FMTLINX             STRIP LEADING T                              
         MVC   BUYLINE(L'BUYLINE-1),BUYLINE+1                                   
         MVI   BUYLINE+L'BUYLINE-1,C' '                                         
         DROP  R1                                                               
                                                                                
FMTLINX  J     EXIT                                                             
*                                                                               
LINETAB  DC    C'ABCDEFGHIJKLMNOP'                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SUBROUTINE TO FORMAT POL PRODUCT ALLOCATIONS                        *         
***********************************************************************         
*                                                                               
FMTPRD   NTR1  BASE=*,LABEL=*                                                   
         MVC   BUYALLOC,SPACES                                                  
         LA    R5,BUYALLOC                                                      
         TM    PBDWTSUM,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTPRD02            NO-EQUAL                                     
         MVC   BYTE1,PBDWTSUM                                                   
         NI    BYTE1,X'FF'-X'80'                                                
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
                                                                                
FMTPRDED EDIT  (B1,0(R1)),(3,0(R5)),ALIGN=LEFT                                  
         AR    R5,R0               BUMP TO NEXT OUTPUT POSITION                 
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  R3,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT NEWSPAPER RATES                                              *         
***********************************************************************         
*                                                                               
FMTNRT   NTR1  BASE=*,LABEL=*                                                   
         MVC   BUYRATE,SPACES                                                   
         MVC   WORK2,SPACES                                                     
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BNE   FMTNRT02                                                         
                                                                                
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,F100000                                                       
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,F100000                                                       
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
FMTNRT02 CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BE    FMTNRT06                                                         
         C     R1,=F'99999999'     SEE IF TOTAL RATE OVER 999,999.99            
         BNH   FMTNRT04                                                         
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'          HAVE ENTERED PENNIES WHEN BUYING             
         LTR   R1,R1               (NO ROOM)                                    
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDIT  (R1),(9,WORK2+5),0,FLOAT=-,ALIGN=LEFT                            
         B     FMTNRT08                                                         
                                                                                
FMTNRT04 EDIT  (R1),(9,WORK2+5),2,FLOAT=-,ALIGN=LEFT                            
         B     FMTNRT08                                                         
                                                                                
FMTNRT06 EDIT  (R1),(11,WORK2+5),5,FLOAT=-,ALIGN=LEFT                           
                                                                                
         LA    R6,WORK2+5          START OF OUTPUT                              
         AR    R6,R0               + LENGTH                                     
         SHI   R6,3                BACK UP TO LAST 3 BYTES                      
         CLC   =C'000',0(R6)                                                    
         BNE   *+10                                                             
         MVC   0(3,R6),SPACES      MOVE SOME BLANKS                             
                                                                                
FMTNRT08 LA    R1,WORK2+5                                                       
                                                                                
* IF COST TYPE NOT 'U' DISPLAY IT, ELSE DISPLAY COST IND IF NOT C' '            
                                                                                
         CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BE    *+12                YES - CHECK PBDCOSIN                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSTY                                                 
         CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTNRT10                                                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
                                                                                
FMTNRT10 CLI   PBDCTYP,C'N'        NET INPUT                                    
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
                                                                                
         TM    PBDRLIND,X'08'      TEST FROZEN RATE                             
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
                                                                                
         MVC   BUYRATE,0(R1)                                                    
         CP    PBDCOS,PZERO                                                     
         JNE   FMTNRTX                                                          
         MVC   BUYRATE,SPACES                                                   
         MVC   BUYRATE(L'FREE),FREE                                             
                                                                                
FMTNRTX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT NON-NEWSPAPER RATE FORMAT                                    *         
***********************************************************************         
*                                                                               
FMTORF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   BUYRATE,SPACES                                                   
         MVC   WORK2,SPACES                                                     
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BNE   FMTORF02                                                         
                                                                                
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,F100000                                                       
         LCR   RF,RF               =NET PCT                                     
         MR    R0,RF                                                            
         L     RF,F100000                                                       
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
FMTORF02 EDIT  (R1),(10,WORK2+2),2,ALIGN=LEFT,FLOAT=-                           
         LA    R1,WORK2+2                                                       
         CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTORF04                                                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
                                                                                
FMTORF04 CLI   PBDCTYP,C'N'        DISPLAY AS NET                               
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
                                                                                
         TM    PBDRLIND,X'08'      TEST FROZEN                                  
         BZ    *+10                                                             
         BCTR  R1,0                                                             
         MVI   0(R1),C'*'                                                       
                                                                                
         MVC   BUYRATE,0(R1)                                                    
         CP    PBDCOS,PZERO                                                     
         JNE   FMTORFX                                                          
         MVC   BUYRATE,SPACES                                                   
         MVC   BUYRATE(L'FREE),FREE                                             
                                                                                
FMTORFX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT NEWSPAPER PREMIUMS                                           *         
***********************************************************************         
*                                                                               
FMTNPR   NTR1  BASE=*,LABEL=*                                                   
         MVC   BUYPREM,SPACES                                                   
         LA    R6,BUYPREM                                                       
         CLI   PBDCL,0                                                          
         BE    FMTNPR02                                                         
         MVC   0(1,R6),PBDCL                                                    
         MVI   1(R6),C'C'                                                       
         MVI   2(R6),C'/'                                                       
         LA    R6,3(R6)                                                         
         B     FMTNPR04                                                         
                                                                                
FMTNPR02 CP    PBDPRCOS,PZERO                                                   
         BE    FMTNPR30                                                         
                                                                                
FMTNPR04 ZAP   DUB,PBDPRCOS                                                     
         CVB   R1,DUB              R1=PREMIUM COST                              
         CLI   PBDPCTYP,C'N'       NET INPUT SO DISPLAY AS NET                  
         BNE   FMTNPR06                                                         
                                                                                
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,F100000                                                       
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,F100000                                                       
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
FMTNPR06 CLI   PBDPRIN,C' '        TEST DEFAULT IND                             
         BNH   *+14                                                             
         MVC   0(1,R6),PBDPRIN                                                  
         AHI   R6,1                                                             
         CLI   PBDPCTYP,C' '       TEST PREMIUM COST TYPE                       
         BNH   *+14                                                             
         MVC   0(1,R6),PBDPCTYP                                                 
         AHI   R6,1                                                             
         CLI   PBDCL,0             CHECK FOR COLOR                              
         BE    FMTNPR20                                                         
                                                                                
         CLI   PBDPRIN,C' '        CHECK FOR PR RATE IND                        
         BH    FMTNPR08            IF SPACE I CAN DISPLAY 8 DIGITS              
         CLI   PBDPCTYP,C' '       CHECK FOR PR COST TYPE                       
         BNH   FMTNPR14            IF SPACE I CAN DISPLAY 8 DIGITS              
                                                                                
FMTNPR08 C     R1,=F'-99999'       CHECK FOR NEGATIVE GREATER                   
         BL    FMTNPR10            THAN -999.99                                 
         C     R1,=F'1000000'      SEE IF OVER 10,000.00                        
         BL    FMTNPR12                                                         
                                                                                
FMTNPR10 CVD   R1,DUB              SEE IF I CAN DROP THE PENNIES                
         DP    DUB,P100                                                         
         CP    DUB+(L'DUB-L'P100)(L'P100),PZERO                                 
         BNE   FMTNPR12                                                         
                                                                                
* IF I CAN'T DROP CENTS -TRUNCATE                                               
* PROBABLY WON'T HAPPEN SINCE THEY WOULD HAVE TO HAVE ENTERED                   
* NNNNNN.N AS THE PREMIUM                                                       
                                                                                
         ZAP   DUB,DUB(L'DUB-L'P100)                                            
         CVB   R1,DUB              WITHOUT PENNIES                              
         EDITR (R1),(7,0(R6)),0,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,       +        
               IZERO=Y                                                          
         B     FMTNPR30                                                         
FMTNPR12 EDITR (R1),(7,0(R6)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,       +        
               IZERO=Y                                                          
         B     FMTNPR30                                                         
                                                                                
FMTNPR14 C     R1,=F'-9999999'     SEE IF NEGATIVE GREATER                      
         BL    FMTNPR16            THAN -99,999.99                              
         C     R1,=F'10000000'     SEE IF OVER 100,000.00                       
         BL    FMTNPR18            IF BELOW - NO PROBLEM                        
                                                                                
FMTNPR16 CVD   R1,DUB              SEE IF I CAN DROP THE PENNIES                
         DP    DUB,P100                                                         
         CP    DUB+(L'DUB-L'P100)(L'P100),PZERO                                 
         BNE   FMTNPR18                                                         
                                                                                
* IF I CAN'T DROP CENTS -TRUNCATE                                               
* PROBABLY WON'T HAPPEN SINCE THEY WOULD HAVE TO HAVE ENTERED                   
* NNNNNN.N AS THE PREMIUM                                                       
                                                                                
         ZAP   DUB,DUB(L'DUB-L'P100)                                            
         CVB   R1,DUB              WITHOUT PENNIES                              
         EDITR (R1),(8,0(R6)),0,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,       +        
               IZERO=Y                                                          
         B     FMTNPR30                                                         
                                                                                
FMTNPR18 EDITR (R1),(8,0(R6)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,       +        
               IZERO=Y                                                          
         B     FMTNPR30                                                         
                                                                                
FMTNPR20 EDITR (R1),(10,0(R6)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,      +        
               IZERO=Y                                                          
*                                                                               
FMTNPR30 DS    0H                                                               
*                                                                               
FMTNPRX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO ESTABLISH CHECK NUMBER                                   *         
***********************************************************************         
                                                                                
GETCHK   NTR1  BASE=*,LABEL=*,WORK=(RC,L'IOKEY)                                 
                                                                                
         MVC   0(L'IOKEY,RC),IOKEY SAVE THE CURRENT RECORD KEY                  
         XC    WORK2(L'IOKEY),WORK2                                             
                                                                                
         LA    R3,IOKEY                                                         
         USING PPCLRST,R3          READ CLEARANCE STATUS RECORDS                
         XC    PPCLKEY,PPCLKEY     TO ESTABLISH CHECK NUMBER                    
         MVC   PPCLAGY,AGY                                                      
         MVC   PPCLMED,PBUYKMED                                                 
         MVI   PPCLTYPE,PPCLTYPQ                                                
         MVC   PPCLCLT,PBUYKCLT                                                 
         MVC   PPCLPUB(L'PBUYKPUB),PBUYKPUB                                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR+IO7'                            
         BNE   GETCHK16                                                         
         B     GETCHK04                                                         
                                                                                
GETCHK02 MVC   WORK2(L'IOKEY),PPCLKEY                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOPRTDIR+IO7'                            
         BNE   GETCHK08                                                         
                                                                                
GETCHK04 OC    WORK2(L'IOKEY),WORK2                                             
         BNZ   GETCHK06                                                         
         CLC   PPCLKEY(PPCLDATE-PPCLKEY),IOKEYSAV                               
         BNE   GETCHK16                                                         
         B     GETCHK02                                                         
                                                                                
GETCHK06 CLC   PPCLKEY(PPCLDATE-PPCLKEY),WORK2                                  
         BNE   GETCHK08                                                         
         CLC   WORK(L'PPCLDATE),PPCLDATE                                        
         BH    GETCHK02                                                         
                                                                                
GETCHK08 MVC   IOKEY,WORK2         RESTORE LAST GOOD KEY                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO7'                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO7'                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,IOADDR                                                        
         LA    R4,PPCLELEM-PPCLRST(R4)                                          
         USING PPCLELEM,R4         TRY FOR PAYMENT ON THIS RECORD               
         SR    R0,R0                                                            
GETCHK10 CLI   PPCLELEM,0          TEST END OF RECORD                           
         BE    GETCHK14                                                         
         CLI   PPCLELEM,PPCLELQ    TEST CLEARANCE ELEMENT                       
         BNE   GETCHK12                                                         
         CLC   PPCLCLRD(L'PPCLCLRD+L'PPCLCLSQ),WORK                             
         BE    GETCHK18                                                         
GETCHK12 IC    R0,PPCLELEM+1       BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     GETCHK10                                                         
                                                                                
GETCHK14 XC    WORK2(L'IOKEY),WORK2                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOPRTDIR+IO7'                            
         BE    GETCHK04            GO PROCESS NEXT RECORD                       
                                                                                
GETCHK16 XC    WORK(L'PPCLCHK+L'PPCLCHDT),WORK                                  
         B     GETCHKX                                                          
                                                                                
GETCHK18 MVC   WORK(L'PPCLCHK),PPCLCHK                                          
         MVC   WORK+L'PPCLCHK(L'PPCLCHDT),PPCLCHDT                              
                                                                                
GETCHKX  MVC   IOKEY,0(RC)         RESTORE SAVED KEY                            
         J     EXIT                                                             
         DROP  R3,R4,RB                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* READ JOB RECORD AND ESTABLISH JOB CAPTION                           *         
***********************************************************************         
*                                                                               
GETCAP   NTR1  BASE=*,LABEL=*,WORK=(RC,L'IOKEY)                                 
                                                                                
         MVC   0(L'IOKEY,RC),IOKEY SAVE THE CURRENT RECORD KEY                  
                                                                                
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
         BE    GETCAP02                                                         
         MVC   LASTJOB,PJOBKEY     NO - SET LAST JOB KEY                        
         XC    LASTCAP,LASTCAP     AND CLEAR LAST CAPTION                       
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR+IO7'                            
         BNE   GETCAP02                                                         
         CLC   LASTJOB,PJOBKEY     TEST CORRECT RECORD FOUND                    
         BNE   GETCAP02                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO7'                           
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
                                                                                
GETCAP02 MVC   BUYADCAP,LASTCAP    SET CAPTION                                  
         MVC   IOKEY,0(RC)         RESTORE SAVED KEY                            
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  R2,R3,RB                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET PUBLICATION RECORDS & BUILD PUBLISHER RECORDS FOR DOWNLOADING   *         
***********************************************************************         
*                                                                               
GETPUB   J     *+12                                                             
         DC    C'*GETPUB*'                                                      
         LR    RB,RF                                                            
         USING GETPUB,RB                                                        
         TM    GIND1,GIONLINE      DON'T EXECUTE IF RUNNING ONLINE              
         BNZ   GETPUBN                                                          
                                                                                
         USING LP_D,R1                                                          
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   GETPUB02                                                         
         XC    PBUKEY(PBUKEYL),PBUKEY                                           
         GOTOR RBUFFRIN,DMCB,('BUFFARDH',APBUFFD),PBUD,ACOMFACS                 
         B     GETPUB04                                                         
                                                                                
GETPUB02 GOTOR RBUFFRIN,DMCB,('BUFFASEQ',APBUFFD),PBUD,ACOMFACS                 
                                                                                
GETPUB04 BNE   GETPUBN                                                          
         L     R1,ALP                                                           
         LA    R0,PBUD                                                          
         STCM  R0,15,LP_ADATA      SET A(PUBLISHER/REP RECORD)                  
         OC    PBUREP,PBUREP       TEST PUBLISHER/REP CODE ESTABLISHED          
         JZ    EXITY               NO - EXIT                                    
                                                                                
         MVC   RBUMED,PBUMED       READ PUBLISHER/REP BUFFER RECORD             
         MVC   RBUREP,PBUREP                                                    
         GOTOR RBUFFRIN,DMCB,('BUFFAGET',ARBUFFD),RBUD,ACOMFACS                 
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
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO3'                            
         BNE   GETPUB06                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO3'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         MVC   RBUNAME,PREPNAME                                                 
                                                                                
GETPUB06 GOTOR RBUFFRIN,DMCB,('BUFFAPUT',ARBUFFD),RBUD,ACOMFACS                 
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
GETPUBN  L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS                          
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET PUBLISHER/REP RECORDS FOR DOWNLOADING                           *         
***********************************************************************         
*                                                                               
GETREP   J     *+12                                                             
         DC    C'*GETREP*'                                                      
         LR    RB,RF                                                            
         USING GETREP,RB                                                        
         TM    GIND1,GIONLINE      DON'T EXECUTE IF RUNNING ONLINE              
         BNZ   GETREPN                                                          
                                                                                
         USING LP_D,R1                                                          
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   GETREP02                                                         
         XC    RBUKEY(RBUKEYL),RBUKEY                                           
         GOTOR RBUFFRIN,DMCB,('BUFFARDH',ARBUFFD),RBUD,ACOMFACS                 
         B     GETREP04                                                         
                                                                                
GETREP02 GOTOR RBUFFRIN,DMCB,('BUFFASEQ',ARBUFFD),RBUD,ACOMFACS                 
                                                                                
GETREP04 JNE   GETREPN                                                          
         L     R1,ALP                                                           
         LA    R0,RBUD                                                          
         STCM  R0,15,LP_ADATA      SET A(PUBLISHER/REP RECORD)                  
         J     EXITY                                                            
                                                                                
GETREPN  L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS                          
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO GET CLIENT PROFILES                                      *         
***********************************************************************         
*                                                                               
GETCPR   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO1             R2=A(CLIENT RECORD)                          
         USING PCLTRECD,R2                                                      
         XC    CLTPROFS(CLTPROFL),CLTPROFS                                      
         XC    WORK(12),WORK                                                    
         MVC   WORK+00(4),=C'POB1'                                              
         MVC   WORK+04(L'AGY),AGY                                               
         MVC   WORK+07(L'PCLTKCLT),PCLTKCLT                                     
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(L'PCLTOFF),PCLTOFF                                       
         GOTOR VGETPROF,DMCB,WORK,CLTPRB1,VDATAMGR                              
         MVC   WORK+00(4),=C'pB1X'                                              
         GOTOR VGETPROF,DMCB,WORK,CLTPRB1X,VDATAMGR                             
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* READ BUY RECORD USING SERIAL NUMBER - R1=A(SERIAL NUMBER)           *         
***********************************************************************         
*                                                                               
GETBUY   NTR1  BASE=*,LABEL=*                                                   
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
         GOTOR (#IOEXEC,AIOEXEC),'IORDD+IOPRTDIR+IO2'                           
         BE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         JZ    EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO2+IORDEL'                    
         JE    EXITY                                                            
         TM    IOERR,IOEDEL                                                     
         JNZ   EXITY                                                            
         J     EXITN                                                            
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* READ INVOICE RECORD USING SERIAL NUMBER - R1=A(SERIAL NUMBER)       *         
***********************************************************************         
*                                                                               
GETINV   NTR1  BASE=*,LABEL=*                                                   
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
         MVI   PNVKELMK,X'FF'                                                   
         MVC   PNVKELMK+1(L'PNVKELMK-1),PNVKELMK                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDD+IOPRTDIR+IO2'                           
         BE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         JZ    EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO2+IORDEL'                    
         JE    EXITY                                                            
         TM    IOERR,IOEDEL                                                     
         JNZ   EXITY                                                            
         J     EXITN                                                            
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* FORMAT INSERTION ORDER NUMBER FOR DOWNLOAD                          *         
***********************************************************************         
*                                                                               
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
         NI    WORK,X'FF'-X'C0'                                                 
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
*                                                                               
***********************************************************************         
* INTERFACE TO TSAR (ONLINE) OR BUFFERIN (OFFLINE)                    *         
* R1 -> 1 = USING INVOICE BUFFER                                      *         
*       2 = USING CUSTOM COLUMN BUFFER                                *         
***********************************************************************         
*                                                                               
BUFFER   NTR1  LABEL=NO                                                         
         OC    RMASTC,RMASTC       TEST ONLINE                                  
         JNZ   BUFFR10                                                          
         GOTOR VTSAR,TSARD         YES - USE TSAR                               
         J     EXIT                                                             
*                                                                               
BUFFR10  LHI   R0,BUFFAINI         CONVERT TSAR ACTION CODE TO BUFFERIN         
         CLI   TSACTN,TSAINI                                                    
         JE    BUFFR20                                                          
         LHI   R0,BUFFAPUT                                                      
         CLI   TSACTN,TSAADD                                                    
         JE    BUFFR20                                                          
         CLI   TSACTN,TSAWRT                                                    
         JE    BUFFR20                                                          
         LHI   R0,BUFFASEQ                                                      
         CLI   TSACTN,TSANXT                                                    
         JE    BUFFR20                                                          
*                                                                               
         MVC   TKEYSV_I,IBUKEY     BUY INVOICE BUFFER KEY                       
         LHI   R0,BUFFARDH                                                      
         CLI   TSACTN,TSARDH                                                    
         JE    BUFFR20                                                          
         DC    H'0'                                                             
*                                                                               
BUFFR20  GOTOR RBUFFRIN,DMCB,((R0),AIBUFFD),IBUD,ACOMFACS                       
         CLI   TSACTN,TSARDH                                                    
         JNE   BUFFR90                                                          
*                                                                               
         CLC   TKEYSV_I,IBUKEY     EMULATE TSAR READ HIGH                       
         JE    BUFFR90                                                          
         OI    BUFFERRS-BUFFPARM(R1),BUFFERNF                                   
*                                                                               
BUFFR90  MVC   TSERRS,BUFFERRS-BUFFPARM(R1)                                     
         CLI   TSERRS,0                                                         
         J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* MINIO INTERFACE ROUTINE (INITIALIZATION)                            *         
***********************************************************************         
*                                                                               
MININIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,ALP                                                           
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
*                                                                               
         LA    R0,MNBLKCB          CLEAR MINBLOCK AREA                          
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    MNRTAB,MNRTAB       CLEAR RECORD  TABLE                          
         XC    MNELEM,MNELEM       CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         L     RF,ACOMFACS                                                      
         L     RF,(CRECUP-COMFACSD)(RF)                                         
         STCM  RF,15,MINRECUP                                                   
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,PRTFIL       FILE NAME                                    
         MVC   MINDIR,PRTDIR       DIR NAME                                     
         LA    R1,L'PNVKEY                                                      
         STC   R1,MINFKLEN         KEY LENGTH                                   
*                                                                               
         LA    R1,75               SET SPLIT PERCENTAGE TO 75%                  
         STCM  R1,3,MINSPCT        IE. FULL RECORD RETAINS 75% OF IT'S          
*                                  ELEMENTS ON SPLITING                         
*                                                                               
         MVI   MINNCTL,L'PNVDCNTL  2 CONTROL BYTES                              
         LHI   R1,4000             L'IOS                                        
         STCM  R1,3,MINFRCLM       MAX RECORD LENGTH IS IOAREA LNGTH            
*                                                                               
         MVI   MINEKLEN,L'PNVKELMK      ELEMENT KEY LENGTH                      
         MVI   MINEKDSP,PNVKELMK-PNVKEY ELEMENT KEY DISPLACEMENT                
*                                                                               
         MVC   MINBUFF,AIO1        A(FIRST MINIO BUFFER)                        
         MVI   MINNBUF,2           NUMBER OF BUFFERS                            
*                                                                               
         LA    R1,MNELEM           A(ELEMENT AREA)                              
         ST    R1,MINELEM                                                       
         LHI   R1,L'MNELEM         MAX ELEMENT/CLUSTER LENGTH                   
         STCM  R1,3,MINMAXEL                                                    
*                                                                               
         LA    R1,MNRTAB           A(RECORD TABLE)                              
         ST    R1,MINRTAB                                                       
         LHI   R1,L'MNRTAB                                                      
         STCM  R1,3,MINRTABL       LENGTH OF RECORD TABLE                       
*                                                                               
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R5,R7                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO GET AN ELEMENT IN A MINIO RECORD                         *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE FOUND)                         *         
*               IF LENGTH OF ELEMENT PROVIDED, KEY MATCHING IS DONE   *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED (NOT USED HERE)                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
*                                                                     *         
***********************************************************************         
*                                                                               
MGETEL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RE,R1                                                            
         L     R1,ALP                                                           
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LR    R1,RE                                                            
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINRD            DEFAULT TO DIRECT READ                       
*                                                                               
         CLI   1(R6),0             USE DEFAULT IF NO LENGTH GIVEN               
         BE    MGETEL10                                                         
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       USE READ IF GREATER THAN KEY LENGTH          
         BNL   MGETEL10                                                         
*                                                                               
         LA    R0,MINHI            SET FOR READ HI/EQUAL                        
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
MGETEL10 ZIC   RF,MINEKLEN         GET KEY LENGTH                               
         AHI   RF,-2               DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CMINIO-COMFACSD)(RF)                                         
         GOTOR (RF),PNVPARMS,((R0),MINBLKD)                                     
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         J     EXITY               SET CC                                       
*                                                                               
         LTORG                                                                  
         DROP  RB,R5,R7                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO GET NEXT ELEMENT IN A MINIO RECORD                       *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                    *         
*               IF LENGTH OF ELEMENT PROVIDED, KEY MATCHING IS DONE   *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED (NOT USED HERE)                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
*                                                                     *         
***********************************************************************         
*                                                                               
MNXTEL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RE,R1                                                            
         L     R1,ALP                                                           
         L     R5,LP_BLKS+((B#INVVAL-1)*L'LP_BLKS)                              
         USING INVVALSD,R5                                                      
         LR    R1,RE                                                            
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINSEQ           SEQUENTIAL READ                              
*                                                                               
         CLI   1(R6),0             NO FILTERING IF NO LENGTH GIVEN              
         BE    MNXTEL10                                                         
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
MNXTEL10 ZIC   RF,MINEKLEN         GET KEY LENGTH                               
         AHI   RF,-2               DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CMINIO-COMFACSD)(RF)                                         
         GOTO1 (RF),PNVPARMS,((R0),MINBLKD)                                     
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         J     EXITY               SET CC                                       
*                                                                               
         LTORG                                                                  
         DROP  RB,R5,R7                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*        P1       A(PID)                                              *         
*        P2+0(1)  L'RETURN AREA                                       *         
*        P2+1(3)  A(RETURN AREA)                                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
TRANSPID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         SR    R6,R6                                                            
         IC    R6,4(R1)            SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
* READ PERSON AUTH REC ON CTFILE                                                
*                                                                               
         LA    R4,IOKEY                                                         
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,AGY         SET REQUEST AGY                              
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTL+IO3'                               
*                                                                               
         L     R4,AIO3             POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,IOKEYSAV     RECORD FOUND?                                
         BNE   TPIDNOTF                                                         
*                                                                               
* FIND USER'S ID AND PERSON'S ID ELEMENT                                        
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         MATCH ON ELEMENT CODE?                       
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
* FIND PERSON RECORD                                                            
*                                                                               
         LA    R4,IOKEY                                                         
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
         MVC   SAPEAGY,AGY         SET REQUEST AGY                              
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTL+IO3'                               
*                                                                               
         L     R4,AIO3             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),IOKEYSAV                                
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
* FIND NAME ELEMENT                                                             
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENOT FOUND                          
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 VSQUASH,DMCB,WORK,(R0)                                           
*                                                                               
* MOVE NAME TO OUTPUT AREA                                                      
*                                                                               
         LR    RF,R6               GET RETURN AREA LENGTH                       
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      INIT OUT PUT AREA                            
*                                                                               
         L     RF,4(R1)            SAVE SQUASHED LENGTH                         
*                                                                               
         CR    RF,R6               NAME TOO LONG?                               
         BNH   *+6                                                              
         LR    RF,R6               USE MAX FOR RETURN AREA                      
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        RETURN NAME IN OUTPUT AREA                   
*                                                                               
TRNPIDX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R4,RE                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* BUILD LIST OF MEDIA FOR CFM CLIENT DOWNLOAD                         *         
***********************************************************************         
                                                                                
BLDMED   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ALP                                                           
         L     R7,LP_AWMP-LP_D(RE)                                              
         USING LW_D,R7             R7=A(MEDIA ELEMENT IN POOL)                  
         STCM  R7,7,AMED                                                        
         XC    LW_D(LW_DATA2-LW_D),LW_D                                         
         MVI   LW_TYPE,LQ_TLSTQ                                                 
         LA    R3,LW_DATA2                                                      
         SR    R0,R0                                                            
         LA    R4,C'A'                                                          
         LA    R2,IOKEY                                                         
         USING PAGYRECD,R2         R2=A(MEDIA RECORD KEY)                       
                                                                                
BLDMED02 XC    PAGYKEY,PAGYKEY                                                  
         MVC   PAGYKAGY,AGY                                                     
         STC   R4,PAGYKMED                                                      
         MVI   PAGYKRCD,PAGYKIDQ                                                
                                                                                
         LHI   R1,IOHI+IOPRTDIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   PAGYKAGY,AGY                                                     
         BNE   BLDMED08                                                         
         CLI   PAGYKRCD,PAGYKIDQ                                                
         BE    BLDMED04                                                         
         IC    R4,PAGYKMED                                                      
         BL    BLDMED02                                                         
         B     BLDMED06                                                         
                                                                                
BLDMED04 MVC   0(L'PAGYKMED,R3),PAGYKMED                                        
         AHI   R3,L'PAGYKMED                                                    
         AHI   R0,1                                                             
         IC    R4,PAGYKMED                                                      
                                                                                
BLDMED06 AHI   R4,1                                                             
         B     BLDMED02                                                         
                                                                                
BLDMED08 STCM  R0,3,LW_NUMN                                                     
         LA    R0,LW_D                                                          
         SR    R3,R0                                                            
         STCM  R3,3,LW_LN                                                       
         AR    R7,R3                                                            
         L     RE,ALP                                                           
         ST    R7,LP_AWMP-LP_D(RE)                                              
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  R2,R7,RB                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* BUILD MEDIA/RECORD/CLIENT TABLE FOR CFM PRODUCT DOWNLOAD            *         
***********************************************************************         
*                                                                               
BLDMRC   NTR1  BASE=*,LABEL=*                                                   
         SR    R7,R7                                                            
         ICM   R7,7,ASMC                                                        
         BNZ   *+6                                                              
         DC    H'0'                THERE IS NO INPUT                            
         USING LW_D,R7                                                          
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN                                                     
         LA    R2,LW_DATA2                                                      
         LA    R3,LW_DATA2                                                      
         SR    RF,RF                                                            
                                                                                
BLDMRC02 CLI   0(R2),PRTLETQ       TEST ENTRY FOR ME                            
         BNE   BLDMRC04                                                         
         MVC   0(1,R3),1(R2)       SET MEDIA CODE                               
         MVI   1(R3),X'06'         SET PRODUCT RECORD TYPE                      
         CR    R2,R3                                                            
         BE    *+10                                                             
         MVC   1+L'PPRDKMED(L'PPRDKCLT,R3),1+L'PPRDKMED(R2)                     
         AHI   R3,1+L'PPRDKMED+L'PPRDKCLT                                       
         AHI   RF,1                BUMP N'ENTRIES IN TABLE                      
                                                                                
BLDMRC04 AHI   R2,1+L'PPRDKMED+L'PPRDKCLT                                       
         BCT   R0,BLDMRC02                                                      
         STCM  RF,3,LW_NUMN                                                     
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  R7,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* BUILD MEDIA TABLE FOR CFM VENDOR DOWNLOAD                           *         
***********************************************************************         
*                                                                               
BLDPUM   NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         ICM   R2,7,ASMC                                                        
         BNZ   *+6                                                              
         DC    H'0'                THERE IS NO INPUT                            
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R2)                                            
         LA    R2,LW_DATA2-LW_D(R2)                                             
         USING SMDTABD,R2          R2=A(SYSTEM/MEDIA LIST)                      
         LA    R4,PUBMED           R4=A(MEDIA TABLE)                            
         SR    RE,RE                                                            
BLDPUM02 CLI   SMDTSYS,PRTLETQ     TEST ENTRY FOR ME                            
         BNE   BLDPUM04                                                         
         MVC   0(L'PUBMED,R4),SMDTMED                                           
         AHI   R4,L'PUBMED                                                      
         AHI   RE,1                                                             
BLDPUM04 AHI   R2,SMDTABL                                                       
         BCT   R0,BLDPUM02                                                      
         LTR   RE,RE               TEST ANY PRINT MEDIA FOUND                   
         JZ    EXITN                                                            
         STCM  RE,3,PUBMED#        SET NUMBER OF MEDIA IN LIST                  
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  R2,RB                                                            
*                                                                               
SMDTABD  DSECT                     ** SYSTEM/MEDIA TABLE **                     
SMDTSYS  DS    C                   SYSTEM CODE                                  
SMDTMED  DS    C                   MEDIA CODE                                   
SMDTABL  EQU   *-SMDTABD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* BUILD PUBLICATION TABLE FOR INSERTION DOWNLOAD                      *         
***********************************************************************         
*                                                                               
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
         LHI   R1,IOHI+IOPRTDIR+IO1                                             
                                                                                
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
                                                                                
BLDPUB28 LHI   R1,IOSQ+IOPRTDIR+IO1                                             
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
                                                                                
BLDPUB42 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   PLISKEY(PLISKLIN-PLISKEY),IOKEYSAV                               
         BE    BLDPUB44                                                         
         CLC   IOKEYSAV+(PLISKCLT-PLISKEY)(L'PLISKCLT),ZZZ                      
         JE    EXITN                                                            
         MVC   PLISKEY,IOKEYSAV                                                 
         MVC   PLISKCLT,ZZZ                                                     
         B     BLDPUB42                                                         
                                                                                
BLDPUB44 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO1'                           
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
                                                                                
BLDPUB50 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOPRTDIR+IO1'                            
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
*                                                                               
         LTORG                                                                  
         DROP  R7,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* BUILD TABLE OF ADDITIONAL CHARGE CODES                              *         
***********************************************************************         
*                                                                               
BLDACC   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO6                                                          
         USING ACHTABD,R3          R3=A(ADDITIONAL CHARGES TABLE)               
         LA    R2,IOKEY                                                         
         USING PSPLRECD,R2                                                      
         LA    R0,X'41'            (TO POSITION TO FIRST MEDIA)                 
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
         STC   R0,PSPLKMED                                                      
         LTR   R4,R4               TEST ANY REQUESTED MEDIAS                    
         BZ    *+10                                                             
         MVC   PSPLKMED,0(R4)      YES - SET NEXT MEDIA TO READ FOR             
         MVI   PSPLKRCD,PSPLKIDQ                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR+IO1'                            
         BNE   BLDACCX                                                          
         CLC   PSPLKAGY,AGY                                                     
         BNE   BLDACCX                                                          
         CLI   PSPLKRCD,PSPLKIDQ                                                
         BH    BLDACC08                                                         
         BE    *+12                                                             
         IC    R0,PSPLKMED                                                      
         B     BLDACC02                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO1'                           
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
                                                                                
         SR    R0,R0               BUMP TO NEXT MEDIA CODE                      
         IC    R0,PSPLKMED         FOR ALL MEDIA REQUEST                        
         AHI   R0,1                                                             
         B     BLDACC02                                                         
         DROP  R2                                                               
                                                                                
BLDACCX  XC    ACHTABD(ACHTABL),ACHTABD                                         
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  R3,RB                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* INSERTION HISTORY DOWNLOAD                                          *         
***********************************************************************         
*                                                                               
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
         BE    *+6                                                              
         DC    H'0'                CAN'T READ CLIENT RECORD                     
         GOTOR GETCPR              GET CLIENT PROFILES                          
                                                                                
         L     R4,AIO5             BUILD BUY ADDED ACTIVITY ENTRY               
         LHI   RF,1                                                             
         STCM  RF,3,ICA#                                                        
         USING ICAD,R4                                                          
         MVC   ICATYPE,PP@ADD                                                   
         GOTOR VDATCON,DMCB,(3,PBDBUYDT),(2,ICADATE)                            
         DROP  R4                                                               
                                                                                
         LA    R3,PBUYFRST         R3=A(FIRST BUY RECORD ELEMENT)               
BLDHST02 CLI   0(R3),0             TEST END OF RECORD                           
         BE    BLDHST18                                                         
         CLI   0(R3),PIOELQ        TEST INSERTION ORDER ELEMENT                 
         BE    BLDHST06                                                         
         CLI   0(R3),PBILELQ       TEST BILLING ELEMENT                         
         BE    BLDHST08                                                         
         CLI   0(R3),PPAYELQ       TEST PAYING ELEMENT                          
         BE    BLDHST10                                                         
         CLI   0(R3),PCHGEQ        TEST INSERTION ACTIVITY ELEMENT              
         BE    BLDHST12                                                         
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
                                                                                
         USING PBILELEM,R3         BILLING ELEMENTS                             
BLDHST08 CLI   PBILELEM+1,PBACCODE-PBILELEM                                     
         BH    BLDHST04                                                         
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
         GOTOR FMTINO,DMCB,(PBUYKMED,PBILELEM),IBAINVNO                         
         USING PPBVALDD,WORKAREA                                                
         GOTOR VPPBVAL,DMCB,(C'E',PBILELEM),PPBVALDD                            
         MVC   IBAGROSS,PPBVEEG                                                 
         MVC   IBANET,PPBVEEN                                                   
         ICM   RF,15,IBANET                                                     
         ICM   R1,15,PPBVEEC                                                    
         SR    RF,R1                                                            
         STCM  RF,15,IBANLCDB                                                   
         B     BLDHST04                                                         
         DROP  R3,R4                                                            
                                                                                
         USING PPAYELEM,R3         PAYMENT ELEMENTS                             
BLDHST10 CLI   PPAYELEM+1,PPACCODE-PPAYELEM                                     
         BH    BLDHST04                                                         
         OC    PPDDATE,PPDDATE                                                  
         BZ    BLDHST04                                                         
         SR    R4,R4                                                            
         ICM   R4,3,IPA#                                                        
         LA    RF,1(R4)                                                         
         STCM  RF,3,IPA#                                                        
         MHI   R4,IPAL                                                          
         A     R4,AIO4                                                          
         USING IPAD,R4                                                          
         MVC   IPADATE,PPDDATE                                                  
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
         B     BLDHST04                                                         
         DROP  R3,R4                                                            
                                                                                
         USING PCHGELEM,R3         BUY CHANGE ACTIVITY ELEMENTS                 
BLDHST12 SR    R4,R4                                                            
         ICM   R4,3,ICA#                                                        
         LA    RF,1(R4)                                                         
         STCM  RF,3,ICA#                                                        
         MHI   R4,ICAL                                                          
         A     R4,AIO5                                                          
         USING ICAD,R4                                                          
         MVC   ICADATE,PCHGDAT                                                  
         MVC   ICATYPE,PP@CHA                                                   
         ICM   RF,B'1110',PCHGIND1                                              
         ICM   RF,B'0001',PCHGIND4                                              
         LA    RE,ACTVWHAT                                                      
         LA    R1,ICAWHAT                                                       
         LA    R0,ACTV#                                                         
BLDHST14 TMH   RF,X'8000'                                                       
         BZ    BLDHST16                                                         
         MVC   0(L'ACTVWHAT,R1),0(RE)                                           
         LA    R1,L'ACTVWHAT-1(R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),COMMA                                                      
         AHI   R1,2                                                             
BLDHST16 SLL   RF,1                                                             
         AHI   RE,L'ACTVWHAT                                                    
         BCT   R0,BLDHST14                                                      
         SHI   R1,1                                                             
         CLI   0(R1),COMMA         REMOVE TRAILING COMMA                        
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         B     BLDHST04                                                         
                                                                                
BLDHST18 SR    R4,R4                                                            
         ICM   R4,3,ICA#                                                        
         TM    PBUYCNTL,X'80'      TEST BUY IS DELETED                          
         BNZ   BLDHST20                                                         
         SHI   R4,1                                                             
         MHI   R4,ICAL                                                          
         A     R4,AIO5             NO - POINT TO LAST ACTIVITY ENTRY            
         B     BLDHST22                                                         
                                                                                
BLDHST20 LA    RF,1(R4)            BUY IS DELETED - BUILD DELETED ENTRY         
         STCM  RF,3,ICA#                                                        
         MHI   R4,ICAL                                                          
         A     R4,AIO5                                                          
         MVC   ICATYPE,PP@DEL                                                   
         GOTOR VDATCON,DMCB,(3,PBDDATE),(2,ICADATE)                             
                                                                                
BLDHST22 MVC   ICAWHOM,PBDBUYER    SET BUYER IN LAST ACTIVITY ENTRY             
                                                                                
BLDHSTX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  R2,R3,R4,RB                                                      
         EJECT                                                                  
*                                                                               
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
*                                                                               
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
ONLYQ    EQU   C'O'                                                             
COMMA    EQU   C','                                                             
*                                                                               
NEWSMEDQ EQU   C'N'                NEWSPAPER MEDIA LETTER                       
ODORMEDQ EQU   C'O'                OUTDOOR MEDIA LETTER                         
*                                                                               
LPUBKPZ  EQU   L'PUBKPUB+L'PUBKZON                                              
LPUBKPZE EQU   L'PUBKPUB+L'PUBKZON+L'PUBKED                                     
LPUBKZE  EQU   L'PUBKZON+L'PUBKED                                               
                                                                                
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
BUYTAB   DS    0X                  ** BUY KEY DRIVER TABLE **                   
         DC    AL2(L'PBUYKEY)                                                   
                                                                                
         DC    AL1(PBUYKAGY-PBUYKEY,L'PBUYKAGY-1)                               
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
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
         DC    AL1(LQ_TRNGQ)                                                    
                                                                                
         DC    AL1(PBUYKEST-PBUYKEY,L'PBUYKEST-1)                               
         DC    AL2(AEST-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PBUYKACT-PBUYKEY,L'PBUYKACT-1)                               
         DC    X'00',AL1(0)                                                     
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PBUYKLIN-PBUYKEY,L'PBUYKLIN-1)                               
         DC    AL2(ALLLIN-SAVED)                                                
         DC    AL1(LQ_TRNGQ)                                                    
                                                                                
BUYTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
CLTTAB   DS    0X                  ** CLIENT KEY DRIVER TABLE **                
         DC    AL2(L'PCLTKEY)                                                   
                                                                                
         DC    AL1(PCLTKAGY-PCLTKEY,L'PCLTKAGY-1)                               
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PCLTKMED-PCLTKEY,L'PCLTKMED-1)                               
         DC    AL2(AMED-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PCLTKRCD-PCLTKEY,L'PCLTKRCD-1)                               
         DC    X'02',AL1(0)                                                     
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PCLTKCLT-PCLTKEY,L'PCLTKCLT-1)                               
         DC    AL2(CLTRNG-SAVED)                                                
         DC    AL1(LQ_TRNGQ)                                                    
                                                                                
         DC    AL1(PCLTKCLT+L'PCLTKCLT-PCLTKEY)                                 
         DC    AL1(L'PCLTKEY-(PCLTKCLT+L'PCLTKCLT-PCLTKEY)-1)                   
         DC    AL1(0,0)                                                         
         DC    AL1(LK_ILITQ)                                                    
                                                                                
CLTTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
PRDTAB   DS    0X                  ** PRODUCT KEY DRIVER TABLE **               
         DC    AL2(L'PPRDKEY)                                                   
                                                                                
         DC    AL1(PPRDKAGY-PPRDKEY,L'PPRDKAGY-1)                               
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PPRDKMED-PPRDKEY)                                            
         DC    AL1(L'PPRDKMED+L'PPRDKRCD+L'PPRDKCLT-1)                          
         DC    AL2(ASMC-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PPRDKPRD-PPRDKEY,L'PPRDKPRD-1)                               
         DC    AL2(PRDRNG-SAVED)                                                
         DC    AL1(LQ_TRNGQ)                                                    
                                                                                
         DC    AL1(PPRDKPRD+L'PPRDKPRD-PPRDKEY)                                 
         DC    AL1(L'PPRDKEY-(PPRDKPRD+L'PPRDKPRD-PPRDKEY)-1)                   
         DC    AL1(0,0)                                                         
         DC    AL1(LK_ILITQ)                                                    
                                                                                
PRDTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
PUBTAB   DS    0X                  ** PUBLICATION KEY DRIVER TABLE **           
         DC    AL2(L'PUBKEY)                                                    
                                                                                
         DC    AL1(PUBKMED-PUBKEY,L'PUBKMED-1)                                  
         DC    AL2(PUBMED#-SAVED)                                               
         DC    AL1(LQ_TLSTQ)                                                    
                                                                                
         DC    AL1(PUBKPUB-PUBKEY,L'PUBKPUB+L'PUBKZON+L'PUBKED-1)               
         DC    AL2(PUBRNG-SAVED)                                                
         DC    AL1(LQ_TRNGQ)                                                    
                                                                                
         DC    AL1(PUBKAGY-PUBKEY,L'PUBKAGY-1)                                  
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PUBKCOD-PUBKEY,L'PUBKCOD-1)                                  
         DC    AL1(PUBKCODQ,0)                                                  
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PUBKCOD+L'PUBKCOD-PUBKEY)                                    
         DC    AL1(L'PUBKEY-(PUBKCOD+L'PUBKCOD-PUBKEY))                         
         DC    AL1(0,0)                                                         
         DC    AL1(LK_ILITQ)                                                    
                                                                                
PUBTABX  DC    AL1(LK_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* REQUEST MAPS FOR BUY DOWNLOAD                                       *         
***********************************************************************         
                                                                                
REQUEST  DS    0X                                                               
                                                                                
REQINS   DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQINSX+1-*)                                                 
         DC    AL2(M#DLINS)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTBUY-SVRDEF)                                               
         DC    XL4'00'                                                          
                                                                                
BC#MEDC  EQU   1                                                                
         DC    AL2(D#MEDCOD)                                                    
         DC    CL5'MEDCD'                                                       
         DC    AL1(BC#MEDC)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_IDFTQ+LD_ILSTQ+LD_IDADQ)                
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALMED,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(MEDIND-SAVED)                                                
         DC    AL1(L'PAGYKMED)                                                  
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(0,L'PAGYKMED)                                                
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#MEDCS)                                       
         DC    XL4'00'                                                          
                                                                                
BC#CLTC  EQU   2                                                                
         DC    AL2(D#CLTCOD)                                                    
         DC    CL5'CLTCD'                                                       
         DC    AL1(BC#CLTC)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDFTQ+LD_IDADQ)                
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALCLT,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(CLTIND-SAVED)                                                
         DC    AL1(L'PCLTKCLT)                                                  
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(0,3)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#CLTCS)                                       
         DC    XL4'00'                                                          
                                                                                
BC#PRDC  EQU   3                                                                
         DC    AL2(D#PRDCOD)                                                    
         DC    CL5'PRDCD'                                                       
         DC    AL1(BC#PRDC)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ+LD_IDFTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PRDIND-SAVED)                                                
         DC    AL1(L'PPRDKPRD)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    XL4'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PRDCS)                                       
         DC    XL4'00'                                                          
                                                                                
BC#ESTN  EQU   4                                                                
         DC    AL2(D#ESTNUM)                                                    
         DC    CL5'ESTNO'                                                       
         DC    AL1(BC#ESTN)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ+LD_IRNGQ+LD_IDFTQ+LD_IDADQ)                
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(ESTIND-SAVED)                                                
         DC    AL1(L'PESTKEST)                                                  
         DC    AL1(LD_UBINQ)                                                    
         DC    XL4'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#ESTNS)                                       
         DC    XL4'00'                                                          
                                                                                
BC#SDAT  EQU   5                                                                
         DC    AL2(D#STRDAT)                                                    
         DC    CL5'SDATE'                                                       
         DC    AL1(BC#SDAT)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(STRDATE-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'STRDATE)                                                   
         DC    AL1(LD_BDATQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#STDAT)                                       
         DC    XL4'00'                                                          
                                                                                
BC#EDAT  EQU   6                                                                
         DC    AL2(D#ENDDAT)                                                    
         DC    CL5'EDATE'                                                       
         DC    AL1(BC#EDAT)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(ENDDATE-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'ENDDATE)                                                   
         DC    AL1(LD_BDATQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#ENDAT)                                       
         DC    XL4'00'                                                          
                                                                                
BC#PUBC  EQU   7                                                                
         DC    AL2(D#PUBCOD)                                                    
         DC    CL5'PUBCD'                                                       
         DC    AL1(BC#PUBC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PUBCODE-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'PUBCODE)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD)                                       
         DC    XL4'00'                                                          
                                                                                
BC#ZONE  EQU   8                                                                
         DC    AL2(D#PUBZON)                                                    
         DC    CL5'ZONE '                                                       
         DC    AL1(BC#ZONE)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PUBZONE-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'PUBZONE)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PZONE)                                       
         DC    XL4'00'                                                          
                                                                                
BC#EDTN  EQU   9                                                                
         DC    AL2(D#PUBEDT)                                                    
         DC    CL5'EDTN '                                                       
         DC    AL1(BC#EDTN)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PUBEDTN-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'PUBEDTN)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PEDTN)                                       
         DC    XL4'00'                                                          
                                                                                
BC#PGRP  EQU   7                                                                
         DC    AL2(D#PUBGRP)                                                    
         DC    CL5'PGRP '                                                       
         DC    AL1(BC#PGRP)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PUBPGRP-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'PUBPGRP)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PGRPC)                                       
         DC    XL4'00'                                                          
                                                                                
BC#PLST  EQU   7                                                                
         DC    AL2(D#PUBLST)                                                    
         DC    CL5'PLST '                                                       
         DC    AL1(BC#PLST)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PUBPLST-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'PUBPLST)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PLSTC)                                       
         DC    XL4'00'                                                          
                                                                                
BC#SPCD  EQU   10                                                               
         DC    AL2(D#SPCDSC)                                                    
         DC    CL5'SPCDS'                                                       
         DC    AL1(BC#SPCD)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FILTDESC-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'FILTDESC)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#SDESC)                                       
         DC    XL4'00'                                                          
                                                                                
BC#ADCOD EQU   11                                                               
         DC    AL2(D#ADCODE)                                                    
         DC    CL5'ADNUM'                                                       
         DC    AL1(BC#ADCOD)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FILTADNO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'FILTADNO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#ADNO)                                        
         DC    XL4'00'                                                          
                                                                                
BC#INST  EQU   12                                                               
         DC    AL2(D#INSSTA)                                                    
         DC    CL5'INSTA'                                                       
         DC    AL1(BC#INST)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FILTSTAT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'FILTSTAT)                                                  
         DC    AL1(LD_LBINQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#INSTA)                                       
         DC    XL4'00'                                                          
                                                                                
BC#ICNUM EQU   13                                                               
         DC    AL2(D#ICNUM)                                                     
         DC    CL5'ICNUM'                                                       
         DC    AL1(BC#ICNUM)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FILTICNO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'FILTICNO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#ICCOD)                                       
         DC    XL4'00'                                                          
                                                                                
BC#DLINV EQU   14                                                               
         DC    AL2(D#DLIDAT)                                                    
         DC    CL5'DLINV'                                                       
         DC    AL1(BC#DLINV)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(INVDLSW-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'INVDLSW)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#DLINV)                                       
         DC    XL4'00'                                                          
*                                                                               
BC#CLRST EQU   15                                                               
         DC    AL2(D#CLRSTA)                                                    
         DC    CL5'CLRST'                                                       
         DC    AL1(BC#CLRST)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FLTCLRST-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'FLTCLRST)                                                  
         DC    AL1(LD_LBINQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#CSTAT)                                       
         DC    XL4'00'                                                          
*                                                                               
BC#MATST EQU   16                                                               
         DC    AL2(D#MATSTA)                                                    
         DC    CL5'MATST'                                                       
         DC    AL1(BC#MATST)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FLTMATST-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'FLTMATST)                                                  
         DC    AL1(LD_LBINQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#IMSTA)                                       
         DC    XL4'00'                                                          
                                                                                
REQINSX  DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
* REQUEST MAP FOR ONLINE ONLY INSERTION DOWNLOAD                      *         
***********************************************************************         
                                                                                
*REQION   DS    0XL(LH_LNQ)        NOT USED ANYMORE                             
*         DC    AL2(REQIONX+1-*)                                                
*         DC    AL2(M#DLION)                                                    
*         DC    AL1(LH_IMPTR)                                                   
*         DC    AL2(REQINS-SVRDEF)                                              
*         DC    XL4'00'                                                         
*REQIONX  DC    AL1(LD_EOTQ)                                                    
*                                                                               
***********************************************************************         
* REQUEST MAPS FOR INSERTION HISTORY DOWNLOAD                         *         
***********************************************************************         
                                                                                
REQHST   DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQHSTX+1-*)                                                 
         DC    AL2(M#DLHST)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTHST-SVRDEF)                                               
         DC    XL4'00'                                                          
                                                                                
HC#IKEY  EQU   1                                                                
         DC    AL2(D#INSKEY)                                                    
         DC    CL5'INSKY'                                                       
         DC    AL1(HC#IKEY)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(BUYSER-SAVED)                                                
         DC    AL2(0)                                                           
         DC    AL1(BUYSERSL)                                                    
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#KEY)                                         
         DC    XL4'00'                                                          
                                                                                
REQHSTX  DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
* REQUEST MAPS FOR REFRESH INSERTIONS                                 *         
***********************************************************************         
                                                                                
REQREF   DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQREFX+1-*)                                                 
         DC    AL2(M#DLREF)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTREF-SVRDEF)                                               
         DC    XL4'00'                                                          
                                                                                
RC#IKEY  EQU   1                                                                
         DC    AL2(D#INSKEY)                                                    
         DC    CL5'INSKY'                                                       
         DC    AL1(RC#IKEY)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SERIND-SAVED)                                                
         DC    AL1(BUYSERSL)                                                    
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#KEY)                                         
         DC    XL4'00'                                                          
                                                                                
REQREFX  DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAPS FOR INVOICE DOWNLOAD (BY INVOICE NUMBER)               *         
***********************************************************************         
*                                                                               
REQIVN   DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIVNX+1-*)                                                 
         DC    AL2(M#DLINVN)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTIV#-SVRDEF)                                               
         DC    XL4'00'                                                          
*                                                                               
IC#MEDC  EQU   1                                                                
         DC    AL2(D#MEDCOD)                                                    
         DC    CL5'MEDCD'                                                       
         DC    AL1(IC#MEDC)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_IDFTQ+LD_ILSTQ+LD_IDADQ)                
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALMED,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(MEDIND-SAVED)                                                
         DC    AL1(L'PAGYKMED)                                                  
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(0,L'PAGYKMED)                                                
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#MEDCS)                                       
         DC    XL4'00'                                                          
*                                                                               
IC#CLTC  EQU   2                                                                
         DC    AL2(D#CLTCOD)                                                    
         DC    CL5'CLTCD'                                                       
         DC    AL1(IC#CLTC)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(CLTIND-SAVED)                                                
         DC    AL1(L'PCLTKCLT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,3)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#CLTCS)                                       
         DC    XL4'00'                                                          
*                                                                               
IC#PUBC  EQU   3                                                                
         DC    AL2(D#PUBCOD)                                                    
         DC    CL5'PUBCD'                                                       
         DC    AL1(IC#PUBC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(BUYPUB-SAVED)                                                
         DC    AL2(0)                                                           
         DC    AL1(L'BUYPUB)                                                    
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD)                                       
         DC    XL4'00'                                                          
*                                                                               
IC#INVN  EQU   4                                                                
         DC    AL2(D#VINVNO)                                                    
         DC    CL5'INVNU'                                                       
         DC    AL1(IC#INVN)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#INVVAL)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(INVVNDR#-INVVALSD)                                           
         DC    AL2(0)                                                           
         DC    AL1(L'INVVNDR#)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#VINVN)                                       
         DC    XL4'00'                                                          
*                                                                               
IC#ISDAT EQU   5                                                                
         DC    AL2(D#STRDAT)                                                    
         DC    CL5'ISDAT'                                                       
         DC    AL1(IC#ISDAT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(STRDATE-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'STRDATE)                                                   
         DC    AL1(LD_BDATQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#STDAT)                                       
         DC    XL4'00'                                                          
*                                                                               
IC#ISEND EQU   6                                                                
         DC    AL2(D#ENDDAT)                                                    
         DC    CL5'IEDAT'                                                       
         DC    AL1(IC#ISEND)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(ENDDATE-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'ENDDATE)                                                   
         DC    AL1(LD_BDATQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#ENDAT)                                       
         DC    XL4'00'                                                          
*                                                                               
IC#IMSTA EQU   7                                                                
         DC    AL2(D#IMATST)                                                    
         DC    CL5'IMSTA'                                                       
         DC    AL1(IC#IMSTA)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(INVMATST-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'INVMATST)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#IMSTA)                                       
         DC    XL4'00'                                                          
*                                                                               
REQIVNX  DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAPS FOR INVOICE DOWNLOAD (BY INVOICE KEY)                  *         
***********************************************************************         
*                                                                               
REQIVK   DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIVKX+1-*)                                                 
         DC    AL2(M#DLINVK)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTIVK-SVRDEF)                                               
         DC    XL4'00'                                                          
*                                                                               
IC#DLBUY EQU   1                                                                
         DC    AL2(D#DLBDAT)                                                    
         DC    CL5'DLBUY'                                                       
         DC    AL1(IC#DLBUY)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(INSDLSW-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'INSDLSW)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#DLINS)                                       
         DC    XL4'00'                                                          
*                                                                               
IC#IKEY  EQU   2                                                                
         DC    AL2(D#INVKEY)                                                    
         DC    CL5'INVKY'                                                       
         DC    AL1(IC#IKEY)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SERIND-SAVED)                                                
         DC    AL1(INVKEYSL)                                                    
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#INVKY)                                       
         DC    XL4'00'                                                          
*                                                                               
REQIVKX  DC    AL1(LD_EOTQ)                                                     
*                                                                               
***********************************************************************         
* REQUEST MAPS FOR PROFILE DOWNLOAD                                   *         
***********************************************************************         
*                                                                               
REQPRFD  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQPRFDX+1-*)                                                
         DC    AL2(M#DLPROF)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTPRFV-SVRDEF)                                              
         DC    XL4'00'                                                          
*                                                                               
PD#PRFNM EQU   1                                                                
         DC    AL2(D#PROFNM)                                                    
         DC    CL5'PROFN'                                                       
         DC    AL1(PD#PRFNM)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#PROFVL)                                                    
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PROFNAME-PROFVALD)                                           
         DC    AL2(0)                                                           
         DC    AL1(L'PROFNAME)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#PRFNM)                                       
         DC    XL4'00'                                                          
*                                                                               
PD#MEDCD EQU   2                                                                
         DC    AL2(D#MEDCOD)                                                    
         DC    CL5'MEDCD'                                                       
         DC    AL1(PD#MEDCD)                                                    
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_IDFTQ+LD_ILSTQ+LD_IDADQ)                
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALMED,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(MEDIND-SAVED)                                                
         DC    AL1(L'PAGYKMED)                                                  
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(0,L'PAGYKMED)                                                
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#MEDCS)                                       
         DC    XL4'00'                                                          
*                                                                               
PD#CLTCD EQU   3                                                                
         DC    AL2(D#CLTCOD)                                                    
         DC    CL5'CLTCD'                                                       
         DC    AL1(PD#CLTCD)                                                    
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDFTQ+LD_IDADQ)                
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALCLT,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(CLTIND-SAVED)                                                
         DC    AL1(L'PCLTKCLT)                                                  
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(0,3)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#CLTCS)                                       
         DC    XL4'00'                                                          
*                                                                               
REQPRFDX DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
* REQUEST MAPS FOR CFM CLIENT DOWNLOAD                                *         
***********************************************************************         
                                                                                
REQCFMC  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQCFMCX+1-*)                                                
         DC    AL2(M#CFMCLT)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTCFMC-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
REQCFMCX DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
* REQUEST MAPS FOR CFM PRODUCT DOWNLOAD                               *         
***********************************************************************         
                                                                                
REQCFMP  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQCFMPX+1-*)                                                
         DC    AL2(M#CFMPRD)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTCFMP-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(4)              SYSTEM CODE                                  
         DC    CL5'SysCd'                                                       
         DC    AL1(4)                                                           
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SMCIND-SAVED)                                                
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ+LD_SCOLQ)                                           
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#SCODE)                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(5)              MEDIA CODE                                   
         DC    CL5'MedCd'                                                       
         DC    AL1(4)                                                           
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SMCIND-SAVED)                                                
         DC    AL1(L'PPRDKMED)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#MED)                                         
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(6)              CLIENT CODE                                  
         DC    CL5'CliCd'                                                       
         DC    AL1(4)                                                           
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SMCIND-SAVED)                                                
         DC    AL1(L'PPRDKCLT)                                                  
         DC    AL1(LD_CHARQ+LD_ECOLQ)                                           
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#CLTC)                                        
         DC    XL4'00'                                                          
                                                                                
REQCFMPX DC    AL1(LD_EOTQ)                                                     
                                                                                
REQCFMV  DS    0XL(LH_LNQ)         ** CFM VENDOR DOWNLOAD **                    
         DC    AL2(REQCFMVX+1-*)                                                
         DC    AL2(M#CFMPUB)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTCFMV-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(1)              SYSTEM CODE                                  
         DC    CL5'SysCd'                                                       
         DC    AL1(1)                                                           
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SMCIND-SAVED)                                                
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ+LD_SCOLQ)                                           
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#SCODE)                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(2)              MEDIA CODE                                   
         DC    CL5'MedCd'                                                       
         DC    AL1(2)                                                           
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SMCIND-SAVED)                                                
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ+LD_ECOLQ)                                           
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(PRTSYSQ),AL2(PP#MED)                                         
         DC    XL4'00'                                                          
                                                                                
REQCFMVX DC    AL1(LD_EOTQ)                                                     
                                                                                
                                                                                
REQUESTX DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR BUY DOWNLOAD                                        *         
***********************************************************************         
                                                                                
OUTBUY   DS    0X                                                               
                                                                                
R#BUY    EQU   1                                                                
         DC    AL2(OUTBUYX-*)                                                   
         DC    AL2(R#BUY)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
BUYC     DC    AL2(BUYCX-*)                                                     
         DC    AL2(E#BUY),C'Array'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#BUY)                                                       
         DC    CL5'DLBUY'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYBUY-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
BUYCX    DS    0X                                                               
*                                                                               
INVC     DC    AL2(INVCX-*)                                                     
         DC    AL2(E#INVHDR),C'Array'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00000000'                                                    
         DC    AL2(E#INVHDR)                                                    
         DC    CL5'DLINV'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYINV-SVRDEF),AL1(0,0)                                      
         DC    XL4'00000000'                                                    
INVCX    DS    0X                                                               
*                                                                               
PUBC     DC    AL2(PUBCX-*)                                                     
         DC    AL2(E#PUB),C'Array'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#PUB)                                                       
         DC    CL5'DLPUB'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYPUB-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
PUBCX    DS    0X                                                               
                                                                                
REPC     DC    AL2(REPCX-*)                                                     
         DC    AL2(E#REP),C'Array'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#REP)                                                       
         DC    CL5'DLREP'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYREP-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
REPCX    DS    0X                                                               
                                                                                
OUTBUYX  DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR INSERTION HISTORY DOWNLOAD                          *         
***********************************************************************         
                                                                                
OUTHST   DS    0X                                                               
                                                                                
R#HST    EQU   2                                                                
         DC    AL2(OUTHSTX-*)                                                   
         DC    AL2(R#HST)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
IHINS    DC    AL2(IHINSX-*)                                                    
         DC    AL2(E#BUY),C'Array'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#BUY)                                                       
         DC    CL5'DLINS'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYIHINS-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
IHINSX   DS    0X                                                               
                                                                                
IOAC     DC    AL2(IOACX-*)                                                     
         DC    AL2(E#HSTINS),C'Array'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#HSTINS)                                                    
         DC    CL5'DLIOA'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYIOA-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
IOACX    DS    0X                                                               
                                                                                
ICAC     DC    AL2(ICACX-*)                                                     
         DC    AL2(E#HSTACT),C'Array'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#HSTACT)                                                    
         DC    CL5'DLICA'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYICA-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
ICACX    DS    0X                                                               
                                                                                
IBAC     DC    AL2(IBACX-*)                                                     
         DC    AL2(E#HSTBIL),C'Array'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#HSTBIL)                                                    
         DC    CL5'DLIBA'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYIBA-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
IBACX    DS    0X                                                               
                                                                                
IPAC     DC    AL2(IPACX-*)                                                     
         DC    AL2(E#HSTPAY),C'Array'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#HSTPAY)                                                    
         DC    CL5'DLIPA'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYIPA-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
IPACX    DS    0X                                                               
                                                                                
OUTHSTX  DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR BUY REFRESH DOWNLOAD                                *         
***********************************************************************         
                                                                                
OUTREF   DS    0X                                                               
                                                                                
R#REF    EQU   1                                                                
         DC    AL2(OUTREFX-*)                                                   
         DC    AL2(R#REF)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
REFC     DC    AL2(REFCX-*)                                                     
         DC    AL2(E#BUY),C'Array'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#BUY)                                                       
         DC    CL5'DLREF'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYREF-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
REFCX    DS    0X                                                               
                                                                                
OUTREFX  DS    0X                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* OUTPUT MAPS FOR INVOICE DOWNLOAD (BY INVOICE NUMBER)                *         
***********************************************************************         
*                                                                               
OUTIV#   DS    0X                                                               
*                                                                               
R#IV#    EQU   1                                                                
         DC    AL2(OUTIV#X-*)                                                   
         DC    AL2(R#IV#)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
*                                                                               
IV#C     DC    AL2(IV#CX-*)                                                     
         DC    AL2(E#INVHDR),C'Array'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00000000'                                                    
         DC    AL2(E#INVHDR)                                                    
         DC    CL5'DLIV#'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(AYDIV#-SVRDEF),AL1(0,0)                                      
         DC    XL4'00000000'                                                    
*                                                                               
IV#CX    DS    0X                                                               
*                                                                               
IV#BC    DC    AL2(IV#BCX-*)                                                    
         DC    AL2(E#BUY),C'Array'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#BUY)                                                       
         DC    CL5'DLBUY'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYIVB-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
*                                                                               
IV#BCX   DS    0X                                                               
*                                                                               
OUTIV#X  DS    0X                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* OUTPUT MAPS FOR INVOICE DOWNLOAD (BY INVOICE KEY)                   *         
***********************************************************************         
*                                                                               
OUTIVK   DS    0X                                                               
*                                                                               
R#IVK    EQU   1                                                                
         DC    AL2(OUTIVKX-*)                                                   
         DC    AL2(R#IVK)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
*                                                                               
IVKC     DC    AL2(IVKCX-*)                                                     
         DC    AL2(E#INVHDR),C'Array'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00000000'                                                    
         DC    AL2(E#INVHDR)                                                    
         DC    CL5'DLIVK'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(AYDIVK-SVRDEF),AL1(0,0)                                      
         DC    XL4'00000000'                                                    
*                                                                               
IVKCX    DS    0X                                                               
*                                                                               
IVKBC    DC    AL2(IVKBCX-*)                                                    
         DC    AL2(E#BUY),C'Array'                                              
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#BUY)                                                       
         DC    CL5'DLBUY'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYIVB-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
*                                                                               
IVKBCX   DS    0X                                                               
*                                                                               
OUTIVKX  DS    0X                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* OUTPUT MAPS FOR PROFILE DOWNLOAD                                    *         
***********************************************************************         
*                                                                               
OUTPRFV  DS    0X                                                               
*                                                                               
R#PRF    EQU   1                                                                
         DC    AL2(OUTPRFVX-*)                                                  
         DC    AL2(R#PRF)                                                       
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
*                                                                               
PRFV     DC    AL2(PRFVX-*)                                                     
         DC    AL2(E#PROFVL),C'Array'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#PROFVL)                                                    
         DC    CL5'DLPRF'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYPROF-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
*                                                                               
PRFVX    DS    0X                                                               
*                                                                               
OUTPRFVX DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR CFM CLIENT DOWNLOAD                                 *         
***********************************************************************         
                                                                                
OUTCFMC  DS    0X                  CFM CLIENT DOWNLOAD                          
                                                                                
         DC    AL2(OUTCFMCX-*)                                                  
         DC    AL2(3)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(OUTCFMCX-*)                                                  
         DC    AL2(3),C'Dummy'                                                  
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(1)                                                           
         DC    C'Array'                                                         
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYCLT-SVRDEF)                                               
         DC    AL1(0,0)                                                         
         DC    XL4'00'                                                          
                                                                                
OUTCFMCX DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR CFM PRODUCT DOWNLOAD                                *         
***********************************************************************         
                                                                                
OUTCFMP  DS    0X                  CFM PRODUCT DOWNLOAD                         
                                                                                
         DC    AL2(OUTCFMPX-*)                                                  
         DC    AL2(3)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(OUTCFMPX-*)                                                  
         DC    AL2(3),C'Dummy'                                                  
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(1)                                                           
         DC    C'Array'                                                         
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYPRD-SVRDEF)                                               
         DC    AL1(0,0)                                                         
         DC    XL4'00'                                                          
                                                                                
OUTCFMPX DS    0X                                                               
                                                                                
OUTCFMV  DS    0X                  ** CFM VENDOR DOWNLOAD **                    
                                                                                
         DC    AL2(OUTCFMVX-*)                                                  
         DC    AL2(3)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(OUTCFMVX-*)                                                  
         DC    AL2(3),C'Dummy'                                                  
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(3)                                                           
         DC    C'Array'                                                         
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYVEN-SVRDEF)                                               
         DC    AL1(0,0)                                                         
         DC    XL4'00'                                                          
                                                                                
OUTCFMVX DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR BUY DOWNLOAD                                   *         
***********************************************************************         
                                                                                
ARYBUY   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(NXTBUY-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(BUYCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
BUYCOL   DS    0X                                                               
                                                                                
         DC    AL2(E#BUY),C'Array'                                              
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYBUYV-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
BUYCOLN  EQU   (*-BUYCOL)/LX_COLSL                                              
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE DOWNLOAD                               *         
***********************************************************************         
*                                                                               
ARYINV   DS    0X                                                               
*                                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(NXTINV-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(INVCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
INVCOL   DS    0X                                                               
*                                                                               
         DC    AL2(E#INVHDR),C'Array'                                           
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYIHV-SVRDEF)                                               
         DC    XL4'00000000'                                                    
*                                                                               
INVCOLN  EQU   (*-INVCOL)/LX_COLSL                                              
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE DOWNLOAD (BY INVOICE NUMBER)           *         
***********************************************************************         
*                                                                               
AYDIV#   DS    0X                                                               
*                                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(NXTIV#-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(IV#COLN)                                                     
         DC    XL4'00'                                                          
                                                                                
IV#COL   DS    0X                                                               
*                                                                               
         DC    AL2(E#INVHDR),C'Array'                                           
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYIHV-SVRDEF)                                               
         DC    XL4'00000000'                                                    
*                                                                               
IV#COLN  EQU   (*-IV#COL)/LX_COLSL                                              
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE DOWNLOAD (BY INVOICE KEY)              *         
***********************************************************************         
*                                                                               
AYDIVK   DS    0X                                                               
*                                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(NXTIVK-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(IVKCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
IVKCOL   DS    0X                                                               
*                                                                               
         DC    AL2(E#INVHDR),C'Array'                                           
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYIHV-SVRDEF)                                               
         DC    XL4'00000000'                                                    
*                                                                               
IVKCOLN  EQU   (*-IVKCOL)/LX_COLSL                                              
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR BUY REFRESH DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYREF   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(NXTSER-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(REFCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
REFCOL   DS    0X                                                               
                                                                                
         DC    AL2(E#BUY),C'Array'                                              
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYBUYV-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
REFCOLN  EQU   (*-REFCOL)/LX_COLSL                                              
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR BUY VALUES DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYBUYV  DS    0X                                                               
                                                                                
         DC    AL1(B#SAVED+LX_INELQ,0,0)                                        
         DC    AL2(BUYVALS-SAVED)                                               
         DC    AL2(1)              SINGLE ROW                                   
         DC    AL2(0,0)                                                         
         DC    AL1(BUYVCOLN)                                                    
         DC    XL4'00'                                                          
                                                                                
BUYVCOL  DS    0X                                                               
                                                                                
         DC    AL2(D#INSKEY),C'InsKy'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYSER-SAVED),AL1(BUYSERLL)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PRDCOD),C'PrdCd'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBUYKPRD-PBUYKEY),AL1(L'PBUYKPRD)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ESTNUM),C'Estno'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBUYKEST-PBUYRECD),AL1(L'PBUYKEST)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PUBCOD),C'PubCd'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPUB-SAVED),AL1(L'BUYPUB)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PUBREP),C'P/Rep'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(PBUREC-SAVED+(PBUREP-PBUD)),AL1(L'PBUREP)                    
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#SPCDSC),C'Space'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYSPACE-SAVED),AL1(L'BUYSPACE)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REGDSP),C'RegDs'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREGDS-SAVED),AL1(L'BUYREGDS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ILLPAN),C'IllPa'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYILLPA-SAVED),AL1(L'BUYILLPA)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#SHOWGS),C'Shows'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYSHOWS-SAVED),AL1(L'BUYSHOWS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#UNTRAT),C'URate'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYRATE-SAVED),AL1(L'BUYRATE)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PREMUM),C'Prem.'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPREM-SAVED),AL1(L'BUYPREM)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ALLOCS),C'Alloc'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYALLOC-SAVED),AL1(L'BUYALLOC)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ADCODE),C'Ad/No'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDJOB-PBUYKEY),AL1(L'PBDJOB)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ADCAP),C'Adcap'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYADCAP-SAVED),AL1(L'BUYADCAP)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSSTA),C'Bstat'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYBSTAT-SAVED),AL1(L'BUYBSTAT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSIOR),C'IONum'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYORDNO-SAVED),AL1(L'BUYORDNO)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REPROQ),C'Repro'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREPRO-SAVED),AL1(L'BUYREPRO)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#BUYERC),C'Buyer'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDBUYER-PBUYKEY),AL1(L'PBDBUYER)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GST),C'GSTax'                                              
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDGST-PBUYKEY),AL1(L'PBDGST)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PST),C'PSTax'                                              
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPST-SAVED),AL1(L'BUYPST)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSDAT),C'IDate'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBUYKDAT-PBUYKEY),AL1(L'PBUYKDAT)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSBYL),C'ILine'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYLINE-SAVED),AL1(L'BUYLINE)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSDA2),C'IDat2'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDIDAT2-PBUYKEY),AL1(L'PBDIDAT2)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#SPCDAT),C'SCDat'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDCDATE-PBUYKEY),AL1(L'PBDCDATE)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ONSDAT),C'OSDat'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDSDATE-PBUYKEY),AL1(L'PBDSDATE)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#BBLDAT),C'BLDat'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BLBLDT-SAVED),AL1(L'BLBLDT)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PBLDAT),C'PLDat'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDPDATE-PBUYKEY),AL1(L'PBDPDATE)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#SHPDAT),C'ShpDt'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYSHPDT-SAVED),AL1(L'BUYSHPDT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#MCLXDT),C'MCXDt'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYMCLXD-SAVED),AL1(L'BUYMCLXD)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#MCLXDY),C'MCXDy'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYMCXDY-SAVED),AL1(L'BUYMCXDY)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#IORDAT),C'IODat'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYORDDT-SAVED),AL1(L'BUYORDDT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#MCLDAT),C'MCDat'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDMDATE-PBUYKEY),AL1(L'PBDMDATE)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REGCO1),C'RCom1'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREGC1-SAVED),AL1(L'BUYREGC1)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REGCO2),C'RCom2'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREGC2-SAVED),AL1(L'BUYREGC2)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REGCO3),C'RCom3'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREGC3-SAVED),AL1(L'BUYREGC3)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REGCO4),C'RCom4'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREGC4-SAVED),AL1(L'BUYREGC4)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REGCO5),C'RCom5'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREGC5-SAVED),AL1(L'BUYREGC5)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSCO1),C'ICom1'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYINSC1-SAVED),AL1(L'BUYINSC1)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSCO2),C'ICom2'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYINSC2-SAVED),AL1(L'BUYINSC2)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSCO3),C'ICom3'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYINSC3-SAVED),AL1(L'BUYINSC3)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSCO4),C'ICom4'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYINSC4-SAVED),AL1(L'BUYINSC4)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSCO5),C'ICom5'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYINSC5-SAVED),AL1(L'BUYINSC5)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#POSIN1),C'PIns1'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPOSI1-SAVED),AL1(L'BUYPOSI1)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#POSIN2),C'PIns2'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPOSI2-SAVED),AL1(L'BUYPOSI2)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#POSIN3),C'PIns3'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPOSI3-SAVED),AL1(L'BUYPOSI3)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#POSIN4),C'PIns4'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPOSI4-SAVED),AL1(L'BUYPOSI4)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#POSIN5),C'PIns5'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPOSI5-SAVED),AL1(L'BUYPOSI5)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GRSORD),C'GrsOr'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(GROSS-SAVED),AL1(L'GROSS)                                    
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NETORD),C'NetOr'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYNETOR-SAVED),AL1(L'BUYNETOR)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GLCDO),C'GLCDO'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BLABLE-SAVED),AL1(L'BLABLE)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NLCDO),C'NLCDO'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(PYABLE-SAVED),AL1(L'PYABLE)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#COMPCT),C'ACPct'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYACP-SAVED),AL1(L'BUYACP)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#COMAMT),C'ACAmt'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(AGYCOM-SAVED),AL1(L'AGYCOM)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#DSCPCT),C'CDPct'                                           
         DC    AL1(B#SAVED,0,LX_IXZEQ)                                          
         DC    AL2(BUYCDPCT-SAVED),AL1(L'BUYCDPCT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#DSCAMT),C'CDAmt'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(CSHDSC-SAVED),AL1(L'CSHDSC)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TAXPCT),C'TXPct'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDTAX-PBUYKEY),AL1(L'PBDTAX)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TAXAMT),C'TXAmt'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(TAX-SAVED),AL1(L'TAX)                                        
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#BLDDAT),C'BDDat'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYBINVD-SAVED),AL1(L'BUYBINVD)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GRSBD),C'GrsBd'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BGROSS-SAVED),AL1(L'BGROSS)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NETBD),C'NetBd'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYNETBD-SAVED),AL1(L'BUYNETBD)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GLCDB),C'GLCBD'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BILLED-SAVED),AL1(L'BILLED)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NLCDB),C'NLCDB'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYBLNCD-SAVED),AL1(L'BUYBLNCD)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INVNUM),C'BInv#'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYBINVN-SAVED),AL1(L'BUYBINVN)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PAYDAT),C'PayDt'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPAYDT-SAVED),AL1(L'BUYPAYDT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GRSPD),C'GrsPd'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(PGROSS-SAVED),AL1(L'PGROSS)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NETPD),C'NetPd'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYNETPD-SAVED),AL1(L'BUYNETPD)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GLCDP),C'GLCDP'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(PAID-SAVED),AL1(L'PAID)                                      
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NLCDP),C'NLCDP'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPLNCD-SAVED),AL1(L'BUYPLNCD)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PAYREP),C'PeRep'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPREP-SAVED),AL1(L'BUYPREP)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CHECKN),C'ChkNo'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYCHKNO-SAVED),AL1(L'BUYCHKNO)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CKDATE),C'ChkDt'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYCHKDT-SAVED),AL1(L'BUYCHKDT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PLCOST),C'PLCst'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDPLCOS-PBUYKEY),AL1(L'PBDPLCOS)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#SPREP),C'SpRep'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYSREP-SAVED),AL1(L'BUYSREP)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#SPECFH),C'SpFiH'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYSFH-SAVED),AL1(L'BUYSFH)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GRSAC),C'GrsAC'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYGRSAC-SAVED),AL1(L'BUYGRSAC)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NETAC),C'NetAC'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYNETAC-SAVED),AL1(L'BUYNETAC)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GLCAC),C'GLCAC'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYGLCAC-SAVED),AL1(L'BUYGLCAC)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NLCAC),C'NLCAC'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYNLCAC-SAVED),AL1(L'BUYNLCAC)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#MCHDAT),C'MchDt'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYMCHDT-SAVED),AL1(L'BUYMCHDT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#MCHINV),C'MchIN'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYMCHIN-SAVED),AL1(L'BUYMCHIN)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TSHAPR),C'TAppr'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYTAPPR-SAVED),AL1(L'BUYTAPPR)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TSHSTA),C'TStat'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYTSTAT-SAVED),AL1(L'BUYTSTAT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TSHNOT),C'PageN'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYPAGEN-SAVED),AL1(L'BUYPAGEN)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TSHCO1),C'TCom1'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYTSHC1-SAVED),AL1(L'BUYTSHC1)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TSHCO2),C'TCom2'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYTSHC2-SAVED),AL1(L'BUYTSHC2)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TSHCO3),C'TCom3'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYTSHC3-SAVED),AL1(L'BUYTSHC3)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TSHCO4),C'TCom4'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYTSHC4-SAVED),AL1(L'BUYTSHC4)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#DEFCIR),C'DECir'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYDECIR-SAVED),AL1(L'BUYDECIR)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REPNTS),C'Repts'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREPTS-SAVED),AL1(L'BUYREPTS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ESTIMP),C'EImps'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYEIMPS-SAVED),AL1(L'BUYEIMPS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACTIMP),C'AImps'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYAIMPS-SAVED),AL1(L'BUYAIMPS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ECPM),C'ECPM '                                             
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYECPM-SAVED),AL1(L'BUYECPM)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACPM),C'ACPM '                                             
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYACPM-SAVED),AL1(L'BUYACPM)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#WSJBUY),C'WSJby'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYWSJ-SAVED),AL1(L'BUYWSJ)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#SITELO),C'Isite'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYISITE-SAVED),AL1(L'BUYISITE)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ICNUM),C'ICnum'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYICNUM-SAVED),AL1(L'BUYICNUM)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TOTIMP),C'ICimp'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYICIMP-SAVED),AL1(L'BUYICIMP)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TOTCPM),C'ICcpm'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYICCPM-SAVED),AL1(L'BUYICCPM)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#TOTRAT),C'ICrat'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYICRAT-SAVED),AL1(L'BUYICRAT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ICINUM),C'ICins'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYICINS-SAVED),AL1(L'BUYICINS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CLICK),C'Click'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYCLICK-SAVED),AL1(L'BUYCLICK)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#VIEWS),C'Views'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYVIEWS-SAVED),AL1(L'BUYVIEWS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#FSINS),C'FSIns'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYFSINS-SAVED),AL1(L'BUYFSINS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CONUVL),C'CUVal'                                           
         DC    AL1(B#BUY,0,LX_IXNUQ)                                            
         DC    AL2(PBDCU-PBUYKEY),AL1(L'PBDCU)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CONLIE),C'ConLE'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYCLE-SAVED),AL1(L'BUYCLE)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REFNUM),C'RefNo'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYREFNO-SAVED),AL1(L'BUYREFNO)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ISSNM),C'IssNm'                                            
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYISSNM-SAVED),AL1(L'BUYISSNM)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#UPLDID),C'UplID'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYUPLID-SAVED),AL1(L'BUYUPLID)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#CLRSTA),C'IClrS'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYCLRST-SAVED),AL1(1)                                       
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#TEAREC),C'ITSRS'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYTSRST-SAVED),AL1(1)                                       
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#MATSTA),C'IMatS'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYMATST-SAVED),AL1(L'BUYMATST)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#DISSTA),C'IDisS'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYDISST-SAVED),AL1(L'BUYDISST)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#NPYBLE),C'NPybl'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYNPYBL-SAVED),AL1(L'BUYNPYBL)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'02000000'                                                    
*                                                                               
         DC    AL2(D#GPYBLE),C'GPybl'                                           
         DC    AL1(B#SAVED,0,LX_IXNUQ)                                          
         DC    AL2(BUYGPYBL-SAVED),AL1(L'BUYGPYBL)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'02000000'                                                    
*                                                                               
* NESTED ARRAY FOR ADDITIONAL CHARGES                                           
*                                                                               
         DC    AL2(E#ACR),C'Array'                                              
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYACR-SVRDEF)                                               
         DC    XL4'00'                                                          
*                                                                               
* NESTED ARRAY FOR INVOICE ELEMENTS IN INSERTION                                
*                                                                               
         DC    AL2(E#INV),C'Array'                                              
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYBINV-SVRDEF)                                              
         DC    XL4'02000000'                                                    
*                                                                               
* NESTED ARRAY FOR CUSTOM COLUMN ELEMENTS IN INSERTION                          
*                                                                               
         DC    AL2(E#BCC),C'Array'                                              
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYBCC-SVRDEF)                                               
         DC    XL4'02000000'                                                    
*                                                                               
BUYVCOLN EQU   (*-BUYVCOL)/LX_COLSL                                             
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE HEADER VALUE DOWNLOAD                  *         
***********************************************************************         
*                                                                               
ARYIHV   DS    0X                                                               
*                                                                               
         DC    AL1(B#INVVAL+LX_INELQ,0,0)                                       
         DC    AL2(0)                                                           
         DC    AL2(1)              SINGLE ROW                                   
         DC    AL2(0,0)                                                         
         DC    AL1(IHVCOLN)                                                     
         DC    XL4'00'                                                          
*                                                                               
IHVCOL   DS    0X                                                               
*                                                                               
         DC    AL2(D#INVKEY),C'InvKy'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVKEY-INVVALSD),AL1(INVKEYSL)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#GENSTA),C'HGSta'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVHGSTA-INVVALSD),AL1(L'INVHGSTA)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#CLTCOD),C'ICltC'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVCLTC-INVVALSD),AL1(L'INVCLTC)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#PUBCOD),C'IPubC'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVHPUBC-INVVALSD),AL1(L'INVHPUBC)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#VINVNO),C'VInv#'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVVNDR#-INVVALSD),AL1(L'INVVNDR#)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#INVSTA),C'IvSta'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVSTAT-INVVALSD),AL1(L'INVSTAT)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#IPRSTR),C'SPStr'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVPSTR-INVVALSD),AL1(L'INVPSTR)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#IPREND),C'SPEnd'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVPEND-INVVALSD),AL1(L'INVPEND)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#INVDAT),C'IvDat'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVDATE-INVVALSD),AL1(L'INVDATE)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#INVTOT),C'IvTot'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVTOTL-INVVALSD),AL1(L'INVTOTL)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#ITOTYP),C'IvTTy'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVTTYP-INVVALSD),AL1(L'INVTTYP)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#SPREP),C'IvSRp'                                            
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVSREP-INVVALSD),AL1(L'INVSREP)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
* NESTED ARRAY FOR HEADER COMMENTS                                              
*                                                                               
         DC    AL2(E#IHDRCM),C'Array'                                           
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYINVC-SVRDEF)                                              
         DC    XL4'00000000'                                                    
*                                                                               
* NESTED ARRAY FOR DETAILS                                                      
*                                                                               
         DC    AL2(E#INVDET),C'Array'                                           
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYIDV-SVRDEF)                                               
         DC    XL4'00000000'                                                    
*                                                                               
IHVCOLN  EQU   (*-IHVCOL)/LX_COLSL                                              
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE DETAIL VALUE DOWNLOAD                  *         
***********************************************************************         
*                                                                               
ARYIDV   DS    0X                                                               
*                                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(GETIDV-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(IDVCOLN)                                                     
         DC    XL4'00'                                                          
*                                                                               
IDVCOL   DS    0X                                                               
*                                                                               
         DC    AL2(D#ITMSQN),C'ItmSN'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVISQN-INVVALSD),AL1(L'INVISQN)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#GENSTA),C'DGSta'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVDGSTA-INVVALSD),AL1(L'INVDGSTA)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#INSKEY),C'InsKy'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVINSKY-INVVALSD),AL1(L'INVINSKY)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#UNTRAT),C'IRate'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVRATE-INVVALSD),AL1(L'INVRATE)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#PREMUM),C'IPrem'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVPREM-INVVALSD),AL1(L'INVPREM)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#NETORD),C'INet '                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVNET-INVVALSD),AL1(L'INVNET)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#GRSORD),C'IGrss'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVGROSS-INVVALSD),AL1(L'INVGROSS)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#NUMLIN),C'I#Lin'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INV#LINE-INVVALSD),AL1(L'INV#LINE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#INSDAT),C'IInsD'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVINSDT-INVVALSD),AL1(L'INVINSDT)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#SPCDSC),C'ISpDs'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVSPDSP-INVVALSD),AL1(L'INVSPDSP)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#ADCAP),C'IAdCp'                                            
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVADCAP-INVVALSD),AL1(L'INVADCAP)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#CLTCOD),C'ICltC'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVCLTCD-INVVALSD),AL1(L'INVCLTCD)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#PRDCOD),C'IPrdC'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVPRDCD-INVVALSD),AL1(L'INVPRDCD)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#PUBCOD),C'IPubC'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVDPUBC-INVVALSD),AL1(L'INVDPUBC)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
* NESTED ARRAY FOR DETAIL COMMENTS                                              
*                                                                               
         DC    AL2(E#IDETCM),C'Array'                                           
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYINVC-SVRDEF)                                              
         DC    XL4'00000000'                                                    
*                                                                               
IDVCOLN  EQU   (*-IDVCOL)/LX_COLSL                                              
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE COMMENT DOWNLOAD                       *         
***********************************************************************         
*                                                                               
ARYINVC  DS    0X                                                               
*                                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(GETIVC-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(IVCCOLN)                                                     
         DC    XL4'00'                                                          
*                                                                               
IVCCOL   DS    0X                                                               
*                                                                               
         DC    AL2(D#GENSTA),C'DGSta'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVCGSTA-INVVALSD),AL1(L'INVCGSTA)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#BUYERC),C'BuyrC'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVPIDNM-INVVALSD),AL1(L'INVPIDNM)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#COMMDT),C'ComDt'                                           
         DC    AL1(B#INVVAL,0,LX_IXNUQ)                                         
         DC    AL2(INVCOMDT-INVVALSD),AL1(L'INVCOMDT)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#COMMNT),C'Array'                                           
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(AARYIVC-SVRDEF)                                              
         DC    XL4'00'                                                          
*                                                                               
IVCCOLN  EQU   (*-IVCCOL)/LX_COLSL                                              
*                                                                               
***********************************************************************         
* ARRAY OF ARRAY DEFINITION FOR INVOICE COMMENTS                      *         
***********************************************************************         
*                                                                               
AARYIVC  DS    0X                                                               
*                                                                               
         DC    AL1(B#INVCOM,0,0)                                                
         DC    AL2(0)                                                           
         DC    AL2(IVCOMMMX)                                                    
         DC    AL2(L'IVCOMMNT,0)                                                
         DC    AL1(AIVCCOLN)                                                    
         DC    XL4'00'                                                          
*                                                                               
AIVCCOLS DS    0X                                                               
*                                                                               
         DC    AL2(D#COMMNT),C'Comnt'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(0),AL1(L'IVCOMMNT)                                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
AIVCCOLN EQU   (*-AIVCCOLS)/LX_COLSL                                            
***********************************************************************         
* ARRAY DEFINITION FOR BUY DOWNLOAD (DRIVEN BY INVOICES)              *         
***********************************************************************         
*                                                                               
ARYIVB   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(NXTIVB-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(IVBCOLN)                                                     
         DC    XL4'00'                                                          
*                                                                               
IVBCOL   DS    0X                                                               
*                                                                               
         DC    AL2(E#BUY),C'Array'                                              
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYBUYV-SVRDEF)                                              
         DC    XL4'00'                                                          
*                                                                               
IVBCOLN  EQU   (*-IVBCOL)/LX_COLSL                                              
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR PROFILE VALUES                                 *         
***********************************************************************         
*                                                                               
ARYPROF  DS    0X                                                               
*                                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(GETPRFV-SVRDEF)                                              
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(PROFCOLN)                                                    
         DC    XL4'00'                                                          
*                                                                               
PROFCOL  DS    0X                                                               
*                                                                               
         DC    AL2(D#PROFVL),C'ProfV'                                           
         DC    AL1(B#PROFVL,0,LX_IXNUQ)                                         
         DC    AL2(PROFILEV-PROFVALD),AL1(L'PROFILEV)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
PROFCOLN EQU   (*-PROFCOL)/LX_COLSL                                             
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR ADDITIONAL CHARGE RATES DOWNLOAD               *         
***********************************************************************         
                                                                                
ARYACR   DS    0X                                                               
         DC    AL1(B#SAVED+LX_INELQ,LX_IRADR,B#SAVED+LX_INEAQ)                  
         DC    AL2(BUYATAB-SAVED)                                               
         DC    AL2(BUYATAB#-SAVED)                                              
         DC    AL2(BUYATABL,0)                                                  
         DC    AL1(ACRCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
ACRCOL   DS    0X                                                               
                                                                                
         DC    AL2(D#ACHCOD),C'ACCod'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUYATCOD-BUYATAB),AL1(L'BUYATCOD)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACHGRS),C'ACGrs'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUYATCRG-BUYATAB),AL1(L'BUYATCRG)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACHSAC),C'ACSAC'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(BUYATCOM-BUYATAB),AL1(L'BUYATCOM)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACHCPT),C'ACCC%'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(BUYATCPC-BUYATAB),AL1(L'BUYATCPC)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_SPAKQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACHCDA),C'ACSCD'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(BUYATSCD-BUYATAB),AL1(L'BUYATSCD)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
ACRCOLN  EQU   (*-ACRCOL)/LX_COLSL                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ARRAY OF ARRAY DEFINITION FOR INVOICE DATA IN BUY RECORD            *         
***********************************************************************         
*                                                                               
ARYBINV  DS    0X                                                               
*                                                                               
         DC    AL1(B#INSARY+LX_INELQ,LX_IRADR,B#INSARY+LX_INEAQ)                
         DC    AL2(BINVTAB-INSVARY)                                             
         DC    AL2(BINVTAB#-INSVARY)                                            
         DC    AL2(BINVTABL,0)                                                  
         DC    AL1(BINVCOLN)                                                    
         DC    XL4'00'                                                          
*                                                                               
BINVCOLS DS    0X                                                               
*                                                                               
         DC    AL2(D#INVKEY),C'BIvKy'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BINVKEY-BINVTAB),AL1(L'BINVKEY)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#ITMSQN),C'BSeqN'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BINVSEQ#-BINVTAB),AL1(L'BINVSEQ#)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#VINVNO),C'BVIv#'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BINVVIV#-BINVTAB),AL1(L'BINVVIV#)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
BINVCOLN EQU   (*-BINVCOLS)/LX_COLSL                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ARRAY OF ARRAY DEFINITION FOR CUSTOM COLUMN DATA IN BUY RECORD  *             
***********************************************************************         
*                                                                               
ARYBCC   DS    0X                                                               
*                                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(GETBCC-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(BCCCOLN)                                                     
         DC    XL4'00'                                                          
*                                                                               
BCCCOLS DS    0X                                                                
*                                                                               
         DC    AL2(D#CCSEQN),C'BCCS#'                                           
         DC    AL1(B#BCCBLK,0,LX_IXNUQ)                                         
         DC    AL2(BUYCCSQ#-BCCVARY),AL1(L'BUYCCSQ#)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(D#CCFDAT),C'BCCFD'                                           
         DC    AL1(B#BCCBLK,0,LX_IXNUQ)                                         
         DC    AL2(BUYCCFDA-BCCVARY),AL1(L'BUYCCFDA)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
BCCCOLN  EQU   (*-BCCCOLS)/LX_COLSL                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ARRAY DEFINITION FOR PUBLICATION RECORDS                            *         
***********************************************************************         
                                                                                
ARYPUB   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(GETPUB-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(PUBCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
PUBCOL   DS    0X                                                               
                                                                                
         DC    AL2(D#MEDCOD),C'Media'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PBUMED-PBUD),AL1(L'PBUMED)                                   
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PUBCOD),C'PCode'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PBUCODE-PBUD),AL1(L'PBUCODE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PUBNAM),C'PName'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PBUNAME-PBUD),AL1(L'PBUNAME)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PZNAME),C'PZnam'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PBUZNAME-PBUD),AL1(L'PBUZNAME)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PCITY),C'Pcity'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(PBUCITY-PBUD),AL1(L'PBUCITY)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PSTATE),C'Pstat'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PBUSTATE-PBUD),AL1(L'PBUSTATE)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
PUBCOLN  EQU   (*-PUBCOL)/LX_COLSL                                              
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR PUBLISHER/REP RECORDS                          *         
***********************************************************************         
                                                                                
ARYREP   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(GETREP-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(REPCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
REPCOL   DS    0X                                                               
                                                                                
         DC    AL2(D#MEDCOD),C'Media'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(RBUMED-RBUD),AL1(L'RBUMED)                                   
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PUBREP),C'RCode'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(RBUREP-RBUD),AL1(L'RBUREP)                                   
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#REPNAM),C'RName'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(RBUNAME-RBUD),AL1(L'RBUNAME)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
REPCOLN  EQU   (*-REPCOL)/LX_COLSL                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION HISTORY DOWNLOAD (SERIAL# NOT FOUND) *         
***********************************************************************         
                                                                                
ARYIHINS DS    0X                                                               
         DC    AL1(B#SAVED+LX_INELQ,0,0)                                        
         DC    AL2(BUYFOUND-SAVED)                                              
         DC    AL2(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(BUYFOUND-BUYFOUND,NOQ)                                       
         DC    AL1(IHINSCLN)                                                    
         DC    XL4'00'                                                          
                                                                                
IHINSCOL DS    0X                                                               
                                                                                
         DC    AL2(D#INSKEY),C'InsKy'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(BUYSER-SAVED),AL1(BUYSERSL)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSSTA),C'Bstat'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(BUYBSTAT-SAVED),AL1(L'BUYBSTAT)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
IHINSCLN EQU   (*-IHINSCOL)/LX_COLSL                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION ORDER HISTORY                        *         
***********************************************************************         
                                                                                
ARYIOA   DS    0X                                                               
         DC    AL1(LX_IADDQ+B#WORKD+LX_INELQ,LX_IRADR,B#SAVED+LX_INERQ)         
         DC    AL2(AIO6-WORKD)                                                  
         DC    AL2(IOA#-SAVED)                                                  
         DC    AL2(IOAL,0)                                                      
         DC    AL1(IOACOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
IOACOL   DS    0X                                                               
                                                                                
         DC    AL2(D#INSDTP),C'IODat'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(IOADATE-IOAD),AL1(L'IOADATE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSNUM),C'IONum'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(IOANUM-IOAD),AL1(L'IOANUM)                                   
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INSTYP),C'IOTyp'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(IOATYPE-IOAD),AL1(L'IOATYPE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
IOACOLN  EQU   (*-IOACOL)/LX_COLSL                                              
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION CHANGE HISTORY                       *         
***********************************************************************         
                                                                                
ARYICA   DS    0X                                                               
         DC    AL1(LX_IADDQ+B#WORKD+LX_INELQ,LX_IRADR,B#SAVED+LX_INERQ)         
         DC    AL2(AIO5-WORKD)                                                  
         DC    AL2(ICA#-SAVED)                                                  
         DC    AL2(ICAL,0)                                                      
         DC    AL1(ICACOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
ICACOL   DS    0X                                                               
                                                                                
         DC    AL2(D#ACTTYP),C'AcTyp'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(ICATYPE-ICAD),AL1(L'ICATYPE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACTDAT),C'AcDat'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(ICADATE-ICAD),AL1(L'ICADATE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACTUSR),C'AcUsr'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(ICAWHOM-ICAD),AL1(L'ICAWHOM)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#ACTACT),C'AcAct'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(ICAWHAT-ICAD),AL1(L'ICAWHAT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
ICACOLN  EQU   (*-ICACOL)/LX_COLSL                                              
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION BILLING HISTORY                      *         
***********************************************************************         
                                                                                
ARYIBA   DS    0X                                                               
         DC    AL1(LX_IADDQ+B#WORKD+LX_INELQ,LX_IRADR,B#SAVED+LX_INERQ)         
         DC    AL2(AIO3-WORKD)                                                  
         DC    AL2(IBA#-SAVED)                                                  
         DC    AL2(IBAL,0)                                                      
         DC    AL1(IBACOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
IBACOL   DS    0X                                                               
                                                                                
         DC    AL2(D#BLDDAT),C'BLDat'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(IBADATE-IBAD),AL1(L'IBADATE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GRSBD),C'BLGrs'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(IBAGROSS-IBAD),AL1(L'IBAGROSS)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NETBD),C'BLNet'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(IBANET-IBAD),AL1(L'IBANET)                                   
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NLCDB),C'BLN-C'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(IBANLCDB-IBAD),AL1(L'IBANLCDB)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#INVNUM),C'BLIn#'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(IBAINVNO-IBAD),AL1(L'IBAINVNO)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
IBACOLN  EQU   (*-IBACOL)/LX_COLSL                                              
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR INSERTION PAYING HISTORY                       *         
***********************************************************************         
                                                                                
ARYIPA   DS    0X                                                               
         DC    AL1(LX_IADDQ+B#WORKD+LX_INELQ,LX_IRADR,B#SAVED+LX_INERQ)         
         DC    AL2(AIO4-WORKD)                                                  
         DC    AL2(IPA#-SAVED)                                                  
         DC    AL2(IPAL,0)                                                      
         DC    AL1(IPACOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
IPACOL   DS    0X                                                               
                                                                                
         DC    AL2(D#PAYDAT),C'PDDat'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(IPADATE-IPAD),AL1(L'IPADATE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_BDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#GRSPD),C'PDGrs'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(IPAGROSS-IPAD),AL1(L'IPAGROSS)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NETPD),C'PDNet'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(IPANET-IPAD),AL1(L'IPANET)                                   
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#NLCDP),C'PDN-C'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(IPANLCDP-IPAD),AL1(L'IPANLCDP)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PAYREP),C'PDRep'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(IPAPAYEE-IPAD),AL1(L'IPAPAYEE)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CHECKN),C'PDCk#'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(IPACHECK-IPAD),AL1(L'IPACHECK)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
IPACOLN  EQU   (*-IPACOL)/LX_COLSL                                              
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR CFM CLIENT DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYCLT   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,LX_INERQ)                                
         DC    AL2(NXTCLT-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(CLTCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
CLTCOL   DS    0X                                                               
                                                                                
         DC    AL2(1),C'MedCd'                                                  
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(PCLTKMED-PCLTKEY),AL1(L'PCLTKMED)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(2),C'CltCd'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PCLTKCLT-PCLTKEY),AL1(L'PCLTKCLT)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(3),C'CltNm'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PCLTNAME-PCLTKEY),AL1(L'PCLTNAME)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
CLTCOLN  EQU   (*-CLTCOL)/LX_COLSL                                              
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR CFM PRODUCT DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYPRD   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,LX_INERQ)                                
         DC    AL2(NXTPRD-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(PRDCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
PRDCOL   DS    0X                                                               
                                                                                
         DC    AL2(1),C'MedCd'                                                  
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(PPRDKMED-PPRDKEY),AL1(L'PPRDKMED)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(2),C'CltCd'                                                  
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(PPRDKCLT-PPRDKEY),AL1(L'PPRDKCLT)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(3),C'PrdCd'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PPRDKPRD-PPRDKEY),AL1(L'PPRDKPRD)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(4),C'PrdNm'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PPRDNAME-PPRDKEY),AL1(L'PPRDNAME)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
PRDCOLN  EQU   (*-PRDCOL)/LX_COLSL                                              
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CFM VENDOR DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYVEN   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,LX_INERQ)                                
         DC    AL2(NXTVEN-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(VENCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
VENCOL   DS    0X                                                               
                                                                                
         DC    AL2(13),C'Array'                                                 
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYPUR-SVRDEF)                                               
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(1),C'MedCd'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PUBKMED-PUBRECD),AL1(L'PUBKMED)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(2),C'PubCd'                                                  
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(BUYPUB-SAVED),AL1(L'BUYPUB)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(3),C'PubNm'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PUBNAME-PUBRECD),AL1(L'PUBNAME)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(4),C'PubRC'                                                  
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(PUBPLSH-PUBRECD),AL1(L'PUBPLSH)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
VENCOLN  EQU   (*-VENCOL)/LX_COLSL                                              
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CFM PUBLICATION REP DOWNLOAD                   *         
***********************************************************************         
                                                                                
ARYPUR   DS    0X                                                               
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,LX_INERQ)                                
         DC    AL2(NXTPUR-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(PURCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
PURCOL   DS    0X                                                               
                                                                                
         DC    AL2(1),C'MedCd'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PREPKMED-PREPRECD),AL1(L'PREPKMED)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(2),C'RepCd'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PREPKREP-PREPRECD),AL1(L'PREPKREP)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(3),C'RepNm'                                                  
         DC    AL1(0,0,0)                                                       
         DC    AL2(PREPNAME-PREPRECD),AL1(L'PREPNAME)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
PURCOLN  EQU   (*-PURCOL)/LX_COLSL                                              
         EJECT                                                                  
***********************************************************************         
* LIST OF PRINT SYSTEM FILES TO BE OPENED                             *         
***********************************************************************         
                                                                                
FILES    DS    0X                                                               
         DC    C'PRINT  '          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'NPRTDIR '                                                      
         DC    C'NPRTFIL '                                                      
         DC    C'NPUBDIR '                                                      
         DC    C'NPUBFIL '                                                      
         DC    C'NCTFILE '                                                      
         DC    C'X'                                                             
                                                                                
ABENDS   DC    C'JNEW,KWAN,BOBY:'                                               
SLOWS    DC    C'JNEW,KWAN,BOBY:'                                               
         EJECT                                                                  
SAVED    DSECT                                                                  
                                                                                
ACTVWHAT DS    0CL30               ** ACTIVITY PHRASES **                       
         DSDDL                                                                  
ACTV#    EQU   (PP@ADD-ACTVWHAT)/L'ACTVWHAT                                     
ACTVTYPE DS    0CL8                DEFINITION FOR ACTION WORDS ETC.             
                                                                                
WVALUES  DS    0D                  ** LITERAL VALUES **                         
                                                                                
OADCONS  DS    0A                  ** RELOCATED ADCONS **                       
ABUYTAB  DS    A                                                                
ACLTTAB  DS    A                                                                
APRDTAB  DS    A                                                                
APUBTAB  DS    A                                                                
VPUBVAL  DS    V                                                                
VPUBEDIT DS    V                                                                
VPPBYOUT DS    V                                                                
VFMTINO  DS    V                                                                
VPPBVAL  DS    V                                                                
APBUFFD  DS    A                                                                
ARBUFFD  DS    A                                                                
AIBUFFD  DS    A                                                                
OADCONSN EQU   (*-OADCONS)/L'OADCONS                                            
                                                                                
F100000  DS    F                                                                
                                                                                
DMKEY    DS    CL8                                                              
PRTDIR   DS    CL8                                                              
PRTFIL   DS    CL8                                                              
PUBDIR   DS    CL8                                                              
PUBFIL   DS    CL8                                                              
ZZZ      DS    CL3                                                              
ALL      DS    CL3                                                              
FREE     DS    CL4                                                              
EFFS     DS    XL4                                                              
ALLLIN   DS    XL2                                                              
PZERO    DS    PL1                                                              
P100     DS    PL2                                                              
CLTRNG   DS    0XL4                                                             
PRDRNG   DS    XL6                                                              
PUBRNG   DS    XL12                                                             
BITLIST  DS    XL8                                                              
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
VGETINS  DS    A                                                                
VPSTVAL  DS    A                                                                
                                                                                
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
                                                                                
STRDATE  DS    XL(L'PBUYKDAT)      REQUEST START DATE                           
ENDDATE  DS    XL(L'PBUYKDAT)      REQUEST END DATE                             
                                                                                
PUBCODE  DS    CL8                 PUBLICATION CODE                             
PUBZONE  DS    CL2                 ........... ZONE                             
PUBEDTN  DS    CL3                 ........... EDITION                          
                                                                                
PUBPGRP  DS    CL5                 PUBLICATION GROUP CODE                       
                                                                                
PUBPLST  DS    CL3                 PUBLICATION LIST CODE                        
                                                                                
FILTADNO DS    CL(L'PBDJOB)        AD NUMBER FILTER                             
FILTICNO DS    CL(L'PICCONN)       INTERNET CONTRACT NUMBER FILTER              
FILTDESC DS    CL(L'PBYOSPC1)      SPACE/DESCRIPTION FILTER                     
                                                                                
FILTSTAT DS    X                   ** INSERTION STATUS FILTER **                
FILTSLUD EQU   X'01'               LIVE UNDELETED BUYS                          
FILTSTUD EQU   X'02'               TEST UNDELETED BUYS                          
FILTSLDE EQU   X'04'               LIVE DELETED BUYS                            
FILTSTDE EQU   X'08'               TEST DELETED BUYS                            
*                                                                               
FLTCLRST DS    X                   INSERTION CLEARANCE STATUS FILTER            
FLTCSCLR EQU   X'01'               CLEARED                                      
FLTCSNCL EQU   X'02'               NOT CLEARED                                  
*                                                                               
FLTMATST DS    X                   INSERTION MATCHING STATUS FILTER             
FLTMSPEN EQU   X'01'               PENDING                                      
FLTMSMAT EQU   X'02'               MATCHED                                      
FLTMSDIS EQU   X'04'               DISCREPANT                                   
*                                                                               
PUBMED#  DS    AL2                 N'ENTRIES IN PUBMED LIST                     
PUBMED   DS    16CL(L'PUBKMED)     LIST OF VENDOR MEDIA CODES                   
                                                                                
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
TKEYSV_I DS    XL(IBUKEYL)         TSAR RECORD KEY SAVE AREA (INVOICE)          
*                                                                               
INSDLSW  DS    C                   INSERTION DOWNLOAD SWITCH                    
INVDLSW  DS    C                   INVOICE DOWNLOAD SWITCH                      
*                                  C'Y' = NEED TO DOWNLOAD DATA                 
*                                                                               
INVDLMOD DS    X                   INVOICE DOWNLOAD MODE SWITCH                 
IVDLMD_# EQU   1                   DOWNLOAD BY INVOICE NUMBER                   
IVDLMD_D EQU   2                   DOWNLOAD BY START AND END DATES              
IVDLMD_B EQU   3                   DOWNLOAD BY BUY (INSERTION) RECORDS          
IVDLMD_K EQU   4                   DOWNLOAD BY INVOICE KEY(S)                   
*                                                                               
INVCMSW  DS    X                   INVOICE COMMENT SWITCH (HDR OR DET)          
*                                  PNVCKDTQ = DETAIL COMMENT                    
*                                                                               
INVMATST DS    CL3                 INVOICE MATCHING STATUS FILTER               
*                                                                               
REQVALL  EQU   *-REQVALS                                                        
                                                                                
ANXTSER  DS    A                   A(NEXT SERIAL NUMBER IN POOL)                
NSER     DS    H                   N'SERIAL NUMBERS LEFT TO PROCESS             
                                                                                
FLAG     DS    X                   GENERAL FLAG BYTE                            
READDELS DS    X                   READ DELETED BUYS FLAG                       
SVPUBKEY DS    0XL(L'PUBKEY)       SAVED PUBLICATION KEY                        
SVCLTKEY DS    XL(L'PCLTKEY)       SAVED CLIENT KEY                             
                                                                                
LASTS    DS    0F                  ** LAST TIME VALUES **                       
LASTREP  DS    H                   LAST REP# PROCESSED                          
LASTMED  DS    CL(L'PBUYKMED)                                                   
LASTCLT  DS    CL(L'PBUYKCLT)                                                   
LASTJOB  DS    XL(PJOBKPUB-PJOBKEY)                                             
LASTCAP  DS    CL(L'PJOBCAP1+L'PJOBCAP2+1)                                      
LASTL    EQU   *-LASTS                                                          
                                                                                
CLTPROFS DS    0C                  ** CLIENT BILLING PROFILES **                
CLTPRB1  DS    CL16                B1 PROFILE                                   
CLTPRB1X DS    CL16                B1X PROFILE                                  
CLTPROFL EQU   *-CLTPROFS                                                       
                                                                                
BUYVALS  DS    0X                  ** EXTRACTED BUY RECORD VALUES **            
                                                                                
BUYABILE DS    AL4                 A(LAST BILL ELEMENT)                         
BUYAPAYE DS    AL4                 A(LAST PAY ELEMENT)                          
BUYAIORE DS    AL4                 A(LAST INSERTION ORDER ELEMENT)              
                                                                                
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
BUYALLOC DS    CL47                ALLOCATION LINE                              
BUYADCAP DS    CL(L'LASTCAP)       JOB CAPTION                                  
                                                                                
BUYCDPCT DS    XL(L'PBDCD)         CASH DISCOUNT PERCENT                        
BUYACP   DS    PL(L'PBDACP)        AGENCY COMMISSION PERCENT                    
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
BUYBINVN DS    CL9                 BILLING INVOICE NUMBER                       
BUYBINVD DS    XL(L'PBLDATE)       BILLING DATE                                 
BUYREFNO DS    CL(L'PBREFNO)       REFERENCE NUMBER                             
BUYCHKNO DS    CL(L'PPCLCHK)       CHECK NUMBER                                 
BUYCHKDT DS    CL(L'PPCLCHDT)      CHECK DATE                                   
BUYCLE   DS    CL12                CONTRACT LINEAGE EQUIVALENCY                 
BUYSPACE DS    CL(L'PBDSPACE)      SPACE DESCRIPTION                            
BUYRATE  DS    CL11                RATE                                         
BUYPREM  DS    CL11                PREMIUM                                      
BUYPST   DS    CL4                 PST (EG - PQ=S)                              
                                                                                
BUYBSTAT DS    X                   ** BUY STATUS **                             
BUYBSLIV EQU   X'01'               LIVE BUY                                     
BUYBSTST EQU   X'02'               TEST BUY                                     
BUYBSDEL EQU   X'04'               DELETED BUY                                  
BUYBSNFD EQU   X'08'               BUY WAS NOT FOUND (REFRESH ONLY)             
BUYBSALL EQU   BUYBSLIV+BUYBSTST+BUYBSDEL                                       
                                                                                
BUYTAPPR DS    C                   TEARSHEET APPROVED                           
BUYPAGEN DS    CL(L'PTSHPAGE)      PAGE NOTATION                                
BUYTSTAT DS    CL5                 TEARSHEET STATUS                             
BUYFSINS DS    CL10                NUMBER OF FREE STANDING INSERTS              
BUYREPRO DS    XL(L'PTSHREPO)      REPRODUCTION QUALITY                         
                                                                                
BUYORDDT DS    XL(L'PIODATE)       INSERTION ORDER DATE                         
BUYORDNO DS    CL16                INSERTION ORDER NUMBER (WITH TYPE)           
                                                                                
BUYVIEWS DS    PL(L'PPAGEVS)       NUMBER OF PAGE VIEWS                         
BUYCLICK DS    PL(L'PCLCKTS)       CLICK THRUS                                  
BUYDECIR DS    PL(L'PBDEC)         DAILY EFFECTIVE CIRCULATION                  
BUYREPTS DS    PL(L'PBRPTNO)       NUMBER OF FREE STANDING INSERTS              
BUYEIMPS DS    PL(L'PIMPRS)        NUMBER OF ESTIMATED IMPRESSIONS              
BUYAIMPS DS    PL(L'PAIMPRS)       NUMBER OF ACTUAL IMPRESSIONS                 
BUYECPM  DS    PL(L'PECPM)         ESTIMATED CPM                                
BUYACPM  DS    PL(L'PACPM)         ACTUAL CPM                                   
                                                                                
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
*                                                                               
BUYNETOR DS    XL4                 NET ORDERED                                  
BUYNETPD DS    XL4                 NET PAID                                     
BUYPLNCD DS    XL4                 PAID NET OF CASH DISCOUNT                    
BUYNETBD DS    XL4                 NET BILLED                                   
BUYBLNCD DS    XL4                 BILLED NET OF CASH DISCOUNT                  
*                                                                               
BUYNPYBL DS    XL4                 NET PAYABLE (CALC'D FROM PAY ELEMS)          
BUYGPYBL DS    XL4                 GRS PAYABLE (CALC'D FROM PAY ELEMS)          
*                                                                               
COMLENQ  EQU   80                  MAXIMUM W'COMMENTS                           
COMMAXQ  EQU   5                   MAXIMUM N'COMMENTS                           
                                                                                
BUYREGC# DS    AL1                 N'REGULAR COMMENT LINES                      
BUYREGC1 DS    CL(COMLENQ)         REGULAR COMMENT LINE 1                       
BUYREGC2 DS    CL(COMLENQ)         .....................2                       
BUYREGC3 DS    CL(COMLENQ)         .....................3                       
BUYREGC4 DS    CL(COMLENQ)         .....................4                       
BUYREGC5 DS    CL(COMLENQ)         .....................5                       
                                                                                
BUYINSC# DS    AL1                 N'INSERTION ORDER COMMENT LINES              
BUYINSC1 DS    CL(COMLENQ)         INSERTION ORDER COMMENT LINE 1               
BUYINSC2 DS    CL(COMLENQ)         .............................2               
BUYINSC3 DS    CL(COMLENQ)         .............................3               
BUYINSC4 DS    CL(COMLENQ)         .............................4               
BUYINSC5 DS    CL(COMLENQ)         .............................5               
                                                                                
BUYPOSI# DS    AL1                 N'POSITION INSTRUCTION LINES                 
BUYPOSI1 DS    CL(COMLENQ)         POSITION INSTRUCTION LINE 1                  
BUYPOSI2 DS    CL(COMLENQ)         ..........................2                  
BUYPOSI3 DS    CL(COMLENQ)         ..........................3                  
BUYPOSI4 DS    CL(COMLENQ)         ..........................4                  
BUYPOSI5 DS    CL(COMLENQ)         ..........................5                  
                                                                                
BUYTSHC# DS    AL1                 N'TEARSHEET COMMENT LINES                    
BUYTSHC1 DS    CL(COMLENQ)         TEARSHEET COMMENT LINE 1                     
BUYTSHC2 DS    CL(COMLENQ)         .......................2                     
BUYTSHC3 DS    CL(COMLENQ)         .......................3                     
BUYTSHC4 DS    CL(COMLENQ)         .......................4                     
BUYTSHC5 DS    CL(COMLENQ)         .......................5                     
                                                                                
BUYISSNM DS    CL(L'PISNAME)       ISSUE NAME                                   
BUYUPLID DS    CL(L'PIUPUSEQ)      UPLOAD ID FROM PBU UPLOADS                   
                                                                                
BUYCLRST DS    CL1                 CLEARANCE STATUS                             
BUYTSRST DS    CL1                 TEARSHEET RECEIVED                           
BUYMATST DS    CL(L'PBMTSTAT)      INVOICE MATCHING STATUS                      
BUYDISST DS    CL(L'PBMTDSTA)      INVOICE MATCHING DISCREPANCY STATUS          
                                                                                
BUYATAB# DS    AL2                 NUMBER OF ADDITIONAL CHARGES                 
                                                                                
BUYATAB  DS    0X                  ** ADDITIONAL CHARGE TABLE **                
BUYATABM EQU   10                  MAXIMUM N'ADDITIONAL CHARGES                 
BUYATCOD DS    CL(L'PACCODE)       ADDITIONAL CHARGE CODE                       
BUYATCRG DS    CL12                ADDITIONAL CHARGE                            
BUYATCOM DS    C                   SUBJECT TO COMMISSION?                       
BUYATCPC DS    XL(L'PACACOM)       COMMISSION%                                  
BUYATSCD DS    C                   SUBJECT TO CASH DISCOUNT?                    
BUYATABL EQU   *-BUYATAB                                                        
         DS    (BUYATABM-1)XL(BUYATABL)                                         
*                                                                               
BUYFOUND DS    C                   BUY FOUND FLAG                               
*                                                                               
I#       DS    0X                  ** INSERTION HISTORY COUNTERS **             
IOA#     DS    AL2                 NUMBER OF INSERTION ORDERS                   
ICA#     DS    AL2                 NUMBER OF INSERTION CHANGES                  
IBA#     DS    AL2                 NUMBER OF INSERTION BILLINGS                 
IPA#     DS    AL2                 NUMBER OF INSERTION PAYINGS                  
I#L      EQU   *-I#                                                             
*                                                                               
         PRINT OFF                                                              
         DS    0D                                                               
       ++INCLUDE PVALUES           PARAMETER BLOCK FOR GETINS                   
         PRINT ON                                                               
*                                                                               
BUYVALSL EQU   *-BUYVALS                                                        
*                                                                               
PBUREC   DS    XL(PBUL)            PUBLICATION RECORD                           
RBUREC   DS    XL(RBUL)            PUBLISHER/REP RECORD                         
IBUREC   DS    XL(IBUL)            INVOICE RECORD                               
*                                                                               
         DS    0D                                                               
WORKAREA DS    XL650                                                            
*                                                                               
SAVEL    EQU   *-SAVED                                                          
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDPSTBLK                                                       
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
PPBVALDD DSECT                                                                  
       ++INCLUDE PPBVALD                                                        
         PRINT ON                                                               
                                                                                
* PPLNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
                                                                                
* PUBREC                                                                        
         PRINT OFF                                                              
PUBRECD  DSECT                                                                  
PUBKCODQ EQU   X'81'               PUBLICATION RECORD CODE                      
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
         PRINT ON                                                               
                                                                                
* PREPREC                                                                       
         PRINT OFF                                                              
PREPRECD DSECT                                                                  
PREPKRCQ EQU   X'11'               PUBLISHER/REP RECORD CODE                    
       ++INCLUDE PREPREC                                                        
         PRINT ON                                                               
                                                                                
* PPCLRST                                                                       
         PRINT OFF                                                              
PPCLTYPQ EQU   X'25'               CLEARANCE STATUS RECORD CODE                 
PPCLELQ  EQU   X'01'               CLEARANCE STATUS ELEMENT CODE                
       ++INCLUDE PPCLRST                                                        
         PRINT ON                                                               
                                                                                
* PGENGRP                                                                       
         PRINT OFF                                                              
       ++INCLUDE PGENGRP                                                        
         PRINT ON                                                               
                                                                                
* PLISREC                                                                       
         PRINT OFF                                                              
PLISRECD DSECT                                                                  
PLISKRCQ EQU   X'17'               PUBLICATION LIST RECORD CODE                 
       ++INCLUDE PLISREC                                                        
PLISFRST DS    0X                                                               
PLISPBD  DSECT                                                                  
PLISBELQ EQU   X'20'               PUBLICATION LIST ELEMENT                     
       ++INCLUDE PLISPBEL                                                       
         PRINT ON                                                               
                                                                                
* PSPCGREC                                                                      
         PRINT OFF                                                              
PSPLRECD DSECT                                                                  
       ++INCLUDE PSPCGREC                                                       
PSPELQ   EQU   X'10'               ADDITIONAL CHARGE DEFINITION ELEMENT         
         PRINT ON                                                               
* PNVREC                           PRINT NEW INVOICE RECORD                     
         PRINT OFF                                                              
       ++INCLUDE PPGENPNV                                                       
                                                                                
* PBUYREC                                                                       
         PRINT OFF                                                              
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
       ++INCLUDE PBILELEM                                                       
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
PCOMEL   DS    X                   ** COMMENT ELEMENTS **                       
PCOMLEN  DS    X                   COMMENT ELEMENT LENGTH                       
PCOMLN1Q EQU   *-PCOMEL                                                         
PCOMMENT DS    0C                  COMMENT (VARIABLE LENGTH)                    
PIOELQ   EQU   X'70'               INSERTION ORDER ELEMENT                      
       ++INCLUDE PIOELEM                                                        
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
*                                                                               
       ++INCLUDE PPPISNMEL         ISSUE NAME (EQUATE DEFINED IN BOOK)          
*                                                                               
       ++INCLUDE PPGENPBMAT        INVOICE MATCHING STATUSES                    
*                                                                               
       ++INCLUDE PPGENBYCC         INSERTION CUSTOM COLUMN ELEM                 
*                                                                               
         PRINT ON                                                               
* PJOBREC                                                                       
         PRINT OFF                                                              
PJOBRECD DSECT                                                                  
PJOBKRCQ EQU   X'15'               JOB RECORD CODE                              
       ++INCLUDE PJOBREC                                                        
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PCOLREC           CUSTOM COLUMN RECORD                         
         PRINT ON                                                               
*                                                                               
PSERRECD DSECT                     ** DSECT TO COVER SERIAL PASSIVE **          
PSERKEY  DS    0XL25                                                            
PSERKAGY DS    CL2                 AGENCY CODE                                  
PSERKMED DS    C                   MEDIA CODE                                   
PSERKTYP DS    X                   RECORD TYPE                                  
PSERKTYQ EQU   X'99'                                                            
PSERKCLT DS    CL3                 CLIENT CODE                                  
PSERKSER DS    CL5                 SERIAL NUMBER                                
*                                                                               
         ORG   PSERKEY+L'PSERKEY                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBYOUTD                                                       
PPBYOUTL EQU   *-PPBYOUTD                                                       
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
EBLOCKD  DSECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
EBLOCKDL EQU   *-EBLOCKD                                                        
         PRINT ON                                                               
*                                                                               
ACHTABD  DSECT                     ** ADDITIONAL CHARGES TABLE **               
ACHMED   DS    CL(L'PSPLKMED)      MEDIA CODE                                   
ACHCODE  DS    CL(L'PSPLCODE)      ADDITIONAL CHARGE RATE CODE                  
ACHTYPE  DS    CL(L'PSPLTYPE)      ADDITIONAL CHARGE TYPE INDICATOR             
ACHDESC  DS    CL(L'PSPLDESC)      ADDITIONAL CHARGE DESCRIPTION                
ACHTABL  EQU   *-ACHTABD                                                        
*                                                                               
PBUD     DSECT                     ** PUBLICATION BUFFER RECORDS **             
PBUKEY   DS    0X                  ** RECORD KEY **                             
PBUMED   DS    CL(L'PUBKMED)       MEDIA CODE                                   
PBUPZE   DS    XL(LPUBKPZE)        PUBLICATION/ZONE/EDITION                     
PBUKEYL  EQU   *-PBUKEY                                                         
PBUDATA  DS    0X                  ** RECORD DATA **                            
PBUCODE  DS    CL15                PUBLICATION CODE                             
PBUREP   DS    CL(L'PUBPLSH)       PUBLISHER/REP                                
PBUNAME  DS    CL(L'PUBNAME)       PUBLICATION NAME                             
PBUZNAME DS    CL(L'PUBZNAME)      PUBLICATION ZONE NAME                        
PBUCITY  DS    CL(L'PUBCITY)       PUBLICATION CITY                             
PBUSTATE DS    CL(L'PUBSTATE)      PUBLICATION STATE                            
PBUDATAL EQU   *-PBUDATA                                                        
PBUL     EQU   *-PBUD                                                           
*                                                                               
RBUD     DSECT                     ** PUBLISHER/REP BUFFER RECORDS **           
RBUKEY   DS    0X                  ** RECORD KEY **                             
RBUMED   DS    CL(L'PREPKMED)      MEDIA CODE                                   
RBUREP   DS    XL(L'PREPKREP)      PUBLISHER/REP CODE                           
RBUKEYL  EQU   *-RBUKEY                                                         
RBUDATA  DS    0X                  ** RECORD DATA **                            
RBUNAME  DS    CL(L'PREPNAME)      PUBLISHER/REP NAME                           
RBUDATAL EQU   *-RBUDATA                                                        
RBUL     EQU   *-RBUD                                                           
*                                                                               
IBUD     DSECT                     ** INVOICE BUFFER RECORD **                  
IBUKEY   DS    0X                  ** RECORD KEY **                             
IBUMEDCD DS    CL(L'PNVKMED)       MEDIA                                        
IBUINVS  DS    CL(L'PNVKSER#)      INVOICE SERIAL NUMBER                        
IBUINSKY DS    CL(BUYSERSL)        INSERTION KEY (MED/CLT/SERIAL#)              
IBUKEYL  EQU   *-IBUKEY                                                         
IBUDATA  DS    0X                  ** RECORD DATA **                            
IBUDATAL EQU   *-IBUDATA                                                        
IBUL     EQU   *-IBUD                                                           
*                                                                               
IOAD     DSECT                     ** INSERTION ORDER ACTIVITY **               
IOADATE  DS    XL(L'PBDIODAT)      INSERTION ORDER DATE                         
IOANUM   DS    CL(L'BUYORDNO)      INSERTION ORDER NUMBER                       
IOATYPE  DS    CL(L'ACTVTYPE)      INSERTION ORDER TYPE                         
IOAL     EQU   *-IOAD                                                           
                                                                                
ICAD     DSECT                     ** INSERTION CHANGE ACTIVITY **              
ICATYPE  DS    CL(L'ACTVTYPE)      INSERTION ACTIVITY TYPE                      
ICADATE  DS    XL(L'PCHGDAT)       INSERTION ACTIVITY DATE                      
ICAWHOM  DS    CL(L'PBDBUYER)      INSERTION ACTIVITY DONE BY WHOM              
ICAWHAT  DS    CL250               INSERTION ACTIVITY TAKEN                     
ICAL     EQU   *-ICAD                                                           
                                                                                
IBAD     DSECT                     ** INSERTION BILLING ACTIVITY **             
IBADATE  DS    XL(L'PBLDATE)       INSERTION BILLING DATE                       
IBAGROSS DS    XL(L'PBGROSS)       INSERTION BILLING GROSS AMOUNT               
IBANET   DS    XL4                 INSERTION BILLING NET AMOUNT                 
IBANLCDB DS    XL4                 INSERTION BILLING NET LESS CD BILLED         
IBAINVNO DS    CL(L'BUYBINVN)      INSERTION BILLING INVOICE NUMBER             
IBAL     EQU   *-IBAD                                                           
                                                                                
IPAD     DSECT                     ** INSERTION PAYMENT ACTIVITY **             
IPADATE  DS    XL(L'PPDDATE)       INSERTION PAYMENT DATE                       
IPAGROSS DS    XL(L'PPGROSS)       INSERTION PAYMENT GROSS AMOUNT               
IPANET   DS    XL4                 INSERTION PAYMENT NET AMOUNT                 
IPANLCDP DS    XL4                 INSERTION PAYMENT NET LESS CD PAID           
IPAPAYEE DS    CL(L'ACTVTYPE)      INSERTION PAYMENT REP CODE                   
IPACHECK DS    CL(L'PPCLCHK)       INSERTION PAYMENT CHECK NUMBER               
IPAL     EQU   *-IPAD                                                           
*                                                                               
INVVALSD DSECT                     ** EXTRACTED INVOICE REC VALUES **           
*                                                                               
* INVOICE HEADER - EXTRACTED DATA                                               
*                                                                               
INVHDRVS DS    0X                  HEADER VALUES START HERE                     
INVKEY   DS    0C                  INVOICE KEY                                  
INVMED   DS    CL(L'PNVKMED)       MEDIA                                        
INVSER#  DS    CL(2*L'PNVKSER#)    SERIAL NUMBER (10 DIGITS)                    
INVKEYSL EQU   *-INVKEY            L'SHORT INVOICE KEY                          
INVHGSTA DS    C                   HEADER GENERAL STATUS                        
INVCLTC  DS    CL(L'PNV1CLT)       INVOICE CLT CODE                             
INVHPUBC DS    CL(L'BUYPUB)        INVOICE PUBLICATION CODE (CHARS)             
INVVNDR# DS    CL(L'PNVHINV#)      INVOICE NUMBER                               
INVSTAT  DS    XL(L'PNVHSTAT)      INVOICE STATUS                               
INVPSTR  DS    XL(L'PNVHSTRT)      INVOICE PERIOD START DATE                    
INVPEND  DS    XL(L'PNVHEND)       INVOICE PERIOD END DATE                      
INVDATE  DS    XL(L'PNVHDATE)      INVOICE DATE                                 
INVTOTL  DS    XL(L'PNVHTOT)       INVOICE TOTAL                                
INVTTYP  DS    XL(L'PNVH$TYP)      INVOICE TOTAL TYPE                           
INVSREP  DS    XL(L'PNVHSREP)      SPECIAL REP                                  
INVHVLQ  EQU   *-INVHDRVS          L'HEADER VALUES                              
*                                                                               
* INVOICE HEADER/DETAIL COMMENT - EXTRACTED DATA                                
*                                                                               
INVCOMVS DS    0X                  COMMENT VALUES START HERE                    
INVCGSTA DS    C                   COMMENT GENERAL STATUS                       
INVPIDNM DS    CL(3*L'SANAME)      PID NAME (FIRST, MIDDLE, LAST)               
INVCOMDT DS    XL(L'PNVCDATE)      COMMENT DATE                                 
         DS    0X                  COMMENT (SEE IVCOMNTD DSECT)                 
INVCVLQ  EQU   *-INVCOMVS          L'COMMENT VALUES                             
*                                                                               
* INVOICE DETAIL - EXTRACTED DATA                                               
*                                                                               
INVDETVS DS    0X                  DETAIL (ITEM) VALUES START HERE              
INVISQN  DS    XL(L'PNVDKSQN)      DETAIL (ITEM) SEQUENCE NUMBER                
INVDGSTA DS    C                   DETAIL GENERAL STATUS                        
INVINSKY DS    XL(BUYSERSL)        SHORT INSERTION KEY                          
INVRATE  DS    CL20                CHARACTER RATE                               
INVPREM  DS    CL20                CHARACTER PREMIUM                            
INVNET   DS    PL(L'PNVDNET)       NET                                          
INVGROSS DS    PL(L'PNVDGRS)       NET                                          
INV#LINE DS    XL(L'PNVD#LIN)      NUMBER OF LINE ITEMS                         
INVINSDT DS    XL(L'PNVDDTE)       LINE ITEM DATE (INSERTION DATE)              
INVSPDSP DS    XL(L'PNVDSPC)       SPACE DESCRIPTION                            
INVADCAP DS    XL(L'PNVDACAP+L'PNVDACP2)                                        
INVCLTCD DS    XL(L'PNVDCLT)       CLT CODE                                     
INVPRDCD DS    XL(L'PNVDPRD)       PRD CODE                                     
INVDPUBC DS    XL(L'BUYPUB)        PUB CODE (CHAR)                              
INVDVLQ  EQU   *-INVDETVS          L'DETAIL (ITEM) VALUES                       
*                                                                               
INVVALSX EQU   *-INVVALSD          END OF EXTRACTED INVOICE VALUES              
*                                                                               
* GENERAL STATUS EQUATES FOR INVOICE DATA                                       
*                                                                               
GST_DELQ EQU   C'D'                DELETED                                      
*                                                                               
* INVOICE WORKING STORAGES                                                      
*                                                                               
INVWKKEY DS    XL(L'PNVKEY)        INVOICE WORKING KEY                          
INVWKSEQ DS    XL(L'PNVDKSQN)      INVOICE DETAIL  WORKING SEQ NUMBER           
INVELMKY DS    XL(L'PNVHKEY)       INVOICE ELEMENT NAVIGATION KEY               
*                                                                               
MINIOWKS DS    0X                  MINIO WORKING STORAGE                        
MNBLKCB  DS    XL(MINBLKL)         MINIO CONTROL BLOCK                          
MNRTAB   DS    CL256               MINIO RECORD TABLE                           
MNELEM   DS    CL256               MINIO ELEMENT AREA                           
QINVKEY  DS    CL32                CURRENT MINIO MASTER KEY                     
         DS    0D                  ALIGNMENT                                    
PNVPARMS DS    XL24                PARAMETER LIST                               
*                                                                               
IVCOMNTD DSECT                     ** INVOICE COMMENT **                        
IVCOMMMX EQU   18                  MAXIMUM N' COMMENTS (4K/220)                 
IVCOMMNT DS    (IVCOMMMX)CL220     COMMENTS                                     
IVCOMMLN EQU   *-IVCOMMNT                                                       
*                                                                               
INSVARY  DSECT                     ARRAYS FOR INSERTION VALUES                  
*                                                                               
BINVTAB# DS    AL2                 NUMBER OF INV ELEM FOUND IN BUY REC          
*                                                                               
BINVTAB  DS    0X                  BUY INVOICE DATA START HERE                  
BINVTABM EQU   30                  MAX N' INVOICE DATA IN BUY RECORD            
BINVKEY  DS    CL(INVKEYSL)        INVOICE KEY                                  
BINVSEQ# DS    CL(L'PBNVDSQN)      SEQUENCE NUMBER                              
BINVVIV# DS    CL(L'PBNVINV#)      VENDOR INVOICE NUMBER                        
BINVTABL EQU   *-BINVTAB                                                        
         DS    (BINVTABM-1)XL(BINVTABL)                                         
*                                                                               
BCCVARY  DSECT                     BUY CUSTOM COLUMN VALUES                     
*                                                                               
BUYCCSQ# DS    XL(L'BYCCSQN)       BUY CUSTOM COLUMN SEQ NUMBER                 
BUYCCFDA DS    XL(255-BYCCHDRL)    BUY CUSTOM COLUMN FIELD DATA                 
*                                                                               
*                                                                               
PROFVALD DSECT                     ** EXTRACTED PROFILE VALUES **               
*                                                                               
PROFVLSS DS    0X                  PROFILE VALUES START HERE                    
PROFILEV DS    CL16                                                             
PROFVLQ  EQU   *-PROFVLSS          L'PROFILE VALUES                             
*                                                                               
PROFNAME DS    CL10                NAME OF PROFILE                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074PPLNK11S  04/30/04'                                      
         END                                                                    
