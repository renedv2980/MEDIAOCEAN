*          DATA SET ACBRA24    AT LEVEL 016 AS OF 04/24/15                      
*PHASE T62424A                                                                  
*INCLUDE XSORT                                                                  
                                                                                
ACBRA24  TITLE '- BrandOcean Resources Download Server'                         
                                                                                
***********************************************************************         
* Level change comments                                                         
* ---------------------                                                         
* NSHE 001 12FEB07 New application version                                      
* SMAN 002 22SEP08 <LO01-7385> Status Report key roles                          
* NSHE 003 23MAR09 Change to audit record                                       
* SMAN 004 18JUN09 Improve GAPLST calls                                         
* SMAN 006 16JUL09 Take Resources offline for GAPLST calls                      
* NSHE 008 28OCT09 Amend limit list look up to use TSAR buffer                  
* NSHE 009 23NOV09 If using teams don't filter by view all                      
* SMAN 010 08JAN10 Bring back all Teams/Roles if on global logon                
* NSHE     21JAN10 <BR30036L> Fix to Status Report                              
* NSHE 011 05MAR10 Allow status report to be filtered by teams roles            
* NSHE 012 19MAY10 Searchng on a team with no people should return zero         
* NSHE 013 04AUG10 TSARREC added to SAVED, GAPLST call amended                  
* NSHE 014 11NOV11 Change to GAPLST calls                                       
* NSHE 015 19JUL13 Change where SETFAC is called                                
* NSHE 016 24Apr15 Amend SETFAC call                                            
*                                                                               
***********************************************************************         
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           +        
               SLOWLIST=SLOWS,FACS=FACS,WORKERKEY=ACBO,ABENDLIST=FAILS,+        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,APPEND=Y,               +        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             +        
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,                                          +        
               B#LP_D,LP_D,                                            +        
               B#TWAD,TWAD,                                            +        
               B#ACCREC,ACCRECD,                                       +        
               B#TEAM,TEARECD,                                         +        
               B#ROLE,ROLRECD,                                         +        
               B#AUDIT,AUDRECD,                                        +        
               B#RESWK,RWKRECD,                                        +        
               B#KYST,KSTRECD,                                         +        
               B#PID,PIDRECD,                                          +        
               B#HOL,PKSRECD,                                          +        
               B#CAMP,RWKRECD,                                         +        
               B#TEMP,TPLRECD,                                         +        
               B#SVRDEF,SVRDEF,                                        +        
               B#PTASK,PKSRECD)                                                 
                                                                                
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO24**,CLEAR=YES,RR=RE                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP         R7=A(RUNPARMS)                               
         USING RUNPARMD,R7                                                      
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         ICM   R8,15,RSVRSAVE      R8=A(7K SAVE AREA)                           
         B     INIT03                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)              BLOCK #1         
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
INIT03   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)              BLOCK #2         
         MVC   ATWA,LP_ATWA        OFFLINE TWA AREA SET BY RUNNER               
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ABUFFRIN,RBUFFRIN                                                
         DROP  R6,R7                                                            
         L     R7,ATWA             R7=A(ON/OFFLINE TWA)                         
         USING TWAD,R7                                                          
         MVI   TWAMODE,0                                                        
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
         L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         CLI   RUNPMODE,RRUNSTRQ   FIRST FOR RUN                                
         BE    RUNSTR                                                           
         CLI   RUNPMODE,RPRCWRKQ   PROCESS WORK                                 
         BE    PRCWRK                                                           
         CLI   RUNPMODE,RRUNREQQ   RUN REQUEST                                  
         BE    RUNREQ                                                           
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   L     R0,ATIA                                                          
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR02            NO                                           
         L     RF,ACOMFACS         YES - LOAD FACILITIES OVERLAYS               
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE SERVER WORKING STORAGE            
         B     RUNSTR04                                                         
                                                                                
RUNSTR02 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         LHI   R1,BRO#GENR                                                      
         CLC   LP_QMAPN,=AL2(A#RSTDL)  Resource management download             
         JNE   *+8                                                              
         LHI   R1,BRO#RESR                                                      
         GOTOR (#SETFAC,ASETFAC)                                                
                                                                                
RUNSTR04 OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
         MVC   LP_BLKS+((B#ROLE-1)*L'LP_BLKS)(L'LP_BLKS),AIO2                   
         MVC   LP_BLKS+((B#PID-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                    
         MVC   LP_BLKS+((B#KYST-1)*L'LP_BLKS)(L'LP_BLKS),AIO5                   
         MVC   LP_BLKS+((B#TEMP-1)*L'LP_BLKS)(L'LP_BLKS),AIO6                   
         MVC   LP_BLKS+((B#RESWK-1)*L'LP_BLKS)(L'LP_BLKS),AIO4                  
         MVC   LP_BLKS+((B#PTASK-1)*L'LP_BLKS)(L'LP_BLKS),AIO7                  
         MVC   LP_BLKS+((B#ACCREC-1)*L'LP_BLKS)(L'LP_BLKS),AGENAREA             
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
         MVC   LP_BLKS+((B#LP_D-1)*L'LP_BLKS)(L'LP_BLKS),ALP                    
                                                                                
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
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   XC    QVALUES(QVALUESL),QVALUES                                        
         XC    DVALUES(DVALUESL),DVALUES                                        
         LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         XR    RE,RE                                                            
         XR    RE,RE                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    RUNI,RUNI                                                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST RUNNING OFFLINE                         
         BZ    PRCWRK02                                                         
                                                                                
         LA    R0,WORKD            CLEAR I/O AREAS                              
         AHI   R0,IOAREA1-WORKD                                                 
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
PRCWRK02 J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2RSRC       set to Resources                             
                                                                                
         GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
         MVC   AGENCY,CUXCPY                                                    
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
                                                                                
RUNREQ02 L     RE,LP_AWMP          BUILD DEFAULT RANGE ELEMENTS IN WMP          
         USING LW_D,RE                                                          
         MVI   LW_TYPE,LW_TALLQ    ALL VALUES                                   
         STCM  RE,7,AALL                                                        
         AHI   RE,LW_LN1Q                                                       
         MVI   LW_TYPE,LW_TNZRQ    NON-ZERO VALUES                              
         STCM  RE,7,ANZR                                                        
         AHI   RE,LW_LN1Q                                                       
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
         MVC   USROFF,SPACES                                                    
         CLI   CUACCS,0                                                         
         JNE   RUNREQ04                                                         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   RUNREQ06                                                         
         MVC   DAOFF,AALL          NO - SET DEFAULT                             
         J     RUNREQ18                                                         
RUNREQ04 TM    SCPYEL+(CPYSTATC-CPYELD),CPYSROFF OFFICES FOR RESOURCES          
         JZ    RUNREQ06                                                         
         MVC   USROFF,CUACCS+2                                                  
         USING OFFALD,R1                                                        
RUNREQ06 L     R1,AOFFAREA                                                      
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   RUNREQ08                                                         
         LA    R0,L'CTPVALUE*2                                                  
         LA    R3,OFFAWORK                                                      
         TM    SCPYEL+(CPYSTATC-CPYELD),CPYSROFF OFFICES FOR RESOURCES          
         JZ    RUNREQ10                                                         
         MVC   USROFF,OFFAWORK                                                  
         MVI   USROFF+1,C' '                                                    
         J     RUNREQ10                                                         
*                                                                               
RUNREQ08 SR    R0,R0                                                            
         ICM   R0,3,OFFAWORK                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R3,OFFAWORK+2       TEST OFFICE IS IN USER ACCESS LIST           
RUNREQ10 CLI   0(R3),X'FF'                                                      
         JE    RUNREQ18                                                         
         CLI   0(R3),C'*'                                                       
         JE    RUNREQ18                                                         
         MVC   WORK(L'KSTKOFF),0(R3)                                            
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   RUNREQ12                                                         
         MVI   WORK+1,C' '                                                      
RUNREQ12 GOTOR LP_AAWMP,DMCB,(L'KSTKOFF,WORK),DOFFIND,DOFFMAXQ,        +        
               LP_D                                                             
RUNREQ14 TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   RUNREQ16                                                         
         LA    R3,1(R3)                                                         
         JCT   R0,RUNREQ10                                                      
         J     RUNREQ18                                                         
*                                                                               
RUNREQ16 LA    R3,L'KSTKOFF(R3)                                                 
         JCT   R0,RUNREQ10                                                      
*                                                                               
RUNREQ18 XC    MAPI,MAPI           INITIALIZE MAP INDICATORS                    
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JNE   *+14                                                             
         MVC   MAPI,L'LP_QMAPN(RF)                                              
         J     *+10                                                             
         AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQX                                                          
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
                                                                                
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
**********************************************************************          
* KEYSTAGE/TEMPLATE DOWNLOAD                                         *          
**********************************************************************          
                                                                                
REQKST   LKREQ H,A#KSTMDL,OUTKST,NEXTREQ=REQTEAM                                
client   LKREQ F,1,(D,B#SAVED,QCLIENT),CHAR,OLEN=L'QCLIENT,            +        
               MAXLEN=L'QCLIENT,TEXT=AC#CLI,COL=*                               
prod     LKREQ F,2,(D,B#SAVED,QPROD),CHAR,OLEN=L'QPROD,                +        
               MAXLEN=L'QPROD,TEXT=AC#PROC,COL=*                                
job      LKREQ F,3,(D,B#SAVED,QJOBC),CHAR,OLEN=L'QJOBC,                +        
               MAXLEN=L'QJOBC,TEXT=AC#JOBC,COL=*                                
onlyks   LKREQ F,4,(D,B#SAVED,QKSTO),CHAR,OLEN=L'QKSTO,                +        
               MAXLEN=L'QKSTO,TEXT=(*,KSTOLIT),COL=*                            
         LKREQ E                                                                
                                                                                
OUTKST   LKOUT H                                                                
                                                                                
KEYSTG   LKOUT R,R#KSTGDL          KEYSTAGE                                     
Array    LKOUT C,1,(A,ARYKST)                                                   
         LKOUT E                                                                
                                                                                
TEMPLT   LKOUT R,R#TEMPDL          TEMPLATE                                     
Array    LKOUT C,2,(A,ARYTMP)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYKST   LKOUT A,(R,NXTKST),MULTIROW=Y,ROWNAME=KSTRECD                          
                                                                                
Kstnum   LKOUT C,21,KSTKNUM,LBIN                                                
Array    LKOUT C,22,(A,ARYKST1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYKST1  LKOUT A,(D,B#KYST,KSTRFST),EOT=EOR,                           +        
               ROWID=(KSTEL,KSTELQ),ROWWIDTH=(V,KSTLN)                          
                                                                                
Kstname  LKOUT C,22,KSTNAME,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET KEYSTAGE RECORDS                                                *         
***********************************************************************         
                                                                                
NXTKST   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTKST02                                                         
                                                                                
         OC    DAKST,DAKST         TEST KEYSTAGE LIST PROVIDED                  
         JNZ   NXTKST02                                                         
         MVC   DAKST,AALL          NO - SET DEFAULT                             
                                                                                
NXTKST02 GOTOR (#NXTREC,ANXTREC),DMCB,KSTKEYT,('B#KYST',SVTPLKEY),     +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
SVRDEF   CSECT ,                                                                
                                                                                
ARYTMP   LKOUT A,(R,NXTTMP),MULTIROW=Y,ROWNAME=TPLRECD                          
                                                                                
Tplnum   LKOUT C,1,TPLKNUM,LBIN                                                 
Array    LKOUT C,2,(A,ARYTMP1)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYTMP1  LKOUT A,(D,B#TEMP,TPLRFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Tplname  LKOUT C,2,NAMEREC,CHAR,LEN=V                                           
Matched  LKOUT C,3,(D,B#SAVED,MATCH),CHAR,ND=Y                                  
Array    LKOUT C,10,(A,ARYKST)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET TEMPLATE RECORDS                                                *         
***********************************************************************         
                                                                                
NXTTMP   MVI   MATCH,C'N'                                                       
         XC    DAKST,DAKST         CLEAR KEYSTAGE LIST                          
         XC    DKSTIND,DKSTIND                                                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTTMP02                                                         
                                                                                
         CLI   QKSTO,C'Y'                                                       
         JE    NOMORE                                                           
         OC    QATPL,QATPL         TEST TEMPLATE LIST PROVIDED                  
         JNZ   NXTTMP02                                                         
         MVC   QATPL,AALL          NO - SET DEFAULT                             
                                                                                
NXTTMP02 GOTOR (#NXTREC,ANXTREC),DMCB,TPLKEYT,('B#TEMP',0),            +        
               (0,SAVED),0,0                                                    
         JNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         USING TPLRECD,R2          R2=A(TEMPLATE RECORD)                        
         LA    R2,TPLRFST                                                       
         USING LIDELD,R2                                                        
NXTTMP04 CLI   LIDEL,0                                                          
         JE    NXTTMP30                                                         
         CLI   LIDEL,LIDELQ                                                     
         JE    NXTTMP08                                                         
NXTTMP06 XR    R0,R0                                                            
         IC    R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     NXTTMP04                                                         
                                                                                
NXTTMP08 CLI   LIDTYPE,LIDTMED                                                  
         JNE   NXTTMP20                                                         
         LA    R3,LIDDATA                                                       
NXTTMP10 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   NXTTMP06                                                         
         CLC   0(L'PMDKMED,R3),QJOBC                                            
         JE    NXTTMP12                                                         
         LA    R3,L'PMDKMED(R3)                                                 
         J     NXTTMP10                                                         
                                                                                
NXTTMP12 MVI   MATCH,C'Y'                                                       
         J     NXTTMP06                                                         
                                                                                
NXTTMP20 CLI   LIDTYPE,LIDTKYST                                                 
         JNE   NXTTMP06                                                         
         LA    R3,LIDDATA                                                       
NXTTMP22 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   NXTTMP06                                                         
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'KSTKNUM,0(R3)),DKSTIND,DKSTMAXQ,       +        
               LP_D                                                             
         LA    R3,L'KSTKNUM(R3)                                                 
         J     NXTTMP22                                                         
                                                                                
NXTTMP30 J     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* TEAM/ROLE DOWNLOAD                                                 *          
**********************************************************************          
                                                                                
REQTEAM  LKREQ H,A#TMRLDL,OUTTEAM,NEXTREQ=REQRSTR                               
client   LKREQ F,1,(D,B#SAVED,QCLIENT),CHAR,OLEN=L'QCLIENT,            +        
               MAXLEN=L'QCLIENT,TEXT=AC#CLI,COL=*                               
prod     LKREQ F,2,(D,B#SAVED,QPROD),CHAR,OLEN=L'QPROD,                +        
               MAXLEN=L'QPROD,TEXT=AC#PROC,COL=*                                
job      LKREQ F,3,(D,B#SAVED,QJOBC),CHAR,OLEN=L'QJOBC,                +        
               MAXLEN=L'QJOBC,TEXT=AC#JOBC,COL=*                                
         LKREQ E                                                                
                                                                                
OUTTEAM  LKOUT H                                                                
                                                                                
TEAM     LKOUT R,R#TEAMDL          TEAM                                         
Array    LKOUT C,1,(A,ARYTEAM)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ROLE     LKOUT R,R#ROLEDL          ROLE                                         
Array    LKOUT C,2,(A,ARYROL)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYTEAM  LKOUT A,(R,NXTTEAM),MULTIROW=Y,ROWNAME=TEARECD                         
                                                                                
Teamnum  LKOUT C,1,TEAKNUM,LBIN                                                 
Array    LKOUT C,2,(A,ARYTEAM1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYTEAM1 LKOUT A,(D,B#TEAM,TEARFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Teamname LKOUT C,2,NAMEREC,CHAR,LEN=V                                           
Matched  LKOUT C,3,(D,B#SAVED,MATCH),CHAR,ND=Y                                  
Array    LKOUT C,4,(A,ARYPID)                                                   
                                                                                
         LKOUT E                                                                
***********************************************************************         
* GET TEAM RECORDS                                                    *         
***********************************************************************         
                                                                                
NXTTEAM  MVI   MATCH,C'N'                                                       
         TM    OVINDS1,OVINAME                                                  
         JNZ   NXTTEA02                                                         
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAPIDB                                                      
         JZ    NXTTEA02                                                         
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         CLEAR ENTRIES                                
         LHI   R1,DPIDMAXQ*L'LIDRPID                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               CLEAR AREA                                   
         DROP  R2                                                               
                                                                                
NXTTEA02 CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTTEA08                                                         
         L     R0,AGENAREA         clear buffer                                 
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    OVINDS1,OVINAME                                                  
         JNZ   NXTTEA04                                                         
         OC    QATEAM,QATEAM       TEST TEAM LIST PROVIDED                      
         JNZ   NXTTEA04                                                         
         MVC   QATEAM,AALL         NO - SET DEFAULT                             
         OI    OVINDS1,OVIAREC                                                  
         J     NXTTEA08                                                         
                                                                                
NXTTEA04 OC    DATEAM,DATEAM       TEST TEAM LIST PROVIDED                      
         JZ    NOMORE                                                           
         MVC   QATEAM,DATEAM       SET FOR KEY DRIVER                           
         J     NXTTEA10                                                         
                                                                                
NXTTEA08 TM    OVINDS1,OVIAREC                                                  
         JNO   NXTTEA10                                                         
         GOTOR (#NXTREC,ANXTREC),DMCB,TEKEY2T,('B#TEAM',SVPKSKEY),     +        
               (0,SAVED),0,0                                                    
         J     NXTTEA11                                                         
NXTTEA10 GOTOR (#NXTREC,ANXTREC),DMCB,TEAKEYT,('B#TEAM',SVPKSKEY),     +        
               (0,SAVED),0,0                                                    
NXTTEA11 JNE   EXITY                                                            
         TM    OVINDS1,OVINAME                                                  
         JNZ   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         USING TEARECD,R2          R2=A(TEMPLATE RECORD)                        
         LA    R2,TEARFST                                                       
         USING LIDELD,R2                                                        
NXTTEA12 CLI   LIDEL,0                                                          
         JE    NXTTEA30                                                         
         CLI   LIDEL,LIDELQ                                                     
         JE    NXTTEA16                                                         
NXTTEA14 XR    R0,R0                                                            
         IC    R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     NXTTEA12                                                         
                                                                                
NXTTEA16 CLI   LIDTYPE,LIDTCPJ                                                  
         JNE   NXTTEA26                                                         
         CLC   QCLIENT,SPACES                                                   
         JNH   NXTTEA14                                                         
         LA    R3,LIDDATA                                                       
NXTTEA18 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   NXTTEA14                                                         
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,8(R1)                                                         
         JE    NXTTEA20                                                         
         CLC   0(0,R3),QCLIENT                                                  
         LA    R3,L'ACTKACT(R3)                                                 
         J     NXTTEA18                                                         
                                                                                
NXTTEA20 CLC   QPROD,SPACES                                                     
         JNH   NXTTEA24                                                         
         AHI   RE,1                                                             
         LR    RF,R3                                                            
         AR    RF,RE                                                            
         LR    R1,RE                                                            
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,8(R1)                                                         
         JE    NXTTEA22                                                         
         CLC   0(0,RF),QPROD                                                    
         LA    R3,L'ACTKACT(R3)                                                 
         J     NXTTEA18                                                         
                                                                                
NXTTEA22 CLC   QJOBC,SPACES                                                     
         JNH   NXTTEA24                                                         
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LR    R1,RE                                                            
         XR    RE,RE                                                            
         IC    RE,PJOBLEN                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,8(R1)                                                         
         JE    NXTTEA22                                                         
         CLC   0(0,RF),QJOBC                                                    
         LA    R3,L'ACTKACT(R3)                                                 
         J     NXTTEA18                                                         
                                                                                
NXTTEA24 MVI   MATCH,C'Y'                                                       
         J     NXTTEA14                                                         
                                                                                
NXTTEA26 CLI   LIDTYPE,LIDTPID                                                  
         JNE   NXTTEA14                                                         
         LA    R3,LIDDATA                                                       
NXTTEA28 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   NXTTEA14                                                         
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'SA0KNUM,0(R3)),DPIDIND,DPIDMAXQ,       +        
               LP_D                                                             
         LA    R3,L'SA0KNUM(R3)                                                 
         J     NXTTEA28                                                         
                                                                                
NXTTEA30 J     EXITY                                                            
         EJECT                                                                  
                                                                                
SVRDEF   CSECT ,                                                                
                                                                                
ARYROL   LKOUT A,(R,NXTROL),MULTIROW=Y,ROWNAME=ROLRECD                          
                                                                                
Rolnum   LKOUT C,1,ROLKNUM,LBIN                                                 
Array    LKOUT C,2,(A,ARYROL1)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYROL1  LKOUT A,(D,B#ROLE,ROLRFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Tplname  LKOUT C,2,NAMEREC,CHAR,LEN=V                                           
Matched  LKOUT C,3,(D,B#SAVED,MATCH),CHAR,ND=Y                                  
Strepkey LKOUT C,4,(D,B#SAVED,STARPKEY),CHAR,ND=Y                               
Array    LKOUT C,4,(A,ARYPID)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET ROLE RECORDS                                                    *         
***********************************************************************         
                                                                                
NXTROL   MVI   MATCH,C'N'                                                       
         MVI   STARPKEY,C'N'                                                    
         TM    OVINDS1,OVINAME                                                  
         JNZ   NXTROL02                                                         
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAPIDB                                                      
         JZ    NXTROL02                                                         
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         CLEAR ENTRIES                                
         LHI   R1,DPIDMAXQ*L'LIDRPID                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               CLEAR AREA                                   
         DROP  R2                                                               
                                                                                
NXTROL02 CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTROL06                                                         
         L     R0,AGENAREA         clear buffer                                 
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    OVINDS1,OVINAME                                                  
         JNZ   NXTROL04                                                         
         OC    QAROLE,QAROLE       TEST ROLE LIST PROVIDED                      
         JNZ   NXTROL07                                                         
         MVC   QAROLE,AALL         NO - SET DEFAULT                             
         OI    OVINDS1,OVIAREC                                                  
         J     NXTROL06                                                         
                                                                                
NXTROL04 OC    DAROLE,DAROLE       TEST ROLE LIST PROVIDED                      
         JZ    NOMORE                                                           
         MVC   QAROLE,DAROLE       SET FOR KEY DRIVER                           
         J     NXTROL07                                                         
                                                                                
NXTROL06 TM    OVINDS1,OVIAREC                                                  
         JNO   NXTROL07                                                         
         GOTOR (#NXTREC,ANXTREC),DMCB,ROKEY2T,('B#ROLE',SVPKSKEY),     +        
               (0,SAVED),0,0                                                    
         J     NXTROL08                                                         
NXTROL07 GOTOR (#NXTREC,ANXTREC),DMCB,ROLKEYT,('B#ROLE',SVPKSKEY),     +        
               (0,SAVED),0,0                                                    
NXTROL08 JNE   EXITY                                                            
         TM    OVINDS1,OVINAME                                                  
         JNZ   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         USING ROLRECD,R2          R2=A(ROLE RECORD)                            
         TM    CPYSTATC,CPYSTEAJ   TEAM LIST IN JOBS?                           
         JNO   NXTROL09                                                         
         TM    ROLRSTAT,ROLSSRKR                                                
         JNO   *+8                                                              
         MVI   STARPKEY,C'Y'                                                    
NXTROL09 LA    R2,ROLRFST                                                       
         USING LIDELD,R2                                                        
NXTROL10 CLI   LIDEL,0                                                          
         JE    NXTROL30                                                         
         CLI   LIDEL,LIDELQ                                                     
         JE    NXTROL14                                                         
NXTROL12 XR    R0,R0                                                            
         IC    R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     NXTROL10                                                         
                                                                                
NXTROL14 CLI   LIDTYPE,LIDTMED                                                  
         JNE   NXTROL20                                                         
         LA    R3,LIDDATA                                                       
NXTROL16 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   NXTROL12                                                         
         CLC   0(L'PMDKMED,R3),QJOBC                                            
         JE    NXTROL18                                                         
         LA    R3,L'PMDKMED(R3)                                                 
         J     NXTROL16                                                         
                                                                                
NXTROL18 MVI   MATCH,C'Y'                                                       
         J     NXTROL12                                                         
                                                                                
NXTROL20 CLI   LIDTYPE,LIDTPID                                                  
         JNE   NXTROL12                                                         
         LA    R3,LIDDATA                                                       
NXTROL22 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   NXTROL12                                                         
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'SA0KNUM,0(R3)),DPIDIND,DPIDMAXQ,       +        
               LP_D                                                             
         LA    R3,L'SA0KNUM(R3)                                                 
         J     NXTROL22                                                         
                                                                                
NXTROL30 J     EXITY                                                            
         EJECT                                                                  
SVRDEF   CSECT ,                                                                
                                                                                
ARYPID   LKOUT A,(R,NXTPID),MULTIROW=Y,ROWNAME=WORKD                            
                                                                                
PidChar  LKOUT C,4,(D,B#SAVED,PIDCDE),CHAR,LEN=L'SAPALPID,ND=Y                  
PidBin   LKOUT C,5,(D,B#WORKD,TEMP2+50),HEXD,LEN=L'SA0KNUM,ND=Y                 
FirstNam LKOUT C,6,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
MidNam   LKOUT C,7,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LastNam  LKOUT C,8,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                        
                                                                                
         LKOUT E                                                                
***********************************************************************         
* GET PID  RECORDS                                                    *         
***********************************************************************         
                                                                                
NXTPID   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTPID02                                                         
                                                                                
         OC    DAPIDB,DAPIDB       ANY PID LIST                                 
         JZ    NOMORE              EXIT                                         
         MVC   SVROLKEY,IOKEY                                                   
                                                                                
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAPIDB                                                      
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        NUMBER OF ENTRIES                            
         LA    R4,LW_DATA2         START OF LIST                                
         ST    R4,APIDLST                                                       
         J     NXTPID04                                                         
         DROP  R2                                                               
                                                                                
NXTPID02 L     R4,APIDLST                                                       
         XC    QPIDB,QPIDB                                                      
         LA    R4,L'SA0KNUM(R4)    GO TO NEXT ENTRY                             
         OC    0(L'SA0KNUM,R4),0(R4)                                            
         JZ    NOMORE              IF EMPTY WE ARE FINISHED                     
NXTPID04 L     R3,AGENAREA                                                      
         USING PIDTABD,R3                                                       
NXTPID06 OC    PIDBIN,PIDBIN                                                    
         JZ    NXTPID10                                                         
         CLC   PIDBIN,0(R4)                                                     
         JE    NXTPID08                                                         
         LA    R3,PIDTABL(R3)                                                   
         J     NXTPID06                                                         
*                                                                               
NXTPID08 MVC   TEMP2+50(L'PIDBIN),PIDBIN                                        
         MVC   TEMP2(L'PIDFNAM),PIDFNAM                                         
         MVC   TEMP2+16(L'PIDLNAM),PIDLNAM                                      
         MVC   TEMP2+32(L'PIDMNAM),PIDMNAM                                      
         MVC   PIDCDE,PIDCHAR                                                   
         J     NXTPID20                                                         
                                                                                
NXTPID10 MVC   TEMP2(L'SA0KNUM),0(R4)                                           
         MVC   QPIDB(L'SA0KNUM),0(R4)                                           
         GOTOR (#GETPID,AGETPID)                                                
         MVC   PIDCDE,TEMP2                                                     
         TM    OVINDS1,OVINONM                                                  
         JNZ   NXTPID14                                                         
         GOTOR (#GETPIN,AGETPIN)                                                
NXTPID12 MVC   PIDFNAM,TEMP2                                                    
         MVC   PIDBIN,TEMP2+50                                                  
         MVC   PIDMNAM,TEMP2+32                                                 
         MVC   PIDLNAM,TEMP2+16                                                 
         MVC   PIDCHAR,PIDCDE                                                   
         J     NXTPID20                                                         
*                                                                               
NXTPID14 MVC   PIDCHAR,PIDCDE                                                   
         MVC   PIDBIN,0(R4)                                                     
*                                                                               
NXTPID20 ST    R4,APIDLST                                                       
         MVC   LP_ADATA,AIO1                                                    
         MVC   IOKEY,SVROLKEY                                                   
         J     EXITY                                                            
                                                                                
PIDTABD  DSECT                                                                  
PIDBIN   DS    XL2                                                              
PIDCHAR  DS    CL8                                                              
PIDFNAM  DS    CL16                                                             
PIDMNAM  DS    CL16                                                             
PIDLNAM  DS    CL16                                                             
PIDTABL  EQU   *-PIDTABD                                                        
         EJECT                                                                  
SVRDEF   CSECT ,                                                                
**********************************************************************          
* STATUS REPORT/CAMPAIGN SEARCH                                      *          
**********************************************************************          
                                                                                
REQRSTR  LKREQ H,A#RSTDL,OUTRSTR,NEXTREQ=REQRSRC                                
viewall  LKREQ F,1,(D,B#SAVED,QVIEWAL),CHAR,OLEN=L'QVIEWAL,            +        
               MAXLEN=L'QVIEWAL,TEXT=(*,VIEWLIT),COL=*                          
client   LKREQ F,2,(D,B#SAVED,QCLIENT),CHAR,OLEN=L'QCLIENT,            +        
               MAXLEN=L'QCLIENT,TEXT=AC#CLI,COL=*                               
camp     LKREQ F,3,(D,B#SAVED,QCAMP),LBIN,OLEN=L'QCAMP,                +        
               TEXT=(*,CAMCLIT),COL=*                                           
prod     LKREQ F,4,(D,B#SAVED,QPROD),CHAR,OLEN=L'QPROD,                +        
               MAXLEN=L'QPROD,TEXT=AC#PROC,COL=*                                
job      LKREQ F,5,(D,B#SAVED,QJOBC),CHAR,OLEN=L'QJOBC,                +        
               MAXLEN=L'QJOBC,TEXT=AC#JOBC,COL=*                                
current  LKREQ F,6,(D,B#SAVED,QCURR),CHAR,OLEN=L'QCURR,                +        
               MAXLEN=L'QCURR,TEXT=(*,STCTLIT),COL=*                            
finish   LKREQ F,7,(D,B#SAVED,QFINS),CHAR,OLEN=L'QFINS,                +        
               MAXLEN=L'QFINS,TEXT=(*,STFDLIT),COL=*                            
obsol    LKREQ F,8,(D,B#SAVED,QOBSO),CHAR,OLEN=L'QOBSO,                +        
               MAXLEN=L'QOBSO,TEXT=(*,STOBLIT),COL=*                            
kstg#    LKREQ F,9,(D,B#SAVED,QKYST),LBIN,OLEN=L'QKYST,                +        
               MAXLEN=L'QKYST,TEXT=(*,KSTNLIT),COL=*                            
frmdate  LKREQ F,10,(D,B#SAVED,QADDFROM),PDAT,OLEN=L'QADDFROM,         +        
               TEXT=(*,ADDFLIT),COL=*                                           
todate   LKREQ F,11,(D,B#SAVED,QADDTO),PDAT,OLEN=L'QADDTO,             +        
               TEXT=(*,ADDTLIT),COL=*                                           
kstFDte  LKREQ F,12,(D,B#SAVED,QKSTFROM),PDAT,OLEN=L'QKSTFROM,         +        
               TEXT=(*,KSDFLIT),COL=*                                           
kstTDte  LKREQ F,13,(D,B#SAVED,QKSTTO),PDAT,OLEN=L'QKSTTO,             +        
               TEXT=(*,KSDTLIT),COL=*                                           
LimLst   LKREQ F,14,(D,B#SAVED,QLMLST),CHAR,OLEN=L'QLMLST,             +        
               MAXLEN=L'QLMLST,TEXT=(*,LMLSLIT),COL=*                           
Init     LKREQ F,15,(D,B#SAVED,QINIT),CHAR,OLEN=L'QINIT,               +        
               MAXLEN=L'QINIT,TEXT=(*,INITLIT),COL=*                            
Team     LKREQ F,16,(I,B#SAVED,QTEAMIND),LBIN,OLEN=L'TEAKNUM,          +        
               LIST=F,TEXT=(*,TMNMLIT),COL=*                                    
Role     LKREQ F,17,(I,B#SAVED,QROLEIND),LBIN,OLEN=L'ROLKNUM,          +        
               LIST=F,TEXT=AC#RLNUM,COL=*                                       
PID      LKREQ F,18,(I,B#SAVED,QPIDIND),HEXD,OLEN=L'PIDKPID,           +        
               LIST=F,TEXT=(*,PIDBLIT),COL=*                                    
         LKREQ E                                                                
                                                                                
OUTRSTR  LKOUT H                                                                
                                                                                
RSRSTR   LKOUT R,R#RSTDL           RESOURCE STATUS REPORT                       
Array    LKOUT C,1,(A,ARYSTR)                                                   
         LKOUT E                                                                
                                                                                
KEYSTGR  LKOUT R,R#KSTGDL          KEYSTAGE                                     
Array    LKOUT C,R#KSTGDL,(A,ARYKST),FILTROUT=TSTQINIT                          
         LKOUT E                                                                
                                                                                
TEMPLTR  LKOUT R,R#TEMPDL          TEMPLATE                                     
Array    LKOUT C,R#TEMPDL,(A,ARYTMP),FILTROUT=TSTQINIT                          
         LKOUT E                                                                
                                                                                
TEAMR    LKOUT R,R#TEAMDL          TEAM                                         
Array    LKOUT C,R#TEAMDL,(A,ARYTEAM),FILTROUT=TSTQINIT                         
         LKOUT E                                                                
                                                                                
ROLER    LKOUT R,R#ROLEDL          ROLE                                         
Array    LKOUT C,R#ROLEDL,(A,ARYROL),FILTROUT=TSTQINIT                          
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYSTR   LKOUT A,(R,NXTRWK),MULTIROW=Y,ROWNAME=RWKRECD                          
                                                                                
PidChar  LKOUT C,1,(D,B#SAVED,PIDCDE),CHAR,LEN=L'SAPALPID,ND=Y                  
FirstNam LKOUT C,2,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
MidNam   LKOUT C,3,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LastNam  LKOUT C,4,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                        
CliCde   LKOUT C,5,(D,B#SAVED,OCLICDE),CHAR,LEN=L'OCLICDE                       
PrdCde   LKOUT C,6,(D,B#SAVED,OPROCDE),CHAR,LEN=L'OPROCDE                       
JobCde   LKOUT C,7,(D,B#SAVED,OJOBCDE),CHAR,LEN=L'OJOBCDE                       
CampCde  LKOUT C,8,RWKKCCDE,LBIN                                                
CliNam   LKOUT C,9,(D,B#SAVED,OCLINAM),CHAR,LEN=L'OCLINAM                       
PrdNam   LKOUT C,10,(D,B#SAVED,OPRONAM),CHAR,LEN=L'OPRONAM                      
JobNam   LKOUT C,11,(D,B#SAVED,OJOBNAM),CHAR,LEN=L'OJOBNAM                      
PRout    LKOUT P,RWKKCCDE,SETQCAMP                                              
Array    LKOUT C,38,(A,ARYCAM)                                                  
Array    LKOUT C,6,(A,ARYKRDT),FILTROUT=TESTKROL                                
Array    LKOUT C,12,(A,ARYKSDT)                                                 
Array    LKOUT C,13,(A,ARYCRTM)                                                 
Status   LKOUT C,14,(D,B#SAVED,OSTATUS),CHAR,LEN=L'OSTATUS                      
TotHrs   LKOUT C,15,(D,B#SAVED,DHRS),SPAK                                       
JobSta   LKOUT C,16,(D,B#SAVED,OJBSTAT),CHAR,LEN=L'OJBSTAT                      
Array    LKOUT C,39,(A,ARYNXT)                                                  
Array    LKOUT C,40,(A,ARYIND)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYKRDT  LKOUT A,(R,NXTKRO),MULTIROW=Y,ROWNAME=WORKD                            
                                                                                
RolNum   LKOUT C,1,(D,B#SAVED,OKEYRO),LBIN,LEN=L'OKEYRO,ND=Y                    
PidChar  LKOUT C,2,(D,B#SAVED,PIDCDE),CHAR,LEN=L'SAPALPID,ND=Y                  
PidBin   LKOUT C,3,TEMP2+50,HEXD,LEN=L'SA0KNUM,ND=Y                             
FirstNam LKOUT C,4,TEMP2,CHAR,LEN=16,ND=Y                                       
MidNam   LKOUT C,5,TEMP2+32,CHAR,LEN=16,ND=Y                                    
LastNam  LKOUT C,6,TEMP2+16,CHAR,LEN=16,ND=Y                                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYKSDT  LKOUT A,(D,B#RESWK,RWKRFST),EOT=EOR,                          +        
               ROWID=(KSTEL,KSTELQ),ROWWIDTH=(V,KSTLN)                          
                                                                                
KstNum   LKOUT C,12,KSTCODE,LBIN                                                
KstEDte  LKOUT C,13,KSTENDT,PDAT                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYCRTM  LKOUT A,(D,B#RESWK,RWKRFST),EOT=EOR,                          +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
                                                                                
PRout    LKOUT P,LIDELD,EXTLPDS                                                 
Array    LKOUT C,13,(A,ARYCRTM1)                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYCRTM1 LKOUT A,(R,NXTPID),MULTIROW=Y,ROWNAME=WORKD                            
                                                                                
PidChar  LKOUT C,12,(D,B#SAVED,PIDCDE),CHAR,LEN=L'SAPALPID,ND=Y                 
PidBin   LKOUT C,13,(D,B#WORKD,TEMP2+50),HEXD,LEN=L'SA0KNUM,ND=Y                
FirstNam LKOUT C,14,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
MidNam   LKOUT C,15,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LastNam  LKOUT C,16,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                       
                                                                                
         LKOUT E                                                                
                                                                                
ARYNXT   LKOUT A,(D,B#RESWK,RWKRFST),EOT=EOR,                          +        
               ROWID=(FFTEL,FFTELQ),ROWWIDTH=(V,FFTLN),NEWEL=N                  
                                                                                
Next     LKOUT C,39,FFTDATA,CHAR,LEN=V,ND=Y                                     
                                                                                
         LKOUT E                                                                
                                                                                
ARYIND   LKOUT A,(R,NXTAUD),NROWS=1,ROWNAME=AUDRECD                             
                                                                                
Indx     LKOUT C,40,AUDRINDX,LBIN                                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET PIDS FROM LIST ELEMENT                                          *         
***********************************************************************         
         SPACE 1                                                                
EXTLPDS  NTR1  ,                                                                
         NI    OVINDS1,X'FF'-OVINONM                                            
         USING LW_D,R2                                                          
EXLPD02  XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAPIDB                                                      
         JZ    EXLPD04                                                          
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         CLEAR ENTRIES                                
         LHI   R1,DPIDMAXQ*L'LIDRPID                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               CLEAR AREA                                   
         DROP  R2                                                               
*                                                                               
EXLPD04  L     R2,LP_AINP                                                       
         USING LIDELD,R2                                                        
         LA    R3,LIDDATA                                                       
EXLPD06  XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   EXITY                                                            
         GOTOR LP_AAWMP,DMCB,(L'LIDRPID,0(R3)),DPIDIND,                +        
               DPIDMAXQ,LP_D                                                    
         LA    R3,L'LIDRPID+L'LIDRHRS(R3)                                       
         J     EXLPD06                                                          
                                                                                
***********************************************************************         
* GET RESOURCE WORK RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
NXTRWK   NI    OVINDS1,X'FF'-(OVIRWK)                                           
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTRWK02                                                         
         L     R0,AGENAREA         clear buffer                                 
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         GOTOR PIDLST                                                           
         CLI   QLMLST,C'Y'         LIMIT LIST OVERRIDE                          
         JE    NXTRWK02            YES - SEE ALL CLIENTS                        
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT2Q',TSARABUF),            +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QRESRCES',0)         
NXTRWK02 MVC   IOKEY,SVRWKKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,RWKKEYT,('B#RESWK',0),           +        
               (0,SAVED),AFLTCAMP,0                                             
         JNE   NXTRWK10                                                         
         MVC   SVRWKKEY,IOKEY                                                   
         GOTOR AFLTKST                                                          
         JNE   NXTRWK02                                                         
         GOTOR AFLTPID                                                          
         JNE   NXTRWK02                                                         
         GOTOR AEXTHRS                                                          
         L     R2,IOADDR                                                        
         USING RWKRECD,R2                                                       
         TM    CPYSTATC,CPYSTEAJ   TEAM LIST IN JOBS?                           
         JNO   *+8                                                              
         OI    OVINDS1,OVIRWK                                                   
         GOTOR AEXTCPJ,DMCB,RWKKACT  EXTRACT CLIENT PRODUCT JOB                 
         JNE   NXTRWK02                                                         
         NI    OVINDS1,X'FF'-OVIRWK                                             
         MVC   IOKEY,SVRWKKEY                                                   
         GOTOR ASETSTAT,DMCB,RWKRSTAT  EXTRACT STATUS                           
         MVC   TEMP2(L'SA0KNUM),RWKRPID                                         
         GOTOR (#GETPID,AGETPID)   GET CHARACTER PID                            
         MVC   PIDCDE,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)   GET NAMES FOR PID                            
         J     EXITY                                                            
*                                                                               
NXTRWK10 CLI   LP_RMODE,LP_RLAST   TEST FIRST TIME                              
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET KEY ROLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
NXTKRO   ST    R9,LP_ADATA                                                      
         L     R3,AKEYROL                                                       
         OC    0(L'LIDROLN,R3),0(R3)                                            
         JZ    NOMORE                                                           
         MVC   TEMP2(L'SA0KNUM),L'LIDROLN(R3)                                   
         MVC   OKEYRO(L'SA0KNUM),0(R3)                                          
         GOTOR (#GETPID,AGETPID)                                                
         MVC   PIDCDE,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)                                                
         LA    R3,L'LIDROLN+L'LIDBPID(R3)                                       
         ST    R3,AKEYROL                                                       
         J     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* RESOURCES (PEOPLE) SEARCH                                          *          
**********************************************************************          
                                                                                
REQRSRC  LKREQ H,A#RSRDL,OUTRES,NEXTREQ=REQMYTS                                 
team     LKREQ F,1,(I,B#SAVED,QTEAMIND),LBIN,OLEN=L'TEAKNUM,           +        
               LIST=F,TEXT=(*,TMNMLIT),COL=*                                    
role     LKREQ F,2,(I,B#SAVED,QROLEIND),LBIN,OLEN=L'ROLKNUM,           +        
               LIST=F,TEXT=AC#RLNUM,COL=*                                       
pid      LKREQ F,3,(I,B#SAVED,QPIDIND),HEXD,OLEN=L'PIDKPID,            +        
               LIST=F,TEXT=(*,PIDBLIT),COL=*                                    
kstg#    LKREQ F,4,(D,B#SAVED,QKYST),LBIN,OLEN=L'QKYST,                +        
               MAXLEN=L'QKYST,TEXT=(*,KSTNLIT),COL=*                            
stdte    LKREQ F,5,(D,B#SAVED,QKSTFROM),PDAT,OLEN=L'QKSTFROM,          +        
               TEXT=(*,ADDFLIT),COL=*                                           
Detail   LKREQ F,6,(D,B#SAVED,QDETL),CHAR,OLEN=L'QDETL,                +        
               MAXLEN=L'QDETL,TEXT=(*,DETLLIT),COL=*                            
         LKREQ E                                                                
                                                                                
OUTRES   LKOUT H                                                                
                                                                                
RESORCE  LKOUT R,R#RSRDL           RESOURCES SEARCH                             
Array    LKOUT C,1,(A,ARYRES)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYRES   LKOUT A,(R,NXTRES),MULTIROW=Y,ROWNAME=RWKRECD                          
                                                                                
PidChar  LKOUT C,1,(D,B#SAVED,PIDCDE),CHAR,LEN=L'SAPALPID,ND=Y                  
PidBin   LKOUT C,2,(D,B#WORKD,TEMP2+50),HEXD,LEN=L'SA0KNUM,ND=Y                 
FirstNam LKOUT C,3,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
MidNam   LKOUT C,4,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LastNam  LKOUT C,5,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                        
                                                                                
Array    LKOUT C,6,(A,ARYFTEA)                                                  
Array    LKOUT C,6,(A,ARYTEAN)                                                  
Array    LKOUT C,7,(A,ARYFROL)                                                  
Array    LKOUT C,7,(A,ARYROLN)                                                  
                                                                                
KstCde   LKOUT C,10,(D,B#SAVED,OKSTCDE),LBIN,LEN=L'OKSTCDE,ND=Y,       +        
               FILTROUT=TESTKYST,SKIPCOLS=1                                     
KstNam   LKOUT C,11,(D,B#SAVED,OKSTNAM),CHAR,LEN=L'OKSTNAM,ND=Y                 
CliCde   LKOUT C,12,(D,B#SAVED,OCLICDE),CHAR,LEN=L'OCLICDE,ND=Y,       +        
               FILTROUT=TESTCLIC,SKIPCOLS=8                                     
CliNam   LKOUT C,13,(D,B#SAVED,OCLINAM),CHAR,LEN=L'OCLINAM,ND=Y                 
PrdCde   LKOUT C,14,(D,B#SAVED,OPROCDE),CHAR,LEN=L'OPROCDE,ND=Y                 
PrdNam   LKOUT C,15,(D,B#SAVED,OPRONAM),CHAR,LEN=L'OPRONAM,ND=Y                 
JobCde   LKOUT C,16,(D,B#SAVED,OJOBCDE),CHAR,LEN=L'OJOBCDE,ND=Y                 
JobNam   LKOUT C,17,(D,B#SAVED,OJOBNAM),CHAR,LEN=L'OJOBNAM,ND=Y                 
CampCde  LKOUT C,18,(D,B#SAVED,DCAMP),LBIN                                      
array    LKOUT C,38,(A,ARYCAM)                                                  
array    LKOUT C,39,(A,ARYNXT)                                                  
array    LKOUT C,19,(A,ARYDAY)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET PERSONAL KEYSTAGE RECORDS FOR RESOURCES                         *         
***********************************************************************         
                                                                                
NXTRES   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTRES70                                                         
                                                                                
         OC    QKSTFROM,QKSTFROM   Check if 'from date' set                     
         JNZ   NXTRES04                                                         
         MVC   QKSTTO,=XL3'FFFFFF'                                              
         J     NXTRES06                                                         
NXTRES04 GOTOR VDATCON,DMCB,(1,QKSTFROM),(0,WORK+6)   SET END DATE              
         GOTOR VADDAY,DMCB,(C'D',WORK+6),WORK,F'31'                             
         GOTOR VDATCON,DMCB,(0,WORK),(1,QKSTTO)                                 
NXTRES06 XC    QENDSTR,QENDSTR     SET RANGE READY FOR KEY DRIVER               
         MVI   QSTREND,X'FF'                                                    
         MVC   QSTREND+1(L'QSTREND-1),QSTREND                                   
         XR    RE,RE                                                            
         ICM   RE,7,QKSTTO                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,QSTRSTR                                                     
         XR    RE,RE                                                            
         ICM   RE,7,QKSTFROM                                                    
         LNR   RE,RE                                                            
         STCM  RE,7,QENDEND                                                     
                                                                                
         GOTOR PIDLST                                                           
                                                                                
         OC    DAPIDB2,DAPIDB2                                                  
         JZ    NOMORE                                                           
         MVC   QACAMP,AALL         NO - SET DEFAULT                             
         MVI   QCURR,C'Y'                                                       
                                                                                
NXTRES42 GOTOR GOTSAR,DMCB,('TSAINI',0)                                         
                                                                                
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAPIDB2                                                     
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        NUMBER OF ENTRIES                            
         LA    R4,LW_DATA2         START OF LIST                                
NXTRES44 MVC   QPIDB,0(R4)         GET PID                                      
         MVI   LP_RMODE,LP_RFRST   SET FIRST TIME                               
         NI    OVINDS1,X'FF'-OVITSKFD    REMOVE TASK FOUND SETTING              
                                                                                
NXTRES46 GOTOR (#NXTREC,ANXTREC),DMCB,PKSKEYT,('B#PTASK',0),           +        
               ('$NXTRXGR',SAVED),AFLTPKS,0                                     
         JNE   NXTRES54                                                         
                                                                                
         LA    R2,IOKEY                                                         
         USING PKSRECD,R2          R2=A(PERSONAL KEY STAGE KEY)                 
         CLI   QDETL,C'Y'          DETAIL CALL                                  
         JE    NXTRES48            YES                                          
         OC    PKSKKYST,PKSKKYST   NO - DON'T ALLOW HOLIDAY LINES               
         JZ    NXTRES46                                                         
                                                                                
BUF      USING RES_D,TSARREC                                                    
NXTRES48 OI    OVINDS1,OVITSKFD                                                 
         XC    TSARREC,TSARREC                                                  
         MVC   BUF.RES_PID,PKSKPIDB                                             
         MVC   BUF.RES_CAMP,PKSKCODE                                            
         MVC   BUF.RES_ACT,PKSKACT                                              
         MVC   BUF.RES_STAT,PKSKSTAT                                            
         MVC   BUF.RES_AMPM,PKSKAMPM                                            
         MVC   BUF.RES_ENDT,PKSKEDT                                             
         MVC   BUF.RES_STDT,PKSKSDT                                             
         MVC   BUF.RES_KYST(L'PKSKKYST+L'PKSKOUT),PKSKKYST                      
         MVC   BUF.RES_DA,PKSKDA                                                
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    NXTRES46                                                         
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   NXTRES52                                                         
         DC    H'0'                Duplicate key                                
NXTRES52 MVC   LP_ERROR,=AL2(AE$MAX#)                                           
         J     QERROR                                                           
*                                                                               
NXTRES54 TM    OVINDS1,OVITSKFD    have we found some work                      
         JNZ   NXTRES58            yes - go to next PID                         
         XC    TSARREC,TSARREC     no - add empty record                        
         MVC   BUF.RES_PID,0(R4)                                                
         GOTOR GOTSAR,DMCB,('TSAADD',0)                                         
         JE    NXTRES58                                                         
         TM    TSARERRS,TSEEOF     Test end of buffer                           
         JNZ   NXTRES56                                                         
         DC    H'0'                Duplicate key                                
NXTRES56 MVC   LP_ERROR,=AL2(AE$MAX#)                                           
         J     QERROR                                                           
*                                                                               
NXTRES58 LA    R4,L'SA0KNUM(R4)                                                 
         JCT   R3,NXTRES44                                                      
*                                                                               
NXTRES60 MVI   LP_RMODE,LP_RNEXT                                                
         XC    TSARREC,TSARREC                                                  
         GOTOR GOTSAR,DMCB,('TSARDH',0)                                         
NXTRES70 TM    TSARERRS,TSEEOF                                                  
         JNZ   NXTRES90                                                         
                                                                                
         NI    OVINDS1,X'FF'-OVIDAYT                                            
         L     R0,AIO4                                                          
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    OKSTCDE,OKSTCDE                                                  
         XC    OCLICDE,OCLICDE                                                  
                                                                                
         MVC   QPIDB,BUF.RES_PID                                                
         OC    BUF.RES_DA,BUF.RES_DA                                            
         JZ    NXTRES74                                                         
         MVC   IODA,BUF.RES_DA                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    NXTRES72                                                         
         DC    H'0'                                                             
                                                                                
NXTRES72 MVC   DCAMP,BUF.RES_CAMP                                               
         GOTOR ASETSTAT,DMCB,BUF.RES_STAT  EXTRACT STATUS                       
         GOTOR AEXTCPJ,DMCB,BUF.RES_ACT  EXTRACT CLIENT PRODUCT JOB             
         CLI   QDETL,C'Y'          DETAIL CALL                                  
         JNE   NXTRES74            NO                                           
         GOTOR AEXTKST,DMCB,BUF.RES_KYST    YES - GET KEYSTAGE                  
NXTRES74 MVC   TEMP2(L'SA0KNUM),BUF.RES_PID                                     
         GOTOR (#GETPID,AGETPID)   GET CHARACTER PID                            
         MVC   PIDCDE,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)   GET NAMES FOR PID                            
         MVC   LP_ADATA,AIO4                                                    
         J     EXITY                                                            
                                                                                
NXTRES90 MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
         DROP  BUF                                                              
         EJECT                                                                  
                                                                                
ARYFTEA  LKOUT A,(R,NXTPTM),MULTIROW=Y,ROWNAME=PIDRECD                          
                                                                                
         LKOUT E                                                                
                                                                                
ARYTEAN  LKOUT A,(R,NXTTEAM),MULTIROW=Y,ROWNAME=TEARECD                         
                                                                                
Teamnum  LKOUT C,5,TEAKNUM,LBIN                                                 
Array    LKOUT C,6,(A,ARYTEAN1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYTEAN1 LKOUT A,(D,B#TEAM,TEARFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Teamname LKOUT C,6,NAMEREC,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET PID PASSIVES TO WORK OUT TEAMS THIS PERSON BELONGS TO           *         
***********************************************************************         
                                                                                
NXTPTM   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTPTM04                                                         
         OI    OVINDS1,OVINAME                                                  
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DATEAM                                                      
         JZ    NXTPTM04                                                         
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         CLEAR ENTRIES                                
         LHI   R1,DTEAMAXQ*L'PIDKNUM                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               CLEAR AREA                                   
         DROP  R2                                                               
NXTPTM04 GOTOR (#NXTREC,ANXTREC),DMCB,PTMKEYT,('B#PID',SVPKSKEY),      +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
         LA    R2,IOKEY                                                         
         USING PIDRECD,R2                                                       
         GOTOR LP_AAWMP,DMCB,(L'PIDKNUM,PIDKNUM),DTEAMIND,DTEAMAXQ,    +        
               LP_D                                                             
         DROP  R2                                                               
         J     NXTPTM04                                                         
                                                                                
                                                                                
SVRDEF   CSECT ,                                                                
                                                                                
ARYFROL  LKOUT A,(R,NXTPRL),MULTIROW=Y,ROWNAME=PIDRECD                          
                                                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYROLN  LKOUT A,(R,NXTROL),MULTIROW=Y,ROWNAME=ROLRECD                          
                                                                                
Rolnum   LKOUT C,7,ROLKNUM,LBIN                                                 
Array    LKOUT C,8,(A,ARYROLN1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYROLN1 LKOUT A,(D,B#ROLE,ROLRFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Rolename LKOUT C,8,NAMEREC,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET PID PASSIVES TO WORK OUT ROLES THIS PERSON BELONGS TO           *         
***********************************************************************         
                                                                                
NXTPRL   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTPRL02                                                         
         OI    OVINDS1,OVINAME                                                  
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAROLE                                                      
         JZ    NXTPRL02                                                         
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         CLEAR ENTRIES                                
         LHI   R1,DROLMAXQ*L'PIDKNUM                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               CLEAR AREA                                   
         DROP  R2                                                               
NXTPRL02 GOTOR (#NXTREC,ANXTREC),DMCB,PRLKEYT,('B#PID',SVPKSKEY),      +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   EXITY                                                            
         LA    R2,IOKEY                                                         
         USING PIDRECD,R2                                                       
         GOTOR LP_AAWMP,DMCB,(L'PIDKNUM,PIDKNUM),DROLEIND,DROLMAXQ,    +        
               LP_D                                                             
         DROP  R2                                                               
         J     NXTPRL02                                                         
                                                                                
SETQCAMP L     R1,LP_AINP                                                       
         MVC   DCAMP,0(R1)                                                      
         BR    RE                                                               
                                                                                
SVRDEF   CSECT ,                                                                
                                                                                
ARYCAM   LKOUT A,(R,NXTCAM),NROWS=1,ROWNAME=RWKRECD                             
                                                                                
Array    LKOUT C,38,(A,ARYCAM1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYCAM1  LKOUT A,(D,B#CAMP,RWKRFST),EOT=EOR,                           +        
               ROWID=(NAMEL,NAMELQ),ROWWIDTH=(V,NAMLN)                          
                                                                                
Campname LKOUT C,38,NAMEREC,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET CAMPAIGN RECORD                                                 *         
***********************************************************************         
                                                                                
NXTCAM   GOTOR (#NXTREC,ANXTREC),DMCB,CAMKEYT,('B#CAMP',0),            +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
                                                                                
ARYDAY   LKOUT A,(R,BLDCAL),MULTIROW=Y                                          
                                                                                
array    LKOUT C,19,(A,ARYDAY1)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYDAY1  LKOUT A,(I,B#WORKD,AIO6),NROWS=31,                            +        
               ROWNAME=DAYTABD,ROWWIDTH=DAYTABL                                 
                                                                                
Day      LKOUT C,19,DAYTYPE,CHAR                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* BUILD CALENDAR TABLE                                                *         
***********************************************************************         
                                                                                
BLDCAL   L     R2,AIO6                                                          
         TM    OVINDS1,OVIDAYT                                                  
         JNZ   NOMORE                                                           
         XC    0(DAYTABL*(DAYMAXQ+1),R2),0(R2)                                  
         MVC   SVRMODE,LP_RMODE                                                 
         MVI   LP_RMODE,LP_RFRST                                                
         USING DAYTABD,R2                                                       
         MVC   DAYDATE,QKSTFROM                                                 
         LA    R0,DAYMAXQ                                                       
BLDCAL02 GOTOR VDATCON,DMCB,(1,DAYDATE),(0,WORK+6)   SET END DATE               
         GOTOR VADDAY,DMCB,(C'D',WORK+6),WORK,F'1'                              
         LA    R2,DAYTABL(R2)                                                   
         GOTOR VDATCON,DMCB,(0,WORK),(1,DAYDATE)                                
         JCT   R0,BLDCAL02                                                      
                                                                                
         L     R0,AGENAREA         clear buffer                                 
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R3,AGENAREA                                                      
         USING DTETABD,R3                                                       
                                                                                
BUF      USING RES_D,TSARREC                                                    
BLDCAL04 XR    RE,RE                                                            
         ICM   RE,7,BUF.RES_ENDT                                                
         LNR   RE,RE                                                            
         STCM  RE,7,DTEEND                                                      
                                                                                
         XR    RE,RE                                                            
         ICM   RE,7,BUF.RES_STDT                                                
         LNR   RE,RE                                                            
         STCM  RE,7,DTESTR                                                      
         MVI   DTETYPE,DTEWRK                                                   
         OC    DTETYPE,BUF.RES_AMPM                                             
         LA    R3,DTETABL(R3)                                                   
                                                                                
         MVC   PRETSRRC,TSARREC                                                 
         XC    TSARREC,TSARREC                                                  
         GOTOR GOTSAR,DMCB,('TSANXT',0)                                         
         TM    TSARERRS,TSEEOF                                                  
         JNZ   BLDCAL06                                                         
         CLI   QDETL,C'Y'                                                       
         JE    BLDCAL06                                                         
         CLC   TSARREC(RES_LN1Q),PRETSRRC                                       
         JE    BLDCAL04                                                         
                                                                                
BLDCAL06 GOTOR (#NXTREC,ANXTREC),DMCB,HOLKEYT,('B#HOL',0),             +        
               ('$NXTRXAD',SAVED),0,0                                           
         JNE   BLDCAL08                                                         
                                                                                
         L     R1,IOADDR                                                        
         USING PKSRECD,R1          R2=A(PERSONAL KEY STAGE KEY)                 
         XR    RE,RE                                                            
         ICM   RE,7,PKSKEDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,DTEEND                                                      
                                                                                
         XR    RE,RE                                                            
         ICM   RE,7,PKSKSDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,DTESTR                                                      
         MVI   DTETYPE,DTEHOL                                                   
         OC    DTETYPE,PKSKAMPM                                                 
         LA    R3,DTETABL(R3)                                                   
         J     BLDCAL06                                                         
                                                                                
BLDCAL08 L     R3,AGENAREA                                                      
         L     R2,AIO6                                                          
BLDCAL10 MVI   DAYTYPE,DAYFREE                                                  
BLDCAL12 CLC   DAYDATE,DTESTR                                                   
         JL    BLDCAL52                                                         
         JE    BLDCAL30                                                         
         CLC   DAYDATE,DTEEND                                                   
         JH    BLDCAL52                                                         
         JL    BLDCAL46                                                         
                                                                                
BLDCAL14 TM    DTETYPE,DTEHOLSA+DTEHOLEP                                        
         JO    BLDCAL46                                                         
         TM    DTETYPE,DTEHOLEA                                                 
         JZ    BLDCAL22                                                         
         TM    DTETYPE,DTEHOL                                                   
         JZ    BLDCAL18                                                         
         CLI   DAYTYPE,DAYASSPM                                                 
         JNE   BLDCAL16                                                         
         MVI   DAYTYPE,DAYAMOPM                                                 
         J     BLDCAL52                                                         
BLDCAL16 MVI   DAYTYPE,DAYAMOUT                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL18 TM    DTETYPE,DTEWRK                                                   
         JZ    BLDCAL52                                                         
         CLI   DAYTYPE,DAYPMOUT                                                 
         JNE   BLDCAL20                                                         
         MVI   DAYTYPE,DAYAMAPM                                                 
         J     BLDCAL52                                                         
BLDCAL20 MVI   DAYTYPE,DAYASSAM                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL22 TM    DTETYPE,DTEHOLEP                                                 
         JZ    BLDCAL46                                                         
         TM    DTETYPE,DTEHOL                                                   
         JZ    BLDCAL26                                                         
         CLI   DAYTYPE,DAYASSAM                                                 
         JNE   BLDCAL24                                                         
         MVI   DAYTYPE,DAYAMAPM                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL24 MVI   DAYTYPE,DAYPMOUT                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL26 TM    DTETYPE,DTEWRK                                                   
         JZ    BLDCAL52                                                         
         CLI   DAYTYPE,DAYAMOUT                                                 
         JNE   BLDCAL28                                                         
         MVI   DAYTYPE,DAYAMOPM                                                 
         J     BLDCAL52                                                         
BLDCAL28 MVI   DAYTYPE,DAYASSPM                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL30 TM    DTETYPE,DTEHOLSA+DTEHOLEP                                        
         JO    BLDCAL46                                                         
         TM    DTETYPE,DTEHOLSA                                                 
         JZ    BLDCAL38                                                         
         TM    DTETYPE,DTEHOL                                                   
         JZ    BLDCAL34                                                         
         CLI   DAYTYPE,DAYASSPM                                                 
         JNE   BLDCAL32                                                         
         MVI   DAYTYPE,DAYAMOPM                                                 
         J     BLDCAL52                                                         
BLDCAL32 MVI   DAYTYPE,DAYAMOUT                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL34 TM    DTETYPE,DTEWRK                                                   
         JZ    BLDCAL52                                                         
         CLI   DAYTYPE,DAYPMOUT                                                 
         JNE   BLDCAL36                                                         
         MVI   DAYTYPE,DAYAMAPM                                                 
         J     BLDCAL52                                                         
BLDCAL36 MVI   DAYTYPE,DAYASSAM                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL38 TM    DTETYPE,DTEHOLSP                                                 
         JZ    BLDCAL46                                                         
         TM    DTETYPE,DTEHOL                                                   
         JZ    BLDCAL42                                                         
         CLI   DAYTYPE,DAYASSAM                                                 
         JNE   BLDCAL40                                                         
         MVI   DAYTYPE,DAYAMAPM                                                 
         J     BLDCAL52                                                         
BLDCAL40 MVI   DAYTYPE,DAYPMOUT                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL42 TM    DTETYPE,DTEWRK                                                   
         JZ    BLDCAL52                                                         
         CLI   DAYTYPE,DAYAMOUT                                                 
         JNE   BLDCAL44                                                         
         MVI   DAYTYPE,DAYAMOPM                                                 
         J     BLDCAL52                                                         
BLDCAL44 MVI   DAYTYPE,DAYASSPM                                                 
         J     BLDCAL52                                                         
                                                                                
BLDCAL46 TM    DTETYPE,DTEHOL                                                   
         JZ    BLDCAL50                                                         
BLDCAL48 MVI   DAYTYPE,DAYOUT                                                   
         J     BLDCAL52                                                         
                                                                                
BLDCAL50 TM    DTETYPE,DTEWRK                                                   
         JZ    BLDCAL52                                                         
         MVI   DAYTYPE,DAYASSN                                                  
*                                                                               
BLDCAL52 LA    R3,DTETABL(R3)                                                   
         OC    DTESTR,DTESTR                                                    
         JNZ   BLDCAL12                                                         
         LA    R2,DAYTABL(R2)                                                   
         L     R3,AGENAREA                                                      
         OC    DAYDATE,DAYDATE                                                  
         JNZ   BLDCAL10                                                         
         MVC   LP_ADATA,AIO6                                                    
         MVC   LP_RMODE,SVRMODE                                                 
         OI    OVINDS1,OVIDAYT                                                  
         J     EXITY                                                            
         DROP  BUF,R1                                                           
DTETABD  DSECT                                                                  
DTESTR   DS    PL3                                                              
DTEEND   DS    PL3                                                              
DTETYPE  DS    CL1                                                              
DTEHOLSA EQU   PKSKSAM                                                          
DTEHOLSP EQU   PKSKSPM                                                          
DTEHOLEA EQU   PKSKEAM                                                          
DTEHOLEP EQU   PKSKEPM                                                          
DTEHOL   EQU   X'08'                                                            
DTEWRK   EQU   X'04'                                                            
DTETABL  EQU   *-DTETABD                                                        
*                                                                               
DAYTABD  DSECT                                                                  
DAYDATE  DS    PL3                 DATE                                         
DAYTYPE  DS    CL1                 TYPE OF DAY                                  
DAYFREE  EQU   C'1'                FREE                                         
DAYASSN  EQU   C'2'                ASSIGNED TO TASK WHOLE DAY                   
DAYOUT   EQU   C'3'                OUT OF OFFICE WHOLE DAY                      
DAYAMOUT EQU   C'4'                OUT OF OFFICE HALF DAY AM                    
DAYPMOUT EQU   C'5'                OUT OF OFFICE HALF DAY PM                    
DAYASSAM EQU   C'6'                ASSIGNED TO TASK HALF DAY AM                 
DAYASSPM EQU   C'7'                ASSIGNED TO TASK HALF DAY PM                 
DAYAMOPM EQU   C'8'                OUT OF OFFICE AM ASSIGNED PM                 
DAYAMAPM EQU   C'9'                ASSIGNED AM OUT OF OFFICE PM                 
DAYTABL  EQU   *-DAYTABD           LENGTH OF TABLE                              
DAYMAXQ  EQU   31                                                               
         EJECT                                                                  
SVRDEF   CSECT ,                                                                
**********************************************************************          
* MY TASK LIST/TASK SEARCH                                           *          
**********************************************************************          
                                                                                
REQMYTS  LKREQ H,A#RMYDL,OUTMYTS,NEXTREQ=REQDETL                                
client   LKREQ F,1,(D,B#SAVED,QCLIENT),CHAR,OLEN=L'QCLIENT,            +        
               MAXLEN=L'QCLIENT,TEXT=AC#CLI,COL=*                               
camp     LKREQ F,2,(D,B#SAVED,QCAMP),LBIN,OLEN=L'QCAMP,                +        
               TEXT=(*,CAMCLIT),COL=*                                           
prod     LKREQ F,3,(D,B#SAVED,QPROD),CHAR,OLEN=L'QPROD,                +        
               MAXLEN=L'QPROD,TEXT=AC#PROC,COL=*                                
job      LKREQ F,4,(D,B#SAVED,QJOBC),CHAR,OLEN=L'QJOBC,                +        
               MAXLEN=L'QJOBC,TEXT=AC#JOBC,COL=*                                
current  LKREQ F,5,(D,B#SAVED,QCURR),CHAR,OLEN=L'QCURR,                +        
               MAXLEN=L'QCURR,TEXT=(*,STCTLIT),COL=*                            
finish   LKREQ F,6,(D,B#SAVED,QFINS),CHAR,OLEN=L'QFINS,                +        
               MAXLEN=L'QFINS,TEXT=(*,STFDLIT),COL=*                            
obsol    LKREQ F,7,(D,B#SAVED,QOBSO),CHAR,OLEN=L'QOBSO,                +        
               MAXLEN=L'QOBSO,TEXT=(*,STOBLIT),COL=*                            
kstg#    LKREQ F,8,(D,B#SAVED,QKYST),LBIN,OLEN=L'QKYST,                +        
               MAXLEN=L'QKYST,TEXT=(*,KSTNLIT),COL=*                            
frmdate  LKREQ F,9,(D,B#SAVED,QADDFROM),PDAT,OLEN=L'QADDFROM,          +        
               TEXT=(*,ADDFLIT),COL=*                                           
todate   LKREQ F,10,(D,B#SAVED,QADDTO),PDAT,OLEN=L'QADDTO,             +        
               TEXT=(*,ADDTLIT),COL=*                                           
kstFDte  LKREQ F,11,(D,B#SAVED,QKSTFROM),PDAT,OLEN=L'QKSTFROM,         +        
               TEXT=(*,KSDFLIT),COL=*                                           
kstTDte  LKREQ F,12,(D,B#SAVED,QKSTTO),PDAT,OLEN=L'QKSTTO,             +        
               TEXT=(*,KSDTLIT),COL=*                                           
Init     LKREQ F,13,(D,B#SAVED,QINIT),CHAR,OLEN=L'QINIT,               +        
               MAXLEN=L'QINIT,TEXT=(*,INITLIT),COL=*                            
         LKREQ E                                                                
                                                                                
OUTMYTS  LKOUT H                                                                
                                                                                
TASKS    LKOUT R,R#RMYDL           RESOURCE MY TASK REPORT                      
Array    LKOUT C,1,(A,ARYRMY)                                                   
         LKOUT E                                                                
                                                                                
KEYSTGM  LKOUT R,R#KSTGDL          KEYSTAGE                                     
Array    LKOUT C,R#KSTGDL,(A,ARYKST),FILTROUT=TSTQINIT                          
         LKOUT E                                                                
                                                                                
TEMPLTM  LKOUT R,R#TEMPDL          TEMPLATE                                     
Array    LKOUT C,R#TEMPDL,(A,ARYTMP),FILTROUT=TSTQINIT                          
         LKOUT E                                                                
                                                                                
TEAMM    LKOUT R,R#TEAMDL          TEAM                                         
Array    LKOUT C,R#TEAMDL,(A,ARYTEAM),FILTROUT=TSTQINIT                         
         LKOUT E                                                                
                                                                                
ROLEM    LKOUT R,R#ROLEDL          ROLE                                         
Array    LKOUT C,R#ROLEDL,(A,ARYROL),FILTROUT=TSTQINIT                          
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYRMY   LKOUT A,(R,NXTMYT),MULTIROW=Y,ROWNAME=RWKRECD                          
                                                                                
CliCde   LKOUT C,1,(D,B#SAVED,OCLICDE),CHAR,LEN=L'OCLICDE,ND=Y                  
PrdCde   LKOUT C,2,(D,B#SAVED,OPROCDE),CHAR,LEN=L'OPROCDE,ND=Y                  
JobCde   LKOUT C,3,(D,B#SAVED,OJOBCDE),CHAR,LEN=L'OJOBCDE,ND=Y                  
CampCde  LKOUT C,4,(D,B#SAVED,DCAMP),LBIN                                       
CliNam   LKOUT C,5,(D,B#SAVED,OCLINAM),CHAR,LEN=L'OCLINAM,ND=Y                  
PrdNam   LKOUT C,6,(D,B#SAVED,OPRONAM),CHAR,LEN=L'OPRONAM,ND=Y                  
JobNam   LKOUT C,7,(D,B#SAVED,OJOBNAM),CHAR,LEN=L'OJOBNAM,ND=Y                  
array    LKOUT C,38,(A,ARYCAM)                                                  
KstCde   LKOUT C,10,(D,B#SAVED,OKSTCDE),LBIN                                    
KstNam   LKOUT C,11,(D,B#SAVED,OKSTNAM),CHAR,LEN=L'OKSTNAM                      
KstStr   LKOUT C,12,(D,B#SAVED,OPKSSTR),PDAT                                    
KstEnd   LKOUT C,13,(D,B#SAVED,OPKSEND),PDAT                                    
Status   LKOUT C,14,(D,B#SAVED,OSTATUS),CHAR,LEN=L'OSTATUS                      
array    LKOUT C,39,(A,ARYNXT)                                                  
Array    LKOUT C,40,(A,ARYIND)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET PERSONAL KEYSTAGE RECORDS FOR RESOURCES                         *         
***********************************************************************         
                                                                                
NXTMYT   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTMYT50                                                         
                                                                                
         MVC   QPIDB,CCTPID                                                     
NXTMYT06 XC    QENDSTR,QENDSTR     SET RANGE READY FOR KEY DRIVER               
         MVI   QSTREND,X'FF'                                                    
         MVC   QSTREND+1(L'QSTREND-1),QSTREND                                   
         XR    RE,RE                                                            
         ICM   RE,7,QKSTTO                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,QSTRSTR                                                     
         XR    RE,RE                                                            
         ICM   RE,7,QKSTFROM                                                    
         LNR   RE,RE                                                            
         STCM  RE,7,QENDEND                                                     
                                                                                
NXTMYT40 OC    QACAMP,QACAMP       TEST CAMPAIGN LIST PROVIDED                  
         JNZ   NXTMYT50                                                         
         MVC   QACAMP,AALL         NO - SET DEFAULT                             
                                                                                
NXTMYT50 MVC   IOKEY,SVPKSKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,PKSKEYT,('B#RESWK',0),           +        
               ('$NXTRXGR',SAVED),AFLTPKS,0                                     
         JNE   EXITY                                                            
                                                                                
         NI    OVINDS1,X'FF'-OVIDAYT                                            
         MVC   SVPKSKEY,IOKEY                                                   
         LA    R2,SVPKSKEY                                                      
         USING PKSRECD,R2          R2=A(PERSONAL KEY STAGE KEY)                 
         MVC   QPIDB,PKSKPIDB                                                   
         MVC   DCAMP,PKSKCODE                                                   
         GOTOR ASETSTAT,DMCB,PKSKSTAT  EXTRACT STATUS                           
         GOTOR AEXTCPJ,DMCB,PKSKACT  EXTRACT CLIENT PRODUCT JOB                 
                                                                                
         XR    RE,RE                                                            
         ICM   RE,7,PKSKEDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,OPKSEND                                                     
         XR    RE,RE                                                            
         ICM   RE,7,PKSKSDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,OPKSSTR                                                     
         GOTOR AEXTKST,DMCB,PKSKKYST                                            
                                                                                
         MVC   IODA,PKSKDA                                                      
         MVC   IOKEY,SVPKSKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    NXTMYT80                                                         
         DC    H'0'                                                             
                                                                                
NXTMYT80 MVC   LP_ADATA,AIO4                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
TSTQINIT CLI   QINIT,C'Y'                                                       
         BR    RE                                                               
**********************************************************************          
* DETAIL RESOURCE DOWNLOAD                                           *          
**********************************************************************          
                                                                                
REQDETL  LKREQ H,A#RDEDL,OUTDETL,NEXTREQ=REQAUDT                                
client   LKREQ F,1,(D,B#SAVED,QCLIENT),CHAR,OLEN=L'QCLIENT,            +        
               MAXLEN=L'QCLIENT,TEXT=AC#CLI,COL=*                               
camp     LKREQ F,2,(D,B#SAVED,QCAMP),LBIN,OLEN=L'QCAMP,                +        
               TEXT=(*,CAMCLIT),COL=*                                           
prod     LKREQ F,3,(D,B#SAVED,QPROD),CHAR,OLEN=L'QPROD,                +        
               MAXLEN=L'QPROD,TEXT=AC#PROC,COL=*                                
job      LKREQ F,4,(D,B#SAVED,QJOBC),CHAR,OLEN=L'QJOBC,                +        
               MAXLEN=L'QJOBC,TEXT=AC#JOBC,COL=*                                
         LKREQ E                                                                
                                                                                
OUTDETL  LKOUT H                                                                
                                                                                
DETAILS  LKOUT R,R#RDEDL           RESOURCE DETAIL DOWNLOAD                     
Array    LKOUT C,1,(A,ARYDETL)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYDETL  LKOUT A,(R,NXTSRW),MULTIROW=Y,ROWNAME=RWKRECD                          
                                                                                
Array    LKOUT C,9,(A,ARYKSTG)                                                  
                                                                                
Array    LKOUT C,12,(A,ARYPDL)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYKSTG  LKOUT A,(D,B#RESWK,RWKRFST),EOT=EOR,                          +        
               ROWID=(KSTEL,KSTELQ),ROWWIDTH=(V,KSTLN)                          
                                                                                
KstNum   LKOUT C,1,KSTCODE,LBIN                                                 
KstSDte  LKOUT C,2,KSTSTDT,PDAT                                                 
KstSAM   LKOUT C,3,KSTSTAT,(R,EDTSAPM)                                          
KstEDte  LKOUT C,4,KSTENDT,PDAT                                                 
KstEAM   LKOUT C,5,KSTSTAT,(R,EDTEAPM)                                          
PRout    LKOUT P,KSTELD,EXTPIDS                                                 
Array    LKOUT C,4,(A,ARYPER)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYPER   LKOUT A,(R,NXTPID),MULTIROW=Y,ROWNAME=WORKD                            
                                                                                
PidChar  LKOUT C,4,(D,B#SAVED,PIDCDE),CHAR,LEN=L'SAPALPID,ND=Y                  
                                                                                
         LKOUT E                                                                
                                                                                
NXTSRW   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSRW04                                                         
                                                                                
         L     R0,AGENAREA         clear buffer                                 
         LH    R1,=Y(GENAREAX-GENAREA)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SR    R6,R6                                                            
         MVC   DCPJ,SPACES                                                      
         IC    R6,PCLILEN                                                       
         SHI   R6,1                                                             
         BASR  RE,0                                                             
         EX    R6,4(RE)                                                         
         MVC   DCPJ(0),QCLIENT                                                  
         CLC   QPROD,SPACES                                                     
         JNH   NXTSRW04                                                         
         AHI   R6,1                                                             
         LA    R4,DCPJ                                                          
         AR    R4,R6                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,R6                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,4(RE)                                                         
         MVC   0(0,R4),QPROD                                                    
         CLC   QJOBC,SPACES                                                     
         JNH   NXTSRW04                                                         
         AHI   RF,1                                                             
         AR    R4,RF                                                            
         SR    R6,R6                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         IC    R6,PJOBLEN                                                       
         SR    R6,RF                                                            
         CHI   R6,L'DJOB                                                        
         JNH   NXTSRW02                                                         
         LA    R6,L'DJOB                                                        
NXTSRW02 SHI   R6,1                                                             
         BASR  RE,0                                                             
         EX    R6,4(RE)                                                         
         MVC   0(0,R4),QJOBC                                                    
NXTSRW04 GOTOR (#NXTREC,ANXTREC),DMCB,SRWKEYT,('B#RESWK',0),           +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* GET PIDS FROM KEY STAGE ELEMENT                                     *         
***********************************************************************         
                                                                                
EXTPIDS  NTR1  ,                                                                
         XC    DAPIDB,DAPIDB                                                    
         XC    DPIDIND,DPIDIND                                                  
         OI    OVINDS1,OVINONM                                                  
         L     R2,LP_AINP                                                       
         USING KSTELD,R2                                                        
         LA    R3,KSTPID                                                        
         XR    RE,RE                                                            
         IC    RE,KSTLN                                                         
         SHI   RE,KSTLN2Q                                                       
         LTR   RE,RE                                                            
         JZ    EXITY                                                            
         SRL   RE,2                DIVIDE BY LENGTH OF PID BINARY CODE          
         STH   RE,DPIDROW                                                       
EXPID02  XR    R1,R1                                                            
         IC    R1,KSTLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   EXITY                                                            
         GOTOR LP_AAWMP,DMCB,(L'SA0KNUM,0(R3)),DPIDIND,DPIDMAXQ,       +        
               LP_D                                                             
         LA    R3,L'SA0KNUM(R3)                                                 
         J     EXPID02                                                          
                                                                                
SVRDEF   CSECT ,                                                                
                                                                                
                                                                                
ARYPDL   LKOUT A,(D,B#RESWK,RWKRFST),EOT=EOR,                          +        
               ROWID=(LIDEL,LIDELQ),ROWWIDTH=(V,LIDLN)                          
                                                                                
PRout    LKOUT P,LIDELD,EXTLIDS                                                 
Array    LKOUT C,12,(A,ARYPRHR)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYPRHR  LKOUT A,(R,NXTPHR),MULTIROW=Y,ROWNAME=WORKD                            
                                                                                
PidChar  LKOUT C,12,(D,B#SAVED,PIDCDE),CHAR,LEN=L'SAPALPID,ND=Y                 
PidBin   LKOUT C,13,(D,B#WORKD,TEMP2+50),HEXD,LEN=L'SA0KNUM,ND=Y                
FirstNam LKOUT C,14,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
MidNam   LKOUT C,15,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LastNam  LKOUT C,16,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                       
TotHrs   LKOUT C,17,(D,B#SAVED,DHRS),SPAK,ND=Y                                  
                                                                                
Array    LKOUT C,18,(A,ARYFTEA)                                                 
Array    LKOUT C,18,(A,ARYTEAN)                                                 
Array    LKOUT C,20,(A,ARYFROL)                                                 
Array    LKOUT C,20,(A,ARYROLN)                                                 
         LKOUT E                                                                
***********************************************************************         
* GET PIDS FROM LIST ELEMENT                                          *         
***********************************************************************         
                                                                                
EXTLIDS  NTR1  ,                                                                
         XC    DAPHR,DAPHR                                                      
         XC    DPHRIND,DPHRIND                                                  
         OC    DALPWMP,DALPWMP                                                  
         JNZ   EXLID02                                                          
         MVC   DALPWMP,LP_AWMP                                                  
EXLID02  MVC   LP_AWMP,DALPWMP                                                  
         L     R2,LP_AINP                                                       
         USING LIDELD,R2                                                        
         LA    R3,LIDDATA                                                       
EXLID04  XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   EXITY                                                            
         GOTOR LP_AAWMP,DMCB,(L'LIDRPID+L'LIDRHRS,0(R3)),DPHRIND,      +        
               DPHRMAXQ,LP_D                                                    
         LA    R3,L'LIDRPID+L'LIDRHRS(R3)                                       
         J     EXLID04                                                          
                                                                                
***********************************************************************         
* GET PID N HRS  RECORDS                                              *         
***********************************************************************         
                                                                                
NXTPHR   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTPHR02                                                         
                                                                                
         OC    DAPHR,DAPHR         ANY PID LIST                                 
         JZ    NOMORE              EXIT                                         
         MVC   SVROLKEY,IOKEY                                                   
                                                                                
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAPHR                                                       
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        NUMBER OF ENTRIES                            
         LA    R4,LW_DATA2         START OF LIST                                
         ST    R4,APIDLST                                                       
         J     NXTPHR10                                                         
         DROP  R2                                                               
                                                                                
NXTPHR02 L     R4,APIDLST                                                       
         XC    QPIDB,QPIDB                                                      
         LA    R4,L'SA0KNUM(R4)    GO TO NEXT ENTRY                             
         LA    R4,L'LIDRHRS(R4)    YES - BUMP FURTHER ALONG                     
NXTPHR04 OC    0(L'SA0KNUM,R4),0(R4)                                            
         JZ    NOMORE              IF EMPTY WE ARE FINISHED                     
NXTPHR10 MVC   TEMP2(L'SA0KNUM),0(R4)                                           
         MVC   QPIDB(L'SA0KNUM),0(R4)                                           
         ZAP   DHRS,PZERO                                                       
         GOTOR (#GETPID,AGETPID)                                                
         MVC   PIDCDE,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)                                                
         ST    R4,APIDLST                                                       
         OC    L'LIDRPID(L'LIDRHRS,R4),L'LIDRPID(R4)                            
         JZ    NXTPHR12                                                         
         ZAP   DHRS,L'LIDRPID(L'LIDRHRS,R4)                                     
NXTPHR12 MVC   LP_ADATA,AIO1                                                    
         MVC   IOKEY,SVROLKEY                                                   
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
**********************************************************************          
* RESOURCE AUDIT DOWNLOAD                                            *          
**********************************************************************          
                                                                                
REQAUDT  LKREQ H,A#RAUDL,OUTAUDT,NEXTREQ=REQOUTO                                
client   LKREQ F,1,(D,B#SAVED,DCLIENT),CHAR,OLEN=L'QCLIENT,            +        
               MAXLEN=L'QCLIENT,TEXT=AC#CLI,COL=*                               
camp     LKREQ F,2,(D,B#SAVED,DCAMP),LBIN,OLEN=L'QCAMP,                +        
               TEXT=(*,CAMCLIT),COL=*                                           
prod     LKREQ F,3,(D,B#SAVED,DPROD),CHAR,OLEN=L'QPROD,                +        
               MAXLEN=L'QPROD,TEXT=AC#PROC,COL=*                                
job      LKREQ F,4,(D,B#SAVED,DJOB),CHAR,OLEN=L'DJOB,                  +        
               MAXLEN=L'QJOBC,TEXT=AC#JOBC,COL=*                                
         LKREQ E                                                                
                                                                                
OUTAUDT  LKOUT H                                                                
                                                                                
AUDIT    LKOUT R,R#RAUDL           RESOURCE AUDIT DOWNLOAD                      
Array    LKOUT C,1,(A,ARYAUD)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYAUD   LKOUT A,(R,NXTAUD),MULTIROW=Y,ROWNAME=AUDRECD                          
                                                                                
Array    LKOUT C,83,(A,ARYSTC)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYSTC   LKOUT A,(D,B#AUDIT,AUDRFST),EOT=EOR,NEWEL=B,                  +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
                                                                                
PRout    LKOUT P,STCELD,GETAUD                                                  
Date     LKOUT C,1,STCRDTE,PDAT                                                 
Time     LKOUT C,2,(D,B#SAVED,AUDTIME),CHAR,LEN=L'AUDTIME                       
PID      LKOUT C,3,(D,B#SAVED,PIDCDE),CHAR,LEN=L'PIDCDE                         
PrsFn    LKOUT C,4,(D,B#SAVED,AUDPRFN),CHAR,LEN=L'AUDPRFN                       
PrsMn    LKOUT C,5,(D,B#SAVED,AUDPRMN),CHAR,LEN=L'AUDPRMN                       
PrsLn    LKOUT C,6,(D,B#SAVED,AUDPRLN),CHAR,LEN=L'AUDPRLN                       
User     LKOUT C,7,(D,B#SAVED,AUDUSER),CHAR,LEN=L'AUDUSER                       
Act      LKOUT C,8,(D,B#SAVED,AUDACTN),CHAR,LEN=L'AUDACTN                       
StrDte   LKOUT C,9,STCRSTDT,PDAT,FILTROUT=TSTSDTE                               
EndDte   LKOUT C,10,STCRENDT,PDAT,FILTROUT=TSTEDTE                              
Cmmts    LKOUT C,11,STCRCMTS,CHAR,LEN=V,FILTROUT=TSTCMNT                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET AUDIT RECORDS                                                   *         
***********************************************************************         
                                                                                
NXTAUD   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTAUD04                                                         
                                                                                
         SR    R6,R6                                                            
         MVC   DCPJ,SPACES                                                      
         IC    R6,PCLILEN                                                       
         SHI   R6,1                                                             
         BASR  RE,0                                                             
         EX    R6,4(RE)                                                         
         MVC   DCPJ(0),DCLIENT                                                  
         CLC   DPROD,SPACES                                                     
         JNH   NXTAUD04                                                         
         AHI   R6,1                                                             
         LA    R4,DCPJ                                                          
         AR    R4,R6                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,R6                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,4(RE)                                                         
         MVC   0(0,R4),DPROD                                                    
         CLC   DJOB,SPACES                                                      
         JNH   NXTAUD04                                                         
         AHI   RF,1                                                             
         AR    R4,RF                                                            
         SR    R6,R6                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         IC    R6,PJOBLEN                                                       
         SR    R6,RF                                                            
         CHI   R6,L'DJOB                                                        
         JNH   NXTAUD02                                                         
         LA    R6,L'DJOB                                                        
NXTAUD02 SHI   R6,1                                                             
         BASR  RE,0                                                             
         EX    R6,4(RE)                                                         
         MVC   0(0,R4),DJOB                                                     
NXTAUD04 MVC   IOKEY,SVAUDKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,AUDKEYT,('B#AUDIT',0),           +        
               (0,SAVED),0,0                                                    
         MVC   SVAUDKEY,IOKEY                                                   
         GOTOR AEXTCPJ,DMCB,IOKEY+(AUDKACT-AUDRECD)                             
         JNE   NXTAUD04                                                         
         J     EXIT                                                             
***********************************************************************         
* PROCESS AUDIT ELEMENTS                                              *         
***********************************************************************         
GETAUD   NTR1  BASE=*                                                           
         L     R3,LP_AINP                                                       
         USING STCELD,R3                                                        
         UNPK  DUB2,STCRTIM                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   AUDTIME,DUB2+2                                                   
         MVC   TEMP2(L'STCRPID),STCRPID                                         
         GOTOR (#GETPID,AGETPID)    USES IO7                                    
         JE    GAUD02                                                           
         MVI   PIDCDE,C'<'        pass <user> if no name found                  
         MVI   PIDCDE+7,C'>'                                                    
         XOUT  STCRPID,PIDCDE+1,2                                               
         J     GAUD10                                                           
GAUD02   MVC   PIDCDE,TEMP2                                                     
GAUD10   GOTOR (#GETPIN,AGETPIN)                                                
         JNE   GAUD20                                                           
         MVC   AUDPRFN,TEMP2                                                    
         MVC   AUDPRLN,TEMP2+16                                                 
         MVC   AUDPRMN,TEMP2+32                                                 
GAUD20   MVC   TEMP2(2),STCRUSR                                                 
         GOTOR (#GETUSR,AGETUSR)                                                
         JE    GAUD30                                                           
         MVI   AUDUSER,C'<'        pass <user> if no name found                 
         MVI   AUDUSER+5,C'>'                                                   
         XOUT  STCTUSR,AUDUSER+1,2                                              
         J     GAUD40                                                           
*                                                                               
GAUD30   MVC   AUDUSER,TEMP2                                                    
*                                                                               
GAUD40   MVC   AUDACTN,SPACES                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,STCRTYP                                                       
         SHI   RF,1                                                             
         LA    R2,L'AC@KDTAD                                                    
         MR    RE,R2                                                            
         LA    R2,AC@RWKAD                                                      
         AR    R2,RF                                                            
         MVC   AUDACTN(L'AC@RWKAD),0(R2)                                        
         CLI   STCRTYP,STCRDTST    dates set                                    
         BE    GAUD42                                                           
         CLI   STCRTYP,STCRDTAM    dates amended                                
         BE    GAUD42                                                           
         CLI   STCRTYP,STCRKSAD    keystage added                               
         BE    GAUD42                                                           
         CLI   STCRTYP,STCRSTAT    status amended                               
         BE    GAUD42                                                           
         CLI   STCRTYP,STCRKSDL    keystage deleted                             
         BNE   GETAUDX                                                          
GAUD42   LA    R2,AUDACTN+L'AUDACTN-1                                           
         LA    RF,L'AUDACTN                                                     
GAUD44   CLI   0(R2),C' '                                                       
         BH    GAUD46                                                           
         SHI   R2,1                                                             
         BCT   RF,GAUD44                                                        
         DC    H'0'                                                             
*                                                                               
GAUD46   LA    R2,2(R2)                                                         
                                                                                
         CLI   STCRTYP,STCRSTAT    status amended                               
         BNE   GAUD50                                                           
         OC    STCRSTA,STCRSTA                                                  
         BNZ   *+8                                                              
         LA    R4,AC@CURNT                                                      
         CLI   STCRSTA,RWKSENDD                                                 
         BNE   *+8                                                              
         LA    R4,AC@FINSD                                                      
         CLI   STCRSTA,RWKSDEAD                                                 
         BNE   GAUD48                                                           
         LA    R4,AC@OBSO                                                       
GAUD48   MVC   0(L'AC@CURNT,R2),0(R4)                                           
         B     GETAUDX                                                          
                                                                                
GAUD50   LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         USING KSTRECD,R4                                                       
         MVI   KSTKTYP,KSTKTYPQ                                                 
         MVI   KSTKSUB,KSTKSUBQ                                                 
         MVC   KSTKCPY,AGENCY                                                   
         MVC   KSTKOFF,SPACES                                                   
         TM    SCPYEL+(CPYSTATC-CPYELD),CPYSROFF OFFICES FOR RESOURCES          
         JZ    GAUD51                                                           
         MVC   KSTKOFF,USROFF                                                   
GAUD51   MVC   KSTKNUM,STCRKSTG                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2                                                          
         LA    R4,KSTRFST                                                       
         USING KSTELD,R4                                                        
         XR    R0,R0                                                            
GAUD52   CLI   KSTEL,0                                                          
         BE    GETAUDX                                                          
         CLI   KSTEL,KSTELQ                                                     
         BE    GAUD54                                                           
         IC    R0,KSTLN                                                         
         AR    R4,R0                                                            
         B     GAUD52                                                           
*                                                                               
GAUD54   XR    RF,RF                                                            
         IC    RF,KSTLN                                                         
         SHI   RF,KSTLN1Q+1                                                     
         BM    GETAUDX                                                          
         MVC   0(0,R2),KSTNAME                                                  
         EX    RF,*-6                                                           
                                                                                
         DROP  R3,R4                                                            
*                                                                               
GETAUDX  J     EXITY                                                            
                                                                                
TSTSDTE  L     R1,LP_AINP                                                       
         USING STCELD,R1                                                        
         CLI   STCLN,STCLN6Q                                                    
         BNER  RE                                                               
         CLI   STCRTYP,STCRDTST                                                 
         JE    TSDTE02                                                          
         CLI   STCRTYP,STCRKSAD                                                 
         JE    TSDTE02                                                          
         CLI   STCRTYP,STCRDTAM                                                 
         BNER  RE                                                               
TSDTE02  OC    STCRSTDT,STCRSTDT                                                
         J     SETCCC                                                           
                                                                                
TSTEDTE  L     R1,LP_AINP                                                       
         USING STCELD,R1                                                        
         CLI   STCLN,STCLN6Q                                                    
         BNER  RE                                                               
         CLI   STCRTYP,STCRDTST                                                 
         JE    TEDTE02                                                          
         CLI   STCRTYP,STCRKSAD                                                 
         JE    TEDTE02                                                          
         CLI   STCRTYP,STCRDTAM                                                 
         BNER  RE                                                               
TEDTE02  OC    STCRENDT,STCRENDT                                                
         J     SETCCC                                                           
                                                                                
TSTCMNT  L     R1,LP_AINP                                                       
         USING STCELD,R1                                                        
         CLI   STCRTYP,STCRCMMT                                                 
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
TESTKYST OC    OKSTCDE,OKSTCDE                                                  
         J     SETCCC                                                           
                                                                                
TESTCLIC OC    OCLICDE,OCLICDE                                                  
         J     SETCCC                                                           
                                                                                
TESTKROL OC    AKEYROL,AKEYROL                                                  
         J     SETCCC                                                           
**********************************************************************          
* OUT OF OFFICE DOWNLOAD                                             *          
**********************************************************************          
                                                                                
REQOUTO  LKREQ H,A#OUTDL,OUTOUTO,NEXTREQ=REQUESTX                               
PidB     LKREQ F,1,(D,B#SAVED,QPIDB),HEXD,OLEN=L'QPIDB,                +        
               MAXLEN=L'QCLIENT,TEXT=(*,PIDBLIT),COL=*                          
frmdate  LKREQ F,2,(D,B#SAVED,QADDFROM),PDAT,OLEN=L'QADDFROM,          +        
               TEXT=(*,ADDFLIT),COL=*                                           
todate   LKREQ F,3,(D,B#SAVED,QADDTO),PDAT,OLEN=L'QADDTO,              +        
               TEXT=(*,ADDTLIT),COL=*                                           
viewall  LKREQ F,4,(D,B#SAVED,QVIEWAL),CHAR,OLEN=L'QVIEWAL,            +        
               MAXLEN=L'QVIEWAL,TEXT=(*,VIEWLIT),COL=*                          
         LKREQ E                                                                
                                                                                
OUTOUTO  LKOUT H                                                                
                                                                                
OUTOFF   LKOUT R,R#OUTDL           RESOURCE OUT OF OFFICE DOWNLOAD              
Array    LKOUT C,1,(A,ARYOUT)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYOUT   LKOUT A,(R,NXTOUT),MULTIROW=Y,ROWNAME=PKSRECD                          
                                                                                
PidChar  LKOUT C,1,(D,B#SAVED,PIDCDE),CHAR,LEN=L'SAPALPID,ND=Y                  
PidBin   LKOUT C,2,(D,B#WORKD,TEMP2+50),HEXD,LEN=L'SA0KNUM,ND=Y                 
FirstNam LKOUT C,3,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                           
MidNam   LKOUT C,4,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                        
LastNam  LKOUT C,5,(D,B#WORKD,TEMP2+16),CHAR,LEN=16,ND=Y                        
StrDte   LKOUT C,6,(D,B#SAVED,OPKSSTR),PDAT                                     
SAmPm    LKOUT C,7,(D,B#SAVED,OPKSST),CHAR,LEN=L'OPKSST                         
EndDte   LKOUT C,8,(D,B#SAVED,OPKSEND),PDAT                                     
EAmPm    LKOUT C,9,(D,B#SAVED,OPKSEN),CHAR,LEN=L'OPKSEN                         
Type     LKOUT C,10,PKSKOUT,CHAR                                                
Array    LKOUT C,11,(A,ARYFFT)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYFFT   LKOUT A,(D,B#PTASK,PKSRFST),EOT=EOR,                          +        
               ROWID=(FFTEL,FFTELQ),ROWWIDTH=(V,FFTLN)                          
                                                                                
Next     LKOUT C,11,FFTDATA,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET OUT OF OFFICE RECORDS                                           *         
***********************************************************************         
                                                                                
NXTOUT   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTOUT40                                                         
                                                                                
         XC    DAPIDB,DAPIDB       CLEAR PID LIST                               
         OC    QPIDB,QPIDB                                                      
         JZ    NXTOUT02                                                         
         GOTOR LP_AAWMP,DMCB,(L'PIDKNUM,QPIDB),DPIDIND,DPIDMX2Q,       +        
               LP_D                                                             
         J     NXTOUT10                                                         
NXTOUT02 CLI   QVIEWAL,C'Y'                                                     
         JE    NXTOUT04                                                         
         GOTOR LP_AAWMP,DMCB,(L'PIDKNUM,CCTPID),DPIDIND,DPIDMX2Q,      +        
               LP_D                                                             
         J     NXTOUT10                                                         
                                                                                
NXTOUT04 GOTOR (#NXTREC,ANXTREC),DMCB,PPRKEYT,('B#PID',0),             +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   NXTOUT10                                                         
                                                                                
         LA    R2,IOKEY                                                         
         USING PIDRECD,R2                                                       
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'SA0KNUM,PIDKPID),DPIDIND,DPIDMX2Q,     +        
               LP_D                                                             
         J     NXTOUT04                                                         
                                                                                
NXTOUT10 MVI   LP_RMODE,LP_RFRST                                                
         XC    QENDSTR,QENDSTR     SET RANGE READY FOR KEY DRIVER               
         MVI   QSTREND,X'FF'                                                    
         MVC   QSTREND+1(L'QSTREND-1),QSTREND                                   
         XR    RE,RE                                                            
         ICM   RE,7,QADDTO                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,QSTRSTR                                                     
         XR    RE,RE                                                            
         ICM   RE,7,QADDFROM                                                    
         LNR   RE,RE                                                            
         STCM  RE,7,QENDEND                                                     
                                                                                
NXTOUT20 OC    DAPIDB,DAPIDB                                                    
         JZ    NOMORE                                                           
                                                                                
NXTOUT40 MVC   IOKEY,SVRWKKEY                                                   
         GOTOR (#NXTREC,ANXTREC),DMCB,OUTKEYT,('B#PTASK',0),           +        
               (0,SAVED),0,0                                                    
         JNE   EXITY                                                            
         MVC   SVRWKKEY,IOKEY                                                   
                                                                                
         L     R2,IOADDR                                                        
         USING PKSRECD,R2          R2=A(PERSONAL KEY STAGE KEY)                 
                                                                                
         MVC   TEMP2(L'SA0KNUM),PKSKPIDB                                        
         GOTOR (#GETPID,AGETPID)   GET CHARACTER PID                            
         MVC   PIDCDE,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)   GET NAMES FOR PID                            
                                                                                
         XR    RE,RE                                                            
         ICM   RE,7,PKSKEDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,OPKSEND                                                     
         XR    RE,RE                                                            
         ICM   RE,7,PKSKSDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,OPKSSTR                                                     
         TM    PKSKAMPM,PKSKSPM    ARE THEY TAKING HOLIDAY AM OR PM             
         JZ    NXTOUT42                                                         
         MVI   OPKSST,C'P'         PM                                           
                                                                                
NXTOUT42 TM    PKSKAMPM,PKSKSAM    ARE THEY TAKING HOLIDAY AM OR PM             
         JZ    NXTOUT52                                                         
         MVI   OPKSST,C'A'         AM                                           
                                                                                
NXTOUT52 TM    PKSKAMPM,PKSKEPM    ARE THEY TAKING HOLIDAY AM OR PM             
         JZ    NXTOUT54                                                         
         MVI   OPKSEN,C'P'                                                      
                                                                                
NXTOUT54 TM    PKSKAMPM,PKSKEAM    ARE THEY TAKING HOLIDAY AM OR PM             
         JZ    NXTOUT60                                                         
         MVI   OPKSEN,C'A'                                                      
                                                                                
NXTOUT60 J     EXITY                                                            
         DROP  R2                                                               
***********************************************************************         
* End of requests                                                     *         
***********************************************************************         
                                                                                
REQUESTX LKREQ X                                                                
                                                                                
         LKARY T                                                                
         EJECT                                                                  
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
***********************************************************************         
* Create WMP area of PIDs from the request                            *         
***********************************************************************         
         SPACE 1                                                                
PIDLST   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*PIDLST*'                                                      
                                                                                
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAPIDB2                                                     
         JZ    PIDLST02                                                         
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         CLEAR ENTRIES                                
         LHI   R1,DPIDMAXQ*L'LIDRPID                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               CLEAR AREA                                   
         DROP  R2                                                               
                                                                                
PIDLST02 OC    QATEAM,QATEAM       TEST TEAM LIST PROVIDED                      
         JZ    PIDLST14                                                         
PIDLST04 GOTOR (#NXTREC,ANXTREC),DMCB,TEAKEYT,('B#TEAM',0),            +        
               (0,SAVED),0,0                                                    
         JNE   PIDLST14                                                         
                                                                                
         L     R2,IOADDR                                                        
         USING TEARECD,R2          R2=A(TEMPLATE RECORD)                        
         LA    R2,TEARFST                                                       
         USING LIDELD,R2                                                        
PIDLST06 CLI   LIDEL,0                                                          
         JE    PIDLST04                                                         
         CLI   LIDEL,LIDELQ                                                     
         JE    PIDLST10                                                         
PIDLST08 XR    R0,R0                                                            
         IC    R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     PIDLST06                                                         
                                                                                
PIDLST10 CLI   LIDTYPE,LIDTPID                                                  
         JNE   PIDLST08                                                         
         LA    R3,LIDDATA                                                       
PIDLST12 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   PIDLST08                                                         
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'SA0KNUM,0(R3)),DPIDIND2,DPIDMX2Q,      +        
               LP_D                                                             
         LA    R3,L'SA0KNUM(R3)                                                 
         J     PIDLST12                                                         
                                                                                
PIDLST14 MVI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         OC    QAROLE,QAROLE       TEST ROLE LIST PROVIDED                      
         JZ    PIDLST26                                                         
PIDLST16 GOTOR (#NXTREC,ANXTREC),DMCB,ROLKEYT,('B#ROLE',0),            +        
               (0,SAVED),0,0                                                    
         JNE   PIDLST26                                                         
                                                                                
         L     R2,IOADDR                                                        
         USING TEARECD,R2          R2=A(TEMPLATE RECORD)                        
         LA    R2,TEARFST                                                       
         USING LIDELD,R2                                                        
PIDLST18 CLI   LIDEL,0                                                          
         JE    PIDLST16                                                         
         CLI   LIDEL,LIDELQ                                                     
         JE    PIDLST22                                                         
PIDLST20 XR    R0,R0                                                            
         IC    R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     PIDLST18                                                         
                                                                                
PIDLST22 CLI   LIDTYPE,LIDTPID                                                  
         JNE   PIDLST20                                                         
         LA    R3,LIDDATA                                                       
PIDLST24 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=END OF ELEMENT                            
         CR    R3,R1               CHECK R3 HASN'T OVERSHOT                     
         JNL   PIDLST20                                                         
                                                                                
         GOTOR LP_AAWMP,DMCB,(L'SA0KNUM,0(R3)),DPIDIND2,DPIDMX2Q,      +        
               LP_D                                                             
         LA    R3,L'SA0KNUM(R3)                                                 
         J     PIDLST24                                                         
                                                                                
PIDLST26 MVI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         OC    QAPIDB,QAPIDB                                                    
         JZ    EXITY                                                            
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,QAPIDB                                                      
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        NUMBER OF ENTRIES                            
         LA    R4,LW_DATA2         START OF LIST                                
PIDLST28 GOTOR LP_AAWMP,DMCB,(L'SA0KNUM,0(R4)),DPIDIND2,DPIDMX2Q,      +        
               LP_D                                                             
         LA    R4,L'SA0KNUM(R4)                                                 
         JCT   R3,PIDLST28                                                      
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Edit start date AM/PM                                               *         
***********************************************************************         
         SPACE 1                                                                
EDTSAPM  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),KSTSSAM                                                    
         JZ    EDTSAPM2                                                         
         MVI   0(R4),C'A'                                                       
         J     EDTSAPM6                                                         
                                                                                
EDTSAPM2 TM    0(R2),KSTSSPM                                                    
         JZ    EDTSAPM6                                                         
         MVI   0(R4),C'P'                                                       
         J     EDTSAPM6                                                         
                                                                                
EDTSAPM6 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit end date AM/PM                                                 *         
***********************************************************************         
         SPACE 1                                                                
EDTEAPM  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),KSTSEAM                                                    
         JZ    EDTEAPM2                                                         
         MVI   0(R4),C'A'                                                       
         J     EDTEAPM6                                                         
                                                                                
EDTEAPM2 TM    0(R2),KSTSEPM                                                    
         JZ    EDTEAPM6                                                         
         MVI   0(R4),C'P'                                                       
         J     EDTEAPM6                                                         
                                                                                
EDTEAPM6 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* EXTRACT CLIENT PRODUCT JOB CODE                                     *         
***********************************************************************         
         SPACE 1                                                                
EXTCPJ   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*EXTCPJ*'                                                      
         NI    OVINDS1,X'FF'-OVITSKFD                                           
         XC    DTLSTIND,DTLSTIND                                                
         XC    DATLST,DATLST                                                    
         XC    AKEYROL,AKEYROL                                                  
         MVC   SVRMODE,LP_RMODE                                                 
         MVC   SVRAIO,LP_ADATA                                                  
         MVC   OCLICDE(3*L'OCLICDE+3*L'OCLINAM),SPACES                          
         MVC   DCLIENT(L'DCLIENT+L'DPROD+L'DJOB),SPACES                         
         L     R2,0(R1)                                                         
         OC    0(L'ACTKACT,R2),0(R2)                                            
         JZ    ECPJ100                                                          
         SR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         MVC   OCLICDE(0),0(R2)                                                 
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    R4,0(R2)                                                         
         AR    R4,RE                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         MVC   OPROCDE(0),0(R4)                                                 
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R4,RF                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         IC    RE,PJOBLEN                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         MVC   OJOBCDE(0),0(R4)                                                 
         EX    RE,*-6                                                           
         MVC   DCLIENT,OCLICDE                                                  
         MVC   DPROD,OPROCDE                                                    
         MVC   DJOB,OJOBCDE                                                     
         MVC   TEMP2(L'ACTKULA),SPACES                                          
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),PRODUL                                
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(L'OCLICDE),OCLICDE                     
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OCLINAM,TEMP2                                                    
         L     R3,AIO3                                                          
         USING ACTRECD,R3                                                       
         LA    R4,ACTRFST                                                       
         USING PPRELD,R4                                                        
         SR    R0,R0                                                            
ECPJ002  CLI   PPREL,0                                                          
         BE    ECPJ010                                                          
         CLI   PPREL,PPRELQ                                                     
         BE    ECPJ006                                                          
         CLI   PPREL,LIDELQ                                                     
         BE    ECPJ008                                                          
ECPJ004  LLC   R0,PPRLN                                                         
         AR    R4,R0                                                            
         B     ECPJ002                                                          
*                                                                               
ECPJ006  MVC   RS#SJOFF,PPRGAOFF                                                
         OC    RS#SJOFF,SPACES                                                  
         B     ECPJ004                                                          
         DROP  R3,R4                                                            
*                                                                               
         USING LIDELD,R4                                                        
ECPJ008  CLI   LIDTYPE,LIDTTEAJ                                                 
         BNE   ECPJ004                                                          
         GOTOR PROCTEA                                                          
         B     ECPJ004                                                          
*                                                                               
ECPJ010  CLC   OPROCDE,SPACES                                                   
         BNH   ECPJ100                                                          
         MVC   TEMP2(L'ACTKULA),SPACES                                          
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),PRODUL                                
         SR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         SHI   RE,1                                                             
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(0),0(R2)                               
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OPRONAM,TEMP2                                                    
         L     R3,AIO3                                                          
         USING ACTRECD,R3                                                       
         LA    R4,ACTRFST                                                       
         USING PPRELD,R4                                                        
         SR    R0,R0                                                            
ECPJ012  CLI   PPREL,0                                                          
         BE    ECPJ020                                                          
         CLI   PPREL,PPRELQ                                                     
         BE    ECPJ016                                                          
         CLI   PPREL,LIDELQ                                                     
         BE    ECPJ018                                                          
ECPJ014  LLC   R0,PPRLN                                                         
         AR    R4,R0                                                            
         B     ECPJ012                                                          
*                                                                               
ECPJ016  CLI   PPRGAOFF,X'40'                                                   
         JNH   ECPJ020                                                          
         MVC   RS#SJOFF,PPRGAOFF                                                
         OC    RS#SJOFF,SPACES                                                  
         B     ECPJ014                                                          
         DROP  R3,R4                                                            
*                                                                               
         USING LIDELD,R4                                                        
ECPJ018  CLI   LIDTYPE,LIDTTEAJ                                                 
         BNE   ECPJ014                                                          
         GOTOR PROCTEA                                                          
         B     ECPJ014                                                          
*                                                                               
ECPJ020  CLC   OJOBCDE,SPACES                                                   
         BNH   ECPJ100                                                          
         MVC   TEMP2(L'ACTKULA),SPACES                                          
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),PRODUL                                
         XR    RE,RE                                                            
         IC    RE,PJOBLEN                                                       
         SHI   RE,1                                                             
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(0),0(R2)                               
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OJOBNAM,TEMP2                                                    
         L     R3,AIO3                                                          
         USING ACTRECD,R3                                                       
         MVI   OJBSTAT,C'1'        Default open                                 
         TM    ACTRSTAT,ACTSLOCK                                                
         BZ    ECPJ022                                                          
         MVI   OJBSTAT,C'4'        Locked                                       
ECPJ022  TM    ACTRSTAT,ACTSCLOS                                                
         BZ    ECPJ024                                                          
         MVI   OJBSTAT,C'5'        Closed                                       
ECPJ024  TM    ACTRSTAT,ACTSDRFT                                                
         BZ    ECPJ026                                                          
         MVI   OJBSTAT,C'2'        Draft                                        
ECPJ026  LA    R4,ACTRFST                                                       
         USING JOBELD,R4                                                        
         SR    R0,R0                                                            
ECPJ028  CLI   JOBEL,0                                                          
         BE    ECPJ100                                                          
         CLI   JOBEL,JOBELQ                                                     
         BE    ECPJ032                                                          
         CLI   JOBEL,PPRELQ                                                     
         BE    ECPJ034                                                          
         CLI   JOBEL,LIDELQ                                                     
         BE    ECPJ036                                                          
ECPJ030  LLC   R0,JOBLN                                                         
         AR    R4,R0                                                            
         B     ECPJ028                                                          
*                                                                               
ECPJ032  TM    ACTRSTAT,ACTSDRFT                                                
         BNO   ECPJ030                                                          
         TM    JOBSTA2,JOBSREJ                                                  
         BZ    ECPJ030                                                          
         MVI   OJBSTAT,C'3'        Rejected                                     
         B     ECPJ030                                                          
*                                                                               
         USING PPRELD,R4                                                        
ECPJ034  CLI   PPRGAOFF,X'40'                                                   
         BNH   ECPJ030                                                          
         MVC   RS#SJOFF,PPRGAOFF                                                
         OC    RS#SJOFF,SPACES                                                  
         B     ECPJ030                                                          
*                                                                               
         USING LIDELD,R4                                                        
ECPJ036  CLI   LIDTYPE,LIDTTEAJ                                                 
         BNE   ECPJ030                                                          
         GOTOR PROCTEA                                                          
         B     ECPJ030                                                          
         DROP  R3,R4                                                            
                                                                                
ECPJ100  MVC   LP_RMODE,SVRMODE                                                 
         MVC   LP_ADATA,SVRAIO                                                  
         TM    OVINDS1,OVIRWK      Called from nxtwrk+team list in use?         
         JNO   EXITY               No                                           
         TM    OVINDS1,OVITSKFD    Did we find any work for this person         
         JNZ   ECPJ102             Yes                                          
         CLC   CCTPID,SVRWKKEY+(RWKKPID-RWKKEY)                                 
         JE    EXITY                                                            
         CLI   QVIEWAL,C'Y'        No - but can we view all                     
         JE    EXITY               Yes                                          
         J     EXITN               No                                           
                                                                                
         USING LW_D,RE                                                          
ECPJ102  XR    RE,RE               Point to list in WMP                         
         ICM   RE,7,DATLST                                                      
         JZ    EXITN               Do not want them                             
         LA    RF,LW_DATA2                                                      
         ST    RF,AKEYROL                                                       
         XR    R2,R2                                                            
         ICM   R2,3,LW_NUMN        Number of entries                            
         LA    R3,L'LIDROLN+L'LIDBPID Length of each entry/key                  
         DROP  RE                                                               
         GOTO1 VXSORT,DMCB,(RF),(R2),(R3),(R3),0                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Process client team element                                         *         
***********************************************************************         
         SPACE 2                                                                
PROCTEA  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PROCTEA'                                                      
                                                                                
         USING LIDELD,R4                                                        
         TM    OVINDS1,OVIRWK     Called from nxtwrk+team list in use?          
         JNO   PROCTEAY                                                         
         LA    R6,LIDDATA                                                       
         XR    R2,R2                                                            
         IC    R2,LIDLN                                                         
         AR    R2,R4               R2=End of element                            
         CLI   QVIEWAL,C'Y'                                                     
         JE    PROCTEA4                                                         
         CLC   CCTPID,SVRWKKEY+(RWKKPID-RWKKEY)                                 
         JE    PROCTEA4                                                         
PROCTEA2 CR    R6,R2                                                            
         JNL   PROCTEAY                                                         
         CLC   L'LIDROLN(L'LIDBPID,R6),CCTPID                                   
         JE    PROCTEA4                                                         
         AHI   R6,L'LIDROLN+L'LIDBPID                                           
         J     PROCTEA2                                                         
PROCTEA4 OI    OVINDS1,OVITSKFD   Set work found                                
         LA    R6,LIDDATA                                                       
PROCTEA6 CR    R6,R2                                                            
         JNL   PROCTEAY                                                         
         MVC   DROLE,0(R6)    Set role number for key driver                    
         MVI   LP_RMODE,LP_RFRST                                                
         GOTOR (#NXTREC,ANXTREC),DMCB,ROLKEYT2,('B#ROLE',SVROLKEY),    +        
               ('$NXTRXGR',SAVED),0,0                                           
         JNE   PROCTEA8                                                         
         LA    RF,IOKEY                                                         
         USING ROLRECD,RF                                                       
         TM    ROLKSTAT,ROLSSRKR                                                
         JNO   PROCTEA8                                                         
         GOTOR LP_AAWMP,DMCB,(L'LIDROLN+L'LIDBPID,0(R6)),              +        
               DTLSTIND,DTLIMAXQ,LP_D                                           
PROCTEA8 LA    R6,L'LIDROLN+L'LIDBPID(R6)                                       
         J     PROCTEA6                                                         
                                                                                
PROCTEAY J     EXITY                                                            
         DROP  R4,RF                                                            
***********************************************************************         
* EXTRACT KEYSTAGE OR OUT OF OFFICE DETAILS                           *         
***********************************************************************         
         SPACE 2                                                                
EXTKST   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*EXTKST*'                                                      
         L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         MVC   OKSTCDE,=X'FFFE'                                                 
         MVC   OKSTNAM,SPACES                                                   
         OC    0(L'PKSKKYST,R2),0(R2)                                           
         JZ    EKST010                                                          
         MVC   OKSTCDE,0(R2)                                                    
         LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         USING KSTRECD,R4                                                       
         MVI   KSTKTYP,KSTKTYPQ                                                 
         MVI   KSTKSUB,KSTKSUBQ                                                 
         MVC   KSTKCPY,AGENCY                                                   
         MVC   KSTKOFF,SPACES                                                   
         TM    SCPYEL+(CPYSTATC-CPYELD),CPYSROFF Offices for resources          
         JZ    EKST001                                                          
         MVC   KSTKOFF,USROFF                                                   
EKST001  MVC   KSTKNUM,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    EKST002                                                          
         DC    H'0'                                                             
*                                                                               
EKST002  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    EKST004                                                          
         DC    H'0'                                                             
EKST004  L     R4,AIO2                                                          
         LA    R4,KSTRFST                                                       
         USING KSTELD,R4                                                        
         XR    R0,R0                                                            
EKST006  CLI   KSTEL,0                                                          
         JE    EKST100                                                          
         CLI   KSTEL,KSTELQ                                                     
         JE    EKST008                                                          
         IC    R0,KSTLN                                                         
         AR    R4,R0                                                            
         J     EKST006                                                          
*                                                                               
EKST008  XR    RF,RF                                                            
         IC    RF,KSTLN                                                         
         SHI   RF,KSTLN1Q+1                                                     
         JM    EKST100                                                          
         BASR  RE,0                                                             
         MVC   OKSTNAM(0),KSTNAME                                               
         EX    RF,0(RE)                                                         
         J     EKST100                                                          
         DROP  R4                                                               
*                                                                               
EKST010  CLI   2(R2),PKSKOHOL                                                   
         JNE   EKST012                                                          
         MVC   OKSTNAM(L'AC@HLIDY),AC@HLIDY                                     
         J     EKST100                                                          
EKST012  CLI   2(R2),PKSKOAPT                                                   
         JNE   EKST014                                                          
         MVC   OKSTNAM(L'AC@APPTM),AC@APPTM                                     
         J     EKST100                                                          
EKST014  CLI   2(R2),PKSKOOTH                                                   
         JE    EKST016                                                          
         DC    H'0'                                                             
EKST016  MVC   OKSTNAM(L'AC@OTHER),AC@OTHER                                     
EKST100  J     EXITY                                                            
                                                                                
***********************************************************************         
* EXTRACT HOURS FROM RESOURCE RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
SVRDEF   CSECT ,                                                                
EXTHRS   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*EXTHRS*'                                                      
         L     R2,IOADDR                                                        
         ZAP   DHRS,PZERO                                                       
         USING RWKRECD,R2                                                       
         LA    R2,RWKRFST                                                       
         USING LIDELD,R2                                                        
EXHRS02  CLI   LIDEL,0                                                          
         JE    EXITY                                                            
         CLI   LIDEL,LIDELQ                                                     
         JE    EXHRS06                                                          
EXHRS04  XR    RE,RE                                                            
         IC    RE,LIDLN                                                         
         AR    R2,RE                                                            
         J     EXHRS02                                                          
*                                                                               
EXHRS06  CLI   LIDTYPE,LIDTRSPD                                                 
         BNE   EXHRS04                                                          
         LA    R3,LIDDATA                                                       
EXHRS08  XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2               R1=End of element                            
         CR    R3,R1               Check r3 hasn't overshot                     
         JNL   EXHRS04                                                          
         OC    L'LIDRPID(L'LIDRHRS,R3),L'LIDRPID(R3)  Any hours?                
         JZ    EXHRS10                                                          
         AP    DHRS,L'LIDRPID(L'LIDRHRS,R3)                                     
EXHRS10  LA    R3,L'LIDRPID+L'LIDRHRS(R3)                                       
         J     EXHRS08                                                          
         DROP  R2                                                               
***********************************************************************         
* FILTER ACCORDING TO KEYSTAGE AND DATES                              *         
***********************************************************************         
FLTKST   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*FLTKST*'                                                      
         L     R2,IOADDR                                                        
         USING RWKRECD,R2                                                       
         OC    QKYST,QKYST                                                      
         JNZ   FLKST02                                                          
         OC    QKSTTO,QKSTTO                                                    
         JZ    FLTKSTY                                                          
FLKST02  LA    R2,RWKRFST                                                       
         USING KSTELD,R2                                                        
FLKST04  CLI   KSTEL,0                                                          
         JE    EXITN                                                            
         CLI   KSTEL,KSTELQ                                                     
         JE    FLKST08                                                          
FLKST06  XR    RE,RE                                                            
         IC    RE,KSTLN                                                         
         AR    R2,RE                                                            
         J     FLKST04                                                          
                                                                                
FLKST08  OC    QKYST,QKYST         HAVE WE GOT A KEYSTAGE                       
         JZ    FLKST10                                                          
         CLC   QKYST,KSTCODE                                                    
         JNE   FLKST06                                                          
FLKST10  OC    QKSTFROM,QKSTFROM                                                
         JZ    FLTKSTY                                                          
         CLC   QKSTFROM,KSTSTDT                                                 
         JE    FLTKSTY                                                          
         JL    FLKST12                                                          
         CLC   QKSTFROM,KSTENDT                                                 
         JH    FLKST06                                                          
         J     FLTKSTY                                                          
                                                                                
FLKST12  CLC   QKSTTO,KSTSTDT                                                   
         JL    FLKST06                                                          
FLTKSTY  J     EXITY                                                            
                                                                                
***********************************************************************         
* Filter according to PIDs for the keystages                          *         
***********************************************************************         
FLTPID   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*FLTPID*'                                                      
         USING LW_D,RF                                                          
         XR    RF,RF               Point to list in WMP                         
         ICM   RF,7,DAPIDB2        Do we have a list                            
         JZ    FLTPID10            No - no pids to filter allow all             
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        Number of entries in list                    
         JZ    FLTPID10            No entries                                   
         LA    R4,LW_DATA2         Start of list                                
                                                                                
         L     R2,IOADDR                                                        
         USING RWKRECD,R2                                                       
         LA    R2,RWKRFST                                                       
         USING KSTELD,R2                                                        
FLTPID02 CLI   KSTEL,0                                                          
         JE    EXITN                                                            
         CLI   KSTEL,KSTELQ                                                     
         JE    FLTPID06                                                         
FLTPID04 LLC   RE,KSTLN                                                         
         AR    R2,RE                                                            
         J     FLTPID02                                                         
                                                                                
FLTPID06 CLI   KSTLN,KSTLN2Q       Have we got a keystage with PIDs             
         JNH   FLTPID04            No - look for another                        
         LLC   RE,KSTLN                                                         
         SHI   RE,KSTLN2Q                                                       
         SRL   RE,1                RE=Number of PIDs on keystage                
         LA    R1,KSTPID                                                        
FLTPID08 CLC   0(L'KSTPID,R1),0(R4)                                             
         JE    FLTPIDY                                                          
                                                                                
         LA    R4,L'SA0KNUM(R4)                                                 
         JCT   R3,FLTPID08                                                      
                                                                                
         USING LW_D,RF                                                          
         XR    RF,RF               Point to list in WMP                         
         ICM   RF,7,DAPIDB2                                                     
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        Number of entries in list                    
         LA    R4,LW_DATA2         Start of list                                
                                                                                
         LA    R1,L'KSTPID(R1)                                                  
         JCT   RE,FLTPID08                                                      
         J     FLTPID04                                                         
                                                                                
FLTPID10 OC    QAPIDB,QAPIDB       Were we passed any PIDs                      
         JNZ   EXITN                                                            
         OC    QATEAM,QATEAM       Were we passed any teams                     
         JNZ   EXITN                                                            
         OC    QAROLE,QAROLE       Were we passed any roles                     
         JNZ   EXITN                                                            
                                                                                
FLTPIDY  J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
*** Key filter routine for RWKRECD ***                                *         
***********************************************************************         
                                                                                
FLTCAMP  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTCAMP'                                                      
                                                                                
         LA    R2,IOKEY                                                         
         USING RWKRECD,R2                                                       
         OC    QCAMP,QCAMP                                                      
         JZ    FCAMP02                                                          
         CLC   QCAMP,RWKKCCDE                                                   
         JNE   EXITN                                                            
                                                                                
FCAMP02  CLC   QCLIENT,SPACES                                                   
         JNH   FCAMP10                                                          
         SR    R3,R3                                                            
         IC    R3,PCLILEN                                                       
         SHI   R3,1                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         JNE   EXITN                                                            
         CLC   RWKKACT(0),QCLIENT                                               
         AHI   R3,1                                                             
         CLC   QPROD,SPACES                                                     
         JNH   FCAMP10                                                          
         LA    R4,RWKKACT                                                       
         AR    R4,R3                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,R3                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNE   EXITN                                                            
         CLC   QPROD(0),0(R4)                                                   
         CLC   QJOBC,SPACES                                                     
         JNH   FCAMP10                                                          
         AHI   RF,1                                                             
         AR    R4,RF                                                            
         SR    R3,R3                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         IC    R3,PJOBLEN                                                       
         SR    R3,RF                                                            
         SHI   R3,1                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         JNE   EXITN                                                            
         CLC   QJOBC(0),0(R4)                                                   
                                                                                
FCAMP10  OC    RWKKSTAT,RWKKSTAT                                                
         JNZ   FCAMP12                                                          
         CLI   QCURR,C'Y'                                                       
         JNE   EXITN                                                            
         J     FCAMP20                                                          
                                                                                
FCAMP12  TM    RWKKSTAT,RWKSENDD                                                
         JZ    FCAMP14                                                          
         CLI   QFINS,C'Y'                                                       
         JNE   EXITN                                                            
         J     FCAMP20                                                          
                                                                                
FCAMP14  TM    RWKKSTAT,RWKSDEAD                                                
         JZ    FCAMP20                                                          
         CLI   QOBSO,C'Y'                                                       
         JNE   EXITN                                                            
                                                                                
FCAMP20  TM    CPYSTATC,CPYSTEAJ   TEAM LIST IN JOBS?                           
         JO    FCAMP30                                                          
         CLI   QVIEWAL,C'Y'                                                     
         JE    FCAMP30                                                          
         CLC   RWKKPID,CCTPID                                                   
         JNE   EXITN                                                            
                                                                                
FCAMP30  OC    QADDFROM,QADDFROM                                                
         JZ    FCAMP40                                                          
         GOTOR VDATCON,DMCB,(2,RWKKDTE),(1,WORK)                                
         CLC   QADDFROM,WORK                                                    
         JH    EXITN                                                            
         JE    FCAMP40                                                          
         CLC   QADDTO,WORK                                                      
         JL    EXITN                                                            
                                                                                
FCAMP40  GOTOR TESTSJ,DMCB,RWKKACT                                              
         JNE   EXITN                                                            
                                                                                
FCAMP60  J     EXITY                                                            
                                                                                
TESTSJ   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*TESTSJ*'                                                      
         L     R2,0(R1)                                                         
         XC    GAPAREA,GAPAREA                                                  
         LA    R3,GAPAREA                                                       
         USING GAPTABD,R3                                                       
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         BNZ   TESTSJY                                                          
         MVI   DUB1,1              - client                                     
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         AR    RE,R2                                                            
         CLC   0(2,RE),SPACES                                                   
         BE    TSTSJ100                                                         
         MVI   DUB1,2              - product                                    
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         AR    RE,R2                                                            
         CLI   0(RE),C' '                                                       
         BE    TSTSJ100                                                         
         MVI   DUB1,3              - job                                        
                                                                                
TSTSJ100 XR    RF,RF                                                            
         CLI   GAPTLVL,GAPTSL2     is this a high level entry                   
         BNE   TSTSJ110                                                         
         IC    RF,PCLILEN                                                       
         B     TSTSJ130                                                         
TSTSJ110 CLI   DUB1,1              client routine                               
         BE    TSTSJ140                                                         
         CLI   GAPTLVL,GAPTSL3     is entry a product?                          
         BNE   TSTSJ120                                                         
         IC    RF,PPROLEN                                                       
         B     TSTSJ130                                                         
TSTSJ120 CLI   DUB1,2              product routine                              
         BE    TSTSJ140                                                         
         CLI   GAPTLVL,GAPTSL4     is entry a job?                              
         BNE   TSTSJ140                                                         
         IC    RF,PJOBLEN                                                       
TSTSJ130 SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   GAPTACC(0),0(R2)                                                 
         EX    RF,0(RE)                                                         
         BE    TESTSJY                                                          
TSTSJ140 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         BNZ   TESTSJN                                                          
         B     TSTSJ100                                                         
                                                                                
TESTSJY  J     EXITY                                                            
                                                                                
TESTSJN  J     EXITN                                                            
         DROP  R3                                                               
*                                                                               
FLTPKS   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*FLTPKS*'                                                      
         LA    R2,IOKEY                                                         
         USING PKSRECD,R2                                                       
         OC    QCAMP,QCAMP                                                      
         JZ    FLPKS02                                                          
         CLC   QCAMP,PKSKCODE                                                   
         JNE   EXITN                                                            
                                                                                
FLPKS02  CLC   QCLIENT,SPACES                                                   
         JNH   FLPKS10                                                          
         SR    R3,R3                                                            
         IC    R3,PCLILEN                                                       
         SHI   R3,1                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         JNE   EXITN                                                            
         CLC   PKSKACT(0),QCLIENT                                               
         AHI   R3,1                                                             
         CLC   QPROD,SPACES                                                     
         JNH   FLPKS10                                                          
         LA    R4,PKSKACT                                                       
         AR    R4,R3                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,R3                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNE   EXITN                                                            
         CLC   QPROD(0),0(R4)                                                   
         CLC   QJOBC,SPACES                                                     
         JNH   FLPKS10                                                          
         AHI   RF,1                                                             
         AR    R4,RF                                                            
         SR    R3,R3                                                            
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         IC    R3,PJOBLEN                                                       
         SR    R3,RF                                                            
         SHI   R3,1                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         JNE   EXITN                                                            
         CLC   QJOBC(0),0(R4)                                                   
                                                                                
FLPKS10  OC    PKSKSTAT,PKSKSTAT                                                
         JNZ   FLPKS12                                                          
         CLI   QCURR,C'Y'                                                       
         JNE   EXITN                                                            
         J     FLPKS20                                                          
                                                                                
FLPKS12  TM    PKSKSTAT,RWKSENDD                                                
         JZ    FLPKS14                                                          
         CLI   QFINS,C'Y'                                                       
         JNE   EXITN                                                            
         J     FLPKS20                                                          
                                                                                
FLPKS14  TM    PKSKSTAT,RWKSDEAD                                                
         JZ    FLPKS20                                                          
         CLI   QOBSO,C'Y'                                                       
         JNE   EXITN                                                            
                                                                                
FLPKS20  OC    QKYST,QKYST         HAVE WE GOT A KEYSTAGE                       
         JZ    FLPKS30                                                          
         CLC   QKYST,PKSKKYST                                                   
         JNE   EXITN                                                            
                                                                                
FLPKS30  OC    QADDFROM,QADDFROM                                                
         JZ    FLPKS40                                                          
         GOTOR VDATCON,DMCB,(2,PKSKDTE),(1,WORK)                                
         CLC   QADDFROM,WORK                                                    
         JH    EXITN                                                            
         JE    FLPKS40                                                          
         CLC   QADDTO,WORK                                                      
         JL    EXITN                                                            
                                                                                
FLPKS40  J     EXITY                                                            
                                                                                
***********************************************************************         
* SET STATUS OF WORK                                                  *         
***********************************************************************         
         SPACE 1                                                                
SETSTAT  NTR1  ,                                                                
         L     R2,0(R1)                                                         
         OC    0(L'RWKRSTAT,R2),0(R2)                                           
         JNZ   SSTAT02                                                          
         MVI   OSTATUS,C'3'        CURRENT                                      
         J     EXITY                                                            
SSTAT02  TM    0(R2),RWKSENDD                                                   
         JZ    SSTAT04                                                          
         MVI   OSTATUS,C'1'        FINISHED                                     
         J     EXITY                                                            
SSTAT04  TM    0(R2),RWKSDEAD                                                   
         JZ    EXITY                                                            
         MVI   OSTATUS,C'2'        OBSOLETE                                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Interface to TSAR                                                   *         
*                                                                     *         
***********************************************************************         
                                                                                
GOTSAR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GOTSAR*'                                                      
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
         LA    R3,TSARRBLK                                                      
         USING TSARD,R3            R3=A(TSAR block)                             
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOTSAR02                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACTN,0(R2)                                                     
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         MVC   TSACOM,ACOMFACS                                                  
         LHI   R0,1024                                                          
         OC    TSBUFFL,TSBUFFL                                                  
         JNZ   *+8                                                              
         STCM  R0,3,TSBUFFL        Set require 1MB off-line                     
         MVI   TSRECI,TSRTSARB                                                  
         OI    TSRECI,TSRXTN                                                    
         MVI   TSKEYL,RES_KEYL     Set key length                               
         LHI   R0,RES_LEN                                                       
         STCM  R0,3,TSRECL         Set maximum record length                    
         GOTOR VTSAR,TSARD                                                      
         TM    TSINDS,TSIINIOK                                                  
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
GOTSAR02 TM    TSINDS,TSIINIOK     Test initialised                             
         JZ    GOTSAR06                                                         
                                                                                
         MVC   TSACTN,0(R2)        Set action                                   
                                                                                
         CLI   TSACTN,TSASRT       Test sorting                                 
         JNE   GOTSAR04                                                         
         L     R1,4(R2)                                                         
         MVC   TSRTPARM,0(R1)      Yes - set sort parameters                    
                                                                                
GOTSAR04 GOTOR VTSAR,TSARD         Call TSAR                                    
         MVC   TSARERRS,TSERRS     Return TSARERRS                              
         J     GOTSARX                                                          
                                                                                
GOTSAR06 MVI   TSARERRS,TSEEOF                                                  
                                                                                
GOTSARX  CLI   TSARERRS,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* GENERAL EXIT AND DECLARATIONS                                      *          
**********************************************************************          
                                                                                
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
                                                                                
MORE     MVI   LP_RMODE,LP_RMORE   SET NORE TO COME                             
         J     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         J     EXITY                                                            
                                                                                
QERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         J     EXITY                                                            
                                                                                
XERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         MVI   LP_EMSYS,6                                                       
         STCM  R0,3,LP_ERROR                                                    
         J     EXITN                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
                                                                                
EXITN    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITNX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITNX   LTR   RB,RB                                                            
         J     EXIT                                                             
EXITY    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITYX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITYX   CR    RE,RE                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'nCTFILE '         FILE LIST                                    
         DC    C'NGENFIL '                                                      
         DC    C'NGENDIR '                                                      
         DC    C'UACCDIR '                                                      
         DC    C'UACCMST '                                                      
         DC    C'UACCARC '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
                                                                                
FACS     DS    0X                                                               
                                                                                
***********************************************************************         
* GLOBAL LITERALS                                                     *         
***********************************************************************         
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
KSTMDL#  DC    AL2(A#KSTMDL)       KEYSTAGE/TEMPLATE DOWNLOAD                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
TMRLDL#  DC    AL2(A#TMRLDL)       TEAM/ROLE DOWNLOAD                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
RSTRDL#  DC    AL2(A#RSTDL)        STATUS REPORT/WORK SEARCH DOWNLOAD           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
RSRCDL#  DC    AL2(A#RSRDL)        RESOURCES SEARCH DOWNLOAD                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
RMYTDL#  DC    AL2(A#RMYDL)        MY TASKS LIST/SEARCH DOWNLOAD                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
RDETDL#  DC    AL2(A#RDEDL)        WORK DETAIL DOWNLOAD                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
RAUDDL#  DC    AL2(A#RAUDL)        RESOURCES AUDIT DOWNLOAD                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
OUTOFDL# DC    AL2(A#OUTDL)        OUT OF OFFICE DOWNLOAD                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
BITLIST  DC    X'8040201008040201'                                              
EZEROS   DC    C'0000000000'                                                    
PZERO    DC    P'0'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCARC   DC    CL8'ACCARC'                                                      
         EJECT                                                                  
KSTKEYT  LKKEY H,KSTKEY,SAVED        ** KEYSTAGE KEY DRIVER **                  
         LKKEY LIT,KSTKTYP,KSTKTYPQ                                             
         LKKEY LIT,KSTKSUB,KSTKSUBQ                                             
         LKKEY SIN,KSTKCPY,AGENCY                                               
         LKKEY LIT,KSTKREM,0                                                    
         LKKEY SIN,KSTKOFF,USROFF                                               
         LKKEY WMP,KSTKNUM,DAKST                                                
         LKKEY E                                                                
                                                                                
TPLKEYT  LKKEY H,TPLKEY            ** TEMPLATE KEY DRIVER **                    
         LKKEY LIT,TPLKTYP,TPLKTYPQ                                             
         LKKEY LIT,TPLKSUB,TPLKSUBQ                                             
         LKKEY SIN,TPLKCPY,AGENCY                                               
         LKKEY LIT,TPLKREM,0                                                    
         LKKEY SIN,TPLKOFF,USROFF                                               
         LKKEY WMP,TPLKNUM,QATPL                                                
         LKKEY E                                                                
                                                                                
TEAKEYT  LKKEY H,TEAKEY            ** TEAM KEY LIST DRIVER **                   
         LKKEY LIT,TEAKTYP,TEAKTYPQ                                             
         LKKEY LIT,TEAKSUB,TEAKSUBQ                                             
         LKKEY SIN,TEAKCPY,AGENCY                                               
         LKKEY LIT,TEAKREM,0                                                    
         LKKEY SIN,TEAKOFF,USROFF                                               
         LKKEY WMP,TEAKNUM,QATEAM                                               
         LKKEY E                                                                
                                                                                
TEKEY2T  LKKEY H,TEAKEY            ** GLOBAL TEAM KEY LIST DRIVER **            
         LKKEY LIT,TEAKTYP,TEAKTYPQ                                             
         LKKEY LIT,TEAKSUB,TEAKSUBQ                                             
         LKKEY SIN,TEAKCPY,AGENCY                                               
         LKKEY LIT,TEAKREM,0                                                    
         LKKEY ALL,TEAKOFF                                                      
         LKKEY WMP,TEAKNUM,QATEAM                                               
         LKKEY E                                                                
                                                                                
ROLKEYT  LKKEY H,ROLKEY            ** ROLE KEY LIST DRIVER **                   
         LKKEY LIT,ROLKTYP,ROLKTYPQ                                             
         LKKEY LIT,ROLKSUB,ROLKSUBQ                                             
         LKKEY SIN,ROLKCPY,AGENCY                                               
         LKKEY LIT,ROLKREM,0                                                    
         LKKEY SIN,ROLKOFF,USROFF                                               
         LKKEY WMP,ROLKNUM,QAROLE                                               
         LKKEY E                                                                
                                                                                
ROKEY2T  LKKEY H,ROLKEY            ** GLOBAL ROLE KEY LIST DRIVER **            
         LKKEY LIT,ROLKTYP,ROLKTYPQ                                             
         LKKEY LIT,ROLKSUB,ROLKSUBQ                                             
         LKKEY SIN,ROLKCPY,AGENCY                                               
         LKKEY LIT,ROLKREM,0                                                    
         LKKEY ALL,ROLKOFF                                                      
         LKKEY WMP,ROLKNUM,QAROLE                                               
         LKKEY E                                                                
                                                                                
PKSKEYT  LKKEY H,PKSKEY            ** PERSONAL KEYSTAGE KEY DRIVER **           
         LKKEY LIT,PKSKTYP,PKSKTYPQ                                             
         LKKEY LIT,PKSKSUB,PKSKSUBQ                                             
         LKKEY SIN,PKSKCPY,AGENCY                                               
         LKKEY LIT,PKSKREM,0                                                    
*        LKKEY WMP,PKSKPIDB,DAPIDB                                              
         LKKEY SIN,PKSKPIDB,QPIDB                                               
         LKKEY RNG,PKSKEDT,QENDRNG                                              
         LKKEY RNG,PKSKSDT,QSTRRNG                                              
         LKKEY WMP,PKSKCODE,QACAMP                                              
         LKKEY ALL,PKSKUNT                                                      
         LKKEY ALL,PKSKLDG                                                      
         LKKEY ALL,PKSKACT                                                      
         LKKEY ALL,PKSKKYST                                                     
         LKKEY ALL,PKSKOUT                                                      
         LKKEY ALL,PKSKAMPM                                                     
         LKKEY E                                                                
                                                                                
HOLKEYT  LKKEY H,PKSKEY            ** PERSONAL HOLIDAY KEY DRIVER **            
         LKKEY LIT,PKSKTYP,PKSKTYPQ                                             
         LKKEY LIT,PKSKSUB,PKSKSUBQ                                             
         LKKEY SIN,PKSKCPY,AGENCY                                               
         LKKEY LIT,PKSKREM,0                                                    
         LKKEY SIN,PKSKPIDB,QPIDB                                               
         LKKEY RNG,PKSKEDT,QENDRNG                                              
         LKKEY RNG,PKSKSDT,QSTRRNG                                              
         LKKEY LIT,PKSKCODE,0                                                   
         LKKEY LIT,PKSKUNT,0                                                    
         LKKEY LIT,PKSKLDG,0                                                    
         LKKEY LIT,PKSKACT,0                                                    
         LKKEY LIT,PKSKKYST,0                                                   
         LKKEY NZR,PKSKOUT                                                      
         LKKEY ALL,PKSKAMPM                                                     
         LKKEY E                                                                
                                                                                
OUTKEYT  LKKEY H,PKSKEY            ** OUT OF OFFICE KEY DRIVER **               
         LKKEY LIT,PKSKTYP,PKSKTYPQ                                             
         LKKEY LIT,PKSKSUB,PKSKSUBQ                                             
         LKKEY SIN,PKSKCPY,AGENCY                                               
         LKKEY LIT,PKSKREM,0                                                    
         LKKEY WMP,PKSKPIDB,DAPIDB                                              
         LKKEY RNG,PKSKEDT,QENDRNG                                              
         LKKEY RNG,PKSKSDT,QSTRRNG                                              
         LKKEY LIT,PKSKCODE,0                                                   
         LKKEY LIT,PKSKUNT,0                                                    
         LKKEY LIT,PKSKLDG,0                                                    
         LKKEY LIT,PKSKACT,0                                                    
         LKKEY LIT,PKSKKYST,0                                                   
         LKKEY NZR,PKSKOUT                                                      
         LKKEY ALL,PKSKAMPM                                                     
         LKKEY E                                                                
                                                                                
PTMKEYT  LKKEY H,PIDKEY            ** PID TEAM PASSIVE KEY DRIVER **            
         LKKEY LIT,PIDKTYP,PIDKTYPQ                                             
         LKKEY LIT,PIDKSUB,PIDKSUBQ                                             
         LKKEY SIN,PIDKCPY,AGENCY                                               
         LKKEY LIT,PIDKREM,0                                                    
         LKKEY SIN,PIDKPID,QPIDB                                                
         LKKEY LIT,PIDKSTYP,PIDKTEAQ                                            
         LKKEY ALL,PIDKNUM                                                      
         LKKEY SIN,PIDKOFF,USROFF                                               
         LKKEY LIT,PIDKREST,0                                                   
         LKKEY E                                                                
                                                                                
PRLKEYT  LKKEY H,PIDKEY            ** PID ROLE PASSIVE KEY DRIVER **            
         LKKEY LIT,PIDKTYP,PIDKTYPQ                                             
         LKKEY LIT,PIDKSUB,PIDKSUBQ                                             
         LKKEY SIN,PIDKCPY,AGENCY                                               
         LKKEY LIT,PIDKREM,0                                                    
         LKKEY SIN,PIDKPID,QPIDB                                                
         LKKEY LIT,PIDKSTYP,PIDKROLQ                                            
         LKKEY ALL,PIDKNUM                                                      
         LKKEY SIN,PIDKOFF,USROFF                                               
         LKKEY LIT,PIDKREST,0                                                   
         LKKEY E                                                                
                                                                                
PPRKEYT  LKKEY H,PIDKEY            ** PID PERS PASSIVE KEY DRIVER **            
         LKKEY LIT,PIDKTYP,PIDKTYPQ                                             
         LKKEY LIT,PIDKSUB,PIDKSUBQ                                             
         LKKEY SIN,PIDKCPY,AGENCY                                               
         LKKEY LIT,PIDKREM,0                                                    
         LKKEY NZR,PIDKPID                                                      
         LKKEY LIT,PIDKSTYP,PIDKPERQ                                            
         LKKEY ALL,PIDKPER                                                      
         LKKEY LIT,PIDKRES2,0                                                   
         LKKEY E                                                                
                                                                                
RWKKEYT  LKKEY H,RWKKEY            ** RESOURCE WORK KEY DRIVER **               
         LKKEY LIT,RWKKTYP,RWKKTYPQ                                             
         LKKEY LIT,RWKKSUB,RWKKSUBQ                                             
         LKKEY SIN,RWKKCPY,AGENCY                                               
         LKKEY LIT,RWKKREM,0                                                    
         LKKEY WMP,RWKKOFF,DAOFF                                                
         LKKEY ALL,RWKKCCDE                                                     
         LKKEY NZR,RWKKACT                                                      
         LKKEY ALL,RWKKSEQ                                                      
         LKKEY E                                                                
                                                                                
SRWKEYT  LKKEY H,RWKKEY            ** SINGLE RESOUCRE KEY DRIVER **             
         LKKEY LIT,RWKKTYP,RWKKTYPQ                                             
         LKKEY LIT,RWKKSUB,RWKKSUBQ                                             
         LKKEY SIN,RWKKCPY,AGENCY                                               
         LKKEY LIT,RWKKREM,0                                                    
         LKKEY WMP,RWKKOFF,DAOFF                                                
         LKKEY SIN,RWKKCCDE,QCAMP                                               
         LKKEY SIN,RWKKACT,DCPJ                                                 
         LKKEY ALL,RWKKSEQ                                                      
         LKKEY E                                                                
                                                                                
CAMKEYT  LKKEY H,RWKKEY            ** CAMPAIGN KEY DRIVER **                    
         LKKEY LIT,RWKKTYP,RWKKTYPQ                                             
         LKKEY LIT,RWKKSUB,RWKKSUBQ                                             
         LKKEY SIN,RWKKCPY,AGENCY                                               
         LKKEY LIT,RWKKREM,0                                                    
         LKKEY WMP,RWKKOFF,DAOFF                                                
         LKKEY SIN,RWKKCCDE,DCAMP                                               
         LKKEY LIT,RWKKACT,0                                                    
         LKKEY ALL,RWKKSEQ                                                      
         LKKEY E                                                                
                                                                                
AUDKEYT  LKKEY H,AUDKEY            ** RESOURCE AUDIT KEY DRIVER **              
         LKKEY LIT,AUDKTYP,AUDKTYPQ                                             
         LKKEY LIT,AUDKSUB,AUDKSUBQ                                             
         LKKEY SIN,AUDKCPY,AGENCY                                               
         LKKEY LIT,AUDKAUDT,AUDKRES                                             
         LKKEY LIT,AUDKREM,0                                                    
         LKKEY SIN,AUDKCCDE,DCAMP                                               
         LKKEY SIN,AUDKACT,DCPJ                                                 
         LKKEY ALL,AUDKSEQ                                                      
         LKKEY E                                                                
                                                                                
ROLKEYT2 LKKEY H,ROLKEY            ** ROLE KEY LIST DRIVER **                   
         LKKEY LIT,ROLKTYP,ROLKTYPQ   FOR TEAM LIST IN JOBS                     
         LKKEY LIT,ROLKSUB,ROLKSUBQ                                             
         LKKEY SIN,ROLKCPY,AGENCY                                               
         LKKEY LIT,ROLKREM,0                                                    
         LKKEY SIN,ROLKOFF,USROFF                                               
         LKKEY SIN,ROLKNUM,DROLE                                                
         LKKEY E                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
ALLOWCHR DC    256XL1'00'          ALLOWABLE PROGRAM CODE CHARACTERS            
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
                                                                                
VIEWLIT  DC    C'View all'                                                      
CAMCLIT  DC    C'Campaign Code'                                                 
ADDTLIT  DC    C'Added Date To'                                                 
ADDFLIT  DC    C'Added Date From'                                               
KSDTLIT  DC    C'Keystage Date To/End'                                          
KSDFLIT  DC    C'Keystage Date From/Start'                                      
STCTLIT  DC    C'Status:Current'                                                
STOBLIT  DC    C'Status:Obsolete'                                               
STFDLIT  DC    C'Status:Finished'                                               
KSTNLIT  DC    C'Keystage Number'                                               
TMNMLIT  DC    C'Team Number'                                                   
PIDCLIT  DC    C'PID Code'                                                      
PIDBLIT  DC    C'PID Binary code'                                               
KSTOLIT  DC    C'Keystages Only'                                                
OUTTLIT  DC    C'Out Of Office Type'                                            
OUTCLIT  DC    C'Out Of Office Comments'                                        
AMPMLIT  DC    C'AM/PM'                                                         
LMLSLIT  DC    C'Limit List override'                                           
DETLLIT  DC    C'Detail call'                                                   
INITLIT  DC    C'Initial'                                                       
                                                                                
                                                                                
LVALUES  DS    0D                                                               
         DC    A(FLTCAMP)                                                       
         DC    A(FLTPKS)                                                        
         DC    A(EXTCPJ)                                                        
         DC    A(EXTKST)                                                        
         DC    A(EXTHRS)                                                        
         DC    A(FLTKST)                                                        
         DC    A(FLTPID)                                                        
         DC    A(SETSTAT)                                                       
LVALUESL EQU   *-LVALUES                                                        
                                                                                
*                                                                               
DCDICTL  DS    0X                                                               
         DCDDL AC#RWKAD,L'AC@RWKAD                                              
         DCDDL AC#KDTAD,L'AC@KDTAD                                              
         DCDDL AC#KDTAM,L'AC@KDTAM                                              
         DCDDL AC#CMTAD,L'AC@CMTAD                                              
         DCDDL AC#CMTDL,L'AC@CMTDL                                              
         DCDDL AC#CMTAM,L'AC@CMTAM                                              
         DCDDL AC#KSTAD,L'AC@KSTAD                                              
         DCDDL AC#KSTDL,L'AC@KSTDL                                              
         DCDDL AC#STAAM,L'AC@STAAM                                              
         DCDDL AC#HLIDY,L'AC@HLIDY                                              
         DCDDL AC#OTHER,L'AC@OTHER                                              
         DCDDL AC#APPTM,L'AC@APPTM                                              
         DCDDL AC#OBSO,L'AC@OBSO                                                
*&&US*&& DCDDL AC#CUR,L'AC@CURNT                                                
*&&UK*&& DCDDL AC#CURNT,L'AC@CURNT                                              
         DCDDL AC#FINSD,L'AC@FINSD                                              
DCDICTLX DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
B#ROLE   EQU   3                   IO2 - ROLE RECORD                            
AROLREC  EQU   LP_BLKS+((B#ROLE-1)*L'LP_BLKS)                                   
B#TEAM   EQU   3                   IO2 - TEAM RECORD                            
ATEAREC  EQU   LP_BLKS+((B#TEAM-1)*L'LP_BLKS)                                   
B#PID    EQU   4                   IO3 - PID PASSIVE                            
B#HOL    EQU   4                   IO3 - PKSRECD RECORD                         
B#RESWK  EQU   5                   IO4 - RESOURCE WORK RECORD                   
ARWKREC  EQU   LP_BLKS+((B#RESWK-1)*L'LP_BLKS)                                  
B#KYST   EQU   6                   IO5 - KEYSTAGE RECORD                        
B#AUDIT  EQU   6                   IO5 - AUDIT RECORD                           
AAUDREC  EQU   LP_BLKS+((B#AUDIT-1)*L'LP_BLKS)                                  
AKSTREC  EQU   LP_BLKS+((B#KYST-1)*L'LP_BLKS)                                   
B#CAMP   EQU   6                   IO5 - CAMPAIGN RECORD                        
ACAMREC  EQU   LP_BLKS+((B#KYST-1)*L'LP_BLKS)                                   
B#TEMP   EQU   7                   IO6 - TEMPLATE RECORD                        
ATPLREC  EQU   LP_BLKS+((B#TEMP-1)*L'LP_BLKS)                                   
B#PTASK  EQU   8                   IO7 - PERSONAL TASKS PKSRECD                 
APKSREC  EQU   LP_BLKS+((B#PTASK-1)*L'LP_BLKS)                                  
B#ACCREC EQU   9                       - GENERAL AREA                           
AGENREC  EQU   LP_BLKS+((B#ACCREC-1)*L'LP_BLKS)                                 
B#SVRDEF EQU   10                      - SERVER DEFINITION                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   11                      - LP_D                                   
                                                                                
GAPSMAX  EQU   5000                Max number of entries                        
                                                                                
EOR      EQU   0                                                                
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         EJECT                                                                  
SAVED    DSECT                                                                  
                                                                                
                                                                                
WVALUES  DS    0X                                                               
ADCONS   DS    0A                                                               
AFLTCAMP DS    A                                                                
AFLTPKS  DS    A                                                                
AEXTCPJ  DS    A                                                                
AEXTKST  DS    A                                                                
AEXTHRS  DS    A                                                                
AFLTKST  DS    A                                                                
AFLTPID  DS    A                                                                
ASETSTAT DS    A                                                                
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
WVALUEL  EQU   *-WVALUES                                                        
VMASTC   DS    V                                                                
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
APIDLST  DS    F                   ADDRESS OF CURRENT PID ENTRY                 
AKEYROL  DS    F                   ADDRESS OF CURRENT KEY ROLE ENTRY            
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
MAPI1    DS    X                   ** MAP INDICATOR BYTE1 1 **                  
MAP#LLDL EQU   MAPI1                                                            
MAPSLLDL EQU   X'80'               LIM LIST DOWNLOAD                            
MAP#ONAL EQU   MAPI1                                                            
MAPSONAL EQU   X'40'               ONA LIST DOWNLOAD                            
MAPI2    DS    X                   ** MAP INDICATOR BYTE1 2 **                  
RUNI     DS    0XL(L'RUNI1)                                                     
RUNI1    DS    X                   ** RUN INDICATOR BYTE1 1 **                  
RUN#MEDI EQU   RUNI1                                                            
RUNSMEDI EQU   X'80'               MEDIA LIDELD INIT                            
BUFFREC  DS    A                   BUFFER S/R A(RECORD)                         
BUFFRET  DS    X                   BUFFER S/R RETURN VALUES                     
PRETSRRC DS    XL(RES_LEN)         TSAR RECORD                                  
GAPAREA  DS    XL(GAPTLNQ)         TSAR RECORD FOR LIMIT LIST/APPROVAL          
TSARRBLK DS    XL(TSPXTNL)         TSAR BLOCK                                   
TSARABUF DS    XL(TSPXTNL)         TSAR BLOCK FOR LIMIT LIST/APPROVAL           
TSARREC  DS    XL256               TSAR RECORD                                  
ATSRERRS DS    XL(L'TSERRS)        ERROR FOR TSAR BUFFER FOR LIMIT LIST         
                                                                                
DSDICTL  DS    0C                                                               
AC@RWKAD DS    CL30                                                             
AC@KDTAD DS    CL30                                                             
AC@KDTAM DS    CL30                                                             
AC@CMTAD DS    CL30                                                             
AC@CMTDL DS    CL30                                                             
AC@CMTAM DS    CL30                                                             
AC@KSTAD DS    CL30                                                             
AC@KSTDL DS    CL30                                                             
AC@STAAM DS    CL30                                                             
AC@HLIDY DS    CL20                                                             
AC@OTHER DS    CL20                                                             
AC@APPTM DS    CL20                                                             
AC@OBSO  DS    CL20                                                             
AC@CURNT DS    CL20                                                             
AC@FINSD DS    CL20                                                             
                                                                                
XLOCAL   DS    0X                  Local storage                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
                                                                                
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
                                                                                
QKSTIND  DS    X                                                                
QAKST    DS    AL3                 KEYSTAGES                                    
*                                                                               
QTPLIND  DS    X                                                                
QATPL    DS    AL3                 TEMPLATE                                     
*                                                                               
QTEAMIND DS    X                                                                
QATEAM   DS    AL3                 TEAM                                         
*                                                                               
QROLEIND DS    X                                                                
QAROLE   DS    AL3                 ROLE                                         
*                                                                               
QCAMPIND DS    X                                                                
QACAMP   DS    AL3                 CAMPAIGN                                     
*                                                                               
*                                                                               
QPIDIND  DS    X                                                                
QAPIDB   DS    AL3                 PID                                          
*                                                                               
QENDRNG  DS    0XL(L'PKSKEDT*2)                                                 
QENDSTR  DS    XL(L'PKSKEDT)       X'000000'                                    
QENDEND  DS    XL(L'PKSKEDT)       START DATE                                   
*                                                                               
QSTRRNG  DS    0XL(L'PKSKSDT*2)                                                 
QSTRSTR  DS    XL(L'PKSKSDT)       END DATE                                     
QSTREND  DS    XL(L'PKSKSDT)       X'FFFFFF'                                    
QADDTO   DS    XL(L'PKSKSDT)                                                    
QADDFROM DS    XL(L'PKSKSDT)                                                    
*                                                                               
QCLIENT  DS    CL5                 CLIENT CODE                                  
QPROD    DS    CL3                 PRODUCT CODE                                 
QJOBC    DS    CL7                 JOB CODE                                     
QCAMP    DS    XL4                 CAMPAIGN CODE                                
QKSTO    DS    CL1                 KEYSTAGE ONLY                                
QCURR    DS    CL1                 CURRENT CAMPAIGNS                            
QOBSO    DS    CL1                 OBSOLETE CAMPAIGNS                           
QFINS    DS    CL1                 FINSIHED CAMPAIGNS                           
QKYST    DS    XL2                 KEYSTAGE NUMBER                              
QLMLST   DS    CL1                 LIMIT LIST OVERRIDE                          
QDETL    DS    CL1                 DETAIL CALL                                  
QKSTFROM DS    PL3                 KEYSTAGE FROM DATE                           
QKSTTO   DS    PL3                 KEYSTAGE TO DATE                             
QVIEWAL  DS    CL1                 VIEW ALL                                     
QPIDB    DS    XL2                 BINARY PID                                   
QINIT    DS    CL1                 INITIAL CALL                                 
         ORG   QVALUES+L'QVALUES                                                
QVALUESL EQU   *-QVALUES                                                        
                                                                                
         DS    0F                                                               
DVALUES  DS    0X                  ** DERIVED VALUES **                         
GAPLPARM DS    XL1                 GAPLST parameter                             
                                                                                
DPIDIND  DS    X                                                                
DAPIDB   DS    AL3                 BUILT PIDS IN WMP                            
DPIDMAXQ EQU   200                                                              
*                                                                               
DPIDIND2 DS    X                                                                
DAPIDB2  DS    AL3                 BUILT PIDS IN WMP                            
DPIDMX2Q EQU   2000                                                             
*                                                                               
DPHRIND  DS    X                                                                
DAPHR    DS    AL3                 BUILT PIDS N HRS IN WMP                      
DPHRMAXQ EQU   20                                                               
*                                                                               
DTEAMIND DS    X                                                                
DATEAM   DS    AL3                 BUILT TEAM IN WMP                            
DTEAMAXQ EQU   80                                                               
*                                                                               
DROLEIND DS    X                                                                
DAROLE   DS    AL3                 BUILT ROLE IN WMP                            
DROLMAXQ EQU   80                                                               
*                                                                               
DCPJIND  DS    X                                                                
DACPJ    DS    AL3                 BUILT CLIENT/PROD/JOB IN WMP                 
DCPJMAXQ EQU   100                                                              
*                                                                               
DPIDROW  DS    H                                                                
*                                                                               
DKSTIND  DS    X                                                                
DAKST    DS    AL3                 BUILT KEYSTAGES IN WMP                       
DKSTMAXQ EQU   20                                                               
*                                                                               
DOFFIND  DS    X                                                                
DAOFF    DS    AL3                 BUILT OFFICES IN WMP                         
DOFFMAXQ EQU   200                                                              
*                                                                               
DTLSTIND DS    X                                                                
DATLST   DS    AL3          BUILT KEY ROLE TEAM LIST MEMBERS IN WMP             
DTLIMAXQ EQU   14                                                               
*                                                                               
DHRS     DS    PL(L'LIDRHRS)       TOTAL HOURS                                  
*                                                                               
DCLIENT  DS    CL5                 CLIENT CODE                                  
DPROD    DS    CL3                 PRODUCT CODE                                 
DJOB     DS    CL6                 JOB CODE                                     
DCAMP    DS    XL4                 CAMPAIGN CODE                                
DCPJ     DS    CL(L'ACTKACT)                                                    
DALPWMP  DS    A                                                                
DROLE    DS    XL2                 ROLE NUMBER                                  
DVALUESL EQU   *-DVALUES                                                        
*                                                                               
AGENCY   DS    XL(L'CUXCPY)                                                     
USROFF   DS    CL(L'TRNOFFC)                                                    
RS#SJOFF DS    CL(L'TRNOFFC)                                                    
*                                                                               
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
SVTPLKEY DS    XL(L'IOKEY)         SAVED TEMPLATE KEY                           
SVROLKEY DS    XL(L'IOKEY)         SAVED ROLE/TEAM KEY                          
SVPKSKEY DS    XL(L'IOKEY)         SAVED PERSONAL KEYSTAGE KEY                  
SVRWKKEY DS    XL(L'IOKEY)         SAVED RESOURCE WORK KEY                      
SVAUDKEY DS    XL(L'IOKEY)         SAVED AUDIT KEY                              
SVRAIO   DS    F                   SAVED IOAREA                                 
SVRMODE  DS    XL(L'LP_RMODE)      SAVED MODE                                   
                                                                                
OVALUES  DS    0X                  ** OUTPUT VALUES **                          
OVINDS1  DS    XL1                 OVERLAY INDICATORS ONE                       
OVINAME  EQU   X'80'               NAME ONLY                                    
OVINONM  EQU   X'40'               NO NAME REQUIRED                             
OVIDAYT  EQU   X'20'               DAY TABLE BUILT                              
OVITSKFD EQU   X'10'               TASK/WORK FOUND FOR PID                      
OVIRWK   EQU   X'08'               CALLED FROM NXTRWK                           
OVIAREC  EQU   X'04'               WANT GLOBAL RECORDS                          
MATCH    DS    CL1                 MATCH TO MEDIA/CLIENT/PRODUCT                
STARPKEY DS    CL1                 STATUS REPORT KEY ROLE                       
PIDCDE   DS    CL8                 CHARACTER PID CODE                           
AUDTIME  DS    CL6                 TIME                                         
AUDUSER  DS    CL10                USER ID                                      
AUDPRFN  DS    CL16                PERSON FIRST NAME                            
AUDPRMN  DS    CL16                PERSON MIDDLE NAME                           
AUDPRLN  DS    CL16                PERSON LAST NAME                             
AUDACTN  DS    CL90                ACTION COMMENTS                              
OCLICDE  DS    CL6                 CLIENT CODE                                  
OPROCDE  DS    CL6                 PRODUCT CODE                                 
OJOBCDE  DS    CL6                 JOB CODE                                     
OCLINAM  DS    CL36                CLIENT NAME                                  
OPRONAM  DS    CL36                PRODUCT NAME                                 
OJOBNAM  DS    CL36                JOB NAME                                     
OKSTCDE  DS    XL(L'PKSKKYST)      KEYSTAGE CODE OR -1                          
OKSTNAM  DS    CL36                KEYSTAGE NAME/HOLIDAY                        
OSTATUS  DS    CL1                 STATUS OF CAMPAIGN                           
OJBSTAT  DS    CL1                 STATUS OF JOB                                
OPKSSTR  DS    PL3                 KEY STAGE/HOLIDAY START DATE                 
OPKSEND  DS    PL3                 KEY STAGE/HOLIDAY END DATE                   
OPKSST   DS    CL1                 AM/PM START DATE                             
OPKSEN   DS    CL1                 AM/PM END DATE                               
OKEYRO   DS    XL(L'ROLKNUM)       KEY ROLE NUMBER                              
OVALUESL EQU   *-OVALUES                                                        
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* included books and DSECTS                                           *         
***********************************************************************         
                                                                                
RES_D    DSECT                     TSAR RECORD DSECT                            
RES_KEY  DS    0X                                                               
RES_PID  DS    XL2                 PID CODE                                     
RES_CAMP DS    XL4                 CAMPAIGN CODE                                
RES_ACT  DS    CL12                ACCOUNT CODE FOR SJ                          
RES_LN1Q EQU   *-RES_D             LENGTH FOR COMPARE                           
RES_ENDT DS    PL3                 END DATE                                     
RES_STDT DS    PL3                 START DATE                                   
RES_KYST DS    XL2                 KEYSTAGE                                     
RES_OUT  DS    CL1                 OUT OF OFFICE TYPE                           
RES_STAT DS    XL1                 STATUS                                       
RES_AMPM DS    XL1                 STATUS                                       
RES_KEYL EQU   *-RES_KEY           KEY LENGTH                                   
RES_DA   DS    XL4                 DISK ADDRESS OF RECORD                       
RES_LEN  EQU   *-RES_D             DATA LENGTH                                  
*        PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
*        PRINT ON                                                               
PIDRECD  DSECT                                                                  
         ORG   PIDKPER+L'PIDKPER                                                
PIDKRES2 DS    XL(L'PIDKEY-(*-PIDKEY))                                          
         ORG   PIDKOFF+L'PIDKOFF                                                
PIDKREST DS    XL(L'PIDKEY-(*-PIDKEY))                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACBRA24   04/24/15'                                      
         END                                                                    
