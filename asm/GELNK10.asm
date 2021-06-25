*          DATA SET GELNK10    AT LEVEL 002 AS OF 11/29/12                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041173.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE TA0610A                                                                  
GELNK10  TITLE '- CFM/MFM downloads'                                            
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,CODE=CODE,REQUEST=*,AUTOCLEAR=Y,SYSTEM=CTLSYSQ,  +        
               APPEND=Y,SERVERTYPE=TSTCTLD,                            +        
               IDF=Y,SYSPHASE=SYSPHASE,SEGMENT=Y,                      +        
               WORKERKEY=CTFM,BLOCKS=(B#SAVED,SAVED,B#HDR,CFMRECD,     +        
               B#MED,CFMRECD,B#STR,CFMRECD)                                     
                                                                                
CODE     NMOD1 0,**GL10**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   INIT02                                                           
         L     R9,LP_ABLK1         On-line - root provides WORKD/SAVED          
         L     R8,LP_ABLK2                                                      
         J     INIT04                                                           
                                                                                
INIT02   L     R9,LP_ARUNP                                                      
         ICM   R9,7,RUNPARUN-RUNPARMD(R9)                                       
         L     R9,RSVRSAVE-RUNFACSD(R9)                                         
         USING WORKD,R9            R9=A(global w/s)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
         USING SAVED,R8            R8=A(save w/s)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         MVC   ATWA,LP_ATWA        Set A(TWA)                                   
         ST    R5,ALP              Set A(LP_D) for other phases                 
                                                                                
INIT04   L     RF,LP_ARUNP                                                      
         MVC   RUNMODE,RUNPMODE-RUNPARMD(RF)                                    
         ICM   RF,7,RUNPARUN-RUNPARMD(RF)                                       
         MVC   ACOMFACS,RCOMFACS-RUNFACSD(RF)                                   
                                                                                
         ST    RE,SRVRRELO         Save program relocation factor               
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST FIRST FOR RUN                           
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
                                                                                
B#HDR    EQU   3                                                                
B#STR    EQU   3                                                                
B#MED    EQU   4                                                                
RUNSTR02 MVC   LP_BLKS+((B#HDR-1)*L'LP_BLKS),AIO2                               
         MVC   LP_BLKS+((B#MED-1)*L'LP_BLKS),AIO3                               
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    Test 'process work' mode                     
         JNE   RUNREQ                                                           
         XC    REQVALS(REQVALL),REQVALS                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNREQ02                                                         
         GOTOR VDATAMGR,DMCB,DMKEY,GENDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,CTFILE,(4,0),0                               
                                                                                
RUNREQ02 MVC   AGY,LP_AGY          Set agency code                              
                                                                                
         LA    R0,OUTVALS                                                       
         LHI   R1,OUTVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLC   LP_QMAPN,I$MFMIDL   Test MFM initial download                    
         JNE   RUNREQ14                                                         
                                                                                
         LA    R2,SECFLDS          R2=A(field security displacements)           
         LHI   R0,SECFLDN          R0=N'Security fields                         
         L     R3,LP_ASECD                                                      
         USING SECD,R3             R3=A(SECRET control block)                   
RUNREQ04 GOTOR VSECRET,DMCB,('SECPFLDP',SECD),1(R2)                             
         JL    RUNREQ06                                                         
         LA    RF,C'R'             C'R'=read-only access                        
         JH    *+8                                                              
         LA    RF,C'W'             C'W'=read/write access                       
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=displacement to security value            
         LA    RE,SECVALS(RE)                                                   
         STC   RF,0(RE)            Yes - display and change                     
RUNREQ06 AHI   R2,L'SECFLDS        Bump to next displacement                    
         JCT   R0,RUNREQ04         Do for number of security fields             
         DROP  R3                                                               
                                                                                
         LA    R2,IOKEY                                                         
         USING CFMRECD,R2          Read CFM agency header record                
         XC    CFMKEY,CFMKEY                                                    
         MVI   CFMKTYPE,CFMKTYPQ                                                
         MVC   CFMKAGY,AGY                                                      
         MVI   CFMKSUBR,CFMKSHDR                                                
         LHI   R0,1                                                             
         STCM  R0,15,CFMKRNOD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    RUNREQ08                                                         
         L     R2,AIO2             Clear IO2 when no header record              
         XC    CFMRECD(CFMFIRST+1-CFMRECD),CFMRECD                              
         J     RUNREQ10                                                         
                                                                                
RUNREQ08 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         JE    RUNREQ10                                                         
         DC    H'0'                                                             
                                                                                
RUNREQ10 LA    R2,IOKEY            Return highest node number                   
         USING CFMPAS,R2                                                        
         XC    CFMPAS,CFMPAS                                                    
         MVI   CFMPTYPE,CFMPTYPQ                                                
         MVC   CFMPAGY,AGY                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO1'                             
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   CFMPAS(CFMPRNOD-CFMPAS),IOKEYSAV                                 
         JNE   *+16                                                             
         MVC   HIGHNODE,CFMPRNOD                                                
         XC    HIGHNODE,EFFS                                                    
                                                                                
         LA    R2,IOKEY            Return user-id name                          
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LP_USRID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R2,CTIDATA                                                       
         USING CTORGD,R2                                                        
         SR    R0,R0                                                            
RUNREQ12 CLI   CTORGEL,0                                                        
         JE    RUNREQ14                                                         
         CLI   CTORGEL,CTORGELQ                                                 
         JE    *+14                                                             
         IC    R0,CTORGLEN                                                      
         AR    R2,R0                                                            
         J     RUNREQ12                                                         
         MVC   USERNAME,CTORGNAM                                                
         J     RUNREQ14                                                         
         DROP  R2                                                               
                                                                                
RUNREQ14 GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         J     EXITY               Exit back TO DDLINK                          
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Request map for MFM initial download                                *         
***********************************************************************         
                                                                                
MFMIDL   LKREQ H,I#MFMIDL,OUTIDL,NEXTREQ=MFMSDL                                 
                                                                                
Levls    LKREQ F,1,(D,B#SAVED,LEVELS),UBIN,TEXT=CT#NOLEV,COL=*                  
XPtrs    LKREQ F,2,(D,B#SAVED,POINTERS),CHAR,TEXT=CT#NOPTR,COL=*                
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Initial download replies                                            *         
***********************************************************************         
                                                                                
OUTIDL   LKOUT H                                                                
                                                                                
IDL1     LKOUT R,2                                                              
UsrNm    LKOUT C,1,(D,B#SAVED,USERNAME),CHAR,ND=Y                               
Point    LKOUT C,2,(D,B#SAVED,SECPOINT),CHAR,ND=Y                               
HiNod    LKOUT C,3,(D,B#SAVED,HIGHNODE),UBIN,ND=Y                               
         LKOUT E                                                                
                                                                                
LNM      LKOUT R,3                                                              
Array    LKOUT C,3,(A,ARYLNM)                                                   
         LKOUT E                                                                
                                                                                
MED      LKOUT R,4                                                              
Array    LKOUT C,4,(A,ARYMED)                                                   
         LKOUT E                                                                
                                                                                
STR      LKOUT R,18                                                             
Array    LKOUT C,18,(A,ARYSTR)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* Array definition for level names download                           *         
***********************************************************************         
                                                                                
ARYLNM   LKOUT A,(D,B#HDR,CFMFIRST),NEWEL=Y,EOT=0,                     +        
               ROWID=(LNMD,LNMELQ),ROWWIDTH=(V,LNMLN)                           
                                                                                
RecTy    LKOUT C,1,LNMREC,UBIN                                                  
RecNm    LKOUT C,2,LNMNAME,CHAR,LEN=V                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for media download                                 *         
***********************************************************************         
                                                                                
ARYMED   LKOUT A,(R,NXTMED),MULTIROW=Y,ROWNAME=CFMRECD                          
                                                                                
MedCd    LKOUT C,1,CFMKCODE,CHAR                                                
MedNd    LKOUT C,3,CFMKRNOD,UBIN                                                
MedNm    LKOUT C,255,(A,ARYMNM)                                                 
MedPt    LKOUT C,255,(A,ARYMPT)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read media records for initial download                             *         
***********************************************************************         
                                                                                
NXTMED   GOTOR (#NXTREC,ANXTREC),DMCB,MEDKEYT,('B#MED',0),SAVED,0               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Array definition for media name download                            *         
***********************************************************************         
                                                                                
ARYMNM   LKOUT A,(D,B#MED,CFMFIRST),EOT=0,                             +        
               ROWID=(RNMD,RNMELQ),ROWWIDTH=(V,RNMLN)                           
                                                                                
MName    LKOUT C,2,RNMNAME,CHAR,ND=Y,LEN=V                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for media pointer info download                    *         
***********************************************************************         
                                                                                
ARYMPT   LKOUT A,(D,B#MED,CFMFIRST),EOT=0,                             +        
               ROWID=(MPTRD,MPTRELQ),ROWWIDTH=(V,MPTRLN)                        
                                                                                
MedSy    LKOUT C,4,MPTRSYS,CHAR,ND=Y                                            
MedMC    LKOUT C,5,MPTRMED,CHAR,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for structure download                             *         
***********************************************************************         
                                                                                
ARYSTR   LKOUT A,(R,NXTSTR),MULTIROW=Y,ROWNAME=CFMRECD                          
                                                                                
PNode    LKOUT C,1,CFMKPNOD,UBIN                                                
RNode    LKOUT C,2,CFMKRNOD,UBIN                                                
RecTy    LKOUT C,3,CFMKSUBR,UBIN                                                
RecCd    LKOUT C,4,CFMKCODE,CHAR                                                
RecNm    LKOUT C,255,(A,ARYSNM)                                                 
Array    LKOUT C,21,(A,ARYVPR),FILTROUT=FLTPTR                                  
Array    LKOUT C,19,(A,ARYCPR),FILTROUT=FLTPTR                                  
Array    LKOUT C,20,(A,ARYBPR),FILTROUT=FLTPTR                                  
                                                                                
         LKOUT E                                                                
                                                                                
FLTPTR   CLI   POINTERS,0          Test pointers required                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Read structure records for stucture download                        *         
***********************************************************************         
                                                                                
NXTSTR   MVC   LP_ADATA,AIO2                                                    
         LA    R2,IOKEY                                                         
         USING CFMKEY,R2           R2=A(record key)                             
                                                                                
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTSTR02                                                         
         CLI   POINTERS,0          Test pointers required                       
         JNE   *+8                                                              
         GOTOR SETSPT              Yes - set Spotpak media contexts             
                                                                                
         CLC   LP_QMAPN,I$MFMIDL   Test initial download                        
         JNE   *+12                                                             
         CLI   LEVELS,0            Test structure download requested            
         JE    NXTSTRX                                                          
                                                                                
         CLI   LEVELS,0            Set levels to maximum if not known           
         JNE   *+8                                                              
         MVI   LEVELS,FF                                                        
                                                                                
         OC    CONTEXT,CONTEXT     Set initial context if not known             
         JNZ   *+12                                                             
         LHI   R0,1                                                             
         STCM  R0,15,CONTEXT                                                    
                                                                                
         XC    CFMKEY,CFMKEY       Initialize for structure reading             
         MVI   CFMKTYPE,CFMKTYPQ                                                
         MVC   CFMKAGY,AGY                                                      
         MVC   CFMKPNOD,CONTEXT                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   CFMKEY(CFMKSUBR-CFMKEY),IOKEYSAV                                 
         JNE   NXTSTRX                                                          
                                                                                
         LA    R3,KEYSTACK         Point to start of key stack                  
         ST    R3,ACURRENT                                                      
         MVI   CURRLEV,1                                                        
         J     NXTSTR14                                                         
                                                                                
NXTSTR02 CLC   CURRLEV,LEVELS      Test at maximum depth                        
         JE    NXTSTR08                                                         
         OC    CFMKRNOD,CFMKRNOD   Test current node has children               
         JZ    NXTSTR08                                                         
         MVC   CFMKPNOD,CFMKRNOD   Initialize for next level read               
         XC    CFMKSUBR(L'CFMKEY-(CFMKSUBR-CFMKEY)),CFMKSUBR                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   CFMKEY(CFMKSUBR-CFMKEY),IOKEYSAV                                 
         JE    NXTSTR06                                                         
                                                                                
NXTSTR04 L     R3,ACURRENT         No children - back to parent                 
         MVC   CFMKEY,0(R3)                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    NXTSTR08                                                         
         DC    H'0'                                                             
                                                                                
NXTSTR06 L     R3,ACURRENT         Initialize for next level reading            
         AHI   R3,L'CFMKEY                                                      
         ST    R3,ACURRENT                                                      
         SR    R0,R0                                                            
         IC    R0,CURRLEV                                                       
         AHI   R0,1                                                             
         STC   R0,CURRLEV                                                       
         J     NXTSTR14                                                         
                                                                                
NXTSTR08 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
                                                                                
NXTSTR10 JE    *+6                                                              
         DC    H'0'                                                             
         CLC   CFMKEY(CFMKSUBR-CFMKEY),IOKEYSAV                                 
         JNE   NXTSTR12                                                         
         CLI   CFMKSUBR,CFMKSMED   Is this a pointer record?                    
         JL    NXTSTR14                                                         
         MVI   CFMKSUBR,CFMKSBRD+1 Yes - skip reads                             
         XC    CFMKCODE(L'CFMKCODE+L'CFMKRNOD+L'CFMKSPAR),CFMKCODE              
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         J     NXTSTR10                                                         
                                                                                
NXTSTR12 SR    R0,R0               Back up to previous parent                   
         IC    R0,CURRLEV                                                       
         SHI   R0,1                                                             
         JZ    NXTSTRX             No more parent nodes - all done              
         STC   R0,CURRLEV                                                       
         L     R3,ACURRENT                                                      
         SHI   R3,L'CFMKEY                                                      
         ST    R3,ACURRENT                                                      
         J     NXTSTR04                                                         
                                                                                
NXTSTR14 CLI   CFMKSUBR,CFMKSMED   Ignore pointers                              
         JNL   NXTSTR12                                                         
                                                                                
         L     R3,ACURRENT         Set current record key in stack              
         MVC   0(L'CFMKEY,R3),CFMKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
NXTSTRX  J     NOMORE                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Set spot cable media context                                        *         
***********************************************************************         
                                                                                
SETSPT   NTR1  LABEL=NO                                                         
         XC    CABMED,CABMED                                                    
         XC    SPTMED(SPTMEDL),SPTMED                                           
         XC    IOKEY,IOKEY                                                      
         MVC   SVADATA,LP_ADATA                                                 
                                                                                
SETSPT02 GOTOR (#NXTREC,ANXTREC),DMCB,MEDKEYT,('B#MED',0),SAVED,0               
         JNE   SETSPTX                                                          
                                                                                
         L     R2,IOADDR           Point to media record                        
         USING CFMRECD,R2                                                       
         LA    R3,CFMFIRST                                                      
         USING MPTRD,R3                                                         
         SR    R0,R0                                                            
SETSPT04 CLI   MPTREL,0            Test end of record                           
         JE    SETSPT02                                                         
         CLI   MPTREL,MPTRELQ      Test media pointer element                   
         JE    *+14                                                             
         IC    R0,MPTRLN           Bump to next element                         
         AR    R3,R0                                                            
         J     SETSPT04                                                         
         CLI   MPTRSYS,MPTRSSPT    Test Spot system                             
         JNE   SETSPT02                                                         
         CLI   MPTRMED,SPOTTVQ     Test Spot tv media                           
         JNE   SETSPT06                                                         
         MVC   CABMED,CFMKRNOD     Set cable media record context               
         J     SETSPT02                                                         
                                                                                
SETSPT06 LA    R1,SPTMED           Add entry to Spot media list                 
         LHI   R0,SPTMEDN                                                       
         BASR  RE,0                                                             
         OC    0(L'SPTMED,R1),0(R1)                                             
         JNZ   *+14                                                             
         MVC   0(L'SPTMED,R1),CFMKRNOD                                          
         J     SETSPT02                                                         
         AHI   R1,L'SPTMED                                                      
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
                                                                                
SETSPTX  XC    IOKEY,IOKEY                                                      
         MVC   LP_ADATA,SVADATA                                                 
         MVI   LP_RMODE,LP_RFRST                                                
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Array definition for structure record name download                 *         
***********************************************************************         
                                                                                
ARYSNM   LKOUT A,(D,B#STR,CFMFIRST),EOT=0,                             +        
               ROWID=(RNMD,RNMELQ),ROWWIDTH=(V,RNMLN)                           
                                                                                
RName    LKOUT C,5,RNMNAME,CHAR,ND=Y,LEN=V                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for vendor pointer download                        *         
***********************************************************************         
                                                                                
ARYVPR   LKOUT A,(R,NXTPTR),MULTIROW=Y,FILTROUT=SETVPR,ROWNAME=CFMRECD          
                                                                                
MNode    LKOUT C,1,CFMKVMED,UBIN                                                
RecCd    LKOUT C,2,CFMKCODE,CHAR,FILTROUT=TSTSPTN                               
MktNo    LKOUT C,3,CFMKSMKT,CHAR,FILTROUT=TSTSPTY                               
StaCd    LKOUT C,4,CFMKSSTA,CHAR,FILTROUT=TSTSPTY                               
CblCd    LKOUT C,5,CFMKSCBL,CHAR,FILTROUT=TSTCABY,ND=Y                          
                                                                                
         LKOUT E                                                                
                                                                                
SETVPR   MVI   PTRTYPE,CFMKSVEN    Set vendor pointer record filter             
         BR    RE                                                               
                                                                                
TSTSPTN  CLI   SPOT,NOQ            Test not Spot                                
         BR    RE                                                               
                                                                                
TSTSPTY  CLI   SPOT,YESQ           Test Spot                                    
         BR    RE                                                               
                                                                                
TSTCABY  CLI   CABLE,YESQ          Test Spot cable vendor                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Array definition for client pointer download                        *         
***********************************************************************         
                                                                                
ARYCPR   LKOUT A,(R,NXTPTR),MULTIROW=Y,FILTROUT=SETCPR,ROWNAME=CFMRECD          
                                                                                
MNode    LKOUT C,1,CFMKCMED,UBIN                                                
CltCd    LKOUT C,2,CFMKCLTE,CHAR                                                
CltIC    LKOUT C,3,CFMKCLTI,HEXD,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
SETCPR   MVI   PTRTYPE,CFMKSCLT    Set client pointer record filter             
         BR    RE                                                               
                                                                                
***********************************************************************         
* Array definition for brand pointer download                         *         
***********************************************************************         
                                                                                
ARYBPR   LKOUT A,(R,NXTPTR),MULTIROW=Y,FILTROUT=SETBPR,ROWNAME=CFMRECD          
                                                                                
MNode    LKOUT C,1,CFMKBMED,UBIN                                                
ANode    LKOUT C,2,CFMKBADV,UBIN                                                
CltCd    LKOUT C,3,CFMKBCLT,CHAR                                                
BrdCd    LKOUT C,4,CFMKBRDE,CHAR                                                
BrdIC    LKOUT C,5,CFMKBRDI,HEXD                                                
                                                                                
         LKOUT E                                                                
                                                                                
SETBPR   MVI   PTRTYPE,CFMKSBRD    Set brand pointer record filter              
         BR    RE                                                               
                                                                                
***********************************************************************         
* Download vendor/client/brand pointers                               *         
***********************************************************************         
                                                                                
NXTPTR   LA    R2,IOKEY                                                         
         ST    R2,LP_ADATA                                                      
         USING CFMKEY,R2           R2=A(record key)                             
                                                                                
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTPTR02                                                         
         OC    CFMKRNOD,CFMKRNOD   Test record has children                     
         JZ    NOMORE                                                           
         MVC   SVIOKEY,CFMKEY                                                   
         MVC   CFMKPNOD,CFMKRNOD                                                
         MVC   CFMKSUBR,PTRTYPE                                                 
         XC    CFMKCODE(L'CFMKCODE+L'CFMKRNOD+L'CFMKSPAR),CFMKCODE              
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         J     NXTPTR04                                                         
                                                                                
NXTPTR02 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
                                                                                
NXTPTR04 JE    *+6                                                              
         DC    H'0'                                                             
         CLC   CFMKEY(CFMKCODE-CFMKEY),IOKEYSAV                                 
         JNE   NXTPTRX                                                          
                                                                                
         CLI   CFMKSUBR,CFMKSVEN   Test this is a vendor pointer                
         JNE   EXITY                                                            
         MVI   SPOT,NOQ            Set not spot and not cable                   
         MVI   CABLE,NOQ                                                        
                                                                                
         LA    R1,SPTMED           Test if Spot media                           
         LHI   R0,SPTMEDN                                                       
         BASR  RE,0                                                             
         CLC   CFMKRNOD,0(R1)                                                   
         JNE   *+12                                                             
         MVI   SPOT,YESQ                                                        
         J     EXITY                                                            
         AHI   R1,L'SPTMED                                                      
         BCTR  R0,RE                                                            
                                                                                
         CLC   CFMKRNOD,CABMED     Is this Spot cable media                     
         JNE   EXITY                                                            
         MVI   SPOT,YESQ                                                        
         TM    CFMKSSTA,X'F0'      Test this is a cable tv station              
         JNO   EXITY                                                            
         MVI   CABLE,YESQ                                                       
         J     EXITY                                                            
                                                                                
NXTPTRX  MVC   IOKEY,SVIOKEY       Restore saved key                            
         J     NOMORE                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Request map for MFM structure download                              *         
***********************************************************************         
                                                                                
MFMSDL   LKREQ H,I#MFMSDL,OUTSDL,NEXTREQ=CFMIDL                                 
                                                                                
RNode    LKREQ F,1,(D,B#SAVED,CONTEXT),UBIN,TEXT=CT#RCONT,COL=*                 
Levls    LKREQ F,2,(D,B#SAVED,LEVELS),UBIN,TEXT=CT#NOLEV,COL=*                  
XPtrs    LKREQ F,3,(D,B#SAVED,POINTERS),CHAR,TEXT=CT#NOPTR,COL=*                
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Structure download replies                                          *         
***********************************************************************         
                                                                                
OUTSDL   LKOUT H                                                                
                                                                                
STR2     LKOUT R,18                                                             
Array    LKOUT C,18,(A,ARYSTR)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* Request map for CFM initial download                                *         
***********************************************************************         
                                                                                
CFMIDL   LKREQ *,I#CFMIDL,OUTCID,NEXTREQ=REQEND                                 
                                                                                
OUTCID   LKOUT H                                                                
                                                                                
CIDSEN   LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYSEN),FILTROUT=BLDSEN                                   
         LKOUT E                                                                
                                                                                
CIDADV   LKOUT R,2                                                              
Array    LKOUT C,1,(A,ARYADV)                                                   
         LKOUT E                                                                
                                                                                
CIDAGY   LKOUT R,3                                                              
Array    LKOUT C,1,(A,ARYAGY)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYSEN   LKOUT A,(D,B#SAVED,SE_LST),ROWWIDTH=SE_LSTL,ROWNAME=SE_LST,   +        
               NEWEL=Y,EOT=FF                                                   
                                                                                
SEOvSys  LKOUT C,1,SE_LSEOV,LBIN                                                
SENum    LKOUT C,2,SE_LSENO,HEXD                                                
SEName   LKOUT C,3,SE_LNAME,CHAR                                                
SEAdvNo  LKOUT C,4,SE_LADVN,LBIN                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Build SE_LST for sending                                            *         
***********************************************************************         
                                                                                
BLDSEN   GOTOR VDATAMGR,DMCB,DMREAD,SYSFLES,(0,1)                               
         ICM   R2,15,12(R1)                                                     
         SHI   R2,4                Back up to pointer adcon                     
         L     R2,0(R2)            R2=A(updative ADV table)                     
         GOTOR VSWITCH,DMCB,X'FEFFFFFF'                                         
         L     R3,0(R1)                                                         
         L     RE,SFVSSB-SFSYSFACD(R3)                                          
         L     RE,SSBAFID-SSBD(RE) RE=A(FACPAK id table)                        
         AHI   RE,L'FACITAB        Bump over first entry                        
         ST    RE,VFACITAB                                                      
         L     R3,SFVSELIST-SFSYSFACD(R3)                                       
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3          R3=A(SELIST)                                 
                                                                                
         LA    R4,SE_LST                                                        
         USING SE_LST,R4           R4=A(output array)                           
BLDSEN02 CLI   SEOVSYS,2           Test Spot system                             
         JE    BLDSEN04                                                         
         CLI   SEOVSYS,3           Test Net system                              
         JE    BLDSEN04                                                         
         CLI   SEOVSYS,4           Test Print system                            
         JE    BLDSEN04                                                         
         CLI   SEOVSYS,6           Test Acc system                              
         JNE   BLDSEN06                                                         
BLDSEN04 MVC   SE_LSEOV,SEOVSYS    Build SE_LST entry                           
         MVC   SE_LSENO,SESYS                                                   
         MVC   SE_LNAME,SENAME                                                  
         LLC   R1,SESYS                                                         
         AR    R1,R2                                                            
         MVC   SE_LADVN,0(R1)      Set updative ADV number                      
         AHI   R4,SE_LSTL                                                       
         MVI   SE_LSEOV,FF                                                      
BLDSEN06 BRXLE R3,RE,BLDSEN02                                                   
         J     EXITY                                                            
         DROP  R3,R4                                                            
                                                                                
ARYADV   LKOUT A,(I,B#SAVED,VFACITAB),ROWWIDTH=L'FACITAB,              +        
               ROWNAME=FACITABD,EOT=FF                                          
                                                                                
FacId    LKOUT C,1,FACIID,LBIN                                                  
FacName  LKOUT C,2,FACISN4,CHAR                                                 
FacInds  LKOUT C,3,FACIFL,HEXD                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYAGY   LKOUT A,(R,NXTAGY),ROWNAME=AG_VALS,MULTIROW=Y                          
                                                                                
AgyAlpha LKOUT C,1,AG_ALPHA,CHAR                                                
AgySESpt LKOUT C,2,AG_SESPT,HEXD,ND=Y                                           
AgySENet LKOUT C,3,AG_SENET,HEXD,ND=Y                                           
AgySEPrt LKOUT C,4,AG_SEPRT,HEXD,ND=Y                                           
AgySEAcc LKOUT C,5,AG_SEACC,HEXD,ND=Y                                           
AgySecAg LKOUT C,6,AG_SECAG,CHAR,ND=Y                                           
AgyPrnId LKOUT C,7,AG_PRNCD,CHAR,ND=Y                                           
AgyPrnNm LKOUT C,8,AG_PRNNM,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Resolve agency values for sending                                   *         
***********************************************************************         
                                                                                
NXTAGY   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTAGY04                                                         
                                                                                
         MVI   AGYIND,1            Build single entry agency list               
         LA    R0,AGYLIST          for connected agency                         
         STCM  R0,7,AAGY                                                        
         MVC   AGYLIST(L'LP_AGY),LP_AGY                                         
                                                                                
K        USING CT5KEY,IOKEY        Read access record                           
         XC    K.CT5KEY,K.CT5KEY                                                
         MVI   K.CT5KTYP,CT5KTYPQ                                               
         MVC   K.CT5KALPH,LP_AGY                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         AHI   R2,CT5DATA-CT5REC                                                
         USING CTCAGD,R2           Locate CFM agency list element               
         SR    R0,R0                                                            
NXTAGY02 CLI   CTCAGEL,0           Test end of record                           
         JE    NXTAGY04                                                         
         CLI   CTCAGEL,CTCAGELQ    Test CFM agency list element                 
         JE    *+14                                                             
         IC    R0,CTCAGLEN         No - bump to next                            
         AR    R2,R0                                                            
         J     NXTAGY02                                                         
                                                                                
         LLC   R1,CTCAGLEN                                                      
         SHI   R1,CTCAGLNQ         R1=L'agency list                             
         LR    R0,R1                                                            
         SRL   R0,1                R0=number of agencies in agency list         
         STC   R0,AGYIND                                                        
         LA    R0,AGYLIST                                                       
         LA    RE,CTCAGAGY                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               Move agency list to working storage          
         DROP  R2                                                               
                                                                                
NXTAGY04 SR    R1,R1               Process first/next agency alpha id           
         ICM   R1,1,AGYIND                                                      
         JZ    NOMORE                                                           
         XC    AG_VALS(AG_VALL),AG_VALS                                         
         LA    R0,AG_VALS                                                       
         ST    R0,LP_ADATA         Point to AG_VALS                             
         SHI   R1,1                                                             
         STC   R1,AGYIND                                                        
         ICM   R1,7,AAGY                                                        
         MVC   AG_ALPHA,0(R1)                                                   
         AHI   R1,L'AG_ALPHA                                                    
         STCM  R1,7,AAGY                                                        
                                                                                
K        USING CT5KEY,IOKEY                                                     
         XC    K.CT5KEY,K.CT5KEY                                                
         MVI   K.CT5KTYP,CT5KTYPQ                                               
         MVC   K.CT5KALPH,AG_ALPHA                                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         JNE   EXITY                                                            
         L     R2,AIO1                                                          
         USING CT5REC,R2                                                        
         LA    R2,CT5DATA                                                       
                                                                                
         USING CTSYSD,R2                                                        
NXTAGY06 CLI   CTSYSEL,0           Test end of record                           
         JE    NXTAGY16                                                         
                                                                                
         CLI   CTSYSEL,CTSYSELQ    Test system element                          
         JNE   NXTAGY10                                                         
         LA    R1,AG_SESPT                                                      
         CLI   CTSYSNUM,2          Test Spot system                             
         JE    NXTAGY08                                                         
         LA    R1,AG_SENET                                                      
         CLI   CTSYSNUM,3          Test Net system                              
         JE    NXTAGY08                                                         
         LA    R1,AG_SEPRT                                                      
         CLI   CTSYSNUM,4          Test Print system                            
         JE    NXTAGY08                                                         
         LA    R1,AG_SEACC                                                      
         CLI   CTSYSNUM,6          Test Accounting system                       
         JNE   NXTAGY14                                                         
NXTAGY08 MVC   0(L'CTSYSSE,R1),CTSYSSE                                          
         J     NXTAGY14                                                         
                                                                                
         USING CTSEAD,R2                                                        
NXTAGY10 CLI   CTSEAEL,CTSEAELQ                                                 
         JNE   NXTAGY12                                                         
         MVC   AG_SECAG,CTSEAAID   Set security agency alpha                    
         J     NXTAGY14                                                         
                                                                                
         USING CTDSCD,R2                                                        
NXTAGY12 CLI   CTDSCEL,CTDSCELQ                                                 
         JNE   NXTAGY14                                                         
         MVC   AG_PIDNO,CTDSC      Set principal id number                      
                                                                                
NXTAGY14 LLC   R0,CTDSCLEN         Bump to next element                         
         AR    R2,R0                                                            
         J     NXTAGY06                                                         
         DROP  R2                                                               
                                                                                
NXTAGY16 OC    AG_PIDNO,AG_PIDNO   Test principal id found                      
         JZ    EXITY                                                            
                                                                                
K        USING CTIKEY,IOKEY        Read principal user-id record                
         XC    K.CTIKEY,K.CTIKEY                                                
         MVI   K.CTIKTYP,CTIKTYPQ                                               
         MVC   K.CTIKNUM,AG_PIDNO                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         USING CTIREC,R2                                                        
         LA    R2,CTIDATA                                                       
         USING CTDSCD,R2                                                        
                                                                                
NXTAGY18 CLI   CTDSCEL,0                                                        
         JE    EXITY                                                            
         CLI   CTDSCEL,CTDSCELQ                                                 
         JNE   NXTAGY20                                                         
         MVC   AG_PRNCD,CTDSC      Set principal id code                        
         J     NXTAGY22                                                         
                                                                                
         USING CTORGD,R2                                                        
NXTAGY20 CLI   CTORGEL,CTORGELQ                                                 
         JNE   NXTAGY22                                                         
         MVC   AG_PRNNM,CTORGNAM   Set principal id name                        
                                                                                
NXTAGY22 LLC   R0,CTORGLEN         Bump to next element                         
         AR    R2,R0                                                            
         J     NXTAGY18                                                         
         DROP  R2                                                               
         EJECT                                                                  
REQEND   LKREQ X                                                                
                                                                                
         LKARY T                                                                
         EJECT                                                                  
NOMORE   MVI   LP_RMODE,LP_RLAST   Set no more records to come                  
         J     EXITY                                                            
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
DMREAD   DC    C'DMREAD '                                                       
DMKEY    DC    C'DMKEY  '                                                       
SYSFLES  DC    C'SYSFLES'                                                       
GENDIR   DC    C'GENDIR '                                                       
GENFIL   DC    C'GENFIL '                                                       
CTFILE   DC    C'CTFILE '                                                       
                                                                                
I$MFMIDL DC    AL2(I#MFMIDL)                                                    
I$MFMSDL DC    AL2(I#MFMSDL)                                                    
I$CFMIDL DC    AL2(I#CFMIDL)                                                    
                                                                                
SECFLDS  DS    0XL2                ** Field security table **                   
         DC    AL1(SECPOINT-SECVALS,1)                                          
SECFLDN  EQU   (*-SECFLDS)/L'SECFLDS                                            
                                                                                
MEDKEYT  LKKEY H,CFMKEY,SAVED      ** Media key driver table **                 
         LKKEY LIT,CFMKTYPE,CFMKTYPQ                                            
         LKKEY SIN,CFMKAGY,AGY                                                  
         LKKEY LIT,CFMKPNOD,0                                                   
         LKKEY LIT,CFMKSUBR,CFMKSMED                                            
         LKKEY ALL,CFMKCODE,,CFMKSTAT-CFMKCODE                                  
         LKKEY E                                                                
         EJECT                                                                  
SAVED    DSECT ,                   ** Dsect to cover saved storage **           
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
                                                                                
VFACITAB DS    V                   V(FACPAK id table)                           
                                                                                
SPOTTVQ  EQU   C'T'                Spot tv media code                           
AGY      DS    CL(L'LP_AGY)        Agency code                                  
                                                                                
REQVALS  DS    0X                  ** Request values **                         
                                                                                
CONTEXT  DS    XL(L'CFMKPNOD)      Record context                               
CABMED   DS    XL(L'CFMKRNOD)      Spotpak cable media context                  
SPTMEDN  EQU   6                   Other spotpak media contexts                 
SPTMED   DS    (SPTMEDN)XL(L'CFMKRNOD)                                          
SPTMEDL  EQU   *-SPTMED                                                         
SPOT     DS    C                   Y if a spotpak media                         
CABLE    DS    C                   Y if spotpak cable media                     
LEVELS   DS    X                   Number of levels                             
POINTERS DS    C                   N=no pointers                                
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
OUTVALS  DS    0X                  ** Output values **                          
                                                                                
SECVALS  DS    0X                  ** Security values **                        
SECPOINT DS    C                   Pointer fields                               
                                                                                
HIGHNODE DS    XL(L'CFMPRNOD)      Highest node used so far                     
USERNAME DS    CL(L'CTORGNAM)      User-id name                                 
                                                                                
         ORG   OUTVALS                                                          
SE_LST   DS    0X                                                               
SE_LSEOV DS    XL(L'SEOVSYS)       SE overlay system                            
SE_LSENO DS    XL(L'SESYS)         SE number                                    
SE_LNAME DS    XL(L'SENAME)        SE name                                      
SE_LADVN DS    XL(L'FACIID)        Updatable ADV system number                  
SE_LSTL  EQU   *-SE_LST                                                         
                                                                                
         ORG   SE_LST                                                           
AG_VALS  DS    0X                                                               
AG_ALPHA DS    CL(L'CT5KALPH)                                                   
AG_SESPT DS    XL(L'CTSYSSE)                                                    
AG_SENET DS    XL(L'CTSYSSE)                                                    
AG_SEPRT DS    XL(L'CTSYSSE)                                                    
AG_SEACC DS    XL(L'CTSYSSE)                                                    
AG_SECAG DS    XL(L'CTSEAAID)                                                   
AG_PIDNO DS    XL(L'CTIKNUM)                                                    
AG_PRNCD DS    XL(L'CTIKID)                                                     
AG_PRNNM DS    CL(L'CTORGNAM)                                                   
AG_VALL  EQU   *-AG_VALS                                                        
                                                                                
AGYIND   DS    X                   Number of agencies in AGYLIST                
AAGY     DS    AL3                 A(current AGYLIST entry)                     
                                                                                
AGYLIST  DS    XL256               Agency alpha id list                         
                                                                                
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
CURRLEV  DS    X                   Current nest level                           
ACURRENT DS    A                   A(current keystack entry)                    
SVADATA  DS    AL(L'LP_ADATA)      Saved LP_ADATA value                         
PTRTYPE  DS    XL(L'CFMKSUBR)      Pointer record type                          
SVIOKEY  DS    XL(L'CFMKEY)        Saved IOKEY value                            
KEYSTACK DS    0XL(L'CFMKEY)       Node keys                                    
                                                                                
* Included books follow                                                         
         PRINT OFF                                                              
       ++INCLUDE GELNKWRK                                                       
*PREFIX=SF                                                                      
       ++INCLUDE FASYSFACS                                                      
*PREFIX=                                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FACIDTABD                                                      
FACITABL EQU   *-FACITABD                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GELNK10   11/29/12'                                      
         END                                                                    
