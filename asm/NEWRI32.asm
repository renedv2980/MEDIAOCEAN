*          DATA SET NEWRI32    AT LEVEL 054 AS OF 07/12/04                      
*PHASE T32032A,+0                                                               
*INCLUDE MOBILE                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE SPBVAL                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE GETPROF                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T32032 - NET BILLING INTERFACE'                                 
T32032   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE32**,RR=R2                                                 
         USING T32032,RB,R5,RA                                                  
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         LA    RA,2048(R5)                                                      
         LA    RA,2048(RA)                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R6,ATWA         NOTE R6 TEMPORARILY USED FOR TWA                 
         USING T320FFD,R6                                                       
         L     R8,ASPOOLD      R6 LATER USED FOR GLOBALD                        
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1         ANETWS1/WS2=WORKING STORAGE                   
         USING MYD,R7                                                           
         MVC   MYPID,=C'*MYPID**'                                               
         MVC   NBACLI,ANETWS3     CLIENT RECORD IN W3                           
         MVC   NBANBUFF,ANETWS4   NETWORK BUFFERIN W4                           
         ST    R2,RELO                                                          
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANETAPE                                                       
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,NETCLIH                CLIENT                                 
         NETGO NVCLIALL,DMCB                                                    
         MVC   BHAM,NBACTAM        SET AGY/MED FOR BILL HEAD READ               
         XC    BHCLI,BHCLI                                                      
         CLC   NBSELCLI,=C'ALL'                                                 
         BE    *+10                                                             
         MVC   BHCLI,NBEFFCLI      SET CLIENT                                   
*                                                                               
*                                                                               
         LA    R2,NETPRDH                PRODUCT                                
         NETGO NVPRDALL,DMCB                                                    
         MVC   MYOPTS+11(1),NDPRGRMD                                            
         XC    BHPRD,BHPRD                                                      
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    EDT8                                                             
         CLC   NBSELPRD,=C'POL'                                                 
         BE    EDT8                                                             
         MVC   BHPRD,NBSELPRD                                                   
*                                                                               
EDT8     LA    R2,NETESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB                                                    
         MVC   BHEST,NBSELEST                                                   
         MVC   BHESE,NBSELESE                                                   
*                                                                               
         LA    R2,NETNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
         SPACE                                                                  
* - USE START/END DATE ROUTINE FOR INVOICE DATE FILTER                          
* - NEMEDGEN FILLS USERQSTR/END FOR SSPEC                                       
         LA    R2,NETSTRH               INVOICE START DATE                      
         NETGO NVSTRDAT,DMCB                                                    
         MVC   STRDAT,NBSELSTR                                                  
         GOTO1 DATCON,DMCB,(0,STRDAT),(2,BSTRDAT)                               
*                                                                               
         LA    R2,NETENDH               INVOICE END DATE                        
         NETGO NVENDDAT,DMCB                                                    
         MVC   ENDDAT,NBSELEND                                                  
         GOTO1 DATCON,DMCB,(0,ENDDAT),(2,BENDDAT)                               
         SPACE                                                                  
* - NOW SET IN UNIT START/END FILTERS                                           
***      MVC   NBSELSTR,=C'840101'                                              
***      MVC   NBSELEND,=C'991231'                                              
         MVC   NBSELSTR,=C'860101'                                              
         MVC   NBSELEND,=X'FAF1F1F2F3F1'                                        
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                  DIG OUT ANY OPTIONS                          
EDT14    LA    R2,NETOPTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT50                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         SPACE 1                                                                
*                                                                               
OPT3     DS    0H                                                               
         B     EDINV                                                            
         SPACE 1                                                                
OPT20    LA    R3,32(R3)                                                        
         BCT   R0,OPT3                                                          
         SPACE 1                                                                
EDT50    DS    0H                                                               
         SPACE 1                                                                
         LA    R2,NETCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
EDERR    GOTO1 ERREX                                                            
         DROP  R6                                                               
         EJECT                                                                  
*              REPORT INITIALIZATION                                            
         SPACE 2                                                                
REPMOD   NTR1                                                                   
         B     REP00                                                            
*                                                                               
DDNAME   DC    CL8'NETBTP'                                                      
DSNAME   DC    CL20'NETTAPE.NE0GGAA1'                                           
DCBOPEN  DC    C'N'                                                             
*                                                                               
REP00    DS    0H                                                               
         L     RE,BOOKVAL                                                       
         CLC   =X'90EC',0(RE)                                                   
         BNE   REP1                                                             
         LA    RF,NETBTP                                                        
         MVC   0(128,RE),0(RF)                                                  
         MVC   DSNAME+13(2),NBSELAGY                                            
*****    GOTO1 =V(DYNALLOC),DMCB,DDNAME,DSNAME                                  
         L     R4,ATWA         NOTE R6 TEMPORARILY USED FOR TWA                 
         USING T320FFD,R4                                                       
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,DDNAME,DSNAME                                          
         L     R2,ANETAPE                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         DROP  R4                                                               
*                                                                               
REP1     DS    0H                         INITIALIZATION FOR DRIVER             
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9031EB7'  LOAD T31EB7 (GLOBAL STORAGE)          
         L     R6,DMCB                                                          
         USING GLOBALD,R6                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'  LOAD T00A3A (DRIVER)                  
         MVC   DRIVER,DMCB                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A4D'  LOAD T00A4D (NETWORK DRIVER)          
         MVC   GLASYSDR,DMCB                                                    
         GOTO1 CALLOV,DMCB,0,X'D9032009'  LOAD T32009 (DPG PHASE)               
         MVC   GLAPROG,DMCB                                                     
         SPACE 1                                                                
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         LA    R2,HOOK                                                          
         ST    R2,GLAHOOK                                                       
         MVI   GLFHEADL,10                                                      
         OI    GLINDS,X'04'        ALLTOT                                       
*                                  GET STORAGE FOR BILL FORMULA TABLE           
         OC    ABILLFTB,ABILLFTB                                                
         BNZ   REP10                                                            
         GOTO1 =V(COVAIL),DMCB,C'GET',300000,300000                             
         L     R4,4(R1)                                                         
         L     R2,8(R1)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R4,AERRTBL                                                       
         A     R4,=F'10000'                                                     
         ST    R4,ABILLFTB                                                      
         C     R2,=F'300000'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*              NOW CONTROL NETIO                                                
         SPACE 3                                                                
REP10    MVI   NBDATA,C'U'                                                      
         MVI   NBRESUME,NBPROCPK                                                
         OI    NBSPLOPT,X'C0'      SPLIT ALL                                    
         MVI   NBUSER+13,0         DONT SUPPRESS PREEMPTS                       
         MVC   GLOPTS,MYOPTS       MOVE MYOPTS DATA FROM EDIT TO GLOPTS         
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 DRIVER,DMCB,(R6)                                                 
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    GOTONE                                                           
         CLI   NBMODE,NBREQLST                                                  
         BE    ALLDONE                                                          
         CLI   NBERROR,0                                                        
         BE    GETUNIT                                                          
         DC    H'0'                                                             
*                                                                               
GOTONE   DS    0H                                                               
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'10'           GET BILLING ELEMENT                       
         BAS   RE,GETEL                                                         
         B     *+12                                                             
GT10     MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GETUNIT                                                          
         USING NUBILEL,R4                                                       
         CLC   NUBILPRD,NBSPLPRN       CHECK BILL PRD = UNIT PRD                
         BNE   GT10                                                             
         SPACE 1                                                                
* - FILTER ON BILL ELEMENT BILLING DATE VS REQ START/END DATES                  
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,RUNDATE)                             
         CLC   RUNDATE,STRDAT                                                   
         BL    GT10                                                             
         CLC   RUNDATE,ENDDAT                                                   
         BH    GT10                                                             
         SPACE                                                                  
*                                   IF NEW CLIENT/ NEW SPOT00 PROFILE           
         CLC   NBCLICOD,SAVECLT                                                 
         BE    GET20                                                            
         MVC   SAVECLT,NBCLICOD                                                 
         GOTO1 =A(OVERFLOW),DMCB,(0,(RC))                    0=SETDATE          
* - USES SPOT 00 PROFILE TO SET UP START/END DATE TABLE FOR MOS CALLS           
         SPACE                                                                  
* - SET CURRENT INVOICE NUMBER = MONTH(2) + INVO(4)                             
GET20    MVC   INVNUMBR(2),RUNDMM           SET MONTH (2)                       
         MVC   INVNUMBR+2(4),NUBILNUM       SET INVNO (4)                       
         SPACE                                                                  
         MVC   WORKINV,INVNUMBR    ...SET UP SO GETBYMN RETURNS                 
         MVC   WORKYMD,RUNDATE     ...3BYTE 1(Y/M)+2(YMD) IN WORK               
         BAS   RE,GETBYMN                                                       
         MVC   CURRBINV,WORK          SAVE / BINARY Y/M + INV NO                
         MVC   CURBELEM,NUBILEL       SAVE CURRENT BILLING ELEMENT              
         MVC   CURRPRD,NBSPLPRN       SAVE PROD OF CURR BILL ELEMENT            
         BAS   RE,GETMOS           ..GET MO OF SERVICE OF NBACTDAT              
*                                  .. RETURNED IN BYMOS YR/MOS                  
         EJECT                                                                  
*                                                                               
* - PASS THIS BILLING ELEMENT TO DRIVER THEN GET NEXT BILLELEM                  
* - THIS WAY REPORT CAN HANDLE MULTIPLE BILL ELEMS OF SAME UNIT                 
*                                                                               
         SPACE                                                                  
         BAS   RE,CHKIT            IS THERE BILL REC FOR THIS ELEM              
         BNE   *+8                 NO/SKIP ELEM/PRINT OUT ERROR LATER           
         BAS   RE,DRIVIT                                                        
         BAS   RE,SETUNT                       (RESET UNIT READ)                
         B     GT10                                                             
         SPACE 1                                                                
DRIVIT   NTR1                                                                   
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R6)                                                 
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
CHKIT    NTR1             SEE IF BILL HEADER EXISTS FOR BILLING ELEM            
         LA    R2,KEY                                                           
         USING BILLREC,R2                                                       
         XC    BKEY,BKEY           BUILD BILL REC KEY                           
         MVC   BKEYAM,NBACTAM      FROM UNIT BILLING ELEM                       
         MVC   BKEYCLT,NBACTCLI                                                 
         BAS   RE,GETPRD      USES CURRPRD/RETURNS 3BYTE PRD IN WORK            
         MVC   BKEYPRD,WORK                                                     
         MVC   BKEYEST,NBACTEST                                                 
         MVC   BKEYYSRV,BYMOS        BYMOS SET FROM NBACTDAT                    
         MVC   BKEYMSRV,BYMOS+1                                                 
         MVC   BKEYMBIL(3),CURRBINV    M/Y + 2 BYTE INV NO                      
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         BAS   RE,RDHI             ..DOES BILL REC EXIST FOR                    
         CLC   KEY(13),KEYSAVE     ..THIS BILLING ELEM/INVOICE                  
         BE    CHKX                                                             
*                              ADD ELEMENT TO ERROR TABLE                       
         LA    R3,WORK           SET UP ERROR KEY                               
         USING ERCLT,R3                                                         
         LA    R4,CURBELEM                                                      
         USING NUBILEL,R4                                                       
         MVC   ERCLT,NBCLICOD                                                   
         MVC   EREST,NBACTEST                                                   
         MVC   ERNET,NBACTNET                                                   
         MVC   ERPKG,NBPACK                                                     
         MVC   ERPROG,NBACTPRG                                                  
         MVC   ERDATE,NBACTDAT                                                  
         MVC   ERTYPE,NUBILTYP                                                  
         MVC   ERPROD,BKEYPRD                                                   
         MVC   ERINVNO,NUBILNUM                                                 
         L     R3,AERRTBL              .. IS IT ALREADY IN TABLE                
CHK10    CLI   0(R3),0                                                          
         BE    CHK20                                                            
         CLC   0(ERRKLEN,R3),WORK                                               
         BE    CHKNO                   ..YES/EXIT/BUT CC=NO                     
         LA    R3,ERRORLEN(R3)                                                  
         B     CHK10                                                            
CHK20    MVC   0(ERRORLEN,R3),WORK     ..NO/ADD TO ERROR TABLE                  
*                                                                               
CHKNO    CLC   KEY(13),KEYSAVE         RESET UNEQUAL CONDITION                  
CHKX     B     XIT                                                              
         DROP  R3,R2                                                            
         EJECT                                                                  
*                                  GO AND DO THE OUTPUT                         
         SPACE                                                                  
ALLDONE  DS    0H                                                               
         BAS   RE,BILLHDS          PASS BILL HEADERS TO DRIVER                  
         BAS   RE,AGYHDS           WRITE AGY ADDRESS TO DISK                    
*                                                                               
         MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 DRIVER,DMCB,(R6)                                                 
*                                                                               
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         LA    R1,MYHEAD                                                        
         ST    R1,HEADHOOK                                                      
         MVI   ERRORSW,C'Y'                                                     
         BAS   RE,CHKERRTB          GO PRINT ANY ERROR IN ERRTBL                
         MVI   ERRORSW,0                                                        
         MVI   FORCEHED,C'Y'       START RECORD MAP ON NEW PAGE                 
*                                                                               
         L     R2,AERRTBL                                                       
         GOTO1 =V(COVAIL),DMCB,C'FREE',(R2),300000                              
*                                                                               
         L     R2,ANETAPE          CLOSE OUT TAPE                               
         CLOSE ((2))                                                            
*                                                                               
         DS    0H                   ESTABLISH FOR IN TAPE READ                  
         L     RE,BOOKVAL                                                       
         LA    RF,NETBTPIN                                                      
         MVC   0(128,RE),0(RF)                                                  
*                                                                               
         L     R2,ANETAPE         OPEN TAPE/READ/WRITE                          
         OPEN  ((R2),INPUT)                                                     
*                                                                               
*                                                                               
ALD10    GET   (R2),MYPLEN                                                      
         L     R1,MYPLEN           GET LENGTH                                   
         LA    RE,MYP              POINT TO RECORD                              
         LA    RF,P+1              OUTPUT                                       
         DS    0H                                                               
         MOVE  ((RF),(R1)),(RE)                                                 
         DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     ALD10                                                            
ENDFILE  DS    0H                                                               
         CLOSE ((2))                                                            
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              ESTABLISH ADDRESSES OF MY USER ROUTINES                          
*                                                                               
HOOK     NTR1                              HEAD HOOK                            
*                                                                               
         CLI   GLHOOK,GLRESOLV            INITIALIZATION                        
         BNE   HK100                                                            
         CLC   =C'MYOTCLI',GLLABEL                                              
         BNE   HK11                                                             
         LA    R1,MYOTCLI                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK11     CLC   =C'MYININV',GLLABEL                                              
         BNE   HK12                                                             
         LA    R1,MYININV                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK12     CLC   =C'MYOTINV',GLLABEL                                              
         BNE   HK13                                                             
         LA    R1,MYOTINV                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK13     CLC   =C'MYOTPGRP',GLLABEL                                             
         BNE   HK14                                                             
         LA    R1,MYOTPGRP                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK14     CLC   =C'MYOTPRD',GLLABEL                                              
         BNE   HK15                                                             
         LA    R1,MYOTPRD                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK15     CLC   =C'MYOTEST',GLLABEL                                              
         BNE   HK16                                                             
         LA    R1,MYOTEST                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK16     CLC   =C'MYOTPROG',GLLABEL                                             
         BNE   HK17                                                             
         LA    R1,MYOTPROG                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK17     CLC   =C'MYINUN',GLLABEL                                               
         BNE   HK18                                                             
         LA    R1,MYINUN                                                        
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK18     CLC   =C'MYOTUN',GLLABEL                                               
         BNE   HK19                                                             
         LA    R1,MYOTUN                                                        
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK19     CLC   =C'MYINBDET',GLLABEL                                             
         BNE   HK20                                                             
         LA    R1,MYINBDET                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK20     CLC   =C'MYOTBDET',GLLABEL                                             
         BNE   HK21                                                             
         LA    R1,MYOTBDET                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK21     CLC   =C'MYINBGRS',GLLABEL                                             
         BNE   HK22                                                             
         LA    R1,MYINBGRS                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK22     CLC   =C'MYOTBGRS',GLLABEL                                             
         BNE   HK23                                                             
         LA    R1,MYOTBGRS                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK23     CLC   =C'MYINBNET',GLLABEL                                             
         BNE   HK24                                                             
         LA    R1,MYINBNET                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK24     CLC   =C'MYOTBNET',GLLABEL                                             
         BNE   HK25                                                             
         LA    R1,MYOTBNET                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK25     CLC   =C'MYINBBIL',GLLABEL                                             
         BNE   HK26                                                             
         LA    R1,MYINBBIL                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK26     CLC   =C'MYOTBBIL',GLLABEL                                             
         BNE   HK27                                                             
         LA    R1,MYOTBBIL                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
HK27     DS    0H                                                               
         CLC   =C'MYOTMED',GLLABEL                                              
         BNE   HK28                                                             
         LA    R1,MYOTMED                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
*                                                                               
HK28     DS    0H                                                               
         CLC   =C'MYOTFUN',GLLABEL                                              
         BNE   HK29                                                             
         LA    R1,MYOTFUN                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
*                                                                               
HK29     DS    0H                                                               
         CLC   =C'MYINCOM',GLLABEL                                              
         BNE   HK30                                                             
         LA    R1,MYINCOM                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
*                                                                               
HK30     DS    0H                                                               
         CLC   =C'MYINITOT',GLLABEL                                             
         BNE   HK31                                                             
         LA    R1,MYINITOT                                                      
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
*                                                                               
HK31     DS    0H                                                               
         CLC   =C'MYOITOT',GLLABEL                                              
         BNE   HK32                                                             
         LA    R1,MYOITOT                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
*                                                                               
HK32     DS    0H                                                               
         CLC   =C'MYOTFBD',GLLABEL                                              
         BNE   HK33                                                             
         LA    R1,MYOTFBD                                                       
         ST    R1,GLAROUT                                                       
         B     HKXIT                                                            
*                                                                               
HK33     DS    0H                                                               
*                                                                               
HK100    CLI   GLHOOK,GLROUT               MYHEAD ROUTINE                       
         BNE   HKXIT                                                            
*        CLI   GLMODE,GLOUTPUT                                                  
*        BE    HK110                                                            
*        CLI   GLMODE,GLOUTPUT                                                  
*        BE    HK110                                                            
*        CLI   GLMODE,GLINPUT                                                   
*        BNE   HKXIT                                                            
HK110    CLC   =C'MY',GLLABEL                                                   
         BNE   HKXIT                                                            
         L     RF,GLAROUT                                                       
         BASR  RE,RF                                                            
HKXIT    B     XIT                                                              
*                                                                               
         EJECT                                                                  
* - MEDIA                                                                       
MYOTMED  NTR1                                                                   
         L     R2,GLAIFLD                                                       
         XC    MYP,MYP                                                          
         LA    R4,MYP                                                           
         MVC   0(2,R4),=C'ME'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                       FIELD DELIMITER                
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'          NETPAK                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                       FIELD DELIMITER                
         LA    R4,1(R4)                                                         
         MVC   0(1,R4),0(R2)        NET,CABLE,SYN ETC.                          
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                        FIELD DELIMITER               
         LA    R4,1(R4)                                                         
         LA    R3,MEDTBL                                                        
         LA    RE,4                                                             
MOM10    CLC   0(1,R2),0(R3)                                                    
         BNE   MOM12                                                            
         MVC   0(15,R4),0(R3)                                                   
         B     MOM15                                                            
MOM12    LA    R3,15(R3)                                                        
         BCT   RE,MOM10                                                         
         MVC   0(5,R4),=C'OTHER'                                                
MOM15    LA    R1,15                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'15'                                                      
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
*                                                                               
MEDTBL   DS    0H                                                               
         DC    CL15'NETWORK'                                                    
         DC    CL15'CABLE'                                                      
         DC    CL15'SYNDICATION'                                                
         DC    CL15'OTHER'                                                      
         EJECT                                                                  
MYOTCLI  NTR1                                                                   
         NETGO NVSETSPT,DMCB                                                    
         L     R2,GLAIFLD    3 BYTES=AM/CLICODE + 3 BYTES=NNN CLI CODE          
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),0(R2)                                                   
         L     R3,ANETWS3          CLIENT RECORD SITS IN ANETWS3                
         CLC   KEY(13),0(R3)                                                    
         BE    MOC10               ALREADY HAVE IT                              
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         BAS   RE,RDHI                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,ANETWS3                                                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         BAS   RE,SETUNT           RESET UNIT READ SEQ                          
         L     R3,AIO                                                           
         USING CKEY,R3                                                          
*                                                                               
MOC10    XC    MYP,MYP                                                          
         LA    R4,MYP                                                           
         MVC   0(2,R4),=C'CL'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                       FIELD DELIMITER                
         LA    R4,1(R4)                                                         
         MVC   0(3,R4),3(R2)       CLIENT CODE                                  
         LA    R4,3(R4)                                                         
         MVI   0(R4),X'FA'                       FIELD DELIMITER                
         LA    R4,1(R4)                                                         
         MVC   0(8,R4),CCLTIFC      CLIENT INTERFACE CODE                       
         LA    R1,8                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                        FIELD DELIMITER               
         LA    R4,1(R4)                                                         
         MVC   0(20,R4),CNAME        CLIENT NAME                                
         LA    R1,20               MAX INPUT LENGTH                             
         BAS   RE,INPUTLN          RETURN LEN OF SIGNIF INPUT IN R1             
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                        FIELD DELIMITER               
         LA    R4,1(R4)                                                         
         MVC   0(1,R4),COFFICE                                                  
         CLI   0(R4),0             EMPTY                                        
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                        FIELD DELIMITER               
         LA    R4,1(R4)                                                         
         OC    CACCOFC,CACCOFC     LAST FIELD EMPTY                             
         BZ    MOCL40              YES                                          
         MVC   0(2,R4),CACCOFC                                                  
         LA    R1,2                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
MOCL40   MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'15'               END OF RECORD DELIMITER                
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
MYININV  NTR1                                                                   
         XC    MYP,MYP      BUILD INVNUMBER + BILL REC KEY IN MYP               
         LA    R2,MYP                                                           
         MVC   0(7,R2),=C'INVOICE'   TO PRINT IN HEADER                         
         MVC   16(6,R2),INVNUMBR        MM + NNNN                               
         EDIT  (B1,BYMOS),(2,23(R2))     ALSO YYMM OF SERVICE                   
         EDIT  (B1,BYMOS+1),(2,25(R2)),FILL=0                                   
         LA    R2,28(R2)                                                        
         USING BILLREC,R2                                                       
*                                                                               
         CLI   BHREAD,C'Y'         ..ARE WE IN BILL READ MODE                   
         BNE   MINI05                                                           
         MVC   BKEY(13),KEY        ..YES/KEY = BILL REC                         
         EDIT  (B1,KEY+8),(2,MYP+23)     ALSO YYMM OF SERVICE                   
         EDIT  (B1,KEY+9),(2,MYP+25),FILL=0                                     
         B     MINI40              ..AND THAT'S ALL FOLKS!                      
*                                                                               
MINI05   XC    BKEY,BKEY           BUILD BILL REC KEY                           
         MVC   BKEYAM,NBACTAM      FROM UNIT BILLING ELEM                       
         MVC   BKEYCLT,NBACTCLI                                                 
         BAS   RE,GETPRD      USES CURRPRD/RETURNS 3BYTE PRD IN WORK            
         MVC   BKEYPRD,WORK                                                     
         MVC   BKEYEST,NBACTEST                                                 
         MVC   BKEYYSRV,BYMOS        BYMOS SET FROM NBACTDAT                    
         MVC   BKEYMSRV,BYMOS+1                                                 
         MVC   BKEYMBIL(3),CURRBINV    M/Y + 2 BYTE INV NO                      
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(13),BKEY                                                     
         BAS   RE,RDHI               DOES BILL REC EXIST FOR                    
         CLC   KEY(13),KEYSAVE       THIS BILLING ELEM/INVOICE                  
         BE    *+8                                                              
         MVI   INVERROR,C'Y'         NO/SET FLAG                                
*                                                                               
* - ADD BILL FORMULA TO TABLE                                                   
         BAS   RE,GTREC2           USES MYIO2                                   
         SR    RF,RF                                                            
         L     R1,ABILLFTB         BILL FORMULA TABLE                           
         L     R4,AIO                                                           
         USING BILLREC,R4                                                       
         USING BFORMTB,R1                                                       
MINI20   CLC   BFTKEY,0(R4)        IS IT ALREADY IN TABLE                       
         BE    MINI30              YES                                          
         CLI   1(R1),0             NO MORE ENTRIES/ADD IT                       
         BE    MINI25                                                           
         LA    R1,BFTLENE(R1)                                                   
         LA    RF,1(RF)                                                         
         C     RF,=F'10000'        10000=MAX ENTRIES                            
         BNH   MINI20                                                           
         DC    H'0'                KABOOM/NO MORE ROOM                          
MINI25   MVC   BFTKEY,0(R4)        ADD BILL KEY                                 
         MVC   BFTFORM,BILBFB       AND BILL FORMULA                            
         PACK  DUB,BAMT             SAVE BILLAMT FOR MYINITOT                   
         CVB   R1,DUB                                                           
         STCM  R1,15,INVAMT                                                     
*                                                                               
MINI30   NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
*                                                                               
MINI40   L     R2,GLAIFLD              PASS TO DRIVER                           
         MVC   0(41,R2),MYP                                                     
         B     XIT                                                              
         DROP  R1,R2,R4                                                         
         EJECT                                                                  
*-READ BILL RECORD INTO MYIO2 / SITS THERE SO MYOTBDET CAN ACCESS               
*                                                                               
MYOTINV  NTR1                                                                   
         L     R2,GLAIFLD                                                       
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(13),28(R2)                                                   
         BAS   RE,RDHI                                                          
         CLC   KEY(13),KEYSAVE     IF BAD KEY                                   
         BNE   XIT                 XIT AND CATCH AT BILLDET                     
         BAS   RE,GTREC2           PUTS BILL REC INTO MYIO2                     
         BAS   RE,SETUNT                                                        
MOTI10   L     R3,AIO                                                           
         USING BKEY,R3                                                          
         XC    MYP,MYP                                                          
         LA    R4,MYP                                                           
         MVC   0(2,R4),=C'IH'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(6,R4),16(R2)      INVOICE NUMBER                               
         LA    R4,6(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(10,R4),=10C'X'    EXPANDED INVOICE NUMBER                      
         LA    R4,10(R4)                                                        
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(6,R4),BQDATE      INVOICE DATE                                 
         LA    R4,6(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(0,0(R4))                               
         LA    R4,6(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(6,R4),BDATE         RUN DATE                                   
         LA    R4,6(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(1,R4),BTYPE+1     BILLING TYPE                                 
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'         REVERSAL                                      
         TM    BILSTAT2,BSTRVSLQ                                                
         BNO   *+8                                                              
         MVI   0(R4),C'Y'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'                                                       
         TM    BILSTAT,BSTMANQ    MANUAL                                        
         BNO   *+8                                                              
         MVI   0(R4),C'Y'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'        CLEARED                                        
         TM    BILSTAT2,BSTCLRDQ                                                
         BNO   *+8                                                              
         MVI   0(R4),C'Y'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'                                                       
         TM    BILSTAT,BSTCMONQ    COMMISSION                                   
         BNO   *+8                                                              
         MVI   0(R4),C'Y'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'                                                       
         TM    BILSTAT,BSTSCOMQ    SEPARATE COMMISSION                          
         BNO   *+8                                                              
         MVI   0(R4),C'Y'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'                                                       
         TM    BILSTAT,BSTSADJQ    SEPARATE ADJUSTMENT                          
         BNO   *+8                                                              
         MVI   0(R4),C'Y'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'                                                       
         TM    BILSTAT,BSTTAORQ    AOR BILLS                                    
         BNO   *+8                                                              
         MVI   0(R4),C'Y'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'15'         END OF RECORD                                
*                                                                               
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
MYOTPGRP NTR1                    PRODUCT GROUP                                  
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         MVC   0(6,R3),0(R2)                                                    
         MVC   WORK(10),0(R2)                                                   
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
MYOTPRD  NTR1                                                                   
         L     R2,GLAIFLD                                                       
         NETGO NVSETSPT                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(6),0(R2)      AM/CLT/PROD                                  
         BAS   RE,RDHI                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GTREC                                                         
         L     R3,AIO                                                           
         USING PKEY,R3                                                          
         XC    MYP,MYP                                                          
         LA    R4,MYP                                                           
         MVC   0(2,R4),=C'PR'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(3,R4),3(R2)       PRODUCT CODE                                 
         LA    R1,3                                                             
         BAS   RE,INPUTLN          RETURNS SIGNIF INPUT LEN                     
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         CLI   PACCT,X'FF'         ..IS PROD NUMBER 4CL OR 5CL                  
         BNE   MOP15                                                            
         UNPK  0(5,R4),PACCT+1(3)  ..5CL                                        
         LA    R4,5(R4)                                                         
         B     *+14                                                             
MOP15    MVC   0(4,R4),PACCT       ..4CL                                        
         LA    R4,4(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(20,R4),PNAME                                                   
         LA    R1,20                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'15'        END OF RECORD MARKER                          
         BAS   RE,SETUNT                                                        
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
MYOTEST  NTR1                                                                   
         L     R2,GLAIFLD                                                       
         XC    KEY,KEY                                                          
         NETGO NVSETSPT,DMCB                                                    
         MVC   KEY+1(6),1(R2)                                                   
         MVC   KEY+7(1),0(R2)                                                   
         BAS   RE,RDHI                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GTREC                                                         
         XC    MYP,MYP                                                          
         LA    R4,MYP                                                           
         MVC   0(2,R4),=C'ES'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         EDIT  (B1,0(R2)),(3,0(R4)),ALIGN=LEFT   EST NUMBER                     
         LA    R1,3                                                             
         BAS   RE,INPUTLN       RETURNS SIGNIF INPUT LEN                        
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         L     R2,AIO                                                           
         USING ESTHDR,R2                                                        
         MVC   0(20,R4),EDESC        ESTIMATE NAME                              
         LA    R1,20                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'         NO 2ND EST DESCRIPTION                       
         MVI   1(R4),X'15'         END OF RECORD DELIMITER                      
         BAS   RE,PUTIT                                                         
         BAS   RE,SETUNT                                                        
         B     XIT                                                              
*                                                                               
         SPACE                                                                  
*                                                                               
MYOTPROG NTR1                                                                   
         L     R2,GLAIFLD                                                       
         XC    MYP,MYP                                                          
         LA    R4,MYP                                                           
         MVC   0(2,R4),=C'PM'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         OC    0(6,R2),0(R2)       BILL HEADER READ/SO NO PROG                  
         BZ    MYOPX                                                            
         MVC   0(6,R4),0(R2)       PROGRAM CODE                                 
         LA    R1,6                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(16,R4),6(R2)      PROGRAM NAME                                 
         LA    R1,16                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
MYOPX    MVI   0(R4),X'15'        RECORD DELIMITER                              
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
MYINUN   NTR1                 SET DISK ADDRESS PLUS                             
*                             UNIT DESCRIPTION EXCEPT FOR COMMENTS              
*                             COMMENTS CARRIED IN DATA SECTION                  
         DROP  R3                                                               
         L     R3,GLAIFLD                                                       
         USING INUND,R3                                                         
*                                                                               
         CLI   BHREAD,C'Y'         ARE WE IN BILL READ MODE                     
         BNE   *+14                                                             
         XC    INUNDAD,INUNDAD     YES / PASS ZEROS                             
         B     MINUX                                                            
*                                                                               
         MVC   INUNDAD,NBKEY+21    SET DISK ADDRESS OF UNIT                     
         MVC   INUNNET,NBACTNET                          NETWORK                
*                                                                               
         MVC   INUNDAT,NBACTDAT                                                 
*        GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,UDATE)       AIR DATE                
*                                                                               
* - TIME 0600-3000                                                              
         BAS   RE,GETTIME          RETURNED IN WORK                             
         MVC   INUNTIME,WORK+20                                                 
         MVC   INUNSUB,NBACTSUB                                                 
         MVC   INUNLEN,NBLEN                                                    
         MVC   INUNPAK,NBPACK                                                   
         MVC   INUNDROT,NBSDROT                                                 
         CLI   NBPRD2,0            IS THERE PIGGY PRODUCT                       
         BE    INUN10                                                           
         CLC   NBPRD,CURRPRD       ARE WE DEALING WITH 1ST PROD                 
         BNE   INUN5                                                            
         L     R0,=F'10000'        YES/GET SHARE OF 2ND PROD                    
         MVC   HALF,NBP1SHR                                                     
         LH    R1,HALF                                                          
         SR    R0,R1                                                            
         STCM  R0,3,INUNPSHR                                                    
         MVC   BYTE,CURRPRD           SAVE CURRENT PRODUCT                      
         MVC   CURRPRD,NBPRD2         SET 2ND PROD                              
         BAS   RE,GETPRD              RETURNS 3BYTE CODE IN WORK                
         MVC   INUNPRD,WORK                                                     
         MVC   CURRPRD,BYTE           RESET ORIGINAL                            
         B     INUN10                                                           
* - WE ARE DEALING WITH 2ND PROD                                                
INUN5    MVC   INUNPSHR,NBP1SHR       SET 1ST PROD SHARE                        
         MVC   BYTE,CURRPRD           SAVE CURRENT PRODUCT                      
         MVC   CURRPRD,NBPRD          SET 1ST PROD                              
         BAS   RE,GETPRD              RETURNS 3BYTE CODE IN WORK                
         MVC   INUNPRD,WORK                                                     
         MVC   CURRPRD,BYTE           RESET ORIGINAL                            
*                                                                               
INUN10   DS    0H                                                               
         BAS   RE,GETMOS               USES NBACTDAT/RETURNS IN BYMOS           
         EDIT  (B1,BYMOS),(2,INUNYM),FILL=0        YY                           
         EDIT  (B1,BYMOS+1),(2,INUNYM+2),FILL=0    MM                           
         L     R4,NBAIO          GET NON-SPLIT $ FIGURES FROM UNIT              
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R4                                                      
         MVC   INUNACT,NUACTUAL                                                 
         OC    NUACTUAL,NUACTUAL   IF ACTUAL=ZERO                               
         BNZ   INUN20                                                           
         TM    NUUNITST,X'20'       WAS ACTUAL COST INPUT                       
         BNO   INUN20                                                           
         MVC   INUNACT,=4X'FF'     SET ACT=ZERO=ACT INPUT                       
INUN20   MVC   INUNASS,NUASSIGN                                                 
         MVC   INUNINTG,NUINTEG                                                 
MINUX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
MYOTUN   NTR1                                                                   
         B     XIT                                                              
*                                                                               
MYOTFUN  NTR1                  FIRST FOR UNIT OUT                               
         L     R2,GLAIFLD                                                       
         USING INUND,R2                                                         
         XC    MYP,MYP                                                          
         LA    R4,MYP                                                           
*                                                                               
         OC    INUNDAD,INUNDAD     BILL READ MODE                               
         BNZ   OTFN5                                                            
         MVC   0(2,R4),=C'UD'      YES                                          
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'         AND THAT'S ALL                               
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'15'                                                      
         B     COM40                                                            
*                                                                               
OTFN5    DS    0H                  SET UP FOR PRINTED REPORT                    
         MVC   4(4,R4),INUNNET                        NETWORK                   
         GOTO1 DATCON,DMCB,(2,INUNDAT),(5,10(R4))    DATE                       
         MVI   18(R4),C'-'                                                      
         EDIT  (B1,INUNSUB),(3,19(R4)),ALIGN=LEFT     SUB-LINE                  
         EDIT  (B1,INUNPAK),(3,24(R4))                PACKAGE                   
         L     R3,GLAOFLD                                                       
         MVC   0(30,R3),MYP                                                     
*                                                                               
         LA    R4,MYP          NOW SET UP RECORD                                
         XC    MYP,MYP                                                          
         MVC   0(2,R4),=C'UD'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(4,R4),INUNNET     NETWORK                                      
         LA    R1,4                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         GOTO1 DATCON,DMCB,(2,INUNDAT),(0,0(R4))       AIR DATE                 
         LA    R4,6(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(4,R4),INUNTIME      START TIME                                 
         LA    R4,4(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(4,R4),INUNTIME+4     END TIME                                  
         LA    R4,4(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         EDIT  (B1,INUNSUB),(3,0(R4)),ALIGN=LEFT      LINE NUMBER               
         LA    R1,3                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         EDIT  (B1,INUNLEN),(3,0(R4)),ALIGN=LEFT       SECONDS LENGTH           
         LA    R1,3                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         EDIT  (B1,INUNPAK),(3,0(R4)),ALIGN=LEFT       PACKAGE                  
         LA    R1,3                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         LA    R1,INUNDROT                                                      
         TM    0(R1),X'40'         MON                                          
         BNO   *+12                                                             
         MVI   0(R4),C'1'                                                       
         B     OTFN10                                                           
         TM    0(R1),X'20'         TUE                                          
         BNO   *+12                                                             
         MVI   0(R4),C'2'                                                       
         B     OTFN10                                                           
         TM    0(R1),X'10'         WED                                          
         BNO   *+12                                                             
         MVI   0(R4),C'3'                                                       
         B     OTFN10                                                           
         TM    0(R1),X'08'         THUR                                         
         BNO   *+12                                                             
         MVI   0(R4),C'4'                                                       
         B     OTFN10                                                           
         TM    0(R1),X'04'         FRI                                          
         BNO   *+12                                                             
         MVI   0(R4),C'5'                                                       
         B     OTFN10                                                           
         TM    0(R1),X'02'         SAT                                          
         BNO   *+12                                                             
         MVI   0(R4),C'6'                                                       
         B     OTFN10                                                           
         TM    0(R1),X'01'         SUN                                          
         BNO   *+12                                                             
         MVI   0(R4),C'7'                                                       
OTFN10   LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         OC    INUNPRD,INUNPRD             ...IS THERE PIGGY PROD               
         BNZ   OTFN15                                                           
         MVI   0(R4),X'FA'                   ...NO/PASS DELIMITERS              
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'         DELIMITER                                    
         LA    R4,1(R4)                                                         
         B     OTFN20                                                           
OTFN15   MVC   0(3,R4),INUNPRD               ...YES/PRODUCT                     
         LA    R1,3                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         EDIT  (B2,INUNPSHR),(6,0(R4)),2,ALIGN=LEFT  SHARE                      
         LA    R1,6                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
OTFN20   DS    0H                                                               
         MVC   0(4,R4),INUNYM              YYMM OF SERVICE                      
         LA    R4,4(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         OC    INUNACT,INUNACT     ..IF NO ACTUAL COST                          
         BNZ   OTFN21                                                           
         MVI   0(R4),X'FA'         PASS DELIMITER                               
         LA    R4,1(R4)                                                         
         B     OTFN24                                                           
OTFN21   CLC   INUNACT,=4X'FF'     ..ELSE TEST ZERO ACT COST INPUT              
         BNE   OTFN23                                                           
         MVC   0(4,R4),=C'0.00'    PASS ZERO $ ACT COST                         
         LA    R4,4(R4)                                                         
         B     OTFN24                                                           
OTFN23   EDIT  (B4,INUNACT),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT                     
         LA    R1,12                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
OTFN24   MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         OC    INUNASS,INUNASS     IF NO ASS COST                               
         BNZ   OTFN26                                                           
         MVI   0(R4),X'FA'         PASS DELIMITER                               
         LA    R4,1(R4)                                                         
         B     OTFN28                                                           
OTFN26   EDIT  (B4,INUNASS),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT                     
         LA    R1,12                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
OTFN28   MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         OC    INUNINTG,INUNINTG   IF NO INTEG                                  
         BNZ   OTFN30                                                           
         MVI   0(R4),X'FA'         PASS DELIMITER                               
         LA    R4,1(R4)                                                         
         B     COM10               AND CHECK COMMENTS                           
OTFN30   EDIT  (B4,INUNINTG),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT                    
         LA    R1,12                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
*                                                                               
* - ARE THERE COMMENTS                                                          
* - HARD CODED - COMMENTS PASSED IN DATA SECTION OF DPG RECORD                  
* - SEE DPG MODULE FOR LENGTH                                                   
COM10    LA    R2,106(R2)           HARD CODED LENGTH                           
         CLI   0(R2),X'FF'          ARE THERE COMMENTS                          
         BNE   COM20                                                            
         MVI   0(R4),X'15'          NO/SET END OF RECORD                        
         B     COM40                                                            
COM20    DS    0H                                                               
         MVI   0(R4),X'FA'          YES/SET FIELD DELIMITER                     
         LA    R4,1(R4)                                                         
         MVC   0(60,R4),0(R2)         MOVE COMMENTS TO MYP LINE                 
         CLI   60(R2),X'FF'        ..IS THERE 2ND COMM FIELD                    
         BE    COM22               ..NO                                         
         LA    R4,60(R4)           ..YES                                        
         MVI   0(R4),X'FA'         ..SET END FOR 1ST COM FIELD                  
         LA    R4,1(R4)                                                         
         LA    R2,60(R2)           ..BUMP TO NEXT COMMENT AREA                  
         MVC   0(60,R4),0(R2)         ..AND MOVE IT TO MYP                      
COM22    DS    0H                 GET END OF COMMENTS                           
         LA    R1,60                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'15'         SET END OF RECORD                            
*                                                                               
COM40    BAS   RE,PUTIT                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
MYINBDET NTR1                                                                   
         XC    MYWORK,MYWORK       USED FOR TEMP STORAGE                        
         XC    MYWORK2,MYWORK2       USED FOR TEMP STORAGE                      
         XC    INVNUMSV,INVNUMSV   USED FOR TEMP STORAGE                        
         L     R2,GLAIFLD                                                       
         USING MINBD,R2                                                         
*                                                                               
         CLI   BHREAD,C'Y'         BILL READ MODE                               
         BNE   MINB0                                                            
         MVC   MBDGRS,GROSSV       YES/SET $ FROM BILL REC READ                 
         MVC   MBDNET,NETSV                                                     
         MVC   MBDAMT,BILLAMSV                                                  
         B     MYIBXX                                                           
*                                                                               
MINB0    LA    R4,CURBELEM                                                      
         USING NUBILEL,R4                                                       
         MVC   MBDTYP,NUBILTYP                                                  
         MVC   CURBTYP,NUBILTYP                                                 
         CLI   INVERROR,C'Y'                CAN'T READ BILLING RECORD           
         BE    MINI50                       SET ERROR                           
* - SET UP BILL HEADER KEY TO READ BILL FORMULA TABLE                           
         LA    R4,MYKEY                                                         
         USING BKEY,R4                                                          
         XC    MYKEY,MYKEY                                                      
         MVC   BKEYAM,NBACTAM                                                   
         MVC   BKEYCLT,NBACTCLI                                                 
         BAS   RE,GETPRD           USES CURRPRD/RETURNS 3 BYTE PROD             
         MVC   BKEYPRD,WORK                                                     
         MVC   BKEYEST,NBACTEST                                                 
         MVC   BKEYYSRV,BYMOS                                                   
         MVC   BKEYMSRV,BYMOS+1                                                 
         MVC   BKEYMBIL(3),CURRBINV                                             
         L     R4,ABILLFTB                                                      
         USING BFORMTB,R4                                                       
MINI5    CLI   1(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                HAS TO BE HERE                               
         CLC   MYKEY(13),0(R4)                                                  
         BE    *+12                                                             
         LA    R4,BFTLENE(R4)                                                   
         B     MINI5                                                            
         LA    R4,BFTFORM          GET ADDRESS OF BILL FORMULA                  
         L     R3,=A(MYIO2)                                                     
         GOTO1 =V(SPBVAL),DMCB,(C'U',CURBELEM),0(R3),0(R4)                      
         USING SPBVALD,R3                                                       
         MVC   MBDGRS,SPBVEGRS    GROSS                                         
         MVC   MBDNET,SPBVENET    NET                                           
         MVC   MBDAMT,SPBVACT     ACTUAL AMOUNT                                 
         MVC   GROSSV,SPBVEGRS    SAVE FOR IN BILL DETAIL                       
         MVC   NETSV,SPBVENET      "                                            
         MVC   BILLAMSV,SPBVACT    "                                            
         DROP  R4                                                               
* - SET UP TO FIND PREVIOUS BILL NUMBER AND PREVIOUS GROSS AND NET              
         L     R4,NBAIO                                                         
         USING NUBILEL,R4                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     MINI10                                                           
MINI7    BAS   RE,NEXTEL                                                        
         BNE   MYIBX                                                            
MINI10   CLC   CURBELEM,0(R4)      IS IT CURRENT BILL ELEM                      
         BE    MINI7               YES/ANY MORE                                 
         CLC   NUBILTYP,CURBTYP    MUST BE SAME TYPE                            
         BNE   MINI7                                                            
         CLC   NUBILPRD,CURRPRD    MUST BE SAME PRODUCT                         
         BNE   MINI7                                                            
* BUILD INVOICE NUMBER MOS(2)+INV IN WORKINV                                    
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,WORKYMD)                             
         MVC   WORKINV(2),WORKYMD+2                  MOVE MONTHS                
         MVC   WORKINV+2(4),NUBILNUM                 INVOICE NUMBER             
* ONLY ACCEPT LOWER INVOICE NUMBERS                                             
         CLC   WORKINV(6),INVNUMBR    IS IT PREVIOUS                            
         BNL   MINI7               NO/REJECT                                    
         CLI   INVNUMSV,0          FIRST TIME                                   
         BE    MYINB30             YES                                          
         CLC   INVNUMSV,WORKINV    SAVE HIGHEST INV                             
         BH    *+10                                                             
MYINB30  MVC   INVNUMSV,WORKINV    SAVE THE NUMBER                              
* - SET UP BILL HEADER KEY TO READ BILL FORMULA TABLE                           
         LA    R1,MYKEY                                                         
         USING BKEY,R1                                                          
         BAS   RE,GETBYMN          KEY IS SAME BUT FOR NUBILDAT                 
         MVC   BKEYMBIL(3),WORK                                                 
         L     R1,ABILLFTB                                                      
         USING BFORMTB,R1                                                       
MINI45   CLI   1(R1),0                                                          
         BNE   MINI47                                                           
*                                                                               
         ST    R1,FULL             SAVE POINTER INTO BILL FORM TABLE            
         NETGO NVSETSPT,DMCB       READ BILL HEADER FOR PREVIOUS                
         XC    KEY,KEY                                                          
         MVC   KEY(13),MYKEY                                                    
         BAS   RE,RDHI                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         BNE   MINI7        IF NO BILLREC FOR THIS PREVIOUS/SKIP                
*                           SHOULD ADD TO ERROR TABLE                           
         BAS   RE,GTREC2                                                        
         BAS   RE,SETUNT                                                        
         L     R1,FULL            RESET POINTER INTO BILL FORM TABLE            
         MVC   0(13,R1),KEY        SET KEY                                      
         L     RE,=A(MYIO2)        POINT TO REC                                 
         USING BKEY,RE                                                          
         MVC   13(5,R1),BILBFB     SET BILL FROMULA                             
         B     MINI48                                                           
*                                                                               
MINI47   CLC   MYKEY(13),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,BFTLENE(R1)                                                   
         B     MINI45                                                           
MINI48   LA    R1,BFTFORM          GET ADDRESS OF BILL FORMULA                  
         ST    R2,FULL             SAVE R2=GLAIFLD                              
         LR    R2,R1               SET BILLFORMULA TO R2                        
         LA    R3,MYWORK                                                        
         GOTO1 =V(SPBVAL),DMCB,(C'U',0(R4)),0(R3),0(R2)                         
         USING SPBVALD,R3                                                       
         ICM   R2,15,FULL          RESTORE R2                                   
         ICM   R1,15,SPBVEGRS      SAVE PREV GROSS/NET IN DUB/DUB+4             
         ICM   R0,15,MYWORK2                                                    
         AR    R1,R0                                                            
         STCM  R1,15,MYWORK2                                                    
         ICM   R1,15,SPBVENET                                                   
         ICM   R0,15,MYWORK2+4                                                  
         AR    R1,R0                                                            
         STCM  R1,15,MYWORK2+4                                                  
         B     MINI7                                                            
MINI50   MVC   1(10,R2),=C' **ERROR**'                                          
         XC    CURBELEM,CURBELEM                                                
         MVI   INVERROR,0                                                       
MYIBX    OC    INVNUMSV,INVNUMSV   ANY PREVIOUS INVOICES                        
         BZ    MYIBXX              NO/EXIT                                      
         MVC   MBDPINV,INVNUMSV    SET MOST PREVIOUS INV NUM                    
         MVC   MBDPGRS,MYWORK2    SET PREV GROSS                                
         MVC   MBDPNET,MYWORK2+4  SET PREV NET                                  
         MVC   PREVINUM,INVNUMSV   SAVE FOR PRINTED REPORT                      
         MVC   PREVGROS,MYWORK2                                                 
         MVC   PREVNET,MYWORK2+4                                                
MYIBXX   B     XIT                                                              
         DROP  R1,R2,R3,R4,RE                                                   
         SPACE 2                                                                
*                                                                               
MYOTBDET NTR1                                                                   
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         MVI   0(R3),X'40'                                                      
         MVC   1(1,R3),0(R2)       BILL TYPE                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
MYOTFBD  NTR1                     BILLING DETAIL                                
         L     R2,GLAIFLD                                                       
         USING MINBD,R2                                                         
         LA    R4,MYP                                                           
         XC    MYP,MYP                                                          
         MVC   0(2,R4),=C'BD'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(1,R4),MBDTYP     BILL TYPE                                     
         CLI   0(R4),0                                                          
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         CLC   2(4,R2),=C'**ER'    IS IT ERROR                                  
         BNE   OTFBD10                                                          
         MVC   MYP+5(10),2(R2)     YES                                          
         MVI   MYP+15,X'15'                                                     
         B     OTFBX                                                            
OTFBD10  L     R1,=A(MYIO2)        NO/GET BILL RECORD                           
         USING BILLREC,R1                                                       
         MVI   0(R4),C'N'                                                       
         TM    BILBFB,X'10'        BILL BASE=NET                                
         BO    *+8                                                              
         MVI   0(R4),C'G'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'N'                                                       
         TM    BILBFB,X'01'        COMM BASE=NET                                
         BO    *+8                                                              
         MVI   0(R4),C'G'                                                       
         LA    R4,1(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         OC    BILBFP,BILBFP        ANY COMMISSION ADJUSTMENT                   
         BNZ   OTFBD12                                                          
         MVI   0(R4),X'FA'          NO/PASS DELIMITER                           
         LA    R4,1(R4)                                                         
         B     OTFBD14                                                          
* - COMMISSION ADJUSTMENT NEEDS (-)99.9999                                      
OTFBD12  EDIT  (B4,BILBFP),(8,0(R4)),ALIGN=LEFT,FLOAT=-                         
         LA    R1,8                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1              R4 = CHAR AFTER LAST SIGNIF DIGIT             
* - CHANGE NNNNNN TO NN.NNNN                                                    
         A     R4,=F'-4'           POINT R4 TO START OF LAST 4 DIGITS           
         MVC   WORK(4),0(R4)       SAVE LAST 4 DIGITS                           
         MVI   0(R4),C'.'          SET IN DECIMAL                               
         MVC   1(4,R4),WORK        AND RETURN LAST 4 DIGITS                     
         LA    R4,4(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
OTFBD14  EDIT  (B4,MBDGRS),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT    GROSS             
         BAS   R3,BUMP12                                                        
         EDIT  (B4,MBDNET),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT    NET               
         BAS   R3,BUMP12                                                        
* ACTUAL BILL AMOUNT                                                            
         EDIT  (B4,MBDAMT),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT                      
         BAS   R3,BUMP12                                                        
* - SET PREVIOUS BILL NUMBER/GROSS AND NET                                      
         OC    MBDPINV,MBDPINV               CHECK PREVIOUS BILLING             
         BZ    OTFBD20                                                          
         MVC   0(6,R4),MBDPINV                PREVIOUS BILL NUMBER              
         LA    R1,6                                                             
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         EDIT  (B4,MBDPGRS),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT  GROSS              
         BAS   R3,BUMP12                                                        
         EDIT  (B4,MBDPNET),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT  NET                
         BAS   R3,BUMP12                                                        
OTFBD20  DS    0H                  NO MORE DATA                                 
         MVI   0(R4),X'15'         SET END OF RECORD SIGN                       
OTFBX    BAS   RE,PUTIT                                                         
         B     XIT                                                              
*                                                                               
* TRUNCATE BLANKS/BUMP FIELD/AND SET DELIMITER                                  
BUMP12   DS    0H                                                               
         LA    R1,12               MAX BUMP OF 12                               
         BAS   RE,INPUTLN          TRUNCATE END BLANKS                          
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'         SET FIELD DELIMITER                          
         LA    R4,1(R4)            AND BUMP TO NEXT OUT FIELD                   
         BR    R3                                                               
         EJECT                                                                  
*                                                                               
MYINBGRS NTR1                                                                   
         L     R2,GLAIFLD                                                       
*        LA    R4,CURBELEM                                                      
*        USING NUBILEL,R4                                                       
*        MVC   0(4,R2),NUBILGRS                                                 
         MVC   0(4,R2),GROSSV      SET IN INUNIT BY SPBVAL                      
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
MYOTBGRS NTR1                                                                   
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         MVC   0(4,R3),0(R2)                                                    
         MVI   GLHOOK,GLEDIT                                                    
         TM    GLINDS,GLTOTLIN     IS IT TOTAL                                  
         BNO   XIT                                                              
         LA    R4,MYP                                                           
         XC    MYP,MYP                                                          
         MVC   0(2,R4),=C'IT'       INVOICE TOTAL                               
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         EDIT  (B4,0(R2)),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT                       
         BAS   R3,BUMP12                                                        
         LA    R2,4(R2)            POINT AT NET DOLLARS                         
         EDIT  (B4,0(R2)),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT                       
         BAS   R3,BUMP12                                                        
         LA    R2,4(R2)            POINT AT AMT DOLLARS                         
         EDIT  (B4,0(R2)),(12,0(R4)),2,FLOAT=-,ALIGN=LEFT                       
         BAS   R3,BUMP12                                                        
         MVI   0(R4),X'15'         SET END OF RECORD                            
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
MYINBNET NTR1                                                                   
         L     R2,GLAIFLD                                                       
**       LA    R4,CURBELEM                                                      
**       USING NUBILEL,R4                                                       
**       MVC   0(4,R2),NUBILNET                                                 
         MVC   0(4,R2),NETSV                                                    
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
MYOTBNET NTR1                                                                   
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         MVC   0(4,R3),0(R2)                                                    
         MVI   GLHOOK,GLEDIT                                                    
         CLI   GLINDS,GLTOTLIN                                                  
         BNE   XIT                                                              
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
MYINBBIL NTR1                                                                   
         L     R2,GLAIFLD                                                       
         MVC   0(4,R2),BILLAMSV                                                 
         B     XIT                                                              
*                                                                               
MYOTBBIL NTR1                                                                   
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         MVC   0(4,R3),0(R2)                                                    
         MVI   GLHOOK,GLEDIT                                                    
         CLI   GLINDS,GLTOTLIN                                                  
         BNE   XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* - MAX LENGTH OF COMMENTS = 120                                                
MYINCOM  NTR1                                                                   
         SR    R3,R3               R3 = LENGTH OF COMMENTS                      
         L     R2,GLAIFLD                                                       
         XC    0(120,R2),0(R2)                                                  
         MVI   0(R2),X'FF'          SET NO COMMENT INDICATOR                    
         MVI   60(R2),X'FF'         SET NO COMMENT INDICATOR                    
         CLI   BHREAD,C'Y'                 ..BILL READ MODE                     
         BE    MYINCX                      ..YES/EXIT                           
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
MYINC10  BAS   RE,NEXTEL                                                        
         BNE   MYINCX                                                           
         USING NUCOMEL,R4                                                       
         ZIC   R1,1(R4)                                                         
         S     R1,=F'5'                                                         
         AR    R3,R1                                                            
         LA    R3,2(R3)            ADD SPACE CHAR TO COUNT                      
         C     R3,=F'120'                                                       
         BH    MYINCX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NUCOMMNT                                                 
         LA    R1,2(R1)             LEAVE SPACE BETWEEN COMM LINES              
         AR    R2,R1                                                            
         B     MYINC10                                                          
MYINCX   B     XIT                                                              
         DROP  R4                                                               
*                                                                               
MYINITOT NTR1                                                                   
         L     R2,GLAIFLD                                                       
         MVC   0(4,R2),INVAMT                                                   
         XC    INVAMT,INVAMT                                                    
         B     XIT                                                              
*                                                                               
MYOITOT  NTR1                                                                   
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         TM    GLINDS,GLTOTLIN     ONLY PRINT AT TOTALS                         
         BNO   XIT                                                              
         LR    R1,R2                                                            
         A     R1,=F'-12'                                                       
         CLC   0(4,R1),0(R2)       IS GROSS BILL ELEM=INVOICE                   
         BE    MYOI5                                                            
         MVC   0(7,R3),=C'*ERROR*'                                              
MYOI5    EDIT  (B4,0(R2)),(12,7(R3)),2,MINUS=YES                                
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
SETUNT   NTR1                                                                   
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
*                                                                               
RDHI     NTR1                                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     XIT                                                              
*                                                                               
GTREC    NTR1                                                                   
         MVC   AIO,ANETWS2                                                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         B     XIT                                                              
*                                                                               
GTREC2   NTR1                    2ND I/O AREA                                   
         L     R1,=A(MYIO2)                                                     
         STCM  R1,15,AIO                                                        
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         B     XIT                                                              
*                                                                               
GETPRD   NTR1                                                                   
         LA    R4,220                                                           
         L     R3,NBACLI                                                        
         USING CLTHDR,R3                                                        
         LA    R3,CLIST                                                         
GTP10    CLC   CURRPRD,3(R3)                                                    
         BNE   GTP20                                                            
         MVC   WORK(3),0(R3)                                                    
         B     GTPX                                                             
GTP20    LA    R3,4(R3)                                                         
         BCT   R4,GTP10                                                         
         MVC   WORK(3),=C'UNA'                                                  
GTPX     B     XIT                                                              
         DROP  R3                                                               
*                                                                               
PUTIT    NTR1                                                                   
         LA    R2,MYP      GET LENGTH OF DATA IN MYP                            
         LA    R1,1                                                             
PUT10    CLI   0(R2),X'15'         END OF RECORD                                
         BE    PUT12                                                            
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         C     R1,=F'300'                                                       
         BNH   PUT10                                                            
         DC    H'0'                                                             
PUT12    ST    R1,MYPLEN                                                        
         L     R1,ANETAPE                                                       
         PUT   (R1),MYPLEN                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              R1=LEN OF INPUT FIELD, R4=ADDRESS OF FIELD                       
INPUTLN  NTR1                                                                   
         AR    R4,R1                                                            
         BCTR  R4,0           POINT TO LAST POSSIBLE INPUT FIELD                
IPT10    CLI   0(R4),X'40'                                                      
         BH    IPT20                                                            
         BCTR  R4,0                                                             
         BCT   R1,IPT10                                                         
IPT20    XIT1  REGS=(R1)        R1 = LENGTH OF SIGNIFICANT INPUT                
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*          DATA SET NEWRI53    AT LEVEL 029 AS OF 06/24/92                      
GETMOS   DS    0H                                                               
         L     RF,=A(PERLIST)                                                   
GETM4    DS    0H                                                               
         CLC   NBACTDAT,2(RF)      TEST DATE VS START                           
         BNL   *+6                                                              
         DC    H'0'                TIME TO LENGTHEN PERLIST                     
         BE    GETM8                                                            
         CLC   NBACTDAT,4(RF)                                                   
         BNH   GETM8                                                            
         LA    RF,6(RF)            NEXT ENTRY                                   
         B     GETM4               NOTE- LIST ENDS IN FF                        
GETM8    DS    0H                                                               
         MVC   BYMOS,0(RF)         SET YR/MOS                                   
         BR    RE                                                               
         SPACE                                                                  
*                                                                               
GETTIME  NTR1                  PASS BACK HHMM START/END TIME                    
*          DATA SET NEWRIDRIVE AT LEVEL 121 AS OF 08/19/92                      
         LA    R3,WORK+20                                                       
         SR    R1,R1              ADJUST START TIME 6AM=START OF DAY            
         ICM   R1,3,NBTIME                                                      
         C     R1,=F'600'          IF START HOUR LESS THAN 0600                 
         BNL   GTT10                                                            
         A     R1,=F'2400'          ADD 2400                                    
         STCM  R1,3,NBTIME                                                      
GTT10    DS    0H                                                               
         SR    R4,R4                                                            
         ICM   R4,3,NBTIME                                                      
         EDIT  (R4),(4,0(R3)),FILL=0                                            
         ICM   R4,3,NBTIME+2                                                    
         EDIT  (R4),(4,4(R3)),FILL=0                                            
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
* WORKINV=MONTH(2BYTES)+INVNO(4BYTES)                                           
* WORKYMD=YYMMDD OF BILLING RUN DATE                                            
* OUTPUT = 3BYTE 1(Y/M) + 2(RUN DATE)                                           
GETBYMN  NTR1                                                                   
         PACK  DUB,WORKINV+2(4)   .GET BINARY VALUE OF INVNO                    
         CVB   R1,DUB                                                           
         STCM  R1,3,WORK+1                                                      
* - CONVERT BILLING RUN MONTH TO BINARY                                         
         PACK  DUB,WORKINV(2)     .GET BINARY VALUE OF MONTH                    
         CVB   R1,DUB                                                           
         STCM  R1,1,WORK                                                        
* GET YEAR INTO ZONE OF MONTH BYTE                                              
         PACK  BYTE,WORKYMD+1(1)   .SWITCH FN TO NF                             
         NI    BYTE,X'F0'          .MAKE IT N0                                  
         OC    WORK(1),BYTE        .SET IT INTO FIRSTHALF OF MONTH BYTE         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
MYHEAD   NTR1                                                                   
         CLI   ERRORSW,0                                                        
         BE    MYH10                                                            
         MVC   H10+55(22),=C'***** ERROR  MAP *****'                            
         B     *+10                                                             
MYH10    MVC   H10+55(22),=C'***** RECORD MAP *****'                            
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    XIT                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         MVI   0(R5),C'L'                                                       
         LA    R5,130(R5)                                                       
         MVI   0(R5),C'R'                                                       
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,47(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
* GET AGY ADDRESS FFROM CONTROL FILE / PASS IT ALONG                            
*                                                                               
AGYHDS   NTR1                                                                   
         MVC   FILENAME,=C'CTFILE  '                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         L     R1,ATWA                                                          
         MVC   CTIKID+8(2),10(R1)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R2,CTIDATA                                                       
         SR    R3,R3                                                            
CTK10    CLI   0(R2),0                                                          
         BE    CTKX                                                             
         CLI   0(R2),X'30'         DESTINATION DETAIL                           
         BE    CTK20                                                            
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     CTK10                                                            
CTK20    DS    0H                                                               
         USING CTDSTD,R2                                                        
         LA    R4,MYP                                                           
         XC    MYP,MYP                                                          
         MVC   0(2,R4),=C'AG'                                                   
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(33,R4),CTDSTNAM   AGY NAME                                     
         LA    R1,33                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         MVC   0(33,R4),CTDSTADD   AGY ADDRESS                                  
         LA    R1,33                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         CLI   CTDSTLEN,100        ONLY ONE LINE FOR AGY ADDRESS                
         BE    CTK50                                                            
         MVC   0(33,R4),CTDSTAD2   NO/AGY ADDRESS LINE2                         
         LA    R1,33                                                            
         BAS   RE,INPUTLN                                                       
         AR    R4,R1                                                            
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
         OC    CTDSTAD3,CTDSTAD3                                                
         BZ    CTK50                                                            
         MVC   0(33,R4),CTDSTAD3                                                
         LA    R1,33                                                            
         BAS   RE,INPUTLN                                                       
         MVI   0(R4),X'FA'                                                      
         LA    R4,1(R4)                                                         
CTK50    MVI   0(R4),X'15'         END OF REC MARK                              
*                                                                               
CTKX     XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
         SPACE                                                                  
* - READS BILL RECORDS TO FIND THOSE NOT TAGGED ON UNITS                        
* - BILL RECS ARE CHECKED AGAINST BILL FORMULA TABLE AND THOSE                  
* - THAT MATCH ARE SKIPPED                                                      
* - THE REST ARE PASSED TO DRIVER / NETBLOCK FUDGED TO MAKE IT ALL GO           
*                                                                               
BILLHDS  NTR1                                                                   
         MVI   BHREAD,C'Y'                 SET SWITCH                           
         LA    R2,KEY                                                           
         USING BILLREC,R2                                                       
         XC    BKEY,BKEY                   BUILD BILL REC KEY                   
         MVC   BKEYAM,BHAM                                                      
         OC    BHCLI,BHCLI                                                      
         BZ    BH10                                                             
         MVC   BKEYCLT,BHCLI       CLIENT                                       
         OC    BHPRD,BHPRD                                                      
         BZ    BH10                                                             
         MVC   BKEYPRD,BHPRD       PROD                                         
         OC    BHESE,BHESE         IS IT EST RANGE                              
         BNZ   BH10                                                             
         MVC   BKEYEST,BHEST       NO SET EST INTO KEY                          
*                                                                               
BH10     NETGO NVSETSPT,DMCB                                                    
         BAS   RE,RDHI                                                          
         B     BH15                                                             
BHSEQ    MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
* - FILTER BILL KEY AGAINST REQUEST DATA                                        
*                                                                               
BH15     CLC   KEY(2),KEYSAVE       AM/MED                                      
         BNE   BHX                                                              
         CLI   KEY+8,0             MAKE SURE ITS BILL REC                       
         BE    BHSEQ                                                            
         OC    BHCLI,BHCLI         CLIENT                                       
         BZ    *+14                                                             
         CLC   BKEYCLT,BHCLI                                                    
         BNE   BHSEQ                                                            
         CLI   NBSELPGR,0          IS IT PROD GROUP FILTERING                   
         BE    BH17                NO                                           
         MVC   WORK(3),BKEYPRD     YES/GET 1 BYTE PROD CODE                     
         BAS   RE,GETPRD1                                                       
         ZIC   R1,WORK                                                          
         BAS   RE,TESTMASK                                                      
         BNE   BHSEQ                                                            
BH17     OC    BHPRD,BHPRD         PRODUCT                                      
         BZ    *+14                                                             
         CLC   BKEYPRD,BHPRD                                                    
         BNE   BHSEQ                                                            
         OC    BHESE,BHESE         EST RANGE                                    
         BZ    BH20                                                             
         CLC   BKEYEST,BHESE       YES                                          
         BH    BHSEQ                                                            
         CLC   BKEYEST,BHEST                                                    
         BL    BHSEQ                                                            
         B     BH30                                                             
BH20     OC    BHEST,BHEST         EST FILTER                                   
         BZ    BH30                                                             
         CLC   BHEST,BKEYEST                                                    
         BNE   BHSEQ                                                            
BH30     DS    0H                                                               
*                                  CHECK IF BILL HEADER IN TABLE                
         L     R0,=F'10000'        MAX ENTRIES=10000                            
         L     R3,ABILLFTB         BILL FORMULA TABLE                           
         USING BFORMTB,R3                                                       
BH40     CLC   BFTKEY,KEY          IS IT IN TABLE                               
         BE    BHSEQ               YES/SKIP IT                                  
         CLI   BFTKEY+1,0                                                       
         BE    BH42                NO MORE BFORM RECS                           
         LA    R3,BFTLENE(R3)                                                   
         BCT   R0,BH40                                                          
*                                  NOT IN TABLE/SO READ BILL                    
BH42     BAS   RE,GTREC2                                                        
         L     R2,AIO                                                           
         USING BILLREC,R2                                                       
         TM    BILSTAT,BSTTAORQ        IF AOR                                   
         BO    BHSEQ                   SKIP IT                                  
         CLC   BDATE,STRDAT        DATE CHECK                                   
         BL    BHSEQ                                                            
         CLC   BDATE,ENDDAT                                                     
         BH    BHSEQ                                                            
         EJECT                                                                  
         SPACE                                                                  
*                                 PREPARE TO PASS BILL REC TO DRIVER            
         SPACE                                                                  
         L     R4,ANETWS3          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+1       ...DO WE NEED NEW CLIENT HEADER              
         BE    GB50                                                             
         XC    KEY,KEY             ...YES/GET NEW CLIENT RECORD                 
         XC    KEY+1(3),BKEYAM                                                  
         BAS   RE,RDHI                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS3         PUT CLIENT REC IN ANETWS3                    
         GOTO1 GETREC                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),BKEY        RETURN BILL KEY                              
         BAS   RE,RDHI             RESET SEQ READ                               
         SPACE                                                                  
* - FUDGE NETBLOCK                                                              
* - GET 3 BYTE CLI CODE FOR NETBLOCK FUDGE                                      
GB50     DS    0H                                                               
         MVC   INVNUMBR,BINVNO     INVOICE NUMBER                               
         GOTO1 NBCLUNPK,DMCB,(CPROF+6,KEY+2),NBCLICOD                           
         MVC   NBACTEST,BKEYEST          EST                                    
         MVC   WORK(3),BKEYPRD                                                  
         BAS   RE,GETPRD1                                                       
         MVC   NBSPLPRN,WORK              PRODUCT                               
         MVC   NBACTAM(3),KEY+1           AM/CLI                                
         CLI   BLMED,X'40'                                                      
         BH    *+8                                                              
         MVI   BLMED,C'N'          DEFAULT TO NETWORK                           
         MVC   NBPOSTYP,BLMED                                                   
         MVC   NBSTATYP,BLMED                                                   
         XC    NBACTPRG,NBACTPRG  ?WILL THIS WORK FOR PROGBOTH?                 
         XC    NBPROGNM,NBPROGNM                                                
         PACK  DUB,BAMT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,15,GROSSV                                                     
         STCM  R1,15,INVAMT                                                     
         MVC   NETSV,BNET                                                       
         MVC   BILLAMSV,BACTUAL                                                 
*                                                                               
         BAS   RE,DRIVIT                                                        
*                                                                               
         B     BHSEQ                                                            
*                                                                               
BHX      B     XIT                                                              
         EJECT                                                                  
GETPRD1  NTR1                  USES BHACTPRD AND RETURNS                        
         L     R1,ANETWS3      1 BYTE PROD IN WORK                              
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GTP5     CLC   WORK(3),0(R1)                                                    
         BE    GOTPRD                                                           
         LA    R1,4(R1)                                                         
         BCT   R2,GTP5                                                          
         MVC   WORK(1),0                                                        
         B     *+10                                                             
GOTPRD   MVC   WORK(1),3(R1)                                                    
         B     XIT                                                              
         DROP  R1                                                               
*                                                                               
TESTMASK NTR1                     R1 = PROD NUMBER                              
         LA    R2,NBPRDMSK                                                      
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BYTE,0(R2)                                                       
         NC    BYTE,0(R1)                                                       
         CLI   BYTE,0                                                           
         BE    NO                                                               
         B     YES                                                              
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
         EJECT                                                                  
*                                                                               
* - PRINT BILLING ELEMENTS THAT HAD NO BILL HEADER                              
*                                                                               
CHKERRTB NTR1                                                                   
         L     R2,AERRTBL                                                       
         USING ERCLT,R2                                                         
CR2      CLI   0(R2),0                                                          
         BE    CRX                                                              
         MVC   P+2(3),ERCLT                                                     
         EDIT  (B1,EREST),(3,P+6)                                               
         MVC   P+10(4),ERNET                                                    
         EDIT  (B1,ERPKG),(3,P+15)                                              
         MVC   P+20(6),ERPROG                                                   
         GOTO1 DATCON,DMCB,(2,ERDATE),(5,P+27)                                  
         MVC   P+37(1),ERTYPE                                                   
         MVC   P+39(4),ERINVNO                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,ERRORLEN(R2)                                                  
         B     CR2                                                              
CRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
NETBTP   DCB   DDNAME=NETBTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=U,                                                X        
               LRECL=00300,                                            X        
               BLKSIZE=06000,                                          X        
               MACRF=PM                                                         
*                                                                               
NETBTPIN DCB   DDNAME=NETBTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=U,                                                X        
               LRECL=00300,                                            X        
               BLKSIZE=06000,                                          X        
               MACRF=GM,                                               X        
               EODAD=ENDFILE                                                    
         SPACE                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK BILLING INTERFACE'                               
         SSPEC H2,46,C'_________________________'                               
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,47,PERIOD                                                     
         SSPEC H4,99,REPORT                                                     
         SSPEC H5,99,RUN                                                        
         SSPEC H6,120,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
*              LTORG                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
OVERFLOW NMOD1 0,*N32OVFL,RA                                                    
         L     RC,0(R1)            REESTABLISH WORKING STORAGE                  
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1         ANETWS1=WORKING STORAGE                       
         USING MYD,R7                                                           
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVFLOW(RF)                                                       
XIT2     XIT1                                                                   
*                                                                               
OVFLOW   B     SETDATE                                                          
         SPACE 2                                                                
*                                                                               
SETDATE  DS    0D                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBEFFMED         MEDIA                                 
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 =V(GETPROF),DMCB,(0,WORK),MYWORK,(0,(R2))                        
         IC    R0,MYWORK+2       DATE CONTROL                                   
*                                BUILD LONG LIST OF DATE PAIRS                  
         L     R2,=A(MYIO)                                                      
         GOTO1 =V(MOBILE),DMCB,(208,NBSELSTR),((R0),(R2)),RR=RELO               
*                                  FIND FIRST PERIOD OF A NEW YEAR              
SETD4    DS    0H                                                               
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD6               YES                                          
         LA    R2,4(R2)                                                         
         B     SETD4                                                            
*                                  BUILD  A LIST OF YM, START-END               
SETD6    DS    0H                                                               
         L     R3,=A(PERLIST)                                                   
SETD7    DS    0H                                                               
         ZIC   R0,2(R2)                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE             YEAR                                         
         SR    R4,R4               FOR PER SEQUENCE WITHIN YR                   
SETD8    DS    0H                                                               
         LA    R4,1(R4)                                                         
         MVC   0(1,R3),BYTE        YEAR                                         
         STC   R4,1(R3)            MONTH                                        
         MVC   2(4,R3),0(R2)       START-END OF PER                             
         LA    R3,6(R3)                                                         
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    SETD12              EOL                                          
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD7               YES                                          
         B     SETD8                                                            
*                                                                               
SETD12   DS    0H                                                               
*                                                                               
SETDATEX DS    0H                                                               
         B     XIT2                                                             
         SPACE 1                                                                
*                                  FIND START OF NEW YEAR                       
*                                  1) A PERIOD THAT SPANS YEAR CHANGE           
*                                     AND BEGINS NO FURTHER AWAY                
*                                     FROM 12/31 THAN IT ENDS                   
*                             OR   2) A PERIOD THAT STARTS BEFORE 1/14          
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R2)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
*                                                                               
         EJECT                                                                  
*        LTORG                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PERLIST  DS    XL(15*13*6+1)       15YRS X 13 MNTHS X 6                         
*                                  MOS(2) + START(2) + END(2)                   
MYIO     DS    CL2000                                                           
         DS    0D                                                               
MYIO2    DS    CL200                                                            
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
DRIVER   DS    A                                                                
ADRONE   DS    A                                                                
ANETAPE  DS    F                                                                
ABILLFTB DS    A                                                                
AERRTBL  DS    A                                                                
MYOPTS   DS    CL20         SET AT EDIT TIME(GLOBAL WS NOT AVAILABLE)           
*                               AND MOVE TO GLOPTS AT RUN TIME                  
CURRPRD  DS    CL1         CURRENT WORKING PROD OF BILLING ELEM                 
CURRBINV DS    CL3         CURRENT WORKING BINARY Y/M + INVNO                   
STRDAT   DS    CL6                 YYMMDD                                       
ENDDAT   DS    CL6                 YYMMDD                                       
BSTRDAT  DS    CL2                 YMD COMPRESSED                               
BENDDAT  DS    CL2                 YMD                                          
BYMOS    DS    CL2                 BILLING YR/MOS FROM GETMOS                   
RUNDATE  DS    0CL6                                                             
RUNDYY   DS    CL2                                                              
RUNDMM   DS    CL2                                                              
RUNDDD   DS    CL2                                                              
*                                                                               
INVNUMBR DS    CL6         CURRENT INV NUMBER MONTH(2) + NUMBER                 
INVAMT   DS    CL4                                                              
*                                                                               
INVERROR DS    CL1                                                              
CURBELEM DS    CL24          BILLING ELEM SAVE **HARD CODED LENGTH              
INVNUMSV DS    CL6                                                              
CURBTYP  DS    CL1                                                              
ERRORSW  DS    CL1                                                              
WORKINV  DS    CL6                 TEMPORARY INVOICE  WORK AREA                 
WORKYMD  DS    CL6                 TEMPORARY YYMMDD WORK AREA                   
MYKEY    DS    CL20                                                             
SAVECLT  DS    CL3                                                              
GROSSV   DS    CL4                                                              
NETSV    DS    CL4                                                              
BILLAMSV DS    CL4                                                              
PREVINUM DS    CL4                                                              
PREVGROS DS    CL4                                                              
PREVNET  DS    CL4                                                              
*                                                                               
BHREAD   DS    CL1                                                              
BHAM     DS    CL1                 SAVE AREA FOR BILL RECORDS                   
BHCLI    DS    CL2                                                              
BHPRD    DS    CL3                                                              
BHEST    DS    CL1                                                              
BHESE    DS    CL1                                                              
*                                                                               
MYWORK   DS    CL100                                                            
MYWORK2  DS    CL100                                                            
         DS    0D                                                               
MYPID    DS    D                   "MYPID IN DUMP                               
MYPLEN   DS    F                   VARIABLE LENGTH OF MYP FOR TAPE              
MYP      DS    CL200                                                            
         EJECT                                                                  
*                                                                               
BFORMTB  DSECT            BILLING FORMULA TABLE (10000 MAX ENTRY)               
BFTKEY   DS    CL13                                                             
BFTFORM  DS    CL5                                                              
BFTLENE  EQU   *-BFTKEY                                                         
*                                                                               
         EJECT                                                                  
MINBD    DSECT                     IN BILLING DETAIL                            
MBDTYP   DS    CL1                                                              
MBDGRS   DS    CL4                                                              
MBDNET   DS    CL4                                                              
MBDAMT   DS    CL4                                                              
MBDPINV  DS    CL6                 PREVIOUS INVOICE NUMBER                      
MBDPGRS  DS    CL4                 PREVIOUS GROSS                               
MBDPNET  DS    CL4                 PREVIOUS NET                                 
*                                                                               
         SPACE 2                                                                
INUND    DSECT                      MYINUN DSECT                                
INUNDAD  DS    CL4                 DISK ADDRESS                                 
INUNNET  DS    CL4                 NETWORK                                      
INUNDAT  DS    XL2                 NBACTDAT                                     
INUNTIME DS    CL8                 STARTEND TIME HHMMHHMM                       
INUNSUB  DS    XL1                 SUBLINE                                      
INUNLEN  DS    XL1                 LENGTH                                       
INUNPAK  DS    XL1                 PACKAGE                                      
INUNDROT DS    CL1                 ROTATION                                     
INUNPRD  DS    CL3                 PROD CODE                                    
INUNPSHR DS    XL2                 PIGGY SHARE                                  
INUNYM   DS    CL4                 MOS YYMM                                     
INUNACT  DS    XL4                 NBACTUAL                                     
INUNASS  DS    XL4                 NBASSIGN                                     
INUNINTG DS    XL4                 NBINTEG                                      
INUNLENE EQU   *-INUND                                                          
         EJECT                                                                  
ERRTABL  DSECT                                                                  
ERCLT    DS    CL3                                                              
EREST    DS    CL1                                                              
ERNET    DS    CL4                                                              
ERPKG    DS    CL1                                                              
ERPROG   DS    CL6                                                              
ERDATE   DS    CL2                                                              
ERTYPE   DS    CL1                                                              
ERPROD   DS    CL3                                                              
ERRKLEN  EQU   *-ERCLT                                                          
ERINVNO  DS    CL4                                                              
ERRORLEN EQU   *-ERCLT                                                          
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDBD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE CTGENDIC                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE SPBVALD                                                        
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054NEWRI32   07/12/04'                                      
         END                                                                    
