*          DATA SET SEACS07    AT LEVEL 016 AS OF 05/01/02                      
*PHASE TA0D07A,*                                                                
         TITLE 'SEACS07 - FILE MAINTENANCE - DEPARTMENT RECORDS'                
ACS07    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS7**,RR=RE                                                 
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SADPREC,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RD,APWORKA                                                       
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   AGENCYID,OPTAGY                                                  
         B     *+10                                                             
         MVC   AGENCYID,CUAALF                                                  
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELREC              05 - APMDELR                                 
         B     RESREC              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     VALREQ              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     PUTKEY              18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF DEPARTMENT RECORD                        *         
***********************************************************************         
         SPACE   1                                                              
         USING SADPREC,R2                                                       
VALKEY   LA    R2,IOKEY                                                         
         XC    DPTOIDN,DPTOIDN     CLEAR OFFICE NAME FIELD                      
         OI    DPTOIDNH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         MVI   FVMINL,1            READ OFFICE CODE FIELD                       
         GOTO1 AFVAL,DPTOIDH                                                    
         BNE   VALKEYX                                                          
         LA    R1,IOKEY                                                         
         USING SAOFREC,R1                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   VALKEYER                                                         
         L     R1,AIOAREA2                                                      
         GOTO1 AGETONAM            GET OFFICE NAME                              
         MVC   DPTOIDN,APWORK        AND DISPLAY IT                             
         MVC   DPTOID,SADPOID                                                   
         OI    DPTOIDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
*&&UK                                                                           
*                                  TEST OFFICE MANAGER ACCESS                   
         GOTO1 ATSTOMAN,FVIFLD                                                  
         BNE   VALKEYX                                                          
*&&                                                                             
*                                                                               
         XC    SADPKEY,SADPKEY     BUILD DEPARTMENT RECORD KEY                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,FVIFLD      USING OFFICE CODE                            
         MVI   FVMINL,2            READ DEPARTMENT CODE FIELD                   
         GOTO1 AFVAL,DPTDIDH                                                    
         BNE   VALKEYX                                                          
         MVC   SADPDID,FVIFLD        AND MOVE CODE TO KEY                       
*&&US                                                                           
*                                  TEST OFFICE/DEPT MANAGER ACCESS              
         GOTO1 ATSTDMAN,APPARM,SADPOID,SADPDID                                  
         BNE   VALKEYX                                                          
*&&                                                                             
*                                                                               
         MVC   APRECKEY(L'SADPKEY),SADPKEY    SAVE KEY                          
         LA    R1,IORDD+IOCONFIL+IO1          AND READ RECORD                   
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         MVI   APINDS,APIOKADD                                                  
         B     VALKEYY                                                          
*                                                                               
VALKEYER MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALKEYX                                                          
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A DEPARTMENT RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         XC    DPTPIDN,DPTPIDN     CLEAR MANAGER NAME FIELD                     
         OI    DPTPIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    OLDCNT,OLDCNT       INITIALISE STAFF COUNT                       
         CLI   APACTN,ACTADD       ADD ACTION                                   
         BE    VR010                                                            
*                                  ELSE IF CHANGE ACTION                        
         LA    R3,SADPDATA           EXTRACT OLD STAFF COUNT                    
         SR    R0,R0                                                            
VR002    CLI   0(R3),0                                                          
         BE    VR010               END OF RECORD                                
         CLI   0(R3),SAPCTELQ                                                   
         BE    VR004                                                            
         IC    R0,1(R3)            GET NEXT ELEMENT                             
         AR    R3,R0                                                            
         B     VR002                                                            
*                                  STAFF COUNT ELEMENT                          
         USING SAPCTD,R3                                                        
VR004    MVC   OLDCNT,SAPCTVAL     SAVE COUNT                                   
         B     VR010                                                            
*                                  BUILD INITIAL RECORD                         
VR010    XC    SADPKEY(256),SADPKEY                                             
         MVC   SADPKEY(L'SADPKEY),APRECKEY                                      
         LA    R0,SADPDATA+1-SADPREC                                            
         STCM  R0,3,SADPLEN                                                     
*                                                                               
         LA    R3,APELEM           BUILD DEPARTMENT ELEMENT                     
         USING SADPTD,R3                                                        
         XC    SADPTEL(SADPTLNQ),SADPTEL                                        
         MVI   SADPTEL,SADPTELQ                                                 
         MVI   SADPTLN,SADPTLNQ                                                 
*                                                                               
         MVI   FVMINL,1            READ DEPARTMENT NAME                         
         GOTO1 AFVAL,DPTDIDNH                                                   
         BNE   VALRECX                                                          
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SADPTNAM(0),FVIFLD  SAVE NAME AND LENGTH                         
         AH    R1,=Y(SADPTLNQ+1)                                                
         STC   R1,SADPTLN                                                       
*                                                                               
VR020    GOTO1 AADDELS,SADPREC     ADD DEPARTMENT RECORD                        
*                                                                               
         USING SAPCTD,R3                                                        
VR030    XC    SAPCTEL(SAPCTLNQ),SAPCTEL                                        
         MVI   SAPCTEL,SAPCTELQ    BUILD STAFF COUNT ELEMENT                    
         MVI   SAPCTLN,SAPCTLNQ                                                 
         MVC   SAPCTVAL,OLDCNT     RESTORE OLD COUNT                            
         GOTO1 AADDELS,SADPREC     ADD ELEMENT                                  
*                                                                               
         USING SAAPCD,R3                                                        
VR032    EQU   *                                                                
         LA    R3,APELEM           INITIALISE ELEMENT                           
         XC    SAAPCEL(SAAPCLNQ),SAAPCEL                                        
         MVI   SAAPCEL,SAAPCELQ                                                 
         MVI   SAAPCLN,SAAPCLNQ                                                 
         MVI   FVMINL,1            VALIDATE APPROVER GROUP FIELD                
         GOTO1 AFVAL,DPTAPCH                                                    
         BNE   VR040                                                            
         MVC   SAAPCCOD,FVIFLD                                                  
         GOTO1 AGETAPG,SAAPCCOD    GET APPROVER GROUP RECORD DATA               
         BNE   VALRECX                                                          
         MVC   SAAPCNUM,APHALF                                                  
         GOTO1 AADDELS,SADPREC     ADD ELEMENT                                  
         DROP  R3                                                               
*                                                                               
VR040    LA    R4,DPTPIDH          PROCESS MANAGER ID ELEMENTS                  
         LA    R0,MANNUM                                                        
         SR    R8,R8                                                            
         USING MANIDD,R4                                                        
*                                                                               
VR042    LA    R3,APELEM           INITIALISE MANAGER ID ELEMENT                
         USING SAMAND,R3                                                        
         XC    SAMANEL(SAMANLNQ),SAMANEL                                        
         MVI   SAMANEL,SAMANELQ                                                 
         MVI   SAMANLN,SAMANLNQ                                                 
         MVI   FVMINL,1            READ OFFICE NAME FIELD                       
         GOTO1 AFVAL,MANPIDH       READ MANAGER PERSONAL ID                     
         BNE   VR044                                                            
         GOTO1 AGETPNUM,FVIFLD                                                  
         BNE   VALRECX                                                          
         MVC   SAMANID,APHALF      SAVE MANAGER ID NUMBER                       
         LA    R8,1(R8)                                                         
         STC   R8,SAMANORD         SAVE MANAGER ORDER NUMBER                    
         GOTO1 AADDELS,SADPREC     ADD MANGER ID ELEMENT                        
VR044    LA    R4,MANIDL(R4)                                                    
         BCT   R0,VR042                                                         
         DROP  R4                                                               
*                                                                               
VR100    GOTO1 ASETACT,SADPREC     DEFINE ACTIVITY ELEMENT                      
*                                                                               
*                                  UPDATE RECORD ON FILE                        
         LA    R1,IOADD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   DPTOID,SADPOID                                                   
         OI    DPTOIDH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   DPTDID,SADPDID                                                   
         OI    DPTDIDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC DPTDIDNH                                                         
         BAS   RE,REENTER          HANDLE DELETE/RESTORE PROMPT                 
         MVC   DPTDID,SADPDID      DISPLAY KEY DATA                             
         OI    DPTDIDH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    DPTAPCN,DPTAPCN                                                  
         OI    DPTAPCNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R4,DPTPIDH                                                       
         LA    R0,MANNUM                                                        
         USING MANIDD,R4                                                        
DR002    XC    MANPIDN,MANPIDN                                                  
         OI    MANPIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R4,MANIDL(R4)                                                    
         BCT   R0,DR002                                                         
DR004    LA    R4,DPTPIDH                                                       
*                                                                               
         LA    R3,SADPDATA         GET ELEMENT DATA                             
*                                                                               
DR010    CLI   0(R3),0                                                          
         BE    DISRECX             END OF RECORD                                
         CLI   0(R3),SADPTELQ                                                   
         BE    DR030                                                            
         CLI   0(R3),SAMANELQ                                                   
         BE    DR040                                                            
         CLI   0(R3),SAAPCELQ                                                   
         BE    DR050                                                            
*                                                                               
DR020    SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR010                                                            
*                                                                               
         USING SADPTD,R3                                                        
DR030    ZIC   R1,SADPTLN          DEPARTMENT ELEMENT                           
         SH    R1,=Y(SADPTLNQ+1)                                                
         BM    DR032                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DPTDIDN(0),SADPTNAM  DISPLAY DEPARTMENT NAME                     
DR032    OC    SADPTMID,SADPTMID   CHECK MANAGER ID PRESENT                     
         BZ    DR020                                                            
         MVC   MANPID,SADPTMID       AND DISPLAY IT                             
         GOTO1 AGETPNAM,SADPTMID   GET MANAGER NAME                             
         BNE   DR036                                                            
         MVC   MANPIDN,APWORK        AND DISPLAY IT                             
         LA    R4,MANIDL(R4)                                                    
         B     DR020                                                            
DR036    MVC   APHALF,=Y(CS#PIDNF) IF NOT FOUND DISPLAY MESSAGE                 
         GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   MANPIDN,APWORK                                                   
         LA    R4,MANIDL(R4)                                                    
         B     DR020                                                            
         DROP  R3                                                               
*                                                                               
         USING SAMAND,R3                                                        
DR040    EQU   *                   MANAGER ID ELEMENT                           
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   DR046                                                            
         MVC   MANPID,APWORK                                                    
         GOTO1 AGETPNAM,APWORK     GET PERSON NAME                              
         BNE   DR046                                                            
         L     RF,4(R4)                                                         
         MVC   MANPIDN,APWORK      DISPLAY MANAGER NAME                         
         LA    R4,MANIDL(R4)                                                    
         B     DR020                                                            
DR046    MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APHALF,=Y(CS#PIDNF) NAME NOT FOUND MESSAGE                       
         GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   MANPIDN,APWORK      DISPLAY MANAGER NAME                         
         LA    R4,MANIDL(R4)                                                    
         B     DR020                                                            
         DROP  R3                                                               
*                                                                               
         USING SAAPCD,R3                                                        
DR050    OC    SAAPCCOD,SAAPCCOD   APPROVER GROUP CODE                          
         BZ    DR020                                                            
         MVC   DPTAPC,SAAPCCOD                                                  
         GOTO1 AGETAPG,SAAPCCOD                                                 
         BNE   VALRECX                                                          
         MVC   DPTAPCN,APWORK                                                   
         B     DR020                                                            
         DROP  R3                                                               
*                                                                               
DISRECX  GOTO1 ADISACT,SADPREC     DISPLAY ACTIVITY DATE                        
         B     EXIT                                                             
         SPACE 1                                                                
REENTER  CLI   APACTN,ACTDEL       HANDLE DELETE REENTER PROMPT                 
         BE    *+12                                                             
         CLI   APACTN,ACTRES       HANDLE RESTORE REENTER PROMPT                
         BNE   *+8                                                              
*                                  AVOID NO DATA ENTERED SYSTEM MESSAGE         
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A DEPARTMENT RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         LA    R3,SADPDATA         GET STAFF COUNT ELEMENT                      
         SR    R0,R0                                                            
*                                                                               
DREC010  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R3),SAPCTELQ                                                   
         BE    DREC020                                                          
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DREC010                                                          
*                                                                               
         USING SAPCTD,R3                                                        
DREC020  OC    SAPCTVAL,SAPCTVAL   CAN NOT DELETE IF                            
         BNZ   SAEDPC                STAFF COUNT NOT ZERO                       
*                                                                               
DREC030  GOTO1 ASETACT,SADPREC                                                  
         OI    SADPSTAT,X'80'      SET DELETE FLAG                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SADPREC                                                  
         NI    SADPSTAT,X'FF'-X'80'  CLEAR DELETE FLAG                          
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)    *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
*                                                                               
         LA    R2,APRECKEY         BUILD FIRST RECORD KEY                       
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    SADPKEY,SADPKEY                                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
VS010    GOTO1 AFVAL,LSTOIDH       READ OFFICE CODE FILTER                      
         BNE   VS020                                                            
         LA    R1,IOKEY                                                         
         USING SAOFREC,R1                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELOID,FVIFLD                                                    
         MVC   SADPOID,FVIFLD      MOVE TO LIST KEY                             
*                                                                               
VS020    GOTO1 AFVAL,LSTDIDH       READ DEPARTMENT CODE FILTER                  
         BNE   VALSELY                                                          
         OC    SELOID,SELOID                                                    
         BZ    SAEODF                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VS022    CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VS024               FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VS022                                                         
VS024    STC   RE,SELKEYCL                                                      
         MVC   SELDID,FVIFLD                                                    
         MVC   SELDIDL,FVILEN                                                   
         MVC   SELDIDSP,0(RF)                                                   
         MVC   SADPDID,SELDID      MOVE TO LIST KEY                             
*                                                                               
VALSELY  EQU   *                                                                
         MVI   GETSEQF,0           INITIALISE GETSEL APINDS SAVE                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY            READ NEXT LIST RECORD                        
         MVC   SADPKEY,APRECKEY      FROM LAST SAVED KEY                        
         TM    GETSEQF,APILRERD    TEST GETSEL SEQUENCE BROKEN                  
         BZ    GS002                                                            
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GS004                                                            
GS002    TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    GS010                                                            
         NI    APINDS,X'FF'-APILRERD                                            
GS004    GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GS020                                                            
         B     GSEND                                                            
GS010    TM    APINDS,APILNSEQ     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOCONFIL+IOSQ+IO1  ELSE NEXT LIST LINE                        
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
         L     R2,AIOAREA1         CHECK DEPARTMENT RECORD TYPE                 
         MVC   APRECKEY(L'SADPKEY),SADPKEY  SAVE LAST RECORD KEY                
         CLI   SADPTYP,SADPTYPQ                                                 
         BNE   GSEND               IF NOT EXIT LIST                             
         CLI   SADPSUB,SADPSUBQ                                                 
         BNE   GSEND                                                            
         CLC   SADPAGY,AGENCYID                                                 
         BNE   GSEND                                                            
*&&UK                                                                           
*                                  TEST OFFICE MANAGER ACCESS                   
         GOTO1 ATSTOMAN,SADPOID                                                 
         BNE   GS022                                                            
         B     GS024                                                            
*&&                                                                             
*&&US                                                                           
*                                  TEST OFFICE/DEPT MANAGER ACCESS              
         GOTO1 ATSTDMAN,APPARM,SADPOID,SADPDID                                  
         BE    GS024                                                            
*&&                                                                             
GS022    MVC   GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSEL                                                           
*                                                                               
GS024    MVC   GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         OC    SELOID,SELOID       CHECK OFFICE CODE FILTER                     
         BZ    GS030                                                            
         CLC   SADPOID,SELOID                                                   
         BNE   GSEND                                                            
*                                 FILTER ON DEPARTMENT CODE                     
GS030    CLI   SELDIDSP,C' '       OFFICE CODE - FILTER ONLY IF IT              
         BNH   GS100                 CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GS032                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SADPDID(0),SELDID                                                
         BH    GSEND               (NO MORE RELEVENT RECORDS)                   
GS032    GOTO1 ATXTFLT,APPARM,(SELDIDL,SELDID),(L'SADPDID,SADPDID)              
         BNE   GETSEL              READ NEXT RECORD                             
*                                                                               
GS100    EQU   *                   EXIT GETSEL WITH RECORD OK                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GSEND    EQU   *                                                                
         MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTOID,SADPOID     DISPLAY KEY DATA                             
         MVC   LISTDID,SADPDID                                                  
         LA    R3,SADPDATA         GET ELEMENT DATA                             
*                                                                               
DS010    CLI   0(R3),0                                                          
         BE    DISSELX             END OF RECORD                                
         CLI   0(R3),SADPTELQ                                                   
         BE    DS030                                                            
         CLI   0(R3),SAPCTELQ                                                   
         BE    DS040                                                            
         CLI   0(R3),SAMANELQ                                                   
         BE    DS050                                                            
         CLI   0(R3),SAAPCELQ                                                   
         BE    DS060                                                            
*                                                                               
DS020    SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DS010                                                            
*                                                                               
         USING SADPTD,R3                                                        
DS030    ZIC   R1,SADPTLN          DEPARTMENT ELEMENT                           
         SH    R1,=Y(SADPTLNQ+1)                                                
         BM    DS032                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTDIDN(0),SADPTNAM  DISPLAY DEPARTMENT NAME                    
DS032    OC    SADPTMID,SADPTMID   CHECK MANAGER ID PRESENT                     
         BZ    DS020                                                            
         MVC   LISTPID,SADPTMID      AND DISPLAY IT                             
         B     DS020                                                            
*                                                                               
         USING SAPCTD,R3                                                        
DS040    EDIT  SAPCTVAL,(5,LISTCNT),ZERO=NOBLANK,WRK=APWORK,DUB=APDUB           
         B     DS020               DISPLAY STAFF COUNT                          
*                                                                               
         USING SAMAND,R3                                                        
DS050    EQU   *                   MANAGER ID ELEMENT                           
         CLI   SAMANORD,1          ORDER NUMBER 1 ONLY                          
         BNE   DS020                                                            
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   DS020                                                            
         MVC   LISTPID,APWORK                                                   
         B     DS020                                                            
*                                  DISPLAY APPROVER GROUP CODE                  
         USING SAAPCD,R3                                                        
DS060    OC    SAAPCCOD,SAAPCCOD                                                
         BZ    DS020                                                            
         MVC   LISTAPC,SAAPCCOD                                                 
         B     DS020                                                            
*                                                                               
DISSELX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R4,AREP                                                          
         USING REPD,R4             R4=A(REPORT WORK AREA)                       
         XC    APRECKEY,APRECKEY                                                
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                                                               
         LA    R2,APRECKEY                                                      
         XC    SADPKEY,SADPKEY     SET UP INITIAL KEY                           
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
VQ010    GOTO1 AFVAL,REPOIDH       READ OFFICE CODE FILTER                      
         BNE   VQ020                                                            
         LA    R1,IOKEY                                                         
         USING SAOFREC,R1                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELOID,FVIFLD                                                    
         MVC   SADPOID,FVIFLD      MOVE TO LIST KEY                             
*                                                                               
VQ020    GOTO1 AFVAL,REPDIDH       READ DEPARTMENT CODE FILTER                  
         BNE   VQ100                                                            
         OC    SELOID,SELOID                                                    
         BZ    SAEODF                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VQ022    CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VQ024               FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VQ022                                                         
VQ024    STC   RE,SELKEYCL                                                      
         MVC   SELDID,FVIFLD                                                    
         MVC   SELDIDL,FVILEN                                                   
         MVC   SELDIDSP,0(RF)                                                   
         MVC   SADPDID,SELDID      MOVE TO LIST KEY                             
*                                                                               
VQ100    MVCDD REPDESC,CT#DPTL     SET REPORT DESCRIPTION                       
         GOTO1 VDICTAT,APPARM,C'SL  ',REPDESC                                   
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE REPORT                                          *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R4,AREP                                                          
         MVC   LASTOFF,SPACES                                                   
         LA    R2,IOKEY                                                         
         MVC   SADPKEY(L'SADPKEY),APRECKEY SET INITIAL KEY VALUE                
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     PR014               GET FIRST RECORD                             
*                                  GET NEXT RECORD (SEQUENCE BROKEN)            
PR010    LA    R2,IOKEY                                                         
         MVC   SADPKEY(L'SADPKEY),APRECKEY                                      
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PRTREPX                                                          
*                                  GET NEXT RECORD (IN SEQUENCE)                
PR012    LA    R1,IOSQ+IOCONFIL+IO1                                             
PR014    GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
PR020    L     R2,AIOAREA1         CHECK DEPARTMENT KEY TYPE                    
         CLI   SADPTYP,SADPTYPQ                                                 
         BNE   PRTREPX             IF NOT EXIT                                  
         CLI   SADPSUB,SADPSUBQ                                                 
         BNE   PRTREPX                                                          
         CLC   SADPAGY,AGENCYID                                                 
         BNE   PRTREPX                                                          
         MVC   APRECKEY(L'SADPKEY),SADPKEY  SAVE LAST KEY                       
*&&UK                                                                           
*                                  CHECK OFFICE SECURITY MANAGER                
         GOTO1 ATSTOMAN,SADPOID                                                 
         BNE   PR020A1                                                          
         B     PR020A2                                                          
*&&                                                                             
*&&US                                                                           
*                                  CHECK OFFICE/DEPT SECURITY MANAGER           
         GOTO1 ATSTDMAN,APPARM,SADPOID,SADPDID                                  
         BE    PR020A2                                                          
*&&                                                                             
PR020A1  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     PR010                                                            
PR020A2  OC    SELOID,SELOID       CHECK OFFICE CODE FILTER                     
         BZ    PR020A                                                           
         CLC   SADPOID,SELOID                                                   
         BNE   PRTREPX                                                          
*                                 FILTER ON DEPARTMENT CODE                     
PR020A   CLI   SELDIDSP,C' '       OFFICE CODE - FILTER ONLY IF IT              
         BNH   PR020C                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    PR020B                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SADPDID(0),SELDID                                                
         BH    PRTREPX             (NO MORE RELEVENT RECORDS)                   
PR020B   GOTO1 ATXTFLT,APPARM,(SELDIDL,SELDID),(L'SADPDID,SADPDID)              
         BNE   PR012               READ NEXT RECORD                             
*                                                                               
PR020C   CLC   SADPOID,LASTOFF     DISPLAY KEY DATA                             
         BE    PR028                                                            
*                                  NEW OFFICE SUB HEADING                       
         LA    R1,IOKEY                                                         
         USING SAOFREC,R1                                                       
         XC    SAOFKEY,SAOFKEY     BUILD OFFICE KEY AND READ RECORD             
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,SADPOID                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   PR022                                                            
         L     R1,AIOAREA2         DISPLAY OFFICE NAME                          
         GOTO1 AGETONAM                                                         
         MVC   REPH5+20(L'SAOFFNAM),APWORK                                      
PR022    MVC   REPH5+16(L'SADPOID),SADPOID                                      
         MVCDD REPH5+3(8),CT#OFF                                                
         OI    REPHEADI,REPHFRCE                                                
*                                                                               
PR028    MVC   LASTOFF,SADPOID                                                  
         MVC   LINEDID,SADPDID                                                  
         LA    R3,SADPDATA         GET ELEMENT DATA                             
*                                                                               
PR030    CLI   0(R3),0             E-O-R                                        
         BE    PR100                                                            
         CLI   0(R3),SADPTELQ                                                   
         BE    PR050                                                            
         CLI   0(R3),SAPCTELQ                                                   
         BE    PR060                                                            
         CLI   0(R3),SAMANELQ                                                   
         BE    PR070                                                            
PR040    ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     PR030                                                            
*                                                                               
         USING SADPTD,R3                                                        
PR050    ZIC   R1,SADPTLN          DEPARTMENT ELEMENT                           
         SH    R1,=Y(SADPTLNQ+1)                                                
         BM    PR052                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINEDIDN(0),SADPTNAM  SAVE DEPARTMENT NAME                       
PR052    OC    SADPTMID,SADPTMID   CHECK MANAGER ID PRESENT                     
         BZ    PR040                                                            
         MVC   LINEPID,SADPTMID                                                 
         B     PR040                                                            
*                                                                               
         USING SAPCTD,R3                                                        
PR060    EDIT  SAPCTVAL,(5,LINECNT),ZERO=NOBLANK,WRK=APWORK,DUB=APDUB           
         B     PR040               SAVE STAFF COUNT ELEMENT                     
*                                                                               
         USING SAMAND,R3                                                        
PR070    EQU   *                   MANAGER ID ELEMENT                           
         CLI   SAMANORD,1          ORDER NUMBER 1 ONLY                          
         BNE   PR040                                                            
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   PR040                                                            
         MVC   LINEPID,APWORK                                                   
         B     PR040                                                            
*                                                                               
PR100    GOTO1 VREPORT,REPD        PRINT LINE OF REPORT                         
         B     PR010               READ SEQUENCE IS BROKEN                      
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         MVI   0(R3),KEYOID                                                     
         MVI   1(R3),4                                                          
         MVC   2(2,R3),SADPOID                                                  
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         MVI   0(R3),KEYDID                                                     
         MVI   1(R3),5                                                          
         MVC   2(3,R3),SADPDID                                                  
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
PUTKEYX  B     EXIT                                                             
         EJECT                                                                  
*                                  ERROR EXITS                                  
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                INPUT FIELD ERROR                            
SAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXIT                INPUT FIELD TOO LONG                         
SAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXIT                INPUT FIELD NOT NUMERIC                      
SAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXIT                INPUT FIELD TOO SHORT                        
SAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     EXIT                I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXIT                RECORD NOT FOUND                             
SAEDIF   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     EXIT                DUPLICATE                                    
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXIT                ALREADY EXISTS                               
SAEDPC   MVC   FVMSGNO,=AL2(CE#DELPC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                CANNOT DELETE WITH NON-ZERO STAFF            
SAEODF   MVC   FVMSGNO,=AL2(CE#OFFDP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                MUST ENTER OFFICE CODE WITH DEPT.            
         LTORG                                                                  
FFILL    DC    32X'FF'                                                          
SPACES   DC    132C' '                                                          
         EJECT                                                                  
REPSPEC  DS    0X                  REPORT HEADINGS SPECIFICATIONS               
         SPEC  H1,4,RUN                                                         
         SPEC  H1,30,CT#DPTL,30,L                                               
         SPEC  H2,30,CT#DPTL,30,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,4,CT#DPT,10,L                                                 
         SPEC  M2,4,CT#DPT,10,LU                                                
         SPEC  M1,17,CT#NAME,30,L                                               
         SPEC  M2,17,CT#NAME,30,LU                                              
         SPEC  M1,50,CT#MANID,12,L                                              
         SPEC  M2,50,CT#MANID,12,LU                                             
         SPEC  M1,71,CT#STAFF,15,L                                              
         SPEC  M2,71,CT#STAFF,15,LU                                             
         SPEC  END                                                              
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSF8D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD8D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB8D                                                       
         ORG                                                                    
         EJECT                                                                  
MANIDD   DSECT                     ** MANAGER ID FIELD LAYOUT **                
MANPIDH  DS    XL8                                                              
MANPID   DS    CL8                 PERSONAL ID                                  
MANPIDX  DS    XL8                                                              
MANPIDNH DS    XL8                                                              
MANPIDN  DS    CL30                PERSON NAME                                  
MANIDL   EQU   (DPTPI2H-DPTPIDH)                                                
MANNUM   EQU   6                                                                
         SPACE 1                                                                
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTSELH DS    XL8                                                              
LISTSEL  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLINX DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTOID  DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
LISTDID  DS    CL3                 DEPARTMENT CODE                              
         DS    CL1                                                              
LISTDIDN DS    CL30                DEPARTMENT NAME                              
         DS    CL1                                                              
LISTPID  DS    CL8                 MANAGER ID                                   
         DS    CL1                                                              
LISTCNT  DS    CL5                 STAFF COUNT                                  
         DS    CL2                                                              
LISTAPC  DS    CL8                 APPROVER GROUP CODE                          
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
         DS    CL3                 DEPARTMENT CODE                              
LINEDID  DS    CL3                 DEPARTMENT CODE                              
         DS    CL10                                                             
LINEDIDN DS    CL30                DEPARTMENT NAME                              
         DS    CL3                                                              
LINEPID  DS    CL8                 MANAGER ID                                   
         DS    CL13                                                             
LINECNT  DS    CL5                 STAFF COUNT                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
OLDCNT   DS    XL(L'SAPCTVAL)      LAST STAFF COUNT SAVE                        
TODAY    DS    XL2                 TODAYS DATE BINARY COMPRESSED                
TODAYC   DS    XL2                 TODAYS DATE COMPLEMENT                       
GETSEQF  DS    XL1                 GETSEL APINDS FLAGS SAVE                     
*                                                                               
LASTOFF  DS    XL(L'SADPOID)       LAST OFFICE ID FOR PRINT                     
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
SELOPT   DS    0X                  LIST SELECT FILTERS                          
SELOID   DS    XL(L'SADPOID)                                                    
SELDID   DS    XL(L'SADPDID)                                                    
SELDIDL  DS    XL1                                                              
SELDIDSP DS    XL1                                                              
SELKEYCL DS    XL1                                                              
SELOPTL  EQU   *-SELOPT                                                         
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SEACS07   05/01/02'                                      
         END                                                                    
