*          DATA SET SEACS06    AT LEVEL 028 AS OF 05/01/02                      
*PHASE TA0D06A,*                                                                
         TITLE 'SEACS06 - FILE MAINTENANCE - OFFICE RECORDS'                    
ACS06    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS6**,RR=RE                                                 
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2          R2=A(RECORD KEY)                             
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
* ROUTINE TO VALIDATE KEY OF OFFICE RECORD                            *         
***********************************************************************         
         SPACE   1                                                              
VALKEY   LA    R2,IOKEY                                                         
         USING SAOFREC,R2          R2=A(RECORD KEY)                             
         XC    SAOFKEY,SAOFKEY     BUILD OFFICE RECORD KEY                      
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
*                                                                               
         MVI   FVMINL,1            READ OFFICE CODE FIELD                       
         GOTO1 AFVAL,OFFOIDH                                                    
         BNE   VALKEYX                                                          
         MVC   SAOFOID,FVIFLD      MOVE TO KEY                                  
*                                                                               
         GOTO1 ATSTOMAN,FVIFLD     TEST OFFICE MANAGER ACCESS                   
         BNE   VALKEYX                                                          
*                                  SAVE KEY AND READ RECORD                     
         MVC   APRECKEY(L'SAOFKEY),SAOFKEY                                      
         LA    R1,IORDD+IOCONFIL+IO1                                            
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
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN OFFICE RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         XC    OFFPIDN,OFFPIDN     CLEAR MANAGER NAME FIELD                     
         OI    OFFPIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    OLDCNT,OLDCNT       INITIALISE STAFF COUNT                       
         CLI   APACTN,ACTADD       ADD ACTION                                   
         BE    VR010                                                            
*                                  ELSE IF CHANGE                               
         LA    R3,SAOFDATA           EXTRACT OLD STAFF COUNT                    
         SR    R0,R0                                                            
VR002    CLI   0(R3),0                                                          
         BE    VR010                                                            
         CLI   0(R3),SAPCTELQ                                                   
         BE    VR004                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VR002                                                            
*                                                                               
         USING SAPCTD,R3                                                        
VR004    MVC   OLDCNT,SAPCTVAL     SAVE OLD STAFF COUNT                         
         B     VR010                                                            
*                                  BUILD INITIAL RECORD                         
VR010    XC    SAOFKEY(256),SAOFKEY                                             
         MVC   SAOFKEY(L'SAOFKEY),APRECKEY                                      
         LA    R0,SAOFDATA+1-SAOFREC                                            
         STCM  R0,3,SAOFLEN                                                     
*                                                                               
         LA    R3,APELEM           INITIALISE OFFICE ELEMENT                    
         USING SAOFFD,R3                                                        
         XC    SAOFFEL(SAOFFLNQ),SAOFFEL                                        
         MVI   SAOFFEL,SAOFFELQ                                                 
         MVI   SAOFFLN,SAOFFLNQ                                                 
*                                                                               
         MVI   FVMINL,1            READ OFFICE NAME FIELD                       
         GOTO1 AFVAL,OFFOIDNH                                                   
         BNE   VALRECX                                                          
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAOFFNAM(0),FVIFLD  SAVE NAME AND LENGTH                         
         AH    R1,=Y(SAOFFLNQ+1)                                                
         STC   R1,SAOFFLN                                                       
*                                                                               
VR020    GOTO1 AADDELS,SAOFREC     ADD OFFICE ELEMENT                           
*                                                                               
         USING SAPCTD,R3                                                        
VR030    XC    SAPCTEL(SAPCTLNQ),SAPCTEL                                        
         MVI   SAPCTEL,SAPCTELQ    BUILD STAFF COUNT ELEMENT                    
         MVI   SAPCTLN,SAPCTLNQ                                                 
         MVC   SAPCTVAL,OLDCNT                                                  
         GOTO1 AADDELS,SAOFREC       AND ADD IT                                 
*                                                                               
         USING SAAPCD,R3                                                        
VR032    EQU   *                                                                
         LA    R3,APELEM           INITIALISE ELEMENT                           
         XC    SAAPCEL(SAAPCLNQ),SAAPCEL                                        
         MVI   SAAPCEL,SAAPCELQ                                                 
         MVI   SAAPCLN,SAAPCLNQ                                                 
         MVI   FVMINL,1            VALIDATE APPROVER GROUP FIELD                
         GOTO1 AFVAL,OFFAPCH                                                    
         BNE   VR040                                                            
         MVC   SAAPCCOD,FVIFLD                                                  
         GOTO1 AGETAPG,SAAPCCOD    GET APPROVER GROUP RECORD DATA               
         BNE   VALRECX                                                          
         MVC   SAAPCNUM,APHALF                                                  
         GOTO1 AADDELS,SAOFREC     ADD ELEMENT                                  
         DROP  R3                                                               
*                                                                               
VR040    LA    R4,OFFPIDH          PROCESS MANAGER ID ELEMENTS                  
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
         GOTO1 AADDELS,SAOFREC     ADD MANGER ID ELEMENT                        
VR044    LA    R4,MANIDL(R4)                                                    
         BCT   R0,VR042                                                         
         DROP  R4                                                               
*                                                                               
VR100    GOTO1 ASETACT,SAOFREC     DEFINE ACTIVITY ELEMENT                      
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
* ROUTINE TO DISPLAY KEY OF OFFICE RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
*                                                                               
         MVC   OFFOID,SAOFOID                                                   
         OI    OFFOIDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY OFFICE RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         MVC   OFFOID,SAOFOID      DISPLAY KEY DATA                             
         OI    OFFOIDH+(FVOIND-FVIHDR),FVOXMT                                   
         TWAXC OFFOIDNH                                                         
         XC    OFFAPCN,OFFAPCN                                                  
         OI    OFFAPCNH+(FVOIND-FVIHDR),FVOXMT                                  
         BAS   RE,REENTER          HANDLE DELETE/RESTORE PROMPT                 
         LA    R4,OFFPIDH                                                       
         LA    R0,MANNUM                                                        
         USING MANIDD,R4                                                        
DR002    XC    MANPIDN,MANPIDN                                                  
         OI    MANPIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R4,MANIDL(R4)                                                    
         BCT   R0,DR002                                                         
DR004    LA    R4,OFFPIDH                                                       
         LA    R3,SAOFDATA         GET ELEMENT DATA                             
*                                                                               
DR010    CLI   0(R3),0                                                          
         BE    DISRECX             END OF RECORD                                
         CLI   0(R3),SAOFFELQ                                                   
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
         USING SAOFFD,R3                                                        
DR030    ZIC   R1,SAOFFLN          OFFICE ELEMENT                               
         SH    R1,=Y(SAOFFLNQ+1)                                                
         BM    DR032                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFOIDN(0),SAOFFNAM DISPLAY OFFICE NAME                          
DR032    OC    SAOFFMID,SAOFFMID   IF MANAGER ID PRESENT                        
         BZ    DR020                                                            
         MVC   MANPID,SAOFFMID                                                  
         GOTO1 AGETPNAM,SAOFFMID   GET PERSON NAME                              
         BNE   DR036                                                            
         MVC   MANPIDN,APWORK      DISPLAY MANAGER NAME                         
         LA    R4,MANIDL(R4)                                                    
         B     DR020                                                            
DR036    MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APHALF,=Y(CS#PIDNF) NAME NOT FOUND MESSAGE                       
         GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   MANPIDN,APWORK      DISPLAY MANAGER NAME                         
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
         MVC   OFFAPC,SAAPCCOD                                                  
         GOTO1 AGETAPG,SAAPCCOD                                                 
         BNE   VALRECX                                                          
         MVC   OFFAPCN,APWORK                                                   
         B     DR020                                                            
         DROP  R3                                                               
*                                                                               
DISRECX  GOTO1 ADISACT,SAOFREC     DISPLAY ACTIVITY DATE                        
         B     EXIT                                                             
         DROP  R4                                                               
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
* ROUTINE TO DELETE AN OFFICE RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         LA    R3,SAOFDATA         GET STAFF COUNT ELEMENT                      
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
DREC020  OC    SAPCTVAL,SAPCTVAL   CAN NOT DELETE RECORD IF                     
         BNZ   SAEDPC                STAFF COUNT NOT ZERO                       
*                                                                               
         USING SADPREC,R1                                                       
DREC030  LA    R1,IOKEY            CHECK NO ASSOCIATED                          
         MVC   SADPKEY,SAOFKEY       DEPARTMENT RECORDS                         
         MVI   SADPSUB,SADPSUBQ                                                 
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    DREC040                                                          
         L     R1,AIOAREA2                                                      
         CLC   SADPKEY(SADPDID-SADPKEY),IOKEY                                   
         BE    SAEDOD                CAN NOT DELETE                             
*                                                                               
*                                  DELETE OFFICE RECORD                         
DREC040  MVC   IOKEY(L'SAOFKEY),SAOFKEY                                         
         GOTO1 ASETACT,SAOFREC                                                  
         OI    SAOFSTAT,X'80'      SET DELETE FLAG                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
*          DATA SET SEACS05    AT LEVEL 156 AS OF 14/06/91                      
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED OFFICE RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SAOFREC                                                  
         NI    SAOFSTAT,X'FF'-X'80'  CLEAR DELETE FLAG                          
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
         XC    SAOFKEY,SAOFKEY                                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
VSOFF    GOTO1 AFVAL,LSTOIDH       VALIDATE OFFICE CODE FILTER                  
         BNE   VSOFFX                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VSOFF1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VSOFF2              FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VSOFF1                                                        
VSOFF2   STC   RE,SELKEYCL                                                      
         MVC   SELOID,FVIFLD                                                    
         MVC   SELOIDL,FVILEN                                                   
         MVC   SELOIDSP,0(RF)                                                   
VSOFFX   EQU   *                                                                
         MVC   SAOFOID,SELOID                                                   
*                                                                               
         MVI   GETSEQF,0                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY            READ NEXT RECORD                             
         MVC   SAOFKEY,APRECKEY      FROM LAST KEY                              
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
         L     R2,AIOAREA1         CHECK OFFICE RECORD TYPE                     
         MVC   APRECKEY(L'SAOFKEY),SAOFKEY  SAVE LAST RECORD KEY                
         CLI   SAOFTYP,SAOFTYPQ                                                 
         BNE   GSEND                 ELSE EXIT LIST                             
         CLI   SAOFSUB,SAOFSUBQ                                                 
         BNE   GSEND                                                            
         CLC   SAOFAGY,AGENCYID                                                 
         BNE   GSEND                                                            
         GOTO1 ATSTOMAN,SAOFOID    TEST OFFICE MANAGER ACCESS                   
         BE    GS024                                                            
         MVC   GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSEL                                                           
*                                                                               
GS024    MVC   GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
*                                  FILTER ON OFFICE CODE                        
GSOID    CLI   SELOIDSP,C' '       OFFICE CODE - FILTER ONLY IF IT              
         BNH   GSOIDX                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GSOID1                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAOFOID(0),SELOID                                                
         BH    GSEND               (NO MORE RELEVENT RECORDS)                   
GSOID1   GOTO1 ATXTFLT,APPARM,(SELOIDL,SELOID),(L'SAOFOID,SAOFOID)              
         BNE   GETSEL              READ NEXT RECORD                             
GSOIDX   EQU   *                                                                
*                                  SAVE LAST RECORD KEY                         
GS100    EQU   *                   EXIT GETSEL WITH RECORD OK                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GSEND    MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
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
         MVC   LISTOID,SAOFOID     DISPLAY KEY DATA                             
         LA    R3,SAOFDATA         GET ELEMENT DATA                             
*                                                                               
DS010    CLI   0(R3),0                                                          
         BE    DISSELX             END OF RECORD                                
         CLI   0(R3),SAOFFELQ                                                   
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
         USING SAOFFD,R3                                                        
DS030    ZIC   R1,SAOFFLN          OFFICE ELEMENT                               
         SH    R1,=Y(SAOFFLNQ+1)                                                
         BM    DS032                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTOIDN(0),SAOFFNAM  DISPLAY OFFICE NAME                        
DS032    OC    SAOFFMID,SAOFFMID   CHECK MANAGER ID PRESENT                     
         BZ    DS020                                                            
         MVC   LISTPID,SAOFFMID    DISPLAY ID                                   
         B     DS020                                                            
*                                  DISPLAY STAFF COUNT                          
         USING SAPCTD,R3                                                        
DS040    EDIT  SAPCTVAL,(5,LISTCNT),ZERO=NOBLANK,WRK=APWORK,DUB=APDUB           
         B     DS020                                                            
*                                  DISPLAY MANAGER ID                           
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
         XC    SAOFKEY,SAOFKEY     SET UP INITIAL KEY                           
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
*                                                                               
         GOTO1 AFVAL,REPOIDH       GET START OFFICE CODE KEY                    
         BNE   VQ010                                                            
         MVC   SELOID,FVIFLD                                                    
         MVC   SELOIDL,FVILEN                                                   
         MVC   SAOFOID,SELOID                                                   
*                                                                               
VQ010    MVCDD REPDESC,CT#OFFL     SET REPORT DESCRIPTION                       
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
         LA    R2,IOKEY                                                         
         MVC   SAOFKEY(L'SAOFKEY),APRECKEY SET INITIAL KEY VALUE                
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     PR014               GET FIRST RECORD                             
*                                  GET NEXT RECORD (SEQUENCE BROKEN)            
PR010    LA    R2,IOKEY                                                         
         MVC   SAOFKEY(L'SAOFKEY),APRECKEY                                      
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PRTREPX                                                          
*                                  GET NEXT RECORD (IN SEQUENCE)                
PR012    LA    R1,IOSQ+IOCONFIL+IO1                                             
PR014    GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
PR020    L     R2,AIOAREA1         CHECK OFFICE RECORD TYPE                     
         CLI   SAOFTYP,SAOFTYPQ                                                 
         BNE   PRTREPX             EXIT IF NOT                                  
         CLI   SAOFSUB,SAOFSUBQ                                                 
         BNE   PRTREPX                                                          
         CLC   SAOFAGY,AGENCYID                                                 
         BNE   PRTREPX                                                          
         MVC   APRECKEY(L'SAOFKEY),SAOFKEY  SAVE LAST KEY                       
         GOTO1 ATSTOMAN,SAOFOID    CHECK OFFICE SECURITY MANAGER                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     PR010                                                            
         MVC   LINEOID,SAOFOID     MOVE KEY DATA TO BUFFER                      
         LA    R3,SAOFDATA         GET ELEMENT DATA                             
*                                                                               
PR030    CLI   0(R3),0             E-O-R                                        
         BE    PR100                                                            
         CLI   0(R3),SAOFFELQ                                                   
         BE    PR050                                                            
         CLI   0(R3),SAPCTELQ                                                   
         BE    PR060                                                            
         CLI   0(R3),SAMANELQ                                                   
         BE    PR070                                                            
PR040    ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     PR030                                                            
*                                                                               
         USING SAOFFD,R3                                                        
PR050    ZIC   R1,SAOFFLN          OFFICE ELEMENT                               
         SH    R1,=Y(SAOFFLNQ+1)                                                
         BM    PR052                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINEOIDN(0),SAOFFNAM OFFICE NAME                                 
PR052    OC    SAOFFMID,SAOFFMID   IF MANAGER ID PRESENT                        
         BZ    PR040                                                            
         MVC   LINEPID,SAOFFMID                                                 
         B     PR040                                                            
*                                  STAFF COUNT ELEMENT                          
         USING SAPCTD,R3                                                        
PR060    EDIT  SAPCTVAL,(5,LINECNT),ZERO=NOBLANK,WRK=APWORK,DUB=APDUB           
         B     PR040                                                            
*                                  MANAGER ID                                   
         USING SAMAND,R3                                                        
PR070    EQU   *                                                                
         CLI   SAMANORD,1          ORDER NUMBER 1 ONLY                          
         BNE   PR040                                                            
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   PR040                                                            
         MVC   LINEPID,APWORK                                                   
         B     PR040                                                            
*                                                                               
PR100    GOTO1 VREPORT,REPD        PRINT LINE                                   
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
         MVC   2(2,R3),SAOFOID                                                  
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
SAERTB   MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                RECORD TOO BIG                               
SAEDPC   MVC   FVMSGNO,=AL2(CE#DELPC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                CANNOT DELETE WITH NON-ZERO STAFF            
SAEDOD   MVC   FVMSGNO,=AL2(CE#DELOD)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     EXIT                CANNOT DELETE WITH ASSOCIATED DEPTS          
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
FFILL    DC    32X'FF'                                                          
SPACES   DC    32C' '                                                           
         EJECT                                                                  
REPSPEC  DS    0X                  REPORT HEADING SPECIFICATIONS                
         SPEC  H1,4,RUN                                                         
         SPEC  H1,30,CT#OFFL,30,C                                               
         SPEC  H2,30,CT#OFFL,30,CU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,4,CT#OFF,7,L                                                  
         SPEC  M2,4,CT#OFF,7,LU                                                 
         SPEC  M1,14,CT#NAME,30,L                                               
         SPEC  M2,14,CT#NAME,30,LU                                              
         SPEC  M1,47,CT#MANID,12,L                                              
         SPEC  M2,47,CT#MANID,12,LU                                             
         SPEC  M1,62,CT#STAFF,15,L                                              
         SPEC  M2,62,CT#STAFF,15,LU                                             
         SPEC  END                                                              
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSF9D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD9D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB9D                                                       
         ORG                                                                    
         EJECT                                                                  
MANIDD   DSECT                     ** MANAGER ID FIELD LAYOUT **                
MANPIDH  DS    XL8                                                              
MANPID   DS    CL8                 PERSONAL ID                                  
MANPIDX  DS    XL8                                                              
MANPIDNH DS    XL8                                                              
MANPIDN  DS    CL30                PERSON NAME                                  
MANIDL   EQU   (OFFPI2H-OFFPIDH)                                                
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
LISTOIDN DS    CL30                OFFICE NAME                                  
         DS    CL1                                                              
LISTPID  DS    CL8                 MANAGER PERSONAL ID                          
         DS    CL1                                                              
LISTCNT  DS    CL5                 STAFF COUNT                                  
         DS    CL2                                                              
LISTAPC  DS    CL8                 APPROVER GROUP CODE                          
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
         DS    CL3                                                              
LINEOID  DS    CL2                 OFFICE CODE                                  
         DS    CL8                                                              
LINEOIDN DS    CL30                OFFICE NAME                                  
         DS    CL3                                                              
LINEPID  DS    CL8                 MANAGER PERSONAL ID                          
         DS    CL7                                                              
LINECNT  DS    CL5                 STAFF COUNT                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
OLDCNT   DS    XL(L'SAPCTVAL)      OLD STAFF COUNT                              
TODAY    DS    XL2                 TODAYS DATE BINARY COMPRESSED                
TODAYC   DS    XL2                 DATE COMPLEMENTED                            
GETSEQF  DS    XL1                 GETSEL APINDS SAVE                           
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
SELOPT   DS    0X                  LIST SELECT FILTERS                          
SELOID   DS    XL(L'SADPOID)       OFFICE CODE                                  
SELOIDL  DS    XL1                 LENGTH                                       
SELOIDSP DS    XL1                 FIRST NON SPACE CHAR                         
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028SEACS06   05/01/02'                                      
         END                                                                    
