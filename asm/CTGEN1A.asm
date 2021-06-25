*          DATA SET CTGEN1A    AT LEVEL 042 AS OF 02/13/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045046.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE TA0B1AA                                                                  
         TITLE 'CTGEN1A - FEES MAINTENANCE - TVR BANDS'                         
GEN1A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN1A**,RA,RR=RE                                             
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GFEED,R2            R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLI   APMODE,APMVALK      MAINT MODES ?                                
         BE    VALKEY                                                           
         CLI   APMODE,APMVALR                                                   
         BE    VALREC                                                           
         CLI   APMODE,APMDISK      DISP  MODES ?                                
         BE    DISKEY                                                           
         CLI   APMODE,APMDISR                                                   
         BE    DISREC                                                           
         CLI   APMODE,APMVALP      LIST MODES  ?                                
         BE    VALSEL                                                           
         CLI   APMODE,APMGETS                                                   
         BE    GETSEL                                                           
         CLI   APMODE,APMDISS                                                   
         BE    DISSEL                                                           
         CLI   APMODE,APMDELR      DEL  MODE   ?                                
         BE    DELREC                                                           
         CLI   APMODE,APMRESR      RES  MODE                                    
         BE    RESREC                                                           
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD THE KEY OF A TVR BANDS RECORD                      *         
***********************************************************************         
*                                                                               
VALKEY   LA    R2,IOKEY                                                         
         USING GFEED,R2            R2=A(PAY RECORD KEY)                         
         XC    GFKEY,GFKEY                                                      
         MVI   GFKMIN,GFKMINQ      MINOR SYSTEM                                 
         MVI   GFKREC,GFKTVRQ      TVR RECORD TYPE=T                            
*                                                                               
         GOTO1 AFVAL,TVRAGRH                                                    
         TM    FVIIND,FVINUM       AGREEMENT NO. NUMERIC                        
         BNO   VALKEYN                                                          
         ZIC   RF,FVILEN                                                        
         GOTO1 VCASHVAL,APDUB,(C'N',FVIFLD),(RF)                                
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         STC   RE,GFKCSEQ          SET AGREEMENT IN KEY                         
*                                                                               
         MVC   APRECKEY(GFKEYL),GFKEY                                           
*                                                                               
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK080                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL THEREFORE NO REC                     
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VK080    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALKEYX                                                          
VALKEYN  MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALKEYX                                                          
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A TVR RECORD                               *         
***********************************************************************         
*                                                                               
VALREC   XC    LOCALD,LOCALX       CLEAR SAVE AREAS                             
*                                                                               
**********************************************                                  
* VALIDATE THE TVR PARAMETERS EXPIRY DATE    *                                  
**********************************************                                  
*                                                                               
         GOTO1 AFVAL,TVRDTOH                                                    
         BL    VRBADFLD                                                         
         GOTO1 VPERVAL,APDUB,(15,FVIFLD),('PVINSGLS+PVINSGLO',WORKER)           
         LA    R3,WORKER                                                        
         USING PERVALD,R3                                                       
         CLI   4(R1),PVRCONE       VALID SINGLE DATE OUTPUT                     
         BNE   VRBADFLD                                                         
         MVC   BINTDTE,PVALBSTA    STASH DATE                                   
         CLI   APACTN,ACTADD                                                    
         BE    VR008               NO CHECK REQUIRED ON ADD                     
         CLC   PVALBSTA,ASBDAT     EXPIRY > TODAY                               
         BH    VR008                                                            
**********************************************                                  
* THE EXPIRY DATE MUST BE > TODAY TO ENSURE THAT CONTRACTS ARE PAID             
* USING ONLY ONE SET OF PAY PARAMETERS. WE CANT ALLOW A BACKDATING              
* OF A PARAMETER SET AS THIS WOULD THEN ALLOW LIVE CONTRACTS TO BE              
* COSTED (VIA THE FEE-CALCULATION MODULE) ON THE NEXT RUN USING A NEW           
* SET OF PARAMETERS AND THIS WILL SIMPLY NOT WORK.                              
**********************************************                                  
         MVC   FVMSGNO,=AL2(CTNOBACK)                                           
         B     VALEXIT                                                          
*                                                                               
**********************************************                                  
* VALIDATE THE TVR BANDS & PERCENTAGES       *                                  
**********************************************                                  
*                                                                               
VR008    LA    R4,GFMAXBDQ         MAX BANDS                                    
         SR    R8,R8               USE R8 AS INDEX ON SCREEN FIELDS             
         LA    R9,SAVLINES         SAVE LINES VALIDATED INPUT                   
         USING SAVLIND,R9                                                       
*                                                                               
VR030    LA    RF,TVRT1(R8)                                                     
         CLC   0(L'TVRT1,RF),=C'++++++++'                                       
         BNE   VR034                                                            
         MVC   SVTVR,=X'FFFFFF'                                                 
         MVI   UPLIMIT,C'Y'        UPPER LIMIT FOUND                            
         B     VR040                                                            
*                                                                               
VR034    GOTO1 AFVAL,TVRT1H(R8)    BAND UPPER TVR LIMIT                         
         BH    VRBADFLD            BAD INPUT                                    
         BL    VR040               NO INPUT                                     
         ZIC   RF,FVILEN                                                        
         GOTO1 VCASHVAL,APDUB,(2,FVIFLD),(RF)                                   
         CLI   0(R1),0                                                          
         BNE   VRBADFLD                                                         
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         STCM  RE,7,SVTVR          STORE TVR LIMIT                              
*                                                                               
VR040    GOTO1 AFVAL,TVRP1H(R8)    PICK UP PERCENTAGE                           
         BH    VRBADFLD            BAD INPUT                                    
         BL    VR080               NO INPUT                                     
         ZIC   RF,FVILEN                                                        
         GOTO1 VCASHVAL,APDUB,(2,FVIFLD),(RF)                                   
         CLI   0(R1),0                                                          
         BNE   VRBADFLD                                                         
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         CLI   APDUB+4,0                                                        
         BNE   VRBADFLD            IT'S TOO BIG                                 
         STCM  RE,7,SVPER          STORE PERCENTAGE                             
*                                  BUMP TO NEXT LINE                            
VR080    LA    R8,TVRT2H-TVRT1H(R8)                                             
         LA    R9,SVLENQ(R9)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R4,VR030            LOOP BACK FOR NEXT LINE                      
*                                                                               
***********************************************************                     
* CHECK LINE ENTRY IS EMPTY / COMPLETE   * FROM SAVLINES  *                     
*       TABLE ENTRY IS HITVR/PERCENTAGE                   *                     
***********************************************************                     
*                                                                               
         LA    R4,GFMAXBDQ         MAX BANDS                                    
         LA    R9,SAVLINES                                                      
         USING SAVLIND,R9                                                       
         LA    R1,TVRT1H           POSITION CURSOR TO LINE 1                    
VR100    ST    R1,APCURSOR         FORCE CURSOR TO LINE N FIELD 1               
         OC    SVLINE,SVLINE       TEST SAVE ENTRY                              
         BZ    VR110               ITS ALL EMPTY                                
         OC    SVTVR,SVTVR         TVR MUST EXIST                               
         BZ    VRMISVAL            WOOPS                                        
         OC    SVPER,SVPER          % MUST EXIST                                
         BZ    VRMISVAL            WOOPS                                        
         OC    LSTLINE,LSTLINE                                                  
         BZ    VR110               FIRST LOOP                                   
         CLC   SVTVR,LSTTVR                                                     
         BNH   VRBADVAL                                                         
         CLC   SVPER,LSTPER                                                     
         BNL   VRBADVAL                                                         
*                                                                               
VR110    MVC   LSTLINE,SVLINE                                                   
         LA    R9,SVLENQ(R9)       BUMP TO NEXT TABLE ENTRY                     
         LA    R1,TVRT2H-TVRT1H(R1)   & CURSOR TO NEXT LINE                     
         BCT   R4,VR100            LOOP BACK FOR NEXT ENTRY                     
*                                                                               
         CLI   UPLIMIT,C'Y'                                                     
         BNE   VRBADVAL            NO UPPER LIMIT (++++++++) FOUND              
*                                                                               
******************************************                                      
* BUILD PAY ELEMENTS                     *                                      
******************************************                                      
*                                                                               
VR200    L     R2,AIOAREA1                                                      
         MVC   GFKEY,APRECKEY                                                   
         CLI   APACTN,ACTADD                                                    
         BE    VR210               NO ELEMENT TO REMOVE ON ADD                  
         MVI   APELEM,GFAGRELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GFEED       DELETE EXISTING PAY PERIOD ELEMENT           
         MVI   APELEM,GFBNDELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GFEED       DELETE EXISTING TVR BANDS                    
         MVI   APELEM,GFCOMELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GFEED       DELETE EXISTING COMMENTS                     
VR210    LA    R3,APELEM                                                        
         USING GFAGRD,R3                                                        
*                                                                               
*        ************************  BUILD PAY PERIOD ELEMENT                     
         XC    APELEM,APELEM                                                    
         MVI   GFAGREL,GFAGRELQ                                                 
         MVI   GFAGRELL,GFAGRLNQ                                                
         MVC   GFAGRTOD,BINTDTE    TO-DATE    BINARY                            
         GOTO1 AADDELS,GFEED                                                    
*                                                                               
*        ************************  BUILD TVR BAND ELEMENTS                      
         USING GFBNDD,R3                                                        
         MVI   GFBNDEL,GFBNDELQ                                                 
         MVI   GFBNDELL,GFBNDLNQ                                                
         LA    R4,GFMAXBDQ                                                      
         LA    R9,SAVLINES                                                      
VR240    OC    SVLINE,SVLINE       IF TABLE ENTRY EMPTY                         
         BZ    VR250               BYPASS BUILD                                 
         MVC   GFBNDHIT,SVTVR                                                   
         MVC   GFBNDPCT,SVPER                                                   
         GOTO1 AADDELS,GFEED                                                    
VR250    LA    R9,SVLENQ(R9)       BUMP DOWN TABLE                              
         BCT   R4,VR240            LOOP BACK                                    
*                                                                               
         GOTO1 AFVAL,TVRCOMH       ANY COMMENT                                  
         BNE   VR800                                                            
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING GFCOMMD,R3                                                       
         MVI   GFCOML,GFCOMELQ                                                  
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+4                                                           
         MVC   GFCOMMNT(0),FVIFLD                                               
         LA    RE,GFCOMFXQ+1(RE)   CALCULATE ELEMENT LENGTH                     
         STC   RE,GFCOMELL         AND SET IT                                   
         GOTO1 AADDELS,GFEED                                                    
*                                                                               
******************************************                                      
* BUILD ACTIVITY ELEMENT + EXEC THE IO   *                                      
******************************************                                      
*                                                                               
VR800    GOTO1 ASETACT,GFEED       DEFINE ACTIVITY ELEMENT                      
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     VALXXXOK                                                         
*                                  ERROR RETURNS                                
VRBADFLD MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALEXIT                                                          
VRBADVAL MVC   FVMSGNO,=AL2(CTILLOG)                                            
         B     VALEXIT                                                          
VRMISVAL MVC   FVMSGNO,=AL2(CTMISNG)                                            
         B     VALEXIT                                                          
*                                                                               
VALXXXOK MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R1,GENACTH          FORCE CURSOR TO NEXT ACTION                  
         ST    R1,APCURSOR                                                      
VALEXIT  B     EXIT                                                             
*                                                                               
         DROP  R2,R3,R9                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF TVR     RECORD                            *         
***********************************************************************         
*                                                                               
DISKEY   LA    R2,APRECKEY                                                      
         USING GFEED,R2                                                         
         EDIT  (B1,GFKCSEQ),(1,APWORK),WRK=WORKER,DUB=APDUB                     
         GOTO1 DISPFLD,TVRAGRH                                                  
         B     EXIT                                                             
         DROP R2                                                                
         SPACE 1                                                                
*********************************                                               
* ROUTINE TO DISPLAY TVR RECORD *                                               
*********************************                                               
*                                                                               
DISREC   L     R2,AIOAREA1                                                      
         USING GFEED,R2                                                         
*                                  SHOW ANY COMMENT                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GFCOMELQ                                                  
         GOTO1 AGETELS,GFEED                                                    
         ICM   R3,15,APPARM        R3=A(COMMENT ELEMENT)                        
         USING GFCOMMD,R3                                                       
         BZ    DREC02                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,GFCOMELL                                                      
         SH    RE,=Y(GFCOMFXQ+1)                                                
         EX    RE,*+4                                                           
         MVC   APWORK(0),GFCOMMNT                                               
         GOTO1 DISPFLD,TVRCOMH                                                  
*                                                                               
DREC02   XC    APELEM,APELEM                                                    
         MVI   APELEM,GFAGRELQ     FIND PAY PERIOD ELEMENT                      
         GOTO1 AGETELS,GFEED                                                    
         ICM   R3,15,APPARM                                                     
         USING GFAGRD,R3                                                        
         BZ    DREC03                                                           
         GOTO1 VDATCON,APDUB,(3,GFAGRTOD),(17,APWORK)                           
         GOTO1 DISPFLD,TVRDTOH                                                  
*                                  DISPLAY TVR BANDS                            
DREC03   XC    APELEM,APELEM                                                    
         MVI   APELEM,GFBNDELQ                                                  
         GOTO1 AGETELS,GFEED                                                    
         ICM   R3,15,APPARM                                                     
         USING GFBNDD,R3                                                        
         BZ    DISRECX                                                          
*                                                                               
         LA    R4,GFMAXBDQ                                                      
         SR    R8,R8                                                            
*                                                                               
DREC05   LA    R9,TVRT1H(R8)       DISPLAY TVR                                  
         CLC   GFBNDHIT,=X'FFFFFF'                                              
         BNE   DREC07                                                           
         MVC   APWORK(L'TVRT1),=C'++++++++'                                     
         B     DREC09                                                           
DREC07   EDIT  (B3,GFBNDHIT),(8,APWORK),2,                             Z        
               WRK=WORKER,DUB=APDUB,ZERO=NOBLANK                                
DREC09   GOTO1 DISPFLD,(R9)                                                     
         LA    R9,TVRP1H(R8)       DISPLAY PERCENTAGE                           
         EDIT  (B3,GFBNDPCT),(5,APWORK),2,                             Z        
               WRK=WORKER,DUB=APDUB,ZERO=NOBLANK                                
         GOTO1 DISPFLD,(R9)                                                     
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   DISRECX                                                          
*                                  BUMP TO NEXT LINE                            
         LA    R8,TVRT2H-TVRT1H(R8)                                             
         B     DREC05                                                           
*                                                                               
DISRECX  GOTO1 ADISACT,GFEED       DISPLAY ACTIVITY DATE                        
         LA    R1,GENACTH          FORCE CURSOR TO NEXT ACTION                  
         ST    R1,APCURSOR                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO VALIDATE LIST SELECT PARAMETERS                          *         
***********************************************************************         
*                                                                               
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         USING GFEED,R2                                                         
         XC    GFKEY,GFKEY                                                      
         MVI   GFKMAJ,X'FF'        FLAG FOR FIRST PASS                          
         MVI   GFKMIN,GFKMINQ                                                   
         MVI   GFKREC,GFKTVRQ                                                   
         XC    SELKEY,SELKEY                                                    
*                                                                               
         GOTO1 AFVAL,LSTAGRH       VALIDATE AGREEMENT NO (IF INPUT)             
         BNE   VS020                                                            
         TM    FVIIND,FVINUM                                                    
         BNO   VSBADFLD                                                         
         ZIC   RF,FVILEN                                                        
         GOTO1 VCASHVAL,APDUB,(C'N',FVIFLD),(RF)                                
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         STC   RE,SELAG            SET START AGREEMENT                          
*                                                                               
VS020    MVC   GFKCSEQ,SELAG       BUILD AN INITIAL KEY                         
         LA    R0,LSTSEL1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTSEL2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         B     VSXXXOK                                                          
*                                                                               
VSBADFLD MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VSEXIT                                                           
*                                                                               
VSXXXOK  MVC   FVMSGNO,=AL2(FVFOK)                                              
VSEXIT   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
*                                                                               
GETSEL   LA    R2,IOKEY                                                         
         USING GFEED,R2                                                         
         MVC   GFKEY,APRECKEY                                                   
         CLI   GFKMAJ,X'FF'        TEST FIRST TIME FLAG                         
         BNE   GETSEL2                                                          
         MVI   GFKMAJ,0                                                         
         B     GETSEL6             READ HIGH                                    
GETSEL2  TM    APINDS,2            TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,1            TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOGENDIR+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         LA    R2,IOKEY                                                         
         USING GFEED,R2                                                         
         CLI   GFKMIN,GFKMINQ      CHECK STILL FEES RECORD                      
         BNE   GETSELN                                                          
         CLI   GFKREC,GFKTVRQ      CHECK STILL TVR  RECORD                      
         BNE   GETSELN                                                          
         SPACE 1                                                                
*                                                                               
GETSEL14 GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GFAGRELQ                                                  
         GOTO1 AGETELS,GFEED                                                    
         ICM   R3,15,APPARM                                                     
         USING GFAGRD,R3                                                        
         BZ    GETSEL8             SKIP IF NO ELEMENT                           
*                                                                               
GETSELY  MVC   APRECKEY(L'GFKEY),GFKEY                                          
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
*                                                                               
DISSEL   L     R2,AIOAREA1                                                      
         USING GFEED,R2            R2=A(KEY)                                    
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         EDIT  (B1,GFKCSEQ),(1,LISTAGR),WRK=WORKER,DUB=APDUB                    
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GFAGRELQ                                                  
         GOTO1 AGETELS,GFEED                                                    
         ICM   R3,15,APPARM        R3=A(PAY PERIOD ELEMENT)                     
         USING GFAGRD,R3                                                        
         GOTO1 VDATCON,APDUB,(3,GFAGRTOD),(17,LISTTDTE)                         
*                                  DISPLAY ACTIVITY DATE                        
         GOTO1 AGETACT,GFEED       GET ACTIVITY ELEMENT                         
         BNE   DISSELX             CANT FIND ONE                                
         LA    R3,ACTEL            POINT TO ACTIVITY ELEMENT                    
         USING GACTELD,R3                                                       
         GOTO1 VDATCON,APDUB,(3,GACTCDT),(17,LISTADAT)                          
*                                                                               
DISSELX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A TVR BANDS RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
         USING GFEED,R2                                                         
         OI    GFDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GFEED                                                    
         OI    GFFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED TVR BANDS RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         USING GFEED,R2                                                         
         NI    GFDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GFEED                                                    
         NI    GFFSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* GENERAL FIELD XMT IF CHANGED                                       *          
* R1=A(TWAHDR)                                                       *          
* APWORK MUST CONTAIN THE NEW TEXT                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BE    DISPFLD2                                                         
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
DISPFLD2 EX    RF,DISPFLDX         CLEAR APWORK                                 
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDX MVC   APWORK(0),SPACES                                                 
****************************************************************                
         EJECT                                                                  
***********************************************************************         
* FIND NEXT ELEMENT OF SAME CODE WITHIN RECORD                        *         
* NTRY R3=A(CURRENT ELEMENT)                                          *         
*      APELEM = ELCODE TO FIND                                        *         
* EXIT CC EQ - FOUND - R3=A(NEW ELEMENT)                              *         
*      CC NE - NOT FOUND                                              *         
***********************************************************************         
*                                                                               
NEXTEL   ZIC   RF,1(R3)            L'ELEMENT                                    
         AR    R3,RF               A(NEXT ELEMNT)                               
         ICM   RF,1,1(R3)          L'ELEMENT                                    
         BNZ   *+8                                                              
         LTR   RB,RB               FORCE CC NON ZERO                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM                                                   
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* GEGENFEE                                                                      
       ++INCLUDE GEGENFEE                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE5D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENC5D                                                       
         ORG                                                                    
*                                                                               
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
         DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
         DS    CL4                                                              
LISTAGR  DS    CL1                 AGREEMENT                                    
         DS    CL10                                                             
LISTTDTE DS    CL8                 TO   DATE                                    
         DS    CL14                                                             
LISTADAT DS    CL8                 LAST ACTIVE                                  
         ORG   LISTLIN+L'LISTLIN                                                
*                                                                               
SAVLIND  DSECT                     ** DSECT TO COVER SAVED LINE *               
SVLINE   DS    0XL(SVLENQ)                                                      
SVTVR    DS    XL3                 BAND HIGH TVR                                
SVPER    DS    XL3                 BAND PERCENTAGE                              
SVLENQ   EQU   *-SVTVR             SIZE OF SAVLINE ENTRY                        
         SPACE 1                                                                
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
*                                                                               
SAVLINES DS    XL(GFMAXBDQ*SVLENQ) BINARY RESULTS STORE TABLE                   
LSTLINE  DS    0XL(SVLENQ)         PREV LINE FOR ASCEND/DESCEND TEST            
LSTTVR   DS    XL(L'SVTVR)                                                      
LSTPER   DS    XL(L'SVPER)                                                      
WORKER   DS    XL80                                                             
STREG    DS    F                                                                
BINTDTE  DS    XL3                                                              
SELKEY   DS    0XL2                LIST CHAN/AGR SELECTIONS                     
SELAG    DS    XL1                                                              
UPLIMIT  DS    XL1                                                              
*                                                                               
LOCALX   EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042CTGEN1A   02/13/15'                                      
         END                                                                    
