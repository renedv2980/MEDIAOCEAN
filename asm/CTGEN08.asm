*          DATA SET CTGEN08    AT LEVEL 005 AS OF 10/12/09                      
*PHASE TA0B08B                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE NUMVAL                                                                 
         TITLE 'CTGEN08 - TERMINAL RECORD PRINTERQ MAINTENANCE'                 
GEN08    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN8**,RA,R9,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTTREC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         L     R1,ACOM                                                          
         L     R1,CXSORT-COMFACSD(R1)                                           
         ST    R1,VXSORT                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     EXIT                05 - APMDELR                                 
         B     EXIT                06 - APMRESR                                 
         B     EXIT                07 - APMVALP                                 
         B     EXIT                08 - APMGETS                                 
         B     EXIT                09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     EXIT                11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     EXIT                14 - APMLSCR                                 
         B     VALREQ              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     EXIT                18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF TERMINAL RECORD                                    
***********************************************************************         
VALKEY   LA    R2,IOKEY            R2=A(RECORD KEY)                             
*                                                                               
         XC    PELCNT,PELCNT       INITIALISE PRINTERQ ELEMENT LIST             
         XC    PELDEL,PELDEL                                                    
         LA    RF,PELLEN*MAXPEL                                                 
         XCEF  PELIST,(RF)                                                      
         LA    RF,PELIST                                                        
         ST    RF,PELPNTR                                                       
*                                                                               
         XC    PALCNT,PALCNT       INITIALISE KEY PASSWORD LIST                 
         LA    RF,PALIST                                                        
         ST    RF,PALPNTR                                                       
*                                                                               
         XC    CTTKEY,CTTKEY       INITIALISE RECORD KEY                        
         MVI   CTTKTYP,C'T'                                                     
         GOTO1 AFVAL,PRTIDH        VALIDATE TERMINAL ID                         
         BNE   VALKEYX                                                          
         TM    FVIIND,FVINUM       NUMERIC-ASSUME EXISTING UTL NUM              
         BO    VKEY020                                                          
         CLI   FVIFLD,C'#'         #NNNN MEANS TERM OR PSWD NUM INPUT           
         BE    VKEY030                                                          
         SR    R1,R1               READ AND SPACE FILL 8 CHR ID                 
         LA    RF,8                                                             
         IC    R1,FVILEN                                                        
         SR    RF,R1               VTAM IDS CAN BE < 8 CHRS                     
         BM    EIIF                                                             
         BZ    VKEY010                                                          
         LA    R1,FVIFLD(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES                                                   
VKEY010  MVC   CTTKTID(8),FVIFLD                                                
         B     VKEY040                                                          
*                                                                               
VKEY020  XC    APPARM(20),APPARM   VALIDATE TERMINAL NUM                        
         L     RF,ACOM                                                          
         L     RF,CTERMVAL-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,(X'00',FVIHDR)                                       
         SR    R3,R3                                                            
         ICM   R3,7,APPARM+5       ERROR IF NUMBER NOT IN UTL                   
         BZ    EIIF                                                             
         SR    RE,RE                                                            
         ICM   RE,7,APPARM+1       POINT TO RETURN TERMINAL ID                  
         MVC   CTTKTID(8),0(RE)                                                 
         B     VKEY040                                                          
*                                                                               
VKEY030  BAS   RE,VALPSVN          VALIDATE #NNNNN TERM/PSWD NUMBER             
         BNE   EIIF                                                             
         B     VALKEY              BACK TO VALIDATE ALPHA FIELDS                
*                                                                               
VKEY040  XC    PRTID,PRTID         ECHO TERMINAL ID BACK TO TWA                 
         MVC   PRTID(8),CTTKTID                                                 
         MVI   PRTIDH+5,8                                                       
         OI    PRTIDH+6,X'80'                                                   
*                                                                               
         MVI   MOVEPASS,0                                                       
*                                                                               
         CLI   APACTN,ACTMOVE                                                   
         BE    VKEY044                                                          
         MVC   MOVEKEY,IOKEY                                                    
         B     VKEY046                                                          
*                                                                               
VKEY044  MVC   KEYSAVE2,IOKEY                                                   
         B     VKEY046                                                          
VKEY045  MVI   MOVEPASS,1                                                       
         MVC   IOKEY,MOVEKEY                                                    
*                                                                               
VKEY046  GOTO1 AIO,IOREAD+IOCONFIL+IO1  READ MASTER TERMINAL REC                
         BL    EIIO                                                             
         BH    ERNF                MUST EXIST                                   
         L     R2,AIOAREA1                                                      
         TM    CTTSTAT,X'01'                                                    
         BO    ERNF                IGNORE PASSIVE                               
         TM    CTTSTAT,X'04'                                                    
         BNO   ERNF                MUST BE PRINTER TYPE                         
*                                                                               
         BRAS  RE,ADDLST                                                        
*                                  READ ASSOCIATED RECORDS                      
VKEY050  GOTO1 AIO,IOSQ+IOCONFIL+IO2                                            
         BL    EIIO                                                             
         BH    VKEY060             SEQUENTIAL READ TO NEXT LUID                 
         L     R2,AIOAREA2                                                      
         CLC   CTTKEY(CTTKPASS-CTTKEY),IOKEY                                    
         BNE   VKEY060                                                          
         TM    CTTSTAT,X'01'                                                    
         BO    VKEY060                                                          
*                                                                               
         BRAS  RE,ADDLST           ADD PRINTERQ ELEMENTS TO LIST                
         B     VKEY050                                                          
*                                                                               
VKEY060  LA    R4,PELIST           CONVERT ID #S IN LIST TO ALPHA               
         SR    R8,R8                                                            
         LH    R8,PELCNT                                                        
         LTR   R8,R8                                                            
         BZ    VKEY080                                                          
         USING PELISTD,R4                                                       
VKEY070  MVC   IDNUM,PELSRCN                                                    
         BRAS  RE,GETIDA                                                        
         BE    VKEY072             IF ID RECORD NOT VALID                       
         MVI   PELSRCA,0           FLAG ENTRY FOR EARLY SORT                    
         LA    R3,PELSRCA+1                                                     
*                                  AND CONVERT ID# FOR DISPLAY                  
         EDIT  (B2,IDNUM),(9,(R3)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB              
         B     VKEY074                                                          
VKEY072  MVC   PELSRCA,IDALPH                                                   
VKEY074  LA    R4,PELLEN(R4)                                                    
         BCT   R8,VKEY070                                                       
         DROP  R4                                                               
*                                                                               
VKEY080  BRAS  RE,SRTLST           SORT PQ ELEMENT LIST                         
*                                                                               
         CLI   APACTN,ACTMOVE                                                   
         BNE   VKEY090                                                          
*                                                                               
         CLI   MOVEPASS,0                                                       
         BE    VKEY084                                                          
         MVC   PALCNT,MOPALCNT                                                  
         MVC   PALIST,MOPALIST                                                  
         MVC   PALPNTR,MOPALPNT                                                 
         MVC   IOKEY,KEYSAVE2                                                   
         B     VKEY090                                                          
*                                                                               
VKEY084  MVC   MOPALCNT,PALCNT                                                  
         MVC   MOPALIST,PALIST                                                  
         MVC   MOPALPNT,PALPNTR                                                 
*                                                                               
         XC    PELCNT,PELCNT       INITIALISE PRINTERQ ELEMENT LIST             
         XC    PELDEL,PELDEL                                                    
         LA    RF,PELLEN*MAXPEL                                                 
         XCEF  PELIST,(RF)                                                      
         LA    RF,PELIST                                                        
         ST    RF,PELPNTR                                                       
*                                                                               
         XC    PALCNT,PALCNT       INITIALISE KEY PASSWORD LIST                 
         LA    RF,PALIST                                                        
         ST    RF,PALPNTR                                                       
         B     VKEY045                                                          
                                                                                
*----------------------------------                                             
* VALIDATE THIS/LAST ACTIONS                                                    
*----------------------------------                                             
VKEY090  LA    R2,IOKEY            REREAD MASTER RECORD                         
         MVC   APRECKEY(L'CTTKEY),CTTKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BH    *+12                NRF - CHECK IF DELETED                       
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         MVI   APINDS,APIOKADD                                                  
*                                                                               
VALKEYY  DS    0H                                                               
         OI    GENSRVH+FHOID,FHOIMO+FHOITR                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   SAVETID,CTTKTID                                                  
         BE    VALKEYX                                                          
         MVC   SAVETID,CTTKTID     IF CHANGE OF TERMINAL LUID                   
         XC    PELOFF,PELOFF       INITIALISE LIST DISPLAY                      
         MVI   PAGENO,1                                                         
         B     VALKEYX                                                          
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CHANGE TERMINAL RECORDS PRINTERQ DATA                              
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
         MVC   CTTKEY,APRECKEY                                                  
         XC    TERMNUM,TERMNUM     CLEAR PASSIVE DATA SAVE AREAS                
         XC    SVTRMEL,SVTRMEL                                                  
         MVI   SVSTAT,0                                                         
*                                                                               
VRCHA    CLI   APACTN,ACTMOVE      MOVE ACTION   - SAVE ORIG STATUS             
         BE    *+14                                                             
         CLI   APACTN,ACTCHA       CHANGE ACTION - SAVE ORIG STATUS             
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVSTAT,CTTSTAT                                                   
         TM    SVSTAT,X'04'                                                     
         BZ    *+8                                                              
         OI    TERMINFO,X'04'      SET TERMINAL IS A PRINTER                    
         TM    SVSTAT,X'08'                                                     
         BZ    *+8                                                              
         OI    TERMINFO,X'08'      SET TERM IN AUTO MODE                        
         LA    RF,250              INITIALISE PQUEUE MAX# DEFAULT               
         STH   RF,MAXNUM                                                        
         LA    R3,CTTDATA          AND STRIP DOWN RECORD                        
         SR    R4,R4                                                            
VRCHA10  CLI   0(R3),0                                                          
         BE    VRCHAX                                                           
         CLI   0(R3),X'03'                                                      
         BNE   VRCHA20                                                          
         MVC   TERMNUM,2(R3)       SAVE TERMINAL NUMBER PASSIVE                 
         B     VRCHA50                                                          
VRCHA20  CLI   0(R3),X'25'         SAVE TERMINAL DEFN ELEMENT                   
         BNE   VRCHA30                                                          
         MVC   SVTRMEL,0(R3)                                                    
         TM    CTTRMDEV-CTTRMD(R3),X'80'                                        
         BZ    VRCHA70                                                          
         OI    TERMINFO,X'04'      SET DEVICE IS A PRINTER                      
         MVI   MAXNUM,0                                                         
         MVC   MAXNUM+1(1),CTTRMPRQ-CTTRMD(R3)                                  
         CLI   CTTRMDEV-CTTRMD(R3),X'82'                                        
         BNE   *+12                                                             
         OI    TERMINFO,X'18'      SET DEVICE IS SHUTTLE/AUTO                   
         B     VRCHA70                                                          
         TM    CTTRMAT1-CTTRMD(R3),X'02'                                        
         BZ    *+8                                                              
         OI    TERMINFO,X'08'      SET DEVICE IS AUTO                           
         B     VRCHA70                                                          
VRCHA30  CLI   0(R3),X'01'                                                      
         BE    VRCHA50             DELETE ELEMENTS                              
         CLI   0(R3),X'28'                                                      
         BE    VRCHA50                                                          
         CLI   0(R3),X'29'                                                      
         BE    VRCHA50                                                          
         CLI   0(R3),X'21'         GET RID OF SYSTEM ELEMENTS                   
         BE    VRCHA50                                                          
         B     VRCHA70                                                          
*                                                                               
VRCHA50  SR    R0,R0                                                            
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',=C'CTFILE  '),((R0),CTTREC),0,0              
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRCHA10                                                          
*                                                                               
VRCHA70  IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     VRCHA10                                                          
*                                                                               
VRCHAX   B     VRPID                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE PRINTER ID                                                           
***********************************************************************         
VRPID    XC    APELEM,APELEM                                                    
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PRTPRIDH      PRINTER ID MUST BE INPUT                     
         BNE   EXIT                                                             
         MVI   FVMINL,0                                                         
         CLC   FVIFLD(6),=C'DELETE'  REMOVE OR IGNORE                           
         BE    VRPIDX                                                           
         GOTO1 VSCANNER,APPARM,FVIHDR,(2,BLOCK1)                                
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVI   FVINDX,2                                                         
         CLI   4(R1),2                                                          
         BL    EMIF                                                             
         LA    R4,BLOCK1           R4=A(SCAN BLOCK ENTRY)                       
         MVI   FVINDX,1                                                         
*                                  VALIDATE USER-ID                             
         CLI   0(R4),0                                                          
         BE    EIIF                                                             
         CLI   1(R4),0                                                          
         BNE   EIIF                                                             
         MVC   FVIHDR+5(1),0(R4)                                                
         MVC   FVIFLD(10),12(R4)                                                
         MVC   IDALPH,FVIFLD                                                    
         BRAS  RE,GETIDN                                                        
         BNE   EIIF                                                             
         LA    R4,32(R4)                                                        
         MVI   FVINDX,2                                                         
*                                  VALIDATE PRINTER NUMBER                      
         CLI   0(R4),0                                                          
         BE    EIIF                                                             
         CLI   1(R4),0                                                          
         BNE   EIIF                                                             
         TM    2(R4),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R4),4(R4)                                                    
         BZ    EFLM                                                             
         OC    4(3,R4),4(R4)                                                    
         BNZ   EFTB                                                             
         MVC   APDUB(1),7(R4)                                                   
         MVI   FVINDX,0                                                         
*                                  BUILD & ADD PRINTER ID ELEMENT               
         LA    R3,APELEM                                                        
         USING CTPRTD,R3                                                        
         XC    CTPRTEL(20),CTPRTEL                                              
         MVC   CTPRTEL(2),=X'2806'                                              
         MVC   CTPRTID,IDNUM                                                    
         MVC   CTPRTNUM,APDUB                                                   
         GOTO1 AADDELN,CTTREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
VRPIDX   B     VRPRQ                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE PRINTERQ ELEMENT LIST                                                
***********************************************************************         
VRPRQ    CLI   APACTN,ACTMOVE                                                   
         BNE   VRPRQ10                                                          
         BAS   RE,CHKCNT           CHECK PQ MAX COUNT EXCEEDED                  
         BNE   EXIT                                                             
         B     VRPRQX                                                           
*                                                                               
VRPRQ10  BAS   RE,DELRPT           DELETE REPEATED ENTRIES                      
         GOTO1 VALLST,PELOFF       VALIDATE PQ ELEMENT LIST DISPLAYED           
         BNE   EXIT                  INVALID DATA                               
         BAS   RE,CHKCNT           CHECK PQ MAX COUNT EXCEEDED                  
         BNE   EXIT                                                             
         GOTO1 AFVAL,PRTNPQDH      CHECK NEW PQ ELEMENT FIELD                   
         BNE   VRPRQ010                                                         
         L     R1,PELPNTR                                                       
         BAS   RE,VALPQD           VALIDATE NEW ELEMENT                         
         BNE   EXIT                  INVALID DATA                               
         BAS   RE,BUMPLP           BUMP PQ LIST POINTER                         
         BNE   EXIT                  INVALID DATA                               
*                                                                               
VRPRQ010 BRAS  RE,SRTLST           RESORT PRQ EL LIST                           
*                                                                               
VRPRQX   B     VRUPD                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* I/O HANDLING TO UPDATE MASTER,PAGE AND PASSIVE TERMINAL RECORDS               
***********************************************************************         
VRUPD    GOTO1 ASETACN,CTTREC      UPDATE ACTIVITY ELEMENT                      
         BNE   VALRECER            RECORD TOO BIG                               
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
*                                                                               
         OI    CTTSTAT,X'04'       SET IS A PRINTER                             
         NI    CTTSTAT,255-X'08'                                                
         TM    TERMINFO,X'08'                                                   
         BZ    *+8                                                              
         OI    CTTSTAT,X'08'       SET PRINTER IN AUTO MODE                     
*                                  BUILD RECORDS FROM PQ ELEMENT LIST           
         USING PELISTD,R4                                                       
         LA    R4,PELIST           START OF PQ LIST                             
         LA    R8,PALIST           START OF PAGE/PASSWORD CODE LIST             
*                                                                               
         L     R1,AIOAREA3         SAVE BASIC RECORD DATA                       
         SR    RF,RF                 TO REUSE FOR PAGED RECORD                  
         ICM   RF,3,CTTLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(RF,R1),0(R2)                                                   
*                                  BUILD NEXT RECORD                            
VRUPD010 L     R2,AIOAREA1         RESET RECORD KEY/STATUS NOT PASSIVE          
         MVI   CTTKTID-1,X'00'                                                  
         NI    CTTSTAT,X'FF'-X'01'                                              
*                                                                               
VRUPD020 L     RF,PELPNTR          CHECK END OF PQ LIST                         
         CR    R4,RF                                                            
         BNL   VRUCHA                GO BUILD CURRENT RECORD                    
         SR    RF,RF               CHECK MAX RECORD LENGTH                      
         ICM   RF,3,CTTLEN                                                      
         AH    RF,MINLEN                                                        
         CH    RF,MAXLEN                                                        
         BNL   VRUCHA                GO BUILD CURRENT RECORD                    
*                                                                               
         CLI   PELSRCA,DELFLAG     BYPASS DELETED ENTRY                         
         BE    VRUPD030                                                         
         MVC   IDALPH,PELSRCA      GET ID# FROM ALPHA CODE                      
         BRAS  RE,GETIDN                                                        
         BE    *+6                                                              
         DC    H'00'               CRASH IF INVALID ID STILL IN LIST            
         XC    APELEM,APELEM       BUILD ELEMENT                                
         LA    R3,APELEM                                                        
         USING CTPRQD,R3                                                        
         MVI   CTPRQEL,CTPRQELQ                                                 
         MVI   CTPRQLEN,X'08'                                                   
         MVC   CTPRQUSR,IDNUM                                                   
         MVC   CTPRQSUB,PELSUBID                                                
         MVC   CTPRQCLS,PELCLASS                                                
         GOTO1 AADDELN,CTTREC      ADD PQ ELEMENT TO RECORD                     
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
VRUPD030 LA    R4,PELLEN(R4)       BUMP LIST POINTER                            
         B     VRUPD020              GET NEXT ENTRY                             
*                                                                               
VRUCHA   OC    CTTKPASS,CTTKPASS                                                
         BZ    VRUC010             PROCESS FIRST/MASTER RECORD                  
*                                    ELSE PAGED SUB-RECORD                      
         L     RF,PALPNTR          GET NEXT PASSWORD/PAGE# CODE                 
         CR    R8,RF                                                            
         BNL   VRUC010               IF ANY SAVED                               
         CLC   CTTKPASS,0(R8)                                                   
         BL    VRUC010               AND NEXT IN SEQUENCE                       
         MVC   CTTKPASS,0(R8)        ELSE USE NEXT CODE IN SEQUENCE             
         LA    R8,L'CTTKPASS(R8)                                                
*                                  READ RECORD                                  
VRUC010  MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'               IO ERROR                                     
         BE    VRUC020             OVERWRITE IF EXISTS                          
         TM    IOERR,IOEDEL                                                     
         BO    VRUC020                                                          
         TM    IOERR,IOERNF          ELSE ADD                                   
         BO    *+6                                                              
         DC    H'00'                                                            
         LA    R1,IOADD+IOCONFIL+IO1                                            
         B     *+8                                                              
*                                                                               
VRUC020  LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                  PROCESS ASSOCIATED PASSIVE RECORD            
         MVI   CTTKTID-1,C'P'        SET RECORD KEY/STATUS FOR PASSIVE          
         OI    CTTSTAT,X'01'                                                    
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    VRUC030             OVERWRITE IF EXISTS                          
         TM    IOERR,IOEDEL                                                     
         BO    VRUC030                                                          
         TM    IOERR,IOERNF                                                     
         BO    *+6                   ELSE ADD                                   
         DC    H'00'                                                            
         LA    R1,IOADD+IOCONFIL+IO1                                            
         B     *+8                                                              
*                                                                               
VRUC030  LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    VRUNEXT                                                          
         DC    H'00'                                                            
*                                  INITIALISE NEXT RECORD                       
VRUNEXT  L     RF,PELPNTR          EXIT IF END OF PQ LIST                       
         CR    R4,RF                                                            
         BNL   VRUEND                                                           
         L     R1,AIOAREA3         RESTORE BASIC RECORD DATA                    
         SR    RF,RF                                                            
         ICM   RF,3,CTTLEN-CTTREC(R1)                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(RF,R2),0(R1)                                                   
         MVC   CTTREC(L'CTTKEY),IOKEY                                           
         SR    RF,RF                                                            
         IC    RF,CTTKPASS         UPDATE PAGE/PASSWORD CODE IN KEY             
         LTR   RF,RF                                                            
         BNZ   VRUN010                                                          
         MVC   CTTKPASS,SPACES                                                  
         MVI   CTTKPASS,C'1'                                                    
         B     VRUPD010                                                         
VRUN010  LA    RF,1(RF)                                                         
         STC   RF,CTTKPASS                                                      
         B     VRUPD010            BUILD NEXT RECORD                            
*                                                                               
VRUEND   L     RF,PALPNTR          UNLESS END OF PAGE/PASSWORD LIST             
         CR    R8,RF                                                            
         BNL   VALRECX                                                          
         L     R2,AIOAREA1           DELETE UNUSED PAGE/PASSWORD RECS           
         MVC   CTTKPASS,0(R8)                                                   
         MVI   CTTKTID-1,X'00'                                                  
         MVC   DELKEY(L'CTTKEY),CTTKEY                                          
         BAS   RE,DLTREC                                                        
         MVI   CTTKTID-1,C'P'        AND THE PASSIVES                           
         MVC   DELKEY(L'CTTKEY),CTTKEY                                          
         BAS   RE,DLTREC                                                        
         LA    R8,L'CTTKPASS(R8)                                                
         B     VRUEND                                                           
*                                                                               
VALRECX  BAS   RE,INTKEY           REDISPLAY PQ LIST                            
         MVC   FVMSGNO,=AL2(FVFOK)   AND EXIT OK                                
         B     EXIT                                                             
*                                                                               
VALRECER B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF TERMINAL RECORD                                     
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         MVC   PRTID,CTTKTID                                                    
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY TERMINAL RECORD PRINTERQ DATA                              
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         OI    PRTPRIDH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PRTPRID,PRTPRID                                                  
         LA    R3,CTTDATA                                                       
*                                                                               
DREC010  CLI   0(R3),0             READ THROUGH ELEMENTS                        
         BE    DREC100                                                          
         CLI   0(R3),CTPRTELQ      DISPLAY PRINTER ID ELEMENT                   
         BE    DRPID                                                            
*                                                                               
DREC020  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DREC010                                                          
*                                                                               
DREC100  BAS   RE,INTKEY           DISPLAY PRINTERQ LIST                        
         B     DISRECX                                                          
*                                                                               
DISRECX  EQU   *                                                                
         MVC   FVMSGNO,=AL2(FVFOK)   AND EXIT OK                                
         GOTO1 ADISACT,CTTREC                                                   
         B     EXIT                                                             
*                                  DISPLAY PRINTER ID ELEMENT                   
         USING CTPRTD,R3                                                        
DRPID    MVC   IDNUM,CTPRTID                                                    
         BRAS  RE,GETIDA                                                        
         BE    DRPID010                                                         
         EDIT  (B2,IDNUM),(10,PRTPRID),ALIGN=LEFT,WRK=APWORK,DUB=APDUB          
         B     DRPID020                                                         
DRPID010 MVC   PRTPRID(L'IDALPH),IDALPH                                         
DRPID020 LA    R8,PRTPRID+L'IDALPH-1                                            
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C','                                                       
         EDIT  (B1,CTPRTNUM),(3,2(R8)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB          
         B     DREC020                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO INTERPRET ACTION FOR LAST KEY STROKE                               
* CONTROLLING PRINTERQ ELEMENT LIST DISPLAY SCROLLING                           
***********************************************************************         
INTKEY   NTR1                                                                   
         SR    RF,RF                                                            
         IC    RF,APPFKEY                                                       
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     ENTKEY              ENTER KEY                                    
         B     PFUNDF              PFK01 = UNDEFINED                            
         B     PFUNDF              PFK02 = UNDEFINED                            
         B     PFUNDF              PFK03 = UNDEFINED                            
         B     PFUNDF              PFK04 = UNDEFINED                            
         B     PFUNDF              PFK05 = UNDEFINED                            
         B     PFUNDF              PFK06 = UNDEFINED                            
         B     PFBACK              PFK07 = SCROLL BACK PAGE                     
         B     PFFRWD              PFK08 = SCROLL FORWARD PAGE                  
         B     PFUNDF              PFK09 = UNDEFINED                            
         B     PFUNDF              PFK10 = UNDEFINED                            
         B     PFUNDF              PFK11 = UNDEFINED                            
         B     PFUNDF              PFK12 = UNDEFINED                            
*                                                                               
IKEYX    B     IKEYOK                                                           
*                                                                               
IKEYNO   B     NO                                                               
*                                                                               
IKEYOK   B     YES                                                              
         EJECT                                                                  
*                                  PROCESS ENTER KEY                            
ENTKEY   GOTO1 DISLST,PELOFF                                                    
         B     IKEYX                                                            
*                                  SCROLL BACK PAGE                             
PFBACK   SR    RE,RE                                                            
         IC    RE,PAGENO                                                        
         BCT   RE,PFBAC020                                                      
PFBAC010 GOTO1 DISLST,PELOFF                                                    
         B     IKEYX                                                            
*                                                                               
PFBAC020 STC   RE,PAGENO                                                        
         LA    RF,PELPLEN                                                       
         L     R1,PELOFF                                                        
         SR    R1,RF                                                            
         LTR   R1,R1                                                            
         BM    PFBAC010                                                         
         ST    R1,PELOFF                                                        
         GOTO1 DISLST,PELOFF                                                    
         B     IKEYX                                                            
*                                  SCROLL FORWARD PAGE                          
PFFRWD   SR    RE,RE                                                            
         IC    RE,PAGENO                                                        
         CLM   RE,1,NUMPAGES                                                    
         BNE   PFFRW020                                                         
PFFRW010 GOTO1 DISLST,PELOFF                                                    
         B     IKEYX                                                            
*                                                                               
PFFRW020 LA    RE,1(RE)                                                         
         STC   RE,PAGENO                                                        
         LA    RF,PQDLNUM*PELLEN                                                
         L     R1,PELOFF                                                        
         AR    R1,RF                                                            
         L     RF,PELPNTR                                                       
         LA    RE,PELIST                                                        
         SR    RF,RE                                                            
         CR    R1,RF                                                            
         BH    PFFRW010                                                         
         ST    R1,PELOFF                                                        
         GOTO1 DISLST,PELOFF                                                    
         B     IKEYX                                                            
*                                  UNDEFINED PF KEY                             
PFUNDF   GOTO1 DISLST,PELOFF                                                    
         B     IKEYX                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY PRINTER ELEMENT LIST DATA ON SCREEN                        
* R1 POINTS TO START POINT IN LIST                                              
***********************************************************************         
DISLST   NTR1                                                                   
         MVC   PRTPAGE(1),PAGENO   DISPLAY PAGE NUMBER INFO.                    
         OI    PRTPAGE,C'0'                                                     
         MVC   PRTPAGE+2(1),NUMPAGES                                            
         OI    PRTPAGE+2,C'0'                                                   
         OI    PRTPAGEH+FHOID,FHOITR                                            
*                                                                               
         L     R4,0(R1)            GET OFFSET TO A(FIRST PQ LIST ENTRY)         
         TWAXC PRTNPQDH            CLEAR SCREEN                                 
         LA    RF,PELIST                                                        
         AR    R4,RF                                                            
         LA    R3,PRTPQDAH         ADDRESS FIRST POSITION ON SCREEN             
         LR    R8,R3                                                            
         LA    R0,COLNUM           # LIST COLUMNS                               
*                                                                               
DLST010  L     RF,PELPNTR                                                       
         CR    R4,RF                                                            
         BNL   DLSTOK              EXIT IF END OF LIST                          
         CLI   0(R4),DELFLAG       BYPASS ENTRIES FLAG AS DELETED               
         BE    DLST030                                                          
         LR    R1,R4                                                            
*                                  DISPLAY ENTRY                                
         BRAS  RE,DISPQD                                                        
         MVC   FVIFLD-FVIHDR(L'PRTPQDA,R3),APWORK                               
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         NI    FVATRB-FVIHDR(R3),X'FF'-FVAHIGH                                  
*                                                                               
         LA    RF,PELIST           HIGHLIGHT REPEATED ENTRY                     
         CR    R4,RF                                                            
         BNH   DLST012                                                          
         LR    R1,R4                                                            
         LA    RE,PELLEN                                                        
         SR    R1,RE                                                            
         USING PELISTD,R1                                                       
         CLC   PELSRCA+PELLEN,PELSRCA                                           
         BNE   DLST012                                                          
         CLC   PELSUBID+PELLEN,PELSUBID                                         
         BNE   DLST012                                                          
         CLC   PELCLASS+PELLEN,PELCLASS                                         
         BNE   DLST012                                                          
         OI    FVATRB-FVIHDR(R3),FVAHIGH                                        
         DROP  R1                                                               
*                                                                               
DLST012  BCT   R0,DLST020          GET NEXT DISPLAY ADDRESS                     
         LA    R0,COLNUM                                                        
         LR    R3,R8                                                            
         LA    RF,PQDLLEN                                                       
         AR    R3,RF                                                            
         LA    RF,PRTBBARH                                                      
         CR    R3,RF                                                            
         BNL   DLSTOK              EXIT AT END OF SCREEN                        
         LR    R8,R3                                                            
         B     DLST030                                                          
DLST020  LA    RF,PQDFLEN                                                       
         AR    R3,RF                                                            
DLST030  LA    R4,PELLEN(R4)       GET NEXT PQ LIST ENTRY                       
         B     DLST010                                                          
*                                                                               
DLSTNO   B     NO                                                               
DLSTOK   B     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DELETE REPEATED ENTRIES IN PRINTERQ LIST                           
***********************************************************************         
DELRPT   NTR1                                                                   
         USING PELISTD,R4                                                       
         LA    R4,PELIST                                                        
DRPT010  L     RF,PELPNTR          SEARCH DOWN LIST                             
         LA    RE,PELLEN                                                        
         SR    RF,RE                                                            
         CR    R4,RF                                                            
         BNL   DRPTX                                                            
*                                  MATCH USERIDS AND SUBIDS                     
         CLC   PELSRCA+PELLEN,PELSRCA                                           
         BNE   DRPT020                                                          
         CLC   PELSUBID+PELLEN,PELSUBID                                         
         BNE   DRPT020                                                          
         CLC   PELCLASS+PELLEN,PELCLASS                                         
         BNE   DRPT020                                                          
         XC    0(PELLEN,R4),0(R4)                                               
         MVI   0(R4),DELFLAG       FLAG DELETED                                 
         SR    RF,RF                                                            
         LH    RF,PELDEL                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PELDEL                                                        
*                                                                               
DRPT020  LA    R4,PELLEN(R4)                                                    
         B     DRPT010                                                          
*                                                                               
DRPTX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE PRINTER ELEMENT LIST DATA ON SCREEN                       
* R1 POINTS TO START POINT IN LIST                                              
***********************************************************************         
VALLST   NTR1                                                                   
         L     R4,0(R1)                                                         
         LA    RF,PELIST                                                        
         AR    R4,RF               ADDRESS FIRST PQ LIST ENTRY                  
         LA    R3,PRTPQDAH         ADDRESS FIRST DISPLAY FIELD                  
         LR    R8,R3                                                            
         LA    R0,COLNUM           DISPLAY COLUMN #                             
VLST010  CLI   0(R4),DELFLAG                                                    
         BE    VLST030             BYPASS IF FLAG AS DELETED                    
         LR    R1,R3                                                            
         GOTO1 AFVAL                                                            
         BE    VLST020                                                          
         CLI   FVILEN,0            CHECK IF FIELD CLEARED                       
         BNE   VLST030                                                          
         L     RF,PELPNTR                                                       
         CR    R4,RF                                                            
         BNL   VLST030             IGNORE IF PAST END OF LIST                   
         XC    0(PELLEN,R4),0(R4)    ELSE DELETE ENTRY                          
         MVI   0(R4),DELFLAG                                                    
         SR    RF,RF                                                            
         LH    RF,PELDEL                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PELDEL                                                        
         B     VLST030                                                          
*                                                                               
VLST020  XC    0(PELLEN,R4),0(R4)                                               
         LR    R1,R4                                                            
         BAS   RE,VALPQD           VALIDATE PRINTERQ ELEMENT FIELD              
         BNE   VLSTNO                EXIT IF INVALID                            
*                                                                               
VLST030  L     RF,PELPNTR                                                       
         CR    R4,RF                                                            
         BL    VLST040             CHECK END OF PQ LIST                         
         OC    0(PELLEN,R4),0(R4)                                               
         BZ    VLST050               AND NO NEW ENTRY LOADED                    
         BAS   RE,BUMPLP           BUMP PQ LIST POINTER/COUNT                   
         BNE   VLSTNO                EXIT IF INVALID                            
VLST040  LA    R4,PELLEN(R4)       NEXT PQ ELEMENT LIST POINTER                 
*                                                                               
VLST050  BCT   R0,VLST060          GET NEXT SCREEN FIELD ADDRESS                
         LA    R0,COLNUM                                                        
         LR    R3,R8                                                            
         LA    RF,PQDLLEN                                                       
         AR    R3,RF                                                            
         LA    RF,PRTBBARH                                                      
         CR    R3,RF                                                            
         BNL   VLSTOK              EXIT AT END OF SCREEN                        
         LR    R8,R3                                                            
         B     VLST010                                                          
VLST060  LA    RF,PQDFLEN                                                       
         AR    R3,RF                                                            
         B     VLST010                                                          
*                                                                               
VLSTNO   B     NO                  ENTRY INVALID                                
VLSTOK   B     YES                 LIST OK                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE PRINTER ELEMENT DATA FOR DISPLAY ENTRY                    
* DATA RETURNED IN PELIST ENTRY AT R4                                           
***********************************************************************         
VALPQD   NTR1                                                                   
         USING PELISTD,R4                                                       
         LR    R4,R1                                                            
         XC    BLOCK1(3*L'BLOCK1),BLOCK1                                        
         GOTO1 VSCANNER,APPARM,FVIHDR,(3,BLOCK1)                                
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVI   FVINDX,1                                                         
         CLI   4(R1),1                                                          
         BL    EMIF                                                             
         LA    R8,BLOCK1           R8=A(SCAN BLOCK ENTRY)                       
         MVI   FVINDX,1                                                         
*                                  VALIDATE USER-ID                             
         CLI   0(R8),0                                                          
         BE    EIIF                                                             
         CLI   1(R8),0                                                          
         BNE   EIIF                                                             
         MVC   IDALPH,12(R8)                                                    
         BRAS  RE,GETIDN                                                        
         BNE   EIIF                                                             
         MVC   PELSRCA,IDALPH                                                   
*                                  VALIDATE SUB-ID NUMBER                       
         LA    R8,32(R8)                                                        
         MVI   FVINDX,2                                                         
         MVC   PELSUBID,=C'ALL'                                                 
         MVI   PELCLASS,0                                                       
         CLI   0(R8),0                                                          
         BE    VPQD020                                                          
         CLI   1(R8),0                                                          
         BNE   EIIF                                                             
         CLI   0(R8),3                                                          
         BL    *+14                                                             
         MVC   PELSUBID,12(R8)                                                  
         B     VPQD010                                                          
         CLI   0(R8),2                                                          
         BL    EFTS                                                             
         CLI   13(R8),C'*'                                                      
         BNE   EIIF                                                             
         MVC   PELSUBID,12(R8)                                                  
         B     VPQD010                                                          
*                                  VALIDATE CLASS                               
VPQD010  LA    R8,32(R8)                                                        
         MVI   FVINDX,3                                                         
         CLI   1(R8),0                                                          
         BNE   EIIF                                                             
         CLI   0(R8),1                                                          
         BL    VPQD020                                                          
         BE    VPQD012                                                          
         CLI   0(R8),2                                                          
         BH    EIIF                                                             
         CLI   12(R8),C'-'                                                      
         BNE   EIIF                                                             
         LA    R1,13(R8)                                                        
         BAS   RE,ALPNUM                                                        
         BNE   EIIF                                                             
         MVC   PELCLASS,13(R8)                                                  
         NI    PELCLASS,X'BF'                                                   
         B     VPQD020                                                          
VPQD012  LA    R1,12(R8)                                                        
         BAS   RE,ALPNUM                                                        
         BNE   EIIF                                                             
         MVC   PELCLASS,12(R8)                                                  
*                                                                               
VPQD020  MVI   FVINDX,0                                                         
         BAS   RE,CHKRPT           CHECK ENTRY NOT ALREADY IN LIST              
         BNE   EDIF                                                             
         B     VPQDOK                                                           
*                                                                               
VPQDNO   B     NO                  INVALID ENTRY                                
VPQDOK   MVI   FVINDX,0            ENTRY OK                                     
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CHECK BYTE AT R1 IS ALPHA NUMERIC                                  
***********************************************************************         
ALPNUM   NTR1                                                                   
         CLI   0(R1),C'A'                                                       
         BL    ANUM010                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   ANUMOK                                                           
*                                                                               
ANUM010  CLI   0(R1),C'0'                                                       
         BL    ANUMNO                                                           
         CLI   0(R1),C'9'                                                       
         BH    ANUMNO                                                           
         B     ANUMOK                                                           
*                                                                               
ANUMNO   B     NO                  NOT ALPHA NUMERIC                            
ANUMOK   B     YES                 IS ALPHA NUMERIC                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CHECK FOR REPEATED ENTRY IN PRINTERQ LIST                          
***********************************************************************         
CHKRPT   NTR1                                                                   
         USING PELISTD,R4                                                       
         LA    R8,PELIST                                                        
CRPT010  L     RF,PELPNTR                                                       
         CR    R8,RF                                                            
         BNL   CRPTOK                                                           
         CR    R8,R4                                                            
         BE    CRPT020                                                          
         CLI   PELSRCA,DELFLAG                                                  
         BE    CRPT020                                                          
         CLC   PELSRCA,PELSRCA-PELISTD(R8)                                      
         BNE   CRPT020                                                          
         CLC   PELSUBID,PELSUBID-PELISTD(R8)                                    
         BNE   CRPT020                                                          
         CLC   PELCLASS,PELCLASS-PELISTD(R8)                                    
         BE    CRPTNO                                                           
*                                                                               
CRPT020  LA    R8,PELLEN(R8)                                                    
         B     CRPT010                                                          
*                                                                               
CRPTNO   B     NO                                                               
CRPTOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO BUMP PRINTERQ ELEMENT LIST POINTER FOR NEW ENTRY                   
***********************************************************************         
BUMPLP   NTR1                                                                   
         L     R1,PELPNTR                                                       
         LA    RF,PELLEN                                                        
         AR    R1,RF                                                            
         ST    R1,PELPNTR                                                       
         SR    RF,RF                                                            
         LH    RF,PELCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PELCNT                                                        
         BAS   RE,CHKCNT                                                        
         BNE   BPLPNO                                                           
         B     BPLPOK                                                           
*                                                                               
BPLPNO   B     NO                                                               
BPLPOK   B     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CHECK # ELEMENTS IN LIST WITHIN MAXIMUM                            
***********************************************************************         
CHKCNT   NTR1                                                                   
         OC    MAXNUM,MAXNUM                                                    
         BZ    CCNTOK                                                           
         SR    RF,RF                                                            
         LH    RF,PELCNT                                                        
         SH    RF,PELDEL                                                        
         CH    RF,MAXNUM                                                        
         BNH   CCNTOK                                                           
         B     ETOO                                                             
*                                                                               
CCNTOK   B     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* SUBROUTINE TO DELETE A RECORD WITH KEY=DELKEY                                 
***********************************************************************         
DLTREC   NTR1                                                                   
         L     R2,AIOAREA2                                                      
         MVC   DELSAVE,APRECKEY                                                 
         MVC   IOKEY(L'DELKEY),DELKEY                                           
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    27(R2),X'80'        SET DELETE FLAG                              
         LA    R1,IOWRITE+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
DLTRECX  MVC   IOKEY(L'DELSAVE),DELSAVE                                         
         L     R2,AIOAREA1                                                      
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET TERMINAL NAME FROM PASSIVE NUMBER #NNNNNN                                 
***********************************************************************         
VALPSVN  NTR1                                                                   
         SR    R0,R0               R0=ERROR NUMBER (ZERO IS OK)                 
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         AHI   R1,-2               R1=L'NUMBER-1                                
         BM    EFNN                                                             
         MVC   APDUB,=8C'0'        FIELD MUST BE NUMERIC                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   APDUB(0),FVIFLD+1                                                
         CLC   APDUB,=8C'0'                                                     
         BNE   EFNN                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,FVIFLD+1(0)                                                
         CVB   R1,APDUB                                                         
         LTR   R1,R1               NUMBER CAN'T BE ZERO                         
         BZ    EIIF                                                             
         STH   R1,APDUB            MOVE NUMBER TO KEY                           
         MVC   CTTKPASS+8(2),APDUB                                              
         MVC   IOKEY,CTTKEY                                                     
         GOTO1 AIO,IOREAD+IOCONFIL+IO1                                          
         BNE   ERNF                                                             
         L     R3,AIOAREA1                                                      
         LA    R3,CTTDATA-CTTREC(R3)                                            
         SR    R4,R4                                                            
VALPSVN1 CLI   0(R3),0             SEARCH REC FOR POINTER ELEMENT               
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R3),X'03'                                                      
         BE    *+14                                                             
         IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     VALPSVN1                                                         
         USING CTPASD,R3                                                        
         XC    PRTID,PRTID         MOVE TERMINAL ID TO TWA                      
         MVC   PRTID(8),CTPASDTA                                                
         MVI   PRTIDH+5,8                                                       
         NI    PRTIDH+4,X'F1'                                                   
         OI    PRTIDH+6,X'80'                                                   
*                                                                               
VALPSVNX B     YES                 EXIT WITH CC=ZERO IF OK                      
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                                     
***********************************************************************         
VALREQ   L     R8,AREP                                                          
         USING REPD,R8             R8=A(REPORT WORK AREA)                       
         XC    SELDATA,SELDATA                                                  
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REQREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REQWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REQDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         LA    R4,REQTERMH                                                      
         BRAS  RE,VALPARS          GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         LA    R2,APRECKEY         BUILD AN INITIAL KEY                         
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,C'T'                                                     
         MVC   CTTKTID,SELTERM                                                  
*                                                                               
VALREQ20 MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSI,REPMSPAC                                                
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO PRINT TERMINAL LIST                                                
***********************************************************************         
PRTREP   L     R8,AREP                                                          
         USING REPD,R8             R8=A(REPORT WORK AREA)                       
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   CTTKEY,APRECKEY                                                  
         OC    CTTKLINE,CTTKLINE   (IF NO KEY ENTERED                           
         BNZ   *+8                    READ PAST PASSIVES)                       
         MVI   CTTKLINE+L'CTTKLINE-1,1                                          
*                                                                               
         LA    R1,IOHI+IOCONFIL+IO1                                             
         BRAS  RE,GETREC           GO GET REC WIV                               
         BNE   PRTREPX                                                          
         B     PREP100                                                          
*                                                                               
PREP010  TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    PREP020                                                          
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     PREP030                                                          
PREP020  TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    PREP040                                                          
         NI    APINDS,X'FF'-APILRERD                                            
PREP030  LA    R2,IOKEY                                                         
         MVC   CTTKEY,APRECKEY                                                  
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    PREP040                                                          
         B     PRTREPX                                                          
*                                                                               
PREP040  LA    R1,IOSQ+IOCONFIL+IO1                                             
         BRAS  RE,GETREC           GO GET NEXT REC                              
         BNE   PRTREPX                                                          
*                                                                               
PREP100  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTTKEY),CTTKEY                                        
         LA    R4,REPP1-14                                                      
         BRAS  RE,REPREC           GO PROCESS RECORD FOR REPORT OUTPUT          
         B     PREP010                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ERROR EXITS AND CONSTANTS                                                     
***********************************************************************         
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD INVALID                          
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
EIIO     MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
ERID     MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     NO                  RECORD IS DELETED                            
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
EDIF     MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
ETOO     MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     NO                  TOO MANY INPUT FIELDS                        
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
EFLM     MVC   FVMSGNO,=AL2(CE#FVMIN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE < MIN                            
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
*                                                                               
REPDESCL DC    C'PRINTERQ LIST'                                                 
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'PRINTERQ LIST'                                           
         SPEC  H2,57,C'-------------'                                           
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         SPACE 5                                                                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    80C' '                                                           
FFILL    DC    80X'FF'                                                          
CAPFILL  DC    (L'APWORK)X'40'                                                  
MINLEN   DC    H'200'                                                           
MAXLEN   DC    H'1000'                                                          
DELFLAG  EQU   X'FF'                                                            
MAXPEL   EQU   255                                                              
PQDFLEN  EQU   PRTPQDBH-PRTPQDAH                                                
PQDLLEN  EQU   PRTPQDDH-PRTPQDAH                                                
PQDTLEN  EQU   PRTBBARH-PRTPQDAH                                                
COLNUM   EQU   PQDLLEN/PQDFLEN                                                  
LINNUM   EQU   PQDTLEN/PQDLLEN                                                  
PQDLNUM  EQU   COLNUM*LINNUM                                                    
PELPLEN  EQU   PQDLNUM*PELLEN                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONVERT ID NUMBER TO ID ALPHA                                                 
***********************************************************************         
GETIDA   NTR1  BASE=*,LABEL=*                                                   
         MVC   KEYSAVE,IOKEY                                                    
         L     R4,AIOAREA2         READ ID RECORD FROM CONTROL FILE             
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),IDNUM                                                
         NI    CTIKID+8,X'FF'-X'80'   TURN OFF GENERIC ID BIT                   
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         MVC   IOKEY,KEYSAVE                                                    
         BNE   GIDANO                                                           
*                                                                               
GIDA010  LA    R1,CTIDATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
GIDA020  CLI   0(R1),0                                                          
         BE    GIDANO                                                           
         CLI   0(R1),CTDSCELQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     GIDA020                                                          
         MVC   IDALPH,CTDSC-CTDSCD(R1)                                          
         B     GIDAOK                                                           
*                                                                               
GIDAOK   SR    RC,RC                                                            
GIDANO   LTR   RC,RC                                                            
         J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONVERT ID ALPHA TO ID NUMBER                                                 
***********************************************************************         
GETIDN   NTR1  BASE=*,LABEL=*                                                   
         MVC   KEYSAVE,IOKEY                                                    
         L     R4,AIOAREA2         READ ID RECORD FROM CONTROL FILE             
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,IDALPH                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         MVC   IOKEY,KEYSAVE                                                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     GIDNNO                                                           
*                                                                               
GIDN010  LA    R1,CTIDATA          EXTRACT INFO FROM ELEMENTS                   
GIDN020  CLI   0(R1),0                                                          
         BE    GIDN100                                                          
         CLI   0(R1),CTDSCELQ                                                   
         BE    GIDN040                                                          
         CLI   0(R1),X'07'                                                      
         BE    GIDN050                                                          
GIDN030  SR    R0,R0                                                            
         IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     GIDN020                                                          
GIDN040  MVC   IDNUM,CTDSC-CTDSCD(R1)                                           
         B     GIDN030                                                          
GIDN050  OC    IDNUM,IDNUM         TEST ID NUMBER SET                           
         BZ    GIDN030                                                          
         TM    2(R1),X'40'         YES - TEST IF A GENERIC USER-ID              
         BZ    *+8                                                              
         OI    IDNUM,X'80'         YES - SET GENERIC ID FLAG                    
         B     GIDN030                                                          
*                                                                               
GIDN100  OC    IDNUM,IDNUM                                                      
         BZ    GIDNNO                                                           
         B     GIDNOK                                                           
*                                                                               
GIDNOK   SR    RC,RC                                                            
GIDNNO   LTR   RC,RC                                                            
         J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO TRANSFER PRINTER ELEMENT DATA FOR DISPLAY                          
* DATA RETURNED IN APWORK                                                       
***********************************************************************         
DISPQD   NTR1  BASE=*,LABEL=*                                                   
         XC    APWORK,APWORK                                                    
         LA    R3,APWORK                                                        
         LA    R8,L'PRTPQDA(R3)                                                 
         LR    R4,R1                                                            
         USING PELISTD,R1                                                       
*                                  DISPLAY USERID                               
         CLI   0(R4),0                                                          
         BNE   DPQD010                                                          
         LA    R4,1(R4)            BUMP OVER ID NUM DISPLAY FLAG                
*                                                                               
DPQD010  CLI   0(R4),C' '                                                       
         BE    DPQD020                                                          
         MVC   0(1,R3),0(R4)                                                    
         LA    R3,1(R3)                                                         
         CR    R3,R8                                                            
         BNL   DPQD100                                                          
         LA    R4,1(R4)                                                         
         LA    RF,PELSUBID                                                      
         CR    R4,RF                                                            
         BL    DPQD010                                                          
DPQD020  LA    R4,PELSUBID                                                      
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         CR    R3,R8                                                            
         BNL   DPQD100                                                          
*                                  DISPLAY SUBID                                
DPQD030  CLI   0(R4),C' '                                                       
         BE    DPQD040                                                          
         MVC   0(1,R3),0(R4)                                                    
         LA    R3,1(R3)                                                         
         CR    R3,R8                                                            
         BNL   DPQD100                                                          
         LA    R4,1(R4)                                                         
         LA    RF,PELCLASS                                                      
         CR    R4,RF                                                            
         BL    DPQD030                                                          
*                                  DISPLAY CLASS                                
DPQD040  LA    R4,PELCLASS                                                      
         CLI   0(R4),0                                                          
         BE    DPQDX                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         CR    R3,R8                                                            
         BNL   DPQD100                                                          
         MVC   0(1,R3),0(R4)                                                    
         TM    0(R4),X'40'                                                      
         BO    DPQDX                                                            
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         CR    R3,R8                                                            
         BNL   DPQD100                                                          
         MVC   0(1,R3),0(R4)                                                    
         OI    0(R3),X'40'                                                      
         B     DPQDX                                                            
*                                                                               
DPQD100  BCTR  R3,0                                                             
         MVI   0(R3),C'>'                                                       
         B     DPQDX                                                            
*                                                                               
DPQDX    J     EXIT                                                             
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO ADD PRINTERQ ELEMENTS TO LIST                                      
***********************************************************************         
ADDLST   NTR1  BASE=*,LABEL=*                                                   
         OC    CTTKPASS,CTTKPASS                                                
         BZ    ALST002             UNLESS MASTER RECORD                         
         L     R4,PALPNTR          SAVE PAGE/PASSWORD CODE                      
         MVC   0(L'CTTKPASS,R4),CTTKPASS                                        
         LA    R4,L'CTTKPASS(R4)                                                
         ST    R4,PALPNTR                                                       
         SR    RF,RF                                                            
         LH    RF,PALCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PALCNT                                                        
*                                                                               
ALST002  LA    R3,CTTDATA          READ PQ ELEMENTS IN RECORD                   
*                                                                               
ALST010  CLI   0(R3),0                                                          
         BE    ALSTOK                                                           
         CLI   0(R3),CTPRQELQ                                                   
         BE    ALST030                                                          
         SR    RF,RF                                                            
ALST020  IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     ALST010                                                          
*                                                                               
         USING CTPRQD,R3                                                        
         USING PELISTD,R4                                                       
ALST030  L     R4,PELPNTR          SAVE PQ ELEMENT ENTRY IN LIST                
         XC    0(PELLEN,R4),0(R4)                                               
         CLI   CTPRQLEN,X'0C'                                                   
         BE    ALST040                                                          
         MVC   PELSRCN,CTPRQUSR    NEW STYLE SHORT VERSION OF ELEMENT           
         MVC   PELSUBID,CTPRQSUB                                                
         MVC   PELCLASS,CTPRQCLS                                                
         B     ALST050                                                          
*                                                                               
         USING PRENTRY,R8                                                       
ALST040  LA    R8,CTPRQDTA         OLD STYLE LONG ELEMENT                       
         MVC   PELSRCN,PRSRCID                                                  
         MVC   PELSUBID,PRSUBID                                                 
         MVC   PELCLASS,PRCLASS                                                 
*                                                                               
ALST050  LA    R4,PELLEN(R4)       BUMP LIST POINTER/COUNT                      
         ST    R4,PELPNTR                                                       
         SR    RF,RF                                                            
         LH    RF,PELCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PELCNT                                                        
         B     ALST020             GET NEXT ENTRY                               
*                                                                               
ALSTOK   SR    RC,RC                                                            
ALSTNO   LTR   RC,RC                                                            
         J     EXIT                                                             
         DROP  R3,R4,R8                                                         
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO SORT PRINTERQ ELEMENT LIST DATA                                    
***********************************************************************         
SRTLST   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,PELIST           SORT PQ ELEMENT LIST                         
         SR    R0,R0                                                            
         LH    R0,PELCNT                                                        
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R0),PELLEN,PELLEN,0                  
*                                                                               
         LR    R1,R0               CALCULATE MAXIMUM # OF DISPLAY PAGES         
         SR    R0,R0                                                            
         SH    R1,PELDEL                                                        
         LA    RF,PQDLNUM                                                       
         DR    R0,RF                                                            
         LA    R1,1(R1)                                                         
         STC   R1,NUMPAGES                                                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                                       
***********************************************************************         
         USING REQTERMH,R4                                                      
VALPARS  NTR1  BASE=*,LABEL=*                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
                                                                                
*----------------------------------                                             
* TERMINAL                                                                      
*----------------------------------                                             
         GOTO1 AFVAL,REQTERMH      STORE TERMINAL                               
         BNE   VPCTY               (IF ENTERED)                                 
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VPTRM1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VPTRM2              FOR KEY COMPARE IN GETREC                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VPTRM1                                                        
VPTRM2   STC   RE,SELKEYCL                                                      
         MVC   SELTERM,FVIFLD                                                   
         MVC   SELTERML,FVILEN                                                  
         MVC   SELTRMSP,0(RF)                                                   
                                                                                
*----------------------------------                                             
* COUNTRY                                                                       
*----------------------------------                                             
VPCTY    GOTO1 AFVAL,REQCTRYH      VALIDATE COUNTRY                             
         BNE   VPCTYX              (IF ENTERED)                                 
         L     R1,ACTRY                                                         
         LH    RE,0(,R1)                                                        
         L     RF,2(,R1)                                                        
         LA    R1,6(,R1)                                                        
         USING CTRYTABD,R1                                                      
         ZIC   R8,FVILEN                                                        
         AHI   R8,-1                                                            
VPCTY1   EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   CTRYSHR(0),FVIFLD                                                
         BE    VPCTY2                                                           
         BXLE  R1,RE,VPCTY1                                                     
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPCTY2   MVC   SELCTY,CTRYCODE                                                  
         CLI   SELCTY,0            (CANT SET TO ZERO COS                        
         BNE   VPCTYX                                                           
         MVI   SELCTY,X'FF'           THAT WOULD MEAN DONT FILTER)              
VPCTYX   EQU   *                                                                
         DROP  R1                                                               
                                                                                
*----------------------------------                                             
* AGENCY                                                                        
*----------------------------------                                             
VPAGY    GOTO1 AFVAL,REQAGYH       STORE AGENCY                                 
         BNE   VPUID               (IF ENTERED)                                 
         MVC   SELAGY,FVIFLD                                                    
                                                                                
*----------------------------------                                             
* USER ID                                                                       
*----------------------------------                                             
VPUID    GOTO1 AFVAL,REQUIDH                                                    
         BNE   VPRI                                                             
         MVC   IDALPH,FVIFLD                                                    
         BRAS  RE,GETIDN                                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
         MVC   SELUID,IDALPH                                                    
         MVC   SELUIDN,IDNUM                                                    
                                                                                
*----------------------------------                                             
*  SUB ID                                                                       
*----------------------------------                                             
VPRI     GOTO1 AFVAL,REQSIDH                                                    
         BNE   VPCL                                                             
         MVC   SELSUBID,=C'ALL'                                                 
         CLI   FVILEN,3                                                         
         BL    *+14                                                             
         MVC   SELSUBID,FVIFLD                                                  
         B     VPCL                                                             
         CLI   FVILEN,2                                                         
         BL    VPRIER2                                                          
         CLI   FVIFLD+1,C'*'                                                    
         BNE   VPRIER1                                                          
         MVC   SELSUBID,FVIFLD                                                  
         B     VPCL                                                             
VPRIER1  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPRIER2  MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALPARSX                                                         
                                                                                
*----------------------------------                                             
* CLASS                                                                         
*----------------------------------                                             
VPCL     GOTO1 AFVAL,REQCLASH                                                   
         BNE   VPCLX                                                            
         CLI   FVILEN,1                                                         
         BL    VPCLX                                                            
         BE    VPCL020                                                          
         CLI   0(R8),2                                                          
         BH    VPCLER1                                                          
         CLI   FVIFLD,C'-'                                                      
         BNE   VPCLER1                                                          
         LA    R1,FVIFLD+1                                                      
         CLI   0(R1),C'A'                                                       
         BL    VPCL010                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   VPCL012                                                          
*                                                                               
VPCL010  CLI   0(R1),C'0'                                                       
         BL    VPCLER1                                                          
         CLI   0(R1),C'9'                                                       
         BH    VPCLER1                                                          
VPCL012  EQU   *                                                                
         MVC   SELCLASS,FVIFLD+1                                                
         NI    SELCLASS,X'BF'                                                   
         B     VALPARSX                                                         
*                                                                               
VPCL020  LA    R1,FVIFLD                                                        
         CLI   0(R1),C'A'                                                       
         BL    VPCL030                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   VPCL032                                                          
*                                                                               
VPCL030  CLI   0(R1),C'0'                                                       
         BL    VPCLER1                                                          
         CLI   0(R1),C'9'                                                       
         BH    VPCLER1                                                          
VPCL032  EQU   *                                                                
         MVC   SELCLASS,FVIFLD                                                  
         B     VALPARSX                                                         
*                                                                               
VPCLER1  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VPCLX    B     VALPARSX                                                         
*                                                                               
VALPARSX J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET NEXT RECORD FOR REPORT, FILTERING ON I/P PARAMETERS                       
***********************************************************************         
GETREC   NTR1  BASE=*,LABEL=*                                                   
         B     GETRECIO            PRESERVE VALUE OF R1 ON ENTRY                
*                                                                               
GETRECRD TM    GETSEQF,APILRERD    READ NEXT RECORD                             
         BZ    GETRECSQ            CHECK SEQUENCE BROKEN                        
         NI    GETSEQF,X'FF'-APILRERD                                           
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETRECN                                                          
GETRECSQ LA    R1,IOCONFIL+IOSQ+IO1                                             
GETRECIO GOTO1 AIO                                                              
         BNE   GETRECN                                                          
         L     R2,AIOAREA1                                                      
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(CTTKTID-CTTKEY),CTTKEY                                  
         BNE   GETRECN                                                          
         MVC   IOKEYSAV(CTTKTID-CTTKEY),CTTKEY                                  
         OC    CTTKPASS,CTTKPASS                                                
         BNZ   GETRECRD                                                         
         TM    CTTSTAT,X'01'       IGNORE PASSIVE                               
         BO    GETRECRD                                                         
         TM    CTTSTAT,X'04'       MUST BE PRINTER TYPE                         
         BNO   GETRECRD                                                         
*                                                                               
GRTRM    CLI   SELTRMSP,C' '       TERMINAL - FILTER ONLY IF IT                 
         BNH   GRTRMX                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GRTRM1                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTTKTID(0),SELTERM                                               
         BH    GETRECN             (NO MORE RELEVENT RECORDS)                   
GRTRM1   GOTO1 ATXTFLT,APPARM,(SELTERML,SELTERM),(8,CTTKTID)                    
         BNE   GETRECSQ                                                         
GRTRMX   EQU   *                                                                
*                                                                               
GRGTEL   EQU   *                   GET TERMINAL DEFINITION ELEM                 
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'25'                                                     
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         USING CTTRMD,R3                                                        
         BZ    GRGTEL2                                                          
         CLI   CTTRMLEN,32                                                      
         BNE   GRGTEL2             (WRONG EL, USE DEFAULT)                      
         B     GRGTELX                                                          
GRGTEL2  LA    R3,TRMDEF           NO EL, SET UP DEFAULT                        
         XC    TRMDEF,TRMDEF                                                    
         MVI   CTTRMDEV,1                                                       
         MVI   CTTRMTYP,1                                                       
         MVI   CTTRMCTY,1                                                       
         MVC   TRMDEFID,CTTKTID                                                 
GRGTELX  EQU   *                                                                
*                                                                               
GRCTY    EQU   *                                                                
         OC    SELCTY,SELCTY       COUNTRY FILTER                               
         BZ    GRCTYX              (ZERO MEANS DONT FILTER,                     
         CLI   SELCTY,X'FF'           FF MEANS FILTER ON ZERO)                  
         BNE   GRCTY1                                                           
         CLI   CTTRMCTY,0                                                       
         BNE   GETRECSQ                                                         
         B     GRCTYX                                                           
GRCTY1   CLC   SELCTY,CTTRMCTY                                                  
         BNE   GETRECSQ                                                         
GRCTYX   EQU   *                                                                
*                                                                               
GRAGY    OC    SELAGY,SELAGY       AGENCY FILTER                                
         BZ    *+14                                                             
         CLC   SELAGY,CTTRMAGY                                                  
         BNE   GETRECSQ                                                         
GRAGYX   EQU   *                                                                
*                                                                               
GRLS     EQU   *                   INITIALISE PRINTERQ ELEMENT LIST             
         XC    PELCNT,PELCNT                                                    
         XC    PELDEL,PELDEL                                                    
         LA    RF,PELLEN*MAXPEL                                                 
         XCEF  PELIST,(RF)                                                      
         LA    RF,PELIST                                                        
         ST    RF,PELPNTR                                                       
         XC    PALCNT,PALCNT       INITIALISE KEY PASSWORD LIST                 
         LA    RF,PALIST                                                        
         ST    RF,PALPNTR                                                       
*                                                                               
         BRAS  RE,ADDLST           ADD PRINTERQ ELEMENTS TO LIST                
*                                                                               
GRLS010  GOTO1 AIO,IOSQ+IOCONFIL+IO2     READ ASSOCIATED RECORDS                
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    GRLS020             SEQUENTIAL READ TO NEXT LUID                 
         L     R2,AIOAREA2                                                      
         CLC   CTTKEY(CTTKPASS-CTTKEY),IOKEY                                    
         BNE   GRLS020                                                          
         TM    CTTSTAT,X'01'                                                    
         BO    GRLS020                                                          
*                                                                               
         BRAS  RE,ADDLST           ADD PRINTERQ ELEMENTS TO LIST                
         B     GRLS010                                                          
*                                                                               
GRLS020  LA    R4,PELIST           CONVERT ID #S IN LIST TO ALPHA               
         SR    R5,R5                                                            
         LH    R5,PELCNT                                                        
         LTR   R5,R5                                                            
         BZ    GRLSNO                                                           
         USING PELISTD,R4                                                       
GRLS030  MVC   IDNUM,PELSRCN                                                    
         OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         L     RF,=A(GETIDA)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    GRLS040             IF ID RECORD NOT VALID                       
         MVI   PELSRCA,0           FLAG ENTRY FOR EARLY SORT                    
         LA    R3,PELSRCA+1                                                     
*                                  AND CONVERT ID# FOR DISPLAY                  
         EDIT  (B2,IDNUM),(9,(R3)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB              
         B     GRLS050                                                          
GRLS040  MVC   PELSRCA,IDALPH                                                   
*                                                                               
GRLS050  LA    R4,PELLEN(R4)                                                    
         BCT   R5,GRLS030                                                       
         DROP  R4                                                               
*                                  AND CONVERT ID# FOR DISPLAY                  
GRLS080  BRAS  RE,SRTLST           SORT PQ ELEMENT LIST                         
         B     GRLSX                                                            
*                                                                               
GRLSNO   EQU   *                                                                
         L     R2,AIOAREA1                                                      
         B     GETRECRD            IGNORE IF NO ENTRIES                         
GRLSX    EQU   *                                                                
         L     R2,AIOAREA1         RESTORE MASTER RECORD                        
*                                                                               
GRRID    OC    SELUIDN,SELUIDN     REPORT ID FILTERS                            
         BNZ   GRRI010                                                          
         OC    SELSUBID,SELSUBID                                                
         BNZ   GRRI010                                                          
         OC    SELCLASS,SELCLASS                                                
         BNZ   GRRI010                                                          
         B     GRRIDX                                                           
*                                                                               
GRRI010  EQU   *                                                                
         USING PELISTD,R3                                                       
         LA    R3,PELIST                                                        
*                                                                               
GRRI020  EQU   *                                                                
         L     RF,PELPNTR                                                       
         CR    R3,RF                                                            
         BNL   GETRECRD            EXIT IF END OF LIST                          
         CLI   0(R4),DELFLAG       BYPASS ENTRIES FLAG AS DELETED               
         BE    GRRI030                                                          
         OC    SELUID,SELUID                                                    
         BZ    GRRI022                                                          
         CLC   PELSRCA(8),SELUID                                                
         BE    GRRIDX                                                           
GRRI022  OC    SELSUBID,SELSUBID                                                
         BZ    GRRI024                                                          
         CLC   PELSUBID,SELSUBID                                                
         BE    GRRIDX                                                           
GRRI024  OC    SELCLASS,SELCLASS                                                
         BZ    GRRI026                                                          
         CLC   PELCLASS,SELCLASS                                                
         BE    GRRIDX                                                           
GRRI026  B     GRRI030                                                          
*                                                                               
GRRI030  EQU   *                                                                
         LA    R3,PELLEN(R3)       GET NEXT PQ LIST ENTRY                       
         B     GRRI020                                                          
         DROP  R3                                                               
                                                                                
GRRIDX   EQU   *                                                                
*                                                                               
GETRECY  SR    RC,RC               RETURN CC EQUAL RECORD OK                    
GETRECN  LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROCESS RECORD FOR REPORT OUTPUT                                              
***********************************************************************         
REPREC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CTTREC,R2                                                        
         USING REPD,R8             R8=A(REPORT WORK AREA)                       
*                                  TERMINAL ID                                  
RETI     EQU   *                                                                
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY                                                   
         MVC   REPP1(20),=CL20'TERMINAL LUID'                                   
         MVC   REPP1+44(20),=CL20'ACTIVITY'                                     
         MVC   REPP1+22(8),CTTKTID                                              
*                                  ACTIVITY                                     
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'01'        GET ACTIVITY ELEMENT                         
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    RETIX                                                            
         USING CTACTD,R3                                                        
         GOTO1 VDATCON,APPARM,(3,CTACTDT),(8,REPP1+66)                          
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
         DROP  R3                                                               
RETIX    EQU   *                                                                
*                                  DISPLAY PRINTER ID ELEMENT                   
REPI     EQU   *                                                                
         MVC   REPP1(20),=CL20'PRINTER ID'                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTPRTELQ                                                  
         GOTO1 AGETELS,CTTREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    REPIX                                                            
         USING CTPRTD,R3                                                        
         MVC   IDNUM,CTPRTID                                                    
         LA    R4,REPP1+22                                                      
         OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         L     RF,=A(GETIDA)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    REPI010                                                          
         EDIT  (B2,IDNUM),(10,(R4)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB             
         B     REPI020                                                          
REPI010  MVC   0(L'IDALPH,R4),IDALPH                                            
REPI020  LA    R5,L'IDALPH-1(R4)                                                
         CLI   0(R5),C' '                                                       
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C','                                                       
         EDIT  (B1,CTPRTNUM),(3,2(R5)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB          
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
REPIX    EQU   *                                                                
*                                                                               
REPE     EQU   *                                                                
         MVC   REPP1(132),=132C'*'                                              
         MVC   REPP1+40(36),=CL36' LIST OF  USER ID, REPORT ID, CLASS '         
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         LA    R4,PELIST                                                        
         LA    R3,REPP1                                                         
         LA    R0,COLNUM                                                        
*                                                                               
REPE010  L     RF,PELPNTR                                                       
         CR    R4,RF                                                            
         BNL   REPE100             EXIT IF END OF LIST                          
         CLI   0(R4),DELFLAG       BYPASS ENTRIES FLAG AS DELETED               
         BE    REPE030                                                          
         LR    R1,R4                                                            
*                                  FORMAT ENTRY INTO APWORK                     
         BRAS  RE,DISPQD                                                        
         MVC   0(L'PRTPQDA,R3),APWORK                                           
         LR    RE,R3                                                            
         LA    RF,L'PRTPQDA                                                     
         CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
         LA    RF,PELIST           HIGHLIGHT REPEATED ENTRY                     
         CR    R4,RF                                                            
         BNH   REPE012                                                          
         LR    R1,R4                                                            
         LA    RE,PELLEN                                                        
         SR    R1,RE                                                            
         USING PELISTD,R1                                                       
         CLC   PELSRCA+PELLEN,PELSRCA                                           
         BNE   REPE012                                                          
         CLC   PELSUBID+PELLEN,PELSUBID                                         
         BNE   REPE012                                                          
         CLC   PELCLASS+PELLEN,PELCLASS                                         
         BNE   REPE012                                                          
*        OI    FVATRB-FVIHDR(R3),FVAHIGH                                        
         DROP  R1                                                               
*                                                                               
REPE012  BCT   R0,REPE020          GET NEXT DISPLAY ADDRESS                     
         LA    R0,COLNUM                                                        
         GOTO1 VREPORT,REPD                                                     
         LA    R3,REPP1                                                         
         B     REPE030                                                          
REPE020  LA    RF,PQDFLEN                                                       
         AR    R3,RF                                                            
*                                                                               
REPE030  EQU   *                                                                
         LA    R4,PELLEN(R4)       GET NEXT PQ LIST ENTRY                       
         B     REPE010                                                          
*                                                                               
REPE100  EQU   *                                                                
         GOTO1 VREPORT,REPD                                                     
         MVC   REPP1(132),=132C'*'                                              
         MVC   REPP1+40(13),=CL13' END OF LIST '                                
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
*                                                                               
REPEX    EQU   *                                                                
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
RESPACES DC    80C' '                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
* CTGENWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENWRK                                                       
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FAPRQ                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAPRQ                                                          
         PRINT ON                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* TWA                                                                           
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENF7D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB5D                                                       
         EJECT                                                                  
         ORG                                                                    
         ORG   SAVOVER                                                          
*                                  WORKING STORAGE SAVED IN TWA0                
SAVETID  DS    CL(L'CTTKTID)       LAST VALUE OF TERMINAL ID                    
PELOFF   DS    F                   OFFSET TO PELIST START OF DISPLAY            
SAVCLRX  EQU   *-SAVOVER           CLEAR SAVED TWA UPTO HERE                    
PAGENO   DS    XL1                 CURRENT DISPLAYED PAGE NUMBER                
NUMPAGES DS    XL1                 TOTAL NUMBER OF PAGES                        
*                                                                               
MOVEKEY  DS    CL(L'IOKEY)         SAVE THE KEY FOR A MOVE                      
*                                                                               
MOPALCNT DS    XL2                 # ENTRIES IN LIST                            
MOPALPNT DS    XL4                 END OF LIST                                  
MOPALIST DS    9XL(L'CTTKPASS)     LIST OF PASSWORD CODES                       
                                                                                
***********************************************************************         
* LOCAL W/S                                                                     
***********************************************************************         
LOCALD   DSECT                                                                  
DUB      DS    D                                                                
RETURN   DS    F                                                                
VXSORT   DS    A                   A(XSORT)                                     
*                                                                               
MAXNUM   DS    H                   MAXIMUM # PQUEUE ENTRIES FROM CTTRMD         
*                                                                               
IDNUM    DS    H                   USER ID #                                    
IDALPH   DS    CL10                USER ID ALPHA CODE                           
WORK     DS    CL(L'APWORK)                                                     
*                                                                               
SVTRMEL  DS    CL32                CTTRMD SAVE                                  
SVSTAT   DS    XL1                 CTTSTAT SAVE                                 
TERMNUM  DS    H                   TERMINAL #                                   
TERMINFO DS    XL1                                                              
*                                                                               
GETSEQF  DS    XL1                                                              
MOVEPASS DS    X                   MOVE PASS NUMBER                             
*                                                                               
KEYSAVE  DS    XL(L'IOKEY)                                                      
KEYSAVE2 DS    XL(L'IOKEY)                                                      
*                                                                               
DELKEY   DS    CL25                RECORD DELETE KEY AND SAVE                   
DELSAVE  DS    CL25                                                             
TRMDEFID DS    CL8                                                              
TRMDEF   DS    CL32                                                             
*                                                                               
SELDATA  DS    0XL(SELDATAE-SELTERM)                                            
SELTERM  DS    CL8                 TERMINAL                                     
SELTRMSP DS    CL1                 1ST SPECIAL CHAR                             
SELTERML DS    CL1                 (L'DATA ENTERED)                             
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELCTY   DS    XL1                 COUNTRY CODE                                 
SELAGY   DS    CL2                 ALPHA AGENCY                                 
SELUID   DS    CL8                 REPORT USER ID ALPHA                         
SELUIDN  DS    XL2                 REPORT USER ID NUMBER                        
SELSUBID DS    CL3                 REPORT SUB ID                                
SELCLASS DS    CL1                 REPORT CLASS                                 
SELDATAE EQU   *                                                                
*                                                                               
BLOCK1   DS    20CL32              SCANNER BLOCK                                
*                                                                               
*                                  RECORD KEY PASSWORD/PAGE SAVE LIST           
PALCNT   DS    H                   # ENTRIES IN LIST                            
PALPNTR  DS    A                   A(END OF LIST)                               
PALIST   DS    9XL(L'CTTKPASS)     LIST OF PASSWORD CODES                       
*                                                                               
*                                  PQUEUE ELEMENT DATA SAVE LIST                
PELCNT   DS    H                   # ENTRIES IN LIST                            
PELDEL   DS    H                   # DELETED ENTRIES IN LIST                    
PELPNTR  DS    A                   A(END OF LIST)                               
PELIST   DS    XL(PELLEN*MAXPEL)   LIST OF PQ ELEMENT DATA SEE PELISTD          
*                                                                               
LOCALX   EQU   *                   END OF LOCAL WORK STORE                      
*                                                                               
*                                  PELIST ENTRY DSECT                           
PELISTD  DSECT                                                                  
PELSRCA  DS    CL10                USER ID ALPHA                                
         ORG   PELSRCA                                                          
PELSRCN  DS    H                   USER ID #                                    
         DS    XL8                                                              
PELSUBID DS    CL3                 SUB ID                                       
PELCLASS DS    CL1                 CLASS CODE                                   
PELLEN   EQU   *-PELISTD                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTGEN08   10/12/09'                                      
         END                                                                    
