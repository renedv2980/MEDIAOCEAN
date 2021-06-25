*          DATA SET CTGEN26    AT LEVEL 006 AS OF 11/12/10                      
*PHASE TA0B26A                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE NUMVAL                                                                 
*                                                                               
         TITLE 'CTGEN26 - FILE MAINTENANCE - PRINTER NAME RECORDS'              
GEN26    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GE26**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTTREC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         L     R0,=V(SCINKEY)                                                   
         AR    R0,RE                                                            
         ST    R0,VSCINKEY                                                      
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
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     EXIT                                                             
         B     EXIT                                                             
*NOP*    B     VALREQ                                                           
*NOP*    B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF PRINTER TERMINAL RECORD                  *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   EQU   *                                                                
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         XC    PNLCNT,PNLCNT       INITIALISE PRINTER NAME ELEMENT LIST         
         XC    PNLDEL,PNLDEL                                                    
         LA    RF,PNLLEN*MAXPNL                                                 
         XCEF  PNLIST,(RF)                                                      
         LA    RF,PNLIST                                                        
         ST    RF,PNLPNTR                                                       
         XC    CTTKEY,CTTKEY       INITIALISE RECORD KEY                        
         MVI   CTTKTYP,C'T'                                                     
         GOTO1 AFVAL,PNMLUIDH      VALIDATE PRINTER TERMINAL ID                 
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
VKEY040  XC    PNMLUID,PNMLUID     ECHO TERMINAL ID BACK TO TWA                 
         MVC   PNMLUID(8),CTTKTID                                               
         MVI   PNMLUIDH+5,8                                                     
         OI    PNMLUIDH+6,X'80'                                                 
         EJECT                                                                  
*                                  READ MASTER TERMINAL REC                     
         GOTO1 AIO,IOREAD+IOCONFIL+IO1                                          
         BL    EIIO                                                             
         BH    EPND                MUST EXIST                                   
         L     R2,AIOAREA1                                                      
         TM    CTTSTAT,X'01'                                                    
         BO    EPND                IGNORE PASSIVE                               
         TM    CTTSTAT,X'04'                                                    
         BNO   ERNP                MUST BE PRINTER TYPE                         
*                                  EXTRACT PRINTER NAME ELEMENTS                
         L     RF,=A(ADDLST)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
VKEY060  LA    R4,PNLIST           CONVERT ID #S IN LIST TO ALPHA               
         SR    R8,R8                                                            
         LH    R8,PNLCNT                                                        
         LTR   R8,R8                                                            
         BZ    VKEY080                                                          
         USING PNLISTD,R4                                                       
VKEY070  MVC   IDNUM,PNLUIDN                                                    
         L     RF,=A(GETIDA)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    VKEY072             IF ID RECORD NOT VALID                       
         MVI   PNLUIDA,0           FLAG ENTRY FOR EARLY SORT                    
         LA    R3,PNLUIDA+1                                                     
*                                  AND CONVERT ID# FOR DISPLAY                  
         EDIT  (B2,IDNUM),(9,(R3)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB              
         B     VKEY074                                                          
VKEY072  MVC   PNLUIDA,IDALPH                                                   
VKEY074  LA    R4,PNLLEN(R4)                                                    
         BCT   R8,VKEY070                                                       
         DROP  R4                                                               
VKEY080  EQU   *                   SORT PNAME ELEMENT LIST                      
         L     RF,=A(SRTLST)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     VKEY100                                                          
         EJECT                                                                  
* VALIDATE THIS/LAST ACTIONS                                                    
*                                                                               
VKEY100  LA    R2,IOKEY            REREAD MASTER RECORD                         
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
VALKEYY  OI    GENSRVH+FHOID,FHOIMO+FHOITR                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   SAVETID,CTTKTID                                                  
         BE    VALKEYX                                                          
         MVC   SAVETID,CTTKTID     IF CHANGE OF TERMINAL LUID                   
         XC    PNLOFF,PNLOFF         INITIALISE LIST DISPLAY                    
         MVI   PAGENO,1                                                         
         B     VALKEYX                                                          
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A PNAME TYPE RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   CTTKEY,APRECKEY                                                  
         XC    TERMNUM,TERMNUM     CLEAR PASSIVE DATA SAVE AREAS                
         XC    SVTRMEL,SVTRMEL                                                  
         MVI   SVSTAT,0                                                         
*                                                                               
VRCHA    CLI   APACTN,ACTCHA       CHANGE FUNCTION - SAVE ORIG STATUS           
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   PLUID,CTTKTID                                                    
         MVC   SVSTAT,CTTSTAT                                                   
         TM    SVSTAT,X'04'                                                     
         BZ    *+8                                                              
         OI    TERMINFO,X'04'      SET TERMINAL IS A PRINTER                    
         TM    SVSTAT,X'08'                                                     
         BZ    *+8                                                              
         OI    TERMINFO,X'08'      SET TERM IN AUTO MODE                        
         LA    RF,60               INITIALISE PRINTER NAME MAX# DEFAULT         
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
         CLI   0(R3),CTPNXELQ                                                   
         BE    VRCHA50                                                          
         CLI   0(R3),CTPNDELQ                                                   
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
VRCHAX   B     VRPND                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRINTER NAME DESCRIPTION                                   *         
***********************************************************************         
         SPACE 1                                                                
VRPND    XC    APELEM,APELEM                                                    
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PNMDESCH                                                   
         BNE   VRPNDX                                                           
         LA    R3,APELEM                                                        
         USING CTPNDD,R3                                                        
         MVI   CTPNDEL,CTPNDELQ                                                 
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RE,3(RF)                                                         
         STC   RE,CTPNDLEN                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTPNDTXT(0),FVIFLD                                               
         OI    PNMDESCH+6,FVOXMT                                                
         GOTO1 AADDELS,CTTREC                                                   
VRPNDX   EQU   *                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRINTER NAME ELEMENT LIST                                  *         
***********************************************************************         
         SPACE 1                                                                
VRPNM    EQU   *                                                                
         BAS   RE,DELRPT           DELETE REPEATED ENTRIES                      
         GOTO1 VALLST,PNLOFF       VALIDATE PN ELEMENT LIST DISPLAYED           
         BNE   EXIT                                                             
         BAS   RE,CHKCNT           CHECK PQ MAX COUNT EXCEEDED                  
         BNE   EXIT                                                             
*                                                                               
VRPRQ010 EQU   *                   RESORT PNAME EL LIST                         
         L     RF,=A(SRTLST)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
VRPRQX   B     VRUPD                                                            
         EJECT                                                                  
***********************************************************************         
* I/O HANDLING TO UPDATE MASTER,PAGE AND PASSIVE TERMINAL RECORDS     *         
***********************************************************************         
         SPACE 1                                                                
VRUPD    GOTO1 ASETACN,CTTREC      UPDATE ACTIVITY ELEMENT                      
         BNE   VALRECER            RECORD TOO BIG                               
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
*                                                                               
         OI    CTTSTAT,X'04'       SET IS A PRINTER                             
         NI    CTTSTAT,255-X'08'                                                
         TM    TERMINFO,X'08'                                                   
         BZ    *+8                                                              
         OI    CTTSTAT,X'08'       SET PRINTER IN AUTO MODE                     
*                                  BUILD RECORDS FROM PN ELEMENT LIST           
         USING PNLISTD,R4                                                       
         LA    R4,PNLIST           START OF PNAME LIST                          
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
VRUPD020 L     RF,PNLPNTR          CHECK END OF PQ LIST                         
         CR    R4,RF                                                            
         BNL   VRUCHA              GO BUILD CURRENT RECORD                      
         SR    RF,RF               CHECK MAX RECORD LENGTH                      
         ICM   RF,3,CTTLEN                                                      
         AH    RF,MINLEN                                                        
         CH    RF,MAXLEN                                                        
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PNLACT,PNLADELQ     BYPASS DELETED ENTRY                         
         BE    VRUPD022                                                         
         XC    APELEM,APELEM       BUILD ELEMENT                                
         LA    R3,APELEM                                                        
         USING CTPNXD,R3                                                        
         MVI   CTPNXEL,CTPNXELQ                                                 
         MVI   CTPNXLEN,CTPNXLNQ                                                
         MVC   CTPNXUID,PNLUIDN                                                 
         MVC   CTPNXNAM,PNLNAME                                                 
         GOTO1 AADDELN,CTTREC      ADD PQ ELEMENT TO RECORD                     
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
VRUPD022 CLI   PNLACT,0                                                         
         BE    VRUPD023                                                         
         CLI   PNLACT,PNLAADDQ                                                  
         BE    VRUPD024                                                         
         CLI   PNLACT,PNLADELQ                                                  
         BE    VRUPD026                                                         
         CLI   PNLACT,PNLACHGQ                                                  
         BE    VRUPD028                                                         
         DC    H'0'                                                             
*                                  HERE IF NO CHANGE                            
VRUPD023 L     RF,=A(CHKPNM)       CHECK PRINTER NAME ALREADY EXISTS            
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   VRUPD030                                                         
         L     RF,=A(ADDPNM)       IF NOT ADD PNAME RECORD                      
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    VRUPD030                                                         
         DC    H'0'                                                             
*                                  HERE IF ADD NEW PNAME RECORD                 
VRUPD024 L     RF,=A(ADDPNM)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    VRUPD030                                                         
         DC    H'0'                                                             
*                                  HERE IF DELETE PNAME RECORD                  
VRUPD026 L     RF,=A(DELPNM)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    VRUPD030                                                         
         DC    H'0'                                                             
*                                  HERE IF CHANGE PNAME RECORD                  
VRUPD028 L     RF,=A(CHGPNM)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    VRUPD030                                                         
         DC    H'0'                                                             
*                                                                               
VRUPD030 LA    R4,PNLLEN(R4)       BUMP LIST POINTER                            
         B     VRUPD020              GET NEXT ENTRY                             
*                                                                               
VRUCHA   OC    CTTKPASS,CTTKPASS                                                
         BZ    VRUC010             PROCESS FIRST/MASTER RECORD                  
*                                    ELSE PAGED SUB-RECORD                      
         DC    H'0'                                                             
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
VRUNEXT  L     RF,PNLPNTR          EXIT IF END OF PQ LIST                       
         CR    R4,RF                                                            
         BNL   VRUEND                                                           
*                                                                               
VRUEND   EQU   *                                                                
*                                                                               
VALRECX  BAS   RE,INTKEY           REDISPLAY PQ LIST                            
         MVC   FVMSGNO,=AL2(FVFOK)   AND EXIT OK                                
         B     EXIT                                                             
*                                                                               
VALRECER BAS   RE,INTKEY           REDISPLAY PQ LIST                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF PNAME TYPE RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         CLI   CTTKTYP,CTTKTYPQ                                                 
         BE    DISKTRM                                                          
         B     DISKPNM                                                          
DISKTRM  EQU   *                                                                
         MVC   PNMLUID,CTTKTID                                                  
         B     DISKEYX                                                          
DISKPNM  EQU   *                                                                
         DROP  R2                                                               
         USING CTPNKEY,R2                                                       
         CLI   CTPNKTYP,CTPNKTYQ                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEYSAVE,IOKEY                                                    
         MVC   IOKEY(L'CTPNKEY),CTPNKEY                                         
         L     R2,AIOAREA2         READ ID RECORD FROM CONTROL FILE             
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         MVC   IOKEY,KEYSAVE                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,CTPNDATA         READ EACH ELEMENT OF RECORD                  
DSKP020  CLI   0(R3),0             E-O-R                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CTPNLELQ      PRINTER NAME LUID ELEMENT                    
         BE    DSKP040                                                          
*                                                                               
DSKP030  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSKP020                                                          
*                                                                               
         USING CTPNLD,R3                                                        
DSKP040  EQU   *                   DISPLAY PRINTER NAME DESCRIPTION             
         MVC   PNMLUID,CTPNLID                                                  
         B     DISKEYX                                                          
         DROP  R3                                                               
         DROP  R2                                                               
         USING CTTKEY,R2                                                        
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PNAME TYPE RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         OI    PNMDESCH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PNMDESC,PNMDESC                                                  
         LA    R3,CTTDATA                                                       
*                                                                               
DREC010  CLI   0(R3),0             READ THROUGH ELEMENTS                        
         BE    DREC100                                                          
         CLI   0(R3),CTPNDELQ      DISPLAY PRINTER DESCRIPTION ELEMENT          
         BE    DRPND                                                            
*                                                                               
DREC020  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DREC010                                                          
*                                                                               
DREC100  BAS   RE,INTKEY           DISPLAY PRINTER NAME LIST                    
         B     DISRECX                                                          
*                                                                               
DISRECX  EQU   *                                                                
         MVC   FVMSGNO,=AL2(FVFOK)   AND EXIT OK                                
         GOTO1 ADISACT,CTTREC                                                   
         B     EXIT                                                             
*                                  DISPLAY PRINTER DESCRIPTION ELEMENT          
         USING CTPNDD,R3                                                        
DRPND    EQU   *                                                                
         SR    RF,RF                                                            
         IC    RF,CTPNDLEN                                                      
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PNMDESC(0),CTPNDTXT                                              
         OI    PNMDESCH+(FVOIND-FVIHDR),FVOXMT                                  
         B     DREC020                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INTERPRET ACTION FOR LAST KEY STROKE                     *         
* CONTROLLING PRINTER NAME ELEMENT LIST DISPLAY SCROLLING             *         
***********************************************************************         
         SPACE 1                                                                
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
         B     PFBACK              PFK07 = BACK                                 
         B     PFFRWD              PFK08 = FORWARD                              
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
ENTKEY   GOTO1 DISLST,PNLOFF                                                    
         B     IKEYX                                                            
*                                  SCROLL BACK PAGE                             
PFBACK   SR    RE,RE                                                            
         IC    RE,PAGENO                                                        
         BCT   RE,PFBAC020                                                      
PFBAC010 GOTO1 DISLST,PNLOFF                                                    
         B     IKEYX                                                            
*                                                                               
PFBAC020 STC   RE,PAGENO                                                        
         LA    RF,PNLPLEN                                                       
         L     R1,PNLOFF                                                        
         SR    R1,RF                                                            
         LTR   R1,R1                                                            
         BM    PFBAC010                                                         
         ST    R1,PNLOFF                                                        
         GOTO1 DISLST,PNLOFF                                                    
         B     IKEYX                                                            
*                                  SCROLL FORWARD PAGE                          
PFFRWD   SR    RE,RE                                                            
         IC    RE,PAGENO                                                        
         CLM   RE,1,NUMPAGES                                                    
         BNE   PFFRW020                                                         
PFFRW010 GOTO1 DISLST,PNLOFF                                                    
         B     IKEYX                                                            
*                                                                               
PFFRW020 LA    RE,1(RE)                                                         
         STC   RE,PAGENO                                                        
         LA    RF,PNDLNUM*PNLLEN                                                
         L     R1,PNLOFF                                                        
         AR    R1,RF                                                            
         L     RF,PNLPNTR                                                       
         LA    RE,PNLIST                                                        
         SR    RF,RE                                                            
         CR    R1,RF                                                            
         BH    PFFRW010                                                         
         ST    R1,PNLOFF                                                        
         GOTO1 DISLST,PNLOFF                                                    
         B     IKEYX                                                            
*                                  UNDEFINED PF KEY                             
PFUNDF   GOTO1 DISLST,PNLOFF                                                    
         B     IKEYX                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PRINTER NAME ELEMENT LIST DATA ON SCREEN         *         
* R1 POINTS TO START POINT IN LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
DISLST   NTR1                                                                   
         MVC   PNMPAGE(1),PAGENO   DISPLAY PAGE NUMBER INFO.                    
         OI    PNMPAGE,C'0'                                                     
         MVC   PNMPAGE+2(1),NUMPAGES                                            
         OI    PNMPAGE+2,C'0'                                                   
         OI    PNMPAGEH+FHOID,FHOITR                                            
*                                                                               
         L     R4,0(R1)            GET OFFSET TO A(FIRST PN LIST ENTRY)         
         TWAXC PNMPNDAH            CLEAR SCREEN                                 
         LA    RF,PNLIST                                                        
         AR    R4,RF                                                            
         LA    R3,PNMPNDAH         ADDRESS FIRST POSITION ON SCREEN             
         LR    R8,R3                                                            
         LA    R0,COLNUM           # LIST COLUMNS                               
*                                                                               
         USING PNLISTD,R4                                                       
DLST010  L     RF,PNLPNTR                                                       
         CR    R4,RF                                                            
         BNL   DLSTOK              EXIT IF END OF LIST                          
         CLI   PNLACT,PNLADELQ     BYPASS ENTRIES FLAG AS DELETED               
         BE    DLST030                                                          
         LR    R1,R4                                                            
*                                  DISPLAY ENTRY                                
         L     RF,=A(DISPNM)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         MVC   FVIFLD-FVIHDR(L'PNMPNDA,R3),APWORK                               
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         NI    FVATRB-FVIHDR(R3),X'FF'-FVAHIGH                                  
*                                                                               
         LA    RF,PNLIST           HIGHLIGHT REPEATED ENTRY                     
         CR    R4,RF                                                            
         BNH   DLST012                                                          
         LR    R1,R4                                                            
         LA    RE,PNLLEN                                                        
         SR    R1,RE                                                            
         DROP  R4                                                               
         USING PNLISTD,R1                                                       
         CLC   PNLUIDA+PNLLEN(PNLKEYLN),PNLUIDA                                 
         BNE   DLST012                                                          
         OI    FVATRB-FVIHDR(R3),FVAHIGH                                        
         DROP  R1                                                               
*                                                                               
DLST012  BCT   R0,DLST020          GET NEXT DISPLAY ADDRESS                     
         LA    R0,COLNUM                                                        
         LR    R3,R8                                                            
         LA    RF,PNDLLEN                                                       
         AR    R3,RF                                                            
         LA    RF,PNMBBARH                                                      
         CR    R3,RF                                                            
         BNL   DLSTOK              EXIT AT END OF SCREEN                        
         LR    R8,R3                                                            
         B     DLST030                                                          
DLST020  LA    RF,PNDFLEN                                                       
         AR    R3,RF                                                            
DLST030  LA    R4,PNLLEN(R4)       GET NEXT PQ LIST ENTRY                       
         B     DLST010                                                          
*                                                                               
DLSTNO   B     NO                                                               
DLSTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE REPEATED ENTRIES IN PRINTER NAME LIST             *         
***********************************************************************         
         SPACE 1                                                                
DELRPT   NTR1                                                                   
         USING PNLISTD,R4                                                       
         LA    R4,PNLIST                                                        
DRPT010  L     RF,PNLPNTR          SEARCH DOWN LIST                             
         LA    RE,PNLLEN                                                        
         SR    RF,RE                                                            
         CR    R4,RF                                                            
         BNL   DRPTX                                                            
*                                                                               
         CLC   PNLUIDA+PNLLEN(PNLKEYLN),PNLUIDA                                 
         BNE   DRPT020                                                          
         MVI   PNLACT,PNLADELQ     FLAG DELETED                                 
         SR    RF,RF                                                            
         LH    RF,PNLDEL                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PNLDEL                                                        
*                                                                               
DRPT020  LA    R4,PNLLEN(R4)                                                    
         B     DRPT010                                                          
*                                                                               
DRPTX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PRINTER NAME ELEMENT LIST DATA ON SCREEN        *         
* R1 POINTS TO START POINT IN LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
VALLST   NTR1                                                                   
         USING PNLISTD,R4                                                       
         L     R4,0(R1)                                                         
         LA    RF,PNLIST                                                        
         AR    R4,RF               ADDRESS FIRST PQ LIST ENTRY                  
         LA    R3,PNMPNDAH         ADDRESS FIRST DISPLAY FIELD                  
         LR    R8,R3                                                            
         LA    R0,COLNUM           DISPLAY COLUMN #                             
VLST010  CLI   PNLACT,PNLADELQ                                                  
         BE    VLST030             BYPASS IF FLAG AS DELETED                    
         LR    R1,R3                                                            
         GOTO1 AFVAL                                                            
         BE    VLST020                                                          
         CLI   FVILEN,0            CHECK IF FIELD CLEARED                       
         BNE   VLST030                                                          
         L     RF,PNLPNTR                                                       
         CR    R4,RF                                                            
         BNL   VLST030             IGNORE IF PAST END OF LIST                   
         MVI   PNLACT,PNLADELQ                                                  
         SR    RF,RF                                                            
         LH    RF,PNLDEL                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PNLDEL                                                        
         B     VLST030                                                          
*                                                                               
VLST020  EQU   *                                                                
         LR    R1,R4                                                            
         BAS   RE,VALPNM           VALIDATE PRINTER NAME ELEMENT FIELD          
         BNE   VLSTNO                EXIT IF INVALID                            
*                                                                               
VLST030  L     RF,PNLPNTR                                                       
         CR    R4,RF                                                            
         BL    VLST040             CHECK END OF PQ LIST                         
         OC    0(PNLLEN,R4),0(R4)                                               
         BZ    VLST050               AND NO NEW ENTRY LOADED                    
         BAS   RE,BUMPLP           BUMP PNAME LIST POINTER/COUNT                
         BNE   VLSTNO                EXIT IF INVALID                            
VLST040  LA    R4,PNLLEN(R4)       NEXT PQ ELEMENT LIST POINTER                 
*                                                                               
VLST050  BCT   R0,VLST060          GET NEXT SCREEN FIELD ADDRESS                
         LA    R0,COLNUM                                                        
         LR    R3,R8                                                            
         LA    RF,PNDLLEN                                                       
         AR    R3,RF                                                            
         LA    RF,PNMBBARH                                                      
         CR    R3,RF                                                            
         BNL   VLSTOK              EXIT AT END OF SCREEN                        
         LR    R8,R3                                                            
         B     VLST010                                                          
VLST060  LA    RF,PNDFLEN                                                       
         AR    R3,RF                                                            
         B     VLST010                                                          
*                                                                               
VLSTNO   B     NO                  ENTRY INVALID                                
*                                                                               
VLSTOK   B     YES                 LIST OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PRINTER NAME ELEMENT DATA FOR DISPLAY ENTRY     *         
* DATA RETURNED IN PNLIST ENTRY AT R4                                 *         
***********************************************************************         
         SPACE 1                                                                
VALPNM   NTR1                                                                   
         USING PNLISTD,R4                                                       
         LR    R4,R1                                                            
         XC    BLOCK1(2*L'BLOCK1),BLOCK1                                        
         GOTO1 VSCANNER,APPARM,FVIHDR,(2,BLOCK1)                                
         CLI   4(R1),0                                                          
         BE    EIPN                                                             
         CLI   4(R1),2                                                          
         BL    EIPN                                                             
         CLI   4(R1),2                                                          
         BH    EIPN                                                             
         LA    R8,BLOCK1           R8=A(SCAN BLOCK ENTRY)                       
         MVI   FVINDX,1                                                         
*                                  VALIDATE USER-ID                             
         CLI   0(R8),0                                                          
         BE    EIPN                                                             
         CLI   1(R8),0                                                          
         BNE   EIPN                                                             
         CLI   0(R8),2                                                          
         BL    EFTS                                                             
         CLI   0(R8),10                                                         
         BH    EFTL                                                             
         MVC   IDALPH,12(R8)                                                    
         L     RF,=A(GETIDN)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   EUNF                                                             
*                                  VALIDATE PRINTER NAME                        
         LA    R8,32(R8)                                                        
         MVI   FVINDX,2                                                         
         CLI   0(R8),0                                                          
         BE    VPNM020                                                          
         CLI   1(R8),0                                                          
         BNE   EIPN                                                             
         TM    2(R8),X'80'                                                      
         BO    *+12                                                             
         CLI   0(R8),2                                                          
         BL    EFTS                                                             
         CLI   0(R8),4                                                          
         BH    EFTL                                                             
         CLI   12(R8),C'P'                                                      
         BE    EPPQ                                                             
         CLI   12(R8),C'S'                                                      
         BE    EPPQ                                                             
         CLI   12(R8),C'L'                                                      
         BE    EPPQ                                                             
         CLI   12(R8),C'+'                                                      
         BE    EPPQ                                                             
         CLC   12(4,R8),=CL4'DQU'                                               
         BE    EPPQ                                                             
         MVC   PNAME,12(R8)                                                     
         B     VPNM010                                                          
*                                                                               
VPNM010  EQU   *                                                                
         L     RF,PNLPNTR                                                       
         CR    R4,RF                                                            
         BNL   VPNM014             CHECK END OF PQ LIST                         
         OC    0(PNLLEN,R4),0(R4)                                               
         BZ    VPNM014             AND NO NEW ENTRY LOADED                      
         CLC   PNLUIDA,IDALPH                                                   
         BNE   VPNM012                                                          
         CLC   PNLNAME,PNAME                                                    
         BE    VPNMOK              IGNORE IF SAME AS LIST VALUE                 
VPNM012  MVI   PNLACT,PNLACHGQ                                                  
         MVC   PNLOLDID,PNLUIDN                                                 
         MVC   PNLOLDNM,PNLNAME                                                 
         B     VPNM016                                                          
VPNM014  MVI   PNLACT,PNLAADDQ                                                  
VPNM016  MVC   PNLUIDA,IDALPH                                                   
         MVC   PNLUIDN,IDNUM                                                    
         MVC   PNLNAME,PNAME                                                    
*                                                                               
         MVI   FVINDX,0                                                         
         L     RF,=A(CHKPNM)       CHECK PRINTER NAME ALREADY EXISTS            
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   EPAD                                                             
*                                                                               
VPNM020  MVI   FVINDX,0                                                         
         BAS   RE,CHKRPT           CHECK ENTRY NOT ALREADY IN LIST              
         BNE   EDIF                                                             
         B     VPNMOK                                                           
*                                                                               
VPNMNO   B     NO                  INVALID ENTRY                                
*                                                                               
VPNMOK   MVI   FVINDX,0            ENTRY OK                                     
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK BYTE AT R1 IS ALPHA NUMERIC                        *         
***********************************************************************         
         SPACE 1                                                                
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
*                                                                               
ANUMOK   B     YES                 IS ALPHA NUMERIC                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK FOR REPEATED ENTRY IN PRINTER NAME LIST            *         
***********************************************************************         
         SPACE 1                                                                
CHKRPT   NTR1                                                                   
         USING PNLISTD,R4                                                       
         LA    R8,PNLIST                                                        
CRPT010  L     RF,PNLPNTR                                                       
         CR    R8,RF                                                            
         BNL   CRPTOK                                                           
         CR    R8,R4                                                            
         BE    CRPT020                                                          
         CLI   PNLACT,PNLADELQ                                                  
         BE    CRPT020                                                          
         CLC   PNLUIDA(PNLKEYLN),PNLUIDA-PNLISTD(R8)                            
         BE    CRPTNO                                                           
*                                                                               
CRPT020  LA    R8,PNLLEN(R8)                                                    
         B     CRPT010                                                          
*                                                                               
CRPTNO   B     NO                                                               
*                                                                               
CRPTOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUMP PRINTER NAME ELEMENT LIST POINTER FOR NEW ENTRY    *          
***********************************************************************         
         SPACE 1                                                                
BUMPLP   NTR1                                                                   
         L     R1,PNLPNTR                                                       
         LA    RF,PNLLEN                                                        
         AR    R1,RF                                                            
         ST    R1,PNLPNTR                                                       
         SR    RF,RF                                                            
         LH    RF,PNLCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PNLCNT                                                        
         BAS   RE,CHKCNT                                                        
         BNE   BPLPNO                                                           
         B     BPLPOK                                                           
*                                                                               
BPLPNO   B     NO                                                               
*                                                                               
BPLPOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK # ELEMENTS IN LIST WITHIN MAXIMUM                  *         
***********************************************************************         
         SPACE 1                                                                
CHKCNT   NTR1                                                                   
         OC    MAXNUM,MAXNUM                                                    
         BZ    CCNTOK                                                           
         SR    RF,RF                                                            
         LH    RF,PNLCNT                                                        
         SH    RF,PNLDEL                                                        
         CH    RF,MAXNUM                                                        
         BNH   CCNTOK                                                           
         B     ETOO                                                             
*                                                                               
CCNTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO DELETE A RECORD WITH KEY=DELKEY                       *         
***********************************************************************         
         SPACE 1                                                                
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
* GET TERMINAL NAME FROM PASSIVE NUMBER #NNNNNN                       *         
***********************************************************************         
         SPACE 1                                                                
VALPSVN  NTR1                                                                   
         SR    R0,R0               R0=ERROR NUMBER (ZERO IS OK)                 
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         SH    R1,=H'2'            R1=L'NUMBER-1                                
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
         XC    PNMLUID,PNMLUID     MOVE TERMINAL ID TO TWA                      
         MVC   PNMLUID(8),CTPASDTA                                              
         MVI   PNMLUIDH+5,8                                                     
         NI    PNMLUIDH+4,X'F1'                                                 
         OI    PNMLUIDH+6,X'80'                                                 
*                                                                               
VALPSVNX B     YES                 EXIT WITH CC=ZERO IF OK                      
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS FOR ACCESS RECORDS            *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         XC    SELDATA,SELDATA                                                  
*                                                                               
VSLUID   EQU   *                   PRINTER LUID                                 
         GOTO1 AFVAL,LSTLUIDH                                                   
         BNE   VSLUIDX                                                          
         MVC   SELLUID,FVIFLD                                                   
         MVC   LSTLUID,FVIFLD                                                   
         OI    LSTLUIDH+6,X'80'                                                 
VSLUIDX  EQU   *                                                                
*                                                                               
VSUID    EQU   *                   USER ID FILTER                               
         GOTO1 AFVAL,LSTUIDH                                                    
         BNE   VSUIDX                                                           
         MVC   IDALPH,FVIFLD                                                    
         L     RF,=A(GETIDN)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   EIIF                                                             
         MVC   SELUIDN,IDNUM                                                    
         MVC   LSTUID,IDALPH                                                    
         OI    LSTUIDH+6,X'80'                                                  
VSUIDX   EQU   *                                                                
*                                                                               
VSNAME   EQU   *                   PRINTER NAME                                 
         GOTO1 AFVAL,LSTNAMEH                                                   
         BNE   VSNAMEX                                                          
         MVC   SELNAME,FVIFLD                                                   
         MVC   LSTNAME,FVIFLD                                                   
         OI    LSTNAMEH+6,X'80'                                                 
VSNAMEX  EQU   *                                                                
*                                                                               
VSNLST   EQU   *                   PRINTER NAME LIST                            
         GOTO1 AFVAL,LSTNLSTH                                                   
         BNE   VSNLSTX                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   EIIF                                                             
         MVC   SELNLST,FVIFLD                                                   
         MVC   LSTNLST,FVIFLD                                                   
         OI    LSTNLSTH+6,X'80'                                                 
VSNLSTX  EQU   *                                                                
*                                                                               
         LA    R2,APRECKEY         SET UP FIRST LIST RECORD KEY                 
         CLI   SELNLST,C'Y'                                                     
         BE    VALSELA                                                          
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         OC    SELLUID,SELLUID                                                  
         BZ    *+10                                                             
         MVC   CTTKTID,SELLUID                                                  
*                                                                               
         OI    LSTHED1H+6,FVOXMT                                                
         MVC   LSTHED1,DFLTHED1                                                 
         OI    LSTHED2H+6,FVOXMT                                                
         MVC   LSTHED2,DFLTHED2                                                 
         B     VALSELY                                                          
         DROP  R2                                                               
         USING CTPNREC,R2                                                       
VALSELA  XC    CTPNKEY,CTPNKEY                                                  
         MVI   CTPNKTYP,CTPNKTYQ                                                
         OC    SELUIDN,SELUIDN                                                  
         BZ    *+10                                                             
         MVC   CTPNKUIN,SELUIDN                                                 
         OC    SELNAME,SELNAME                                                  
         BZ    *+10                                                             
         MVC   CTPNKNAM,SELNAME                                                 
*                                                                               
         OI    LSTHED1H+6,FVOXMT                                                
         MVC   LSTHED1,NLSTHED1                                                 
         OI    LSTHED2H+6,FVOXMT                                                
         MVC   LSTHED2,NLSTHED2                                                 
         B     VALSELY                                                          
         DROP  R2                                                               
         USING CTTREC,R2                                                        
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST-SELECT RECORD FOR PRINTER TERMINAL RECORDS            *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         CLI   SELNLST,C'Y'        TEST FOR PNAME LIST ORDER                    
         BE    GSNLST                                                           
         MVC   CTTKEY,APRECKEY                                                  
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GETSEL2                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GETSEL4                                                          
         B     GETSELN                                                          
GETSEL2  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GETSEL4                                                          
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GETSEL4  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         L     R2,AIOAREA1                                                      
         CLI   CTTKTYP,CTTKTYPQ                                                 
         BNE   GETSELN                                                          
         OC    CTTKSPAR,CTTKSPAR                                                
         BNZ   GETSELN                                                          
         OC    CTTKTID,CTTKTID                                                  
         BZ    GETSEL4                                                          
         OC    CTTKPASS,CTTKPASS                                                
         BNZ   GETSEL4                                                          
         TM    CTTSTAT,X'01'                                                    
         BO    GETSEL4                                                          
         TM    CTTSTAT,X'04'                                                    
         BZ    GETSEL4                                                          
*                                                                               
         MVI   APFLAG,0                                                         
         LA    R3,CTTDATA          GET ELEMENTS FOR SELECT FILTER               
GETSELA  CLI   0(R3),0             E-O-R                                        
         BE    GETSELF                                                          
         CLI   0(R3),CTPNXELQ      PRINTER NAME X-REF ELEMENT                   
         BE    GSPNX                                                            
*                                                                               
GETSELE  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETSELA                                                          
*                                                                               
GETSELF  EQU   *                                                                
         OC    SELUIDN,SELUIDN                                                  
         BNZ   GETSEL4                                                          
         OC    SELNAME,SELNAME                                                  
         BNZ   GETSEL4                                                          
         CLI   APFLAG,0                                                         
         BE    GETSEL4                                                          
         B     GETSELK                                                          
*                                                                               
         USING CTPNXD,R3                                                        
GSPNX    EQU   *                   PROCESS PRINTER NAME X-REF ELEMENT           
         MVI   APFLAG,X'FF'        FLAG PRINTER NAME FOUND                      
         OC    SELUIDN,SELUIDN     FILTER ON USER ID                            
         BZ    *+14                UNLESS NO SELECTS                            
         CLC   SELUIDN,CTPNXUID                                                 
         BE    GETSELK                                                          
         OC    SELNAME,SELNAME     PRINTER NAME                                 
         BZ    *+14                                                             
         CLC   SELNAME,CTPNXNAM                                                 
         BE    GETSELK                                                          
         B     GETSELE                                                          
*                                                                               
GETSELK  DS    0H                  RECORD SELECTED                              
*                                                                               
GETSELY  MVC   APRECKEY(L'CTTKEY),CTTKEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST-SELECT RECORD FOR PNAME RECORDS                       *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R2                                                               
         USING CTPNREC,R2                                                       
GSNLST   EQU   *                                                                
         MVC   CTPNKEY,APRECKEY                                                 
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GSNL010                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GSNL020                                                          
         B     GETSELN                                                          
GSNL010  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GSNL020                                                          
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GSNL020  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GSNLNO                                                           
         L     R2,AIOAREA1                                                      
         CLI   CTPNKTYP,CTPNKTYQ                                                
         BNE   GSNLNO                                                           
*                                                                               
         OC    SELNAME,SELNAME                                                  
         BZ    *+14                                                             
         CLC   SELNAME,CTPNKNAM                                                 
         BNE   GSNL020                                                          
*                                                                               
         OC    SELUIDN,SELUIDN                                                  
         BZ    *+14                                                             
         CLC   SELUIDN,CTPNKUIN                                                 
         BNE   GSNL020                                                          
*                                                                               
         OC    SELLUID,SELLUID                                                  
         BZ    GSNL200                                                          
*                                                                               
         LA    R3,CTPNDATA         GET ELEMENTS FOR SELECT FILTER               
GSNL100  CLI   0(R3),0             E-O-R                                        
         BE    GSNL020                                                          
         CLI   0(R3),CTPNLELQ      PRINTER NAME LUID ELEMENT                    
         BE    GSNL120                                                          
*                                                                               
GSNL110  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GSNL100                                                          
*                                                                               
         USING CTPNLD,R3                                                        
GSNL120  EQU   *                   PROCESS PRINTER NAME LUID ELEMENT            
         CLC   SELLUID,CTPNLID                                                  
         BE    GSNL200                                                          
         B     GSNL020                                                          
*                                                                               
GSNL200  DS    0H                  RECORD SELECTED                              
*                                                                               
GSNLOK   MVC   APRECKEY(L'CTPNKEY),CTPNKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GSNLX                                                            
*                                                                               
GSNLNO   MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GSNLX    B     EXIT                                                             
         DROP  R2                                                               
         USING CTTREC,R2                                                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE FOR PRINTER TERMINAL RECORDS               *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         CLI   SELNLST,C'Y'                                                     
         BE    DSNLST                                                           
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTLUID,CTTKTID    DISPLAY PRINTER LUID                         
         LA    R3,CTTDATA          READ EACH ELEMENT OF RECORD                  
DSEL2    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),CTPNDELQ      PRINTER NAME DECRIPTION ELEMENT              
         BE    DSPND                                                            
*                                                                               
DSEL4    SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSEL2                                                            
*                                                                               
DISSELX  EQU   *                                                                
         B     EXIT                                                             
         SPACE 2                                                                
         USING CTPNDD,R3                                                        
DSPND    EQU   *                   DISPLAY PRINTER NAME DESCRIPTION             
         SR    RF,RF                                                            
         IC    RF,CTPNDLEN                                                      
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LISTDESC(0),CTPNDTXT                                             
         B     DSEL4                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         DROP  R4                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE FOR PRINTER NAME RECORDS                   *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R2                                                               
         USING CTPNREC,R2                                                       
DSNLST   EQU   *                                                                
         USING NLSTD,R4            R4=A(LIST/SELECT LINE)                       
         CLC   LASTUIDN,CTPNKUIN                                                
         BE    DSNL010                                                          
         MVC   LASTUIDN,CTPNKUIN                                                
         MVC   IDNUM,CTPNKUIN                                                   
         L     RF,=A(GETIDA)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         MVC   LASTUIDA,IDALPH                                                  
         OI    APINDS,APILRERD     FLAG READ SEQUENCE BROKEN                    
DSNL010  MVC   NLSTUID,LASTUIDA                                                 
         MVC   NLSTNAME,CTPNKNAM                                                
         MVC   NLSTLUID,=CL8'????????'                                          
         LA    R3,CTPNDATA         READ EACH ELEMENT OF RECORD                  
DSNL020  CLI   0(R3),0             E-O-R                                        
         BE    DSNLX                                                            
         CLI   0(R3),CTPNLELQ      PRINTER NAME LUID ELEMENT                    
         BE    DSNL040                                                          
*                                                                               
DSNL030  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSNL020                                                          
*                                                                               
         USING CTPNLD,R3                                                        
DSNL040  EQU   *                   DISPLAY PRINTER NAME DESCRIPTION             
         MVC   NLSTLUID,CTPNLID                                                 
*                                                                               
DSNLX    EQU   *                                                                
         B     EXIT                                                             
         DROP  R3                                                               
         DROP  R4                                                               
         DROP  R2                                                               
         USING CTTREC,R2                                                        
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R8,AREP                                                          
         USING REPD,R8             R8=A(REPORT WORK AREA)                       
         XC    SELDATA,SELDATA                                                  
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
         LA    R4,REPUIDH                                                       
         L     RF,=A(VALPARS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF               GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         LA    R2,APRECKEY         BUILD AN INITIAL KEY                         
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,C'T'                                                     
         MVC   CTTKTID,SELLUID                                                  
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
* ROUTINE TO PRINT TERMINAL LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   EQU   *                                                                
         L     R8,AREP                                                          
         USING REPD,R8             R8=A(REPORT WORK AREA)                       
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   CTTKEY,APRECKEY                                                  
         OC    CTTKLINE,CTTKLINE   (IF NO KEY ENTERED                           
         BNZ   *+8                    READ PAST PASSIVES)                       
         MVI   CTTKLINE+L'CTTKLINE-1,1                                          
*                                                                               
         LA    R1,IOHI+IOCONFIL+IO1                                             
         L     RF,=A(GETREC)         GO GET REC WIV                             
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
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
         L     RF,=A(GETREC)       GO GET NEXT REC                              
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   PRTREPX                                                          
*                                                                               
PREP100  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTTKEY),CTTKEY                                        
         LA    R4,REPP1-14                                                      
         L     RF,=A(REPREC)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF               GO PROCESS RECORD FOR REPORT OUTPUT          
         B     PREP010                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*                                  ERROR EXITS                                  
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
ERAE     MVC   FVMSGNO,=AL2(CE#RECAE)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  LUID NOT PRINTER                             
EPND     MVC   FVMSGNO,=AL2(CE#LUNP)                                            
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PRINTER RECORD NOT DEFINED                   
ERNP     MVC   FVMSGNO,=AL2(CE#LUNF)                                            
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD ALREADY EXISTS                        
EUNF     MVC   FVMSGNO,=AL2(CE#IDRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  USER ID RECORD NOT FOUND                     
EIID     MVC   FVMSGNO,=AL2(CE#INVID)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID USER ID                              
EIPN     MVC   FVMSGNO,=AL2(CE#INPNM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID PRINTER NAME SYNTAX                  
EPPQ     MVC   FVMSGNO,=AL2(CE#PNMPQ)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID PNAME =PQ ACTION CONTENTION          
EPAD     MVC   FVMSGNO,=AL2(CE#PNMAD)                                           
         MVC   FVOSYS,ASSYSE                                                    
         MVC   FVXTRA(8),OLDPLUID                                               
         B     NO                  PRINTER NAME ALREADY DEFINED                 
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
*                                  HEADINGS FOR LIST/SEL SCREEN                 
DFLTHED1 DC    CL(L'LSTHED1)'    Printer  Printer Description'                  
DFLTHED2 DC    CL(L'LSTHED2)'Act LUID     -------------------'                  
NLSTHED1 DC    CL(L'LSTHED1)'    Userid     Name Printer'                       
NLSTHED2 DC    CL(L'LSTHED2)'Act ------     ---- LUID'                          
         LTORG                                                                  
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    80C' '                                                           
FFILL    DC    80X'FF'                                                          
CAPFILL  DC    (L'APWORK)X'40'                                                  
MINLEN   DC    H'200'                                                           
MAXLEN   DC    H'1000'                                                          
MAXPNL   EQU   60                                                               
PNDFLEN  EQU   PNMPNDBH-PNMPNDAH                                                
PNDLLEN  EQU   PNMPNDDH-PNMPNDAH                                                
PNDTLEN  EQU   PNMBBARH-PNMPNDAH                                                
COLNUM   EQU   PNDLLEN/PNDFLEN                                                  
LINNUM   EQU   PNDTLEN/PNDLLEN                                                  
PNDLNUM  EQU   COLNUM*LINNUM                                                    
PNLPLEN  EQU   PNDLNUM*PNLLEN                                                   
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RB,RA                                                            
         SPACE 1                                                                
***********************************************************************         
* CONVERT ID NUMBER TO ID ALPHA                                       *         
***********************************************************************         
         DS    0D                                                               
GETIDA   NTR1  BASE=*                                                           
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
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT ID ALPHA TO ID NUMBER                                       *         
***********************************************************************         
         DS    0D                                                               
GETIDN   NTR1  BASE=*                                                           
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
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TRANSFER PRINTER NAME ELEMENT DATA FOR DISPLAY           *         
* DATA RETURNED IN APWORK                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
DISPNM   NTR1  BASE=*                                                           
         XC    APWORK,APWORK                                                    
         LA    R3,APWORK                                                        
         LA    R8,L'PNMPNDA(R3)                                                 
         LR    R4,R1                                                            
         USING PNLISTD,R1                                                       
*                                  DISPLAY USERID                               
         CLI   0(R4),0                                                          
         BNE   DPNM010                                                          
         LA    R4,1(R4)            BUMP OVER ID NUM DISPLAY FLAG                
*                                                                               
DPNM010  CLI   0(R4),C' '                                                       
         BE    DPNM020                                                          
         MVC   0(1,R3),0(R4)                                                    
         LA    R3,1(R3)                                                         
         CR    R3,R8                                                            
         BNL   DPNM100                                                          
         LA    R4,1(R4)                                                         
         LA    RF,PNLNAME                                                       
         CR    R4,RF                                                            
         BL    DPNM010                                                          
DPNM020  LA    R4,PNLNAME                                                       
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         CR    R3,R8                                                            
         BNL   DPNM100                                                          
*                                  DISPLAY PRINTER NAME                         
DPNM030  CLI   0(R4),C' '                                                       
         BE    DPNMX                                                            
         CLI   0(R4),0                                                          
         BE    DPNMX                                                            
         MVC   0(1,R3),0(R4)                                                    
         LA    R3,1(R3)                                                         
         CR    R3,R8                                                            
         BNL   DPNM100                                                          
         LA    R4,1(R4)                                                         
         LA    RF,PNLNAME+L'PNLNAME                                             
         CR    R4,RF                                                            
         BL    DPNM030                                                          
         B     DPNMX                                                            
*                                                                               
DPNM100  BCTR  R3,0                                                             
         MVI   0(R3),C'>'                                                       
         B     DPNMX                                                            
*                                                                               
DPNMX    XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD PRINTER NAME XREF ELEMENTS FROM TERMINAL RECORD      *         
* INTO LIST                                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
ADDLST   NTR1  BASE=*                                                           
*                                                                               
         LA    R3,CTTDATA          READ PNAME XREF ELEMENTS IN RECORD           
*                                                                               
ALST010  CLI   0(R3),0                                                          
         BE    ALSTOK                                                           
         CLI   0(R3),CTPNXELQ                                                   
         BE    ALST030                                                          
         SR    RF,RF                                                            
ALST020  IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     ALST010                                                          
*                                                                               
         USING CTPNXD,R3                                                        
         USING PNLISTD,R4                                                       
ALST030  L     R4,PNLPNTR          SAVE PNAME ELEMENT ENTRY IN LIST             
         XC    0(PNLLEN,R4),0(R4)                                               
         MVC   PNLUIDN,CTPNXUID                                                 
         MVC   PNLNAME,CTPNXNAM                                                 
         B     ALST050                                                          
*                                                                               
ALST050  LA    R4,PNLLEN(R4)       BUMP LIST POINTER/COUNT                      
         ST    R4,PNLPNTR                                                       
         SR    RF,RF                                                            
         LH    RF,PNLCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,PNLCNT                                                        
         B     ALST020             GET NEXT ENTRY                               
*                                                                               
ALSTOK   SR    RC,RC                                                            
ALSTNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SORT PRINTER NAME ELEMENT LIST DATA                      *         
***********************************************************************         
         SPACE 1                                                                
SRTLST   NTR1  BASE=*                                                           
         LA    R4,PNLIST           SORT PNAME ELEMENT LIST                      
         SR    R0,R0                                                            
         LH    R0,PNLCNT                                                        
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R0),PNLLEN,PNLKEYLN,0                
*                                                                               
         LR    R1,R0               CALCULATE MAXIMUM # OF DISPLAY PAGES         
         SR    R0,R0                                                            
         SH    R1,PNLDEL                                                        
         LA    RF,PNDLNUM                                                       
         DR    R0,RF                                                            
         LA    R1,1(R1)                                                         
         STC   R1,NUMPAGES                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                            *          
**********************************************************************          
         SPACE 1                                                                
         USING REPUIDH,R4                                                       
VALPARS  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALPARS,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VPA'    INSERT NAME                                  
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALPARSX XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET NEXT RECORD FOR REPORT, FILTERING ON I/P PARAMETERS            *          
**********************************************************************          
         SPACE 1                                                                
GETREC   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETREC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GRE'    INSERT NAME                                  
*                                                                               
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
GETRECY  SR    RC,RC               RETURN CC EQUAL RECORD OK                    
GETRECN  LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD FOR REPORT OUTPUT                                    *         
***********************************************************************         
         SPACE                                                                  
         DS    0D                                                               
REPREC   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING REPREC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+REP'    INSERT NAME                                  
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
*        OI    REPHEADI,REPHFRCE   FORCE NEW PAGE NEXT TIME                     
*        NI    REPHEADI,X'FF'-(REPHSPAC)                                        
         XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
RESPACES DC    80C' '                                                           
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK PRINTER NAME RECORD FROM PNLISTD ENTRY AT R4       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DROP  R2                                                               
         USING PNLISTD,R4                                                       
CHKPNM   NTR1  BASE=*                                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         USING CTPNREC,R2                                                       
         XC    CTPNREC(256),CTPNREC                                             
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,PNLUIDN                                                 
         MVC   CTPNKNAM,PNLNAME                                                 
         MVC   IOKEY(L'CTPNKEY),CTPNKEY                                         
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         MVC   IOKEY,KEYSAVE                                                    
         BNE   CPNMOK                                                           
*                                                                               
         LA    R1,CTPNDATA                                                      
         SR    R0,R0                                                            
CPNM010  CLI   0(R1),0                                                          
         BE    CPNMOK                                                           
         CLI   0(R1),CTPNLELQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     CPNM010                                                          
         CLC   PLUID,CTPNLID-CTPNLD(R1)                                         
         BE    CPNMOK                                                           
         MVC   OLDPLUID,CTPNLID-CTPNLD(R1)                                      
         B     CPNMNO                                                           
*                                                                               
CPNMOK   SR    RC,RC                                                            
CPNMNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD PRINTER NAME RECORD FROM PNLISTD ENTRY AT R4         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         USING PNLISTD,R4                                                       
ADDPNM   NTR1  BASE=*                                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         USING CTPNREC,R2                                                       
         XC    CTPNREC(256),CTPNREC                                             
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,PNLUIDN                                                 
         MVC   CTPNKNAM,PNLNAME                                                 
         LA    R0,CTPNDATA+1-CTPNREC                                            
         STCM  R0,3,CTPNLEN                                                     
*                                                                               
         LA    R3,APELEM                                                        
         USING CTPNLD,R3                                                        
         XC    CTPNLEL(CTPNLLNQ),CTPNLEL                                        
         MVI   CTPNLEL,CTPNLELQ                                                 
         MVI   CTPNLLEN,CTPNLLNQ                                                
         MVC   CTPNLID,PLUID                                                    
         DROP  R3                                                               
         GOTO1 AADDELS,CTPNREC                                                  
*                                                                               
         GOTO1 ASETACT,CTPNREC                                                  
         MVC   IOKEY(L'CTPNKEY),CTPNKEY                                         
         LA    R1,IORDUPD+IOCONFIL+IO3                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'               IO ERROR                                     
         BE    ADPN010             OVERWRITE IF EXISTS                          
         TM    IOERR,IOEDEL                                                     
         BO    ADPN010                                                          
         TM    IOERR,IOERNF        ELSE ADD                                     
         BO    *+6                                                              
         DC    H'00'                                                            
         LA    R1,IOADD+IOCONFIL+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ADPN020                                                          
*                                                                               
ADPN010  LA    R1,IOWRITE+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
ADPN020  MVC   IOKEY,KEYSAVE                                                    
*                                                                               
ADPNOK   SR    RC,RC                                                            
ADPNNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE PRINTER NAME RECORD FROM PNLISTD ENTRY AT R4      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         USING PNLISTD,R4                                                       
DELPNM   NTR1  BASE=*                                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         USING CTPNREC,R2                                                       
         XC    CTPNKEY,CTPNKEY                                                  
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,PNLUIDN                                                 
         MVC   CTPNKNAM,PNLNAME                                                 
*                                                                               
         MVC   IOKEY(L'CTPNKEY),CTPNKEY                                         
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'               IO ERROR                                     
         BE    DEPN010             OVERWRITE IF EXISTS                          
         TM    IOERR,IOEDEL                                                     
         BO    DEPN020                                                          
         TM    IOERR,IOERNF                                                     
         BO    DEPN020                                                          
         DC    H'00'                                                            
DEPN010  EQU   *                                                                
         GOTO1 ASETACT,CTPNREC                                                  
         OI    CTPNSTAT,X'80'                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DEPN020  EQU   *                                                                
         MVC   IOKEY,KEYSAVE                                                    
*                                                                               
DEPNOK   SR    RC,RC                                                            
DEPNNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE PRINTER NAME RECORD FROM PNLISTD ENTRY AT R4      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         USING PNLISTD,R4                                                       
CHGPNM   NTR1  BASE=*                                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         USING CTPNREC,R2                                                       
         XC    CTPNKEY,CTPNKEY                                                  
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,PNLOLDID                                                
         MVC   CTPNKNAM,PNLOLDNM                                                
*                                                                               
         MVC   IOKEY(L'CTPNKEY),CTPNKEY                                         
         LA    R1,IORDUPD+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'               IO ERROR                                     
         BE    CHPN010             OVERWRITE IF EXISTS                          
         TM    IOERR,IOEDEL                                                     
         BO    CHPN020                                                          
         TM    IOERR,IOERNF                                                     
         BO    CHPN020                                                          
         DC    H'00'                                                            
CHPN010  EQU   *                                                                
         GOTO1 ASETACT,CTPNREC                                                  
         OI    CTPNSTAT,X'80'                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHPN020  L     R2,AIOAREA2                                                      
         XC    CTPNREC(256),CTPNREC                                             
         MVI   CTPNKTYP,CTPNKTYQ                                                
         MVC   CTPNKUIN,PNLUIDN                                                 
         MVC   CTPNKNAM,PNLNAME                                                 
         LA    R0,CTPNDATA+1-CTPNREC                                            
         STCM  R0,3,CTPNLEN                                                     
*                                                                               
         LA    R3,APELEM                                                        
         USING CTPNLD,R3                                                        
         XC    CTPNLEL(CTPNLLNQ),CTPNLEL                                        
         MVI   CTPNLEL,CTPNLELQ                                                 
         MVI   CTPNLLEN,CTPNLLNQ                                                
         MVC   CTPNLID,PLUID                                                    
         DROP  R3                                                               
         GOTO1 AADDELS,CTPNREC                                                  
*                                                                               
         GOTO1 ASETACT,CTPNREC                                                  
         MVC   IOKEY(L'CTPNKEY),CTPNKEY                                         
         LA    R1,IORDUPD+IOCONFIL+IO3                                          
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'00'               IO ERROR                                     
         BE    CHPN030             OVERWRITE IF EXISTS                          
         TM    IOERR,IOEDEL                                                     
         BO    CHPN030                                                          
         TM    IOERR,IOERNF        ELSE ADD                                     
         BO    *+6                                                              
         DC    H'00'                                                            
         LA    R1,IOADD+IOCONFIL+IO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CHPN040                                                          
*                                                                               
CHPN030  LA    R1,IOWRITE+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
CHPN040  MVC   IOKEY,KEYSAVE                                                    
*                                                                               
CHPNOK   SR    RC,RC                                                            
CHPNNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
* GEGENMSG                                                                      
       ++INCLUDE GEGENMSG                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENBED                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENBFD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENC0D                                                       
         ORG   GENTABH                                                          
         ORG                                                                    
         ORG   SAVOVER                                                          
*                                  WORKING STORAGE SAVED IN TWA0                
SAVETID  DS    CL(L'CTTKTID)       LAST VALUE OF TERMINAL ID                    
PNLOFF   DS    F                   OFFSET TO PNLIST START OF DISPLAY            
LASTUIDA DS    CL10                LAST USERID ALPHA FOR PNAME LIST             
LASTUIDN DS    XL2                 LAST USERID NUMBER FOR PNAME LIST            
SAVCLRX  EQU   *-SAVOVER           CLEAR SAVED TWA UPTO HERE                    
PAGENO   DS    XL1                 CURRENT DISPLAYED PAGE NUMBER                
NUMPAGES DS    XL1                 TOTAL NUMBER OF PAGES                        
         SPACE 2                                                                
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL(L'LSTACT1)       ACTION FIELD                                 
LISTLINH DS    XL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTLUID DS    CL8                 PRITNER LUID                                 
         DS    CL1                                                              
LISTDESC DS    CL60                PRINTER DESCRIPTION                          
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
NLSTD    DSECT                     ** PNAME LIST/SELECT LINE LAYOUT **          
NLSTACTH DS    XL8                                                              
NLSTACT  DS    CL(L'LSTACT1)       ACTION FIELD                                 
NLSTLINH DS    XL8                                                              
NLSTLIN  DS    0CL(L'LSTLIN1)                                                   
NLSTUID  DS    CL10                PNAME USER ID                                
         DS    CL1                                                              
NLSTNAME DS    CL4                 PNAME NAME                                   
         DS    CL1                                                              
NLSTLUID DS    CL8                 PNAME PRINTER LUID                           
         DS    CL1                                                              
         ORG   NLSTLIN+L'NLSTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
LINELUID DS    CL8                 PRINTER LUID                                 
         DS    CL2                                                              
LINEUID  DS    CL10                USER ID NSME                                 
         DS    CL2                                                              
LINENAM  DS    CL4                 PRINTER NAME                                 
         DS    CL2                                                              
         ORG   REPP2                                                            
         DS    CL45                                                             
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    6D                                                               
VXSORT   DS    V                                                                
WORK     DS    CL255                                                            
*                                                                               
MAXNUM   DS    H                                                                
IDNUM    DS    XL2                                                              
IDALPH   DS    CL10                                                             
PNAME    DS    CL4                                                              
PLUID    DS    CL8                                                              
OLDPLUID DS    CL8                                                              
*                                                                               
SVTRMEL  DS    CL32                CTTRMD SAVE                                  
SVSTAT   DS    XL1                 CTTSTAT SAVE                                 
TERMNUM  DS    H                   TERMINAL #                                   
TERMINFO DS    XL1                                                              
*                                                                               
GETSEQF  DS    XL1                                                              
*                                                                               
KEYSAVE  DS    XL(L'IOKEY)                                                      
*                                                                               
DELKEY   DS    CL25                RECORD DELETE KEY AND SAVE                   
DELSAVE  DS    CL25                                                             
TRMDEFID DS    CL8                                                              
TRMDEF   DS    CL32                                                             
*                                                                               
*                                                                               
SELDATA  DS    0XL(SELDATAE-SELLUID)                                            
SELLUID  DS    CL8                 PRINTER LUID                                 
SELUIDN  DS    XL2                 USER ID NUMBER                               
SELNAME  DS    CL4                 PRINTER NAME                                 
SELNLST  DS    CL1                 PRINTER NAME LIST ORDER OPTION               
SELDATAE EQU   *                                                                
*                                                                               
VSCINKEY DS    V                   UNSCAN ROUTINE ADDRESS                       
FLDCNT   DS    X                   SUB-FIELD COUNT                              
*                                                                               
OVSYS    DS    XL256                                                            
*                                                                               
BLOCK1   DS    20CL32              SCANNER BLOCK                                
*                                  PRINTER NAME ELEMENT DATA SAVE LIST          
PNLCNT   DS    H                   # ENTRIES IN LIST                            
PNLDEL   DS    H                   # DELETED ENTRIES IN LIST                    
PNLPNTR  DS    A                   A(END OF LIST)                               
PNLDELF  DS    XL1                 DELETED FLAG                                 
PNLIST   DS    XL(PNLLEN*MAXPNL)   LIST OF PNAME ELM. DATA SEE PELISTD          
*                                                                               
LOCALX   EQU   *                   END OF LOCAL WORK STORE                      
*                                                                               
*                                  PRINTER NAME ELEMENT LIST DSECT              
PNLISTD  DSECT                                                                  
PNLUIDA  DS    CL10                USER ID ALPHA                                
PNLNAME  DS    CL4                 PRINTER NAME                                 
PNLKEYLN EQU   *-PNLISTD           SORT KEY LENGTH                              
PNLUIDN  DS    XL2                 USER ID #                                    
PNLACT   DS    CL1                 ACTION CODE                                  
PNLADELQ EQU   C'D'                ENTRY DELETED                                
PNLACHGQ EQU   C'C'                ENTRY CHANGED                                
PNLAADDQ EQU   C'A'                ENTRY ADDED                                  
PNLOLDID DS    XL2                 OLD CHANGED USER ID #                        
PNLOLDNM DS    CL4                 OLD CHANGED PRINTER NAME                     
PNLLEN   EQU   *-PNLISTD                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTGEN26   11/12/10'                                      
         END                                                                    
