*          DATA SET CTGEN09X   AT LEVEL 016 AS OF 05/01/02                      
*PHASE TA0B09A                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'CTGEN09 - PROFILE RECORD MAINTENANCE'                           
GEN09    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN9**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTPREC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         L     R1,=A(ELTAB)                                                     
         A     R1,APRELO                                                        
         ST    R1,AELTAB                                                        
*                                                                               
*        SAVE ADDRESSES OF COMMON ROUTINES                                      
*                                                                               
         L     R1,=A(VCOMMON)      R1 = A(VCOMMON)                              
         A     R1,APRELO                                                        
         SR    RF,RF               RF = 0                                       
         LA    RE,COMMADRS         RE = A(FIRST ROUTINE ADDRESS)                
         LA    R0,VCOUNT           R0 = NUMBER OF CONTROLLER ROUTINES           
*                                                                               
         LTR   R0,R0               IF NO CONTROLLER ROUTINES THEN DONE          
         BZ    GEN9020                                                          
*                                                                               
GEN9010  ST    R1,0(RE)            SAVE A(VCOMMON) IN LAST 3 BYTES              
         STC   RF,0(RE)            SAVE ROUTINE NUMBER IN FIRST BYTE            
*                                                                               
         LA    RF,1(RF)            BUMP ROUTINE NUMBER                          
         LA    RE,4(RE)            BUMP TO NEXT ROUTINE ADDRESS                 
         BCT   R0,GEN9010          LOOP BACK                                    
*                                                                               
GEN9020  EQU   *                                                                
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              APMVALK                                      
         B     VALREC              APMVALR                                      
         B     DISKEY              APMDISK                                      
         B     DISREC              APMDISR                                      
         B     DELREC              APMDELR                                      
         B     RESREC              APMRESR                                      
         B     VALSEL              APMVALP                                      
         B     GETSEL              APMGETS                                      
         B     DISSEL              APMDISS                                      
         B     EXIT                APMVALS                                      
         B     EXIT                APMFLST                                      
         B     PROSEL              APMPROC                                      
         B     EXIT                APMFSCR                                      
         B     LSTSCR              APMLSCR                                      
         B     VALREQ              APMVALQ                                      
         B     PRTREP              APMREPP                                      
         B     SETTWA              APMSETT                                      
         B     PUTKEY              APMPUTK                                      
         B     VALREC              APMNEWK                                      
         B     EXIT                APMFRP                                       
         B     EXIT                APMDISS2                                     
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF PROFILE RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,CTPKTYPQ                                                 
*                                  VALIDATE PROFILE SYSTEM                      
         MVI   FVMINL,1                                                         
         GOTO1 VALPSYS,APPARM,PROSYSTH,CTPKSYS                                  
         BNE   VALKEYX                                                          
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PROPRGMH      VALIDATE PROGRAM                             
         BNE   VALKEYX                                                          
         CLI   FVILEN,2            AND LENGTH OF 2                              
         BNE   EFTS                                                             
         MVC   CTPKPROG,FVIFLD     MOVE PROGRAM TO KEY                          
*                                                                               
         MVI   IFLAG,0                                                          
         XC    IDNUM,IDNUM                                                      
*&&UK*&& NI    PROSDSCH+FHATD,X'FF'-FHATPR                                      
         OI    PROFRMCH+FHATD,FHATPR                                            
         MVI   KLEVEL,1                                                         
         MVI   FVMINL,1                                                         
*&&US*&& MVI   ALLFLAG,C'N'                                                     
*&&UK*&& MVI   ALLFLAG,C'Y'                                                     
         GOTO1 VALPOID,APPARM,PROORGNH,IDNUM                                    
         BNE   VALKEYX                                                          
         OC    IDNUM,IDNUM                                                      
         BNZ   *+14                                                             
         XC    PROFRMC,PROFRMC                                                  
         B     VKEY010                                                          
*&&UK*&& OI    PROSDSCH+FHATD,FHATPR                                            
         NI    PROFRMCH+FHATD,X'FF'-FHATPR                                      
         MVI   KLEVEL,2                                                         
         MVC   KEYSAVE,IOKEY       SAVE KEY                                     
*                                                                               
VKEY010  XC    PROTYPE,PROTYPE     SET DEFAULT PROFILE TYPE                     
         MVI   FVMINL,1                                                         
         GOTO1 VALPTYP,APPARM,PROPROTH,PROTYPE                                  
         BNE   VALKEYX                                                          
*                                                                               
VKEY100  LA    R1,PROSYSTH                                                      
         ST    R1,FVADDR                                                        
         XC    FVINDX,FVINDX                                                    
         CLI   APACTN,ACTCHA       CANT CHANGE KEY ON ACTION CHANGE             
         BNE   VKEY110                                                          
         MVI   APACTN,ACTDIS       SET APACT TO DISP IF KEY CHANGED             
         CLC   CTPKEY,SAVKEY                                                    
         BNE   VKEY110                                                          
         CLC   PROTYPE,LASTPRO     PROFILE MUST BE SAME ALSO                    
         BNE   VKEY110                                                          
         CLC   IDNUM,LIDNUM                                                     
         BNE   VKEY110                                                          
         MVI   APACTN,ACTCHA       RESET APACTN IF KEYS OK                      
VKEY110  CLI   APACTN,ACTADD                                                    
         BNE   *+12                                                             
         CLI   KLEVEL,1            CANT ADD SYS/PRG/ID RECORD                   
         BNE   EFNV                                                             
         CLI   APACTN,ACTCPY                                                    
         BNE   VKEY200                                                          
         CLI   KLEVEL,1            CANT COPY SYS/PRG/ID RECORD                  
         BNE   EFNV                                                             
         EJECT                                                                  
VKEY200  MVC   APRECKEY(L'CTPKEY),CTPKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BH    *+12                NRF - CHECK IF DELETED                       
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VKEY210                                                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    VKEY202                                                          
         CLI   APACTN,ACTCPY                                                    
         BE    VALKEYX                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VKEY210                                                          
VKEY202  MVI   APINDS,APIOKADD                                                  
         CLI   APACTN,ACTCPY                                                    
         BNE   VKEY300                                                          
         TM    ACLFMIND,ACLFMIFK   FIRST VALKEY                                 
         BNZ   VALKEYX             ???                                          
         MVC   IOKEY,SAVKEY                                                     
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO1                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VKEY220                                                          
*                                                                               
VKEY210  CLI   APACTN,ACTCPY                                                    
         BNE   VKEY220                                                          
         TM    ACLFMIND,ACLFMIFK   FIRST VALKEY                                 
         BNZ   VKEY220                                                          
         MVC   IOKEY,SAVKEY                                                     
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO1                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
VKEY220  MVC   SAVKEY,APRECKEY                                                  
         CLI   KLEVEL,1                                                         
         BE    VKEY300                                                          
*                                                                               
VKEY230  GOTO1 AIO,IORDD+IOCONFIL+IO3                                           
         BL    VALKEYX             I/O ERROR EXIT                               
         BE    VKEY232                                                          
         TM    IOERR,IOEDEL        ERROR IF RECORD IS DELETED                   
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    R1,PROORGNH                                                      
         ST    R1,FVADDR                                                        
         B     EIIF                                                             
*                                                                               
VKEY232  MVC   CTPKORIG,IDNUM      MOVE ID TO KEY                               
         MVC   APRECKEY(L'CTPKEY),CTPKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BH    VKEY240             NRF - CHECK IF DELETED                       
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VKEY300                                                          
VKEY240  TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    VKEY250                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VKEY300                                                          
VKEY250  CLI   APACTN,ACTCHA                                                    
         BNE   VKEY260                                                          
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         MVI   APACTN,ACTADD                                                    
         MVC   SAVRECK,APRECKEY    DODGY FIX FOR A DODGY BUG                    
         MVI   KLEVEL,3                                                         
         B     VKEY270                                                          
VKEY260  MVI   KLEVEL,1                                                         
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         MVC   APRECKEY(L'CTPKEY),SAVKEY                                        
VKEY270  MVC   IOKEY,SAVKEY                                                     
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO1                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VKEY300                                                          
*                                                                               
VKEY300  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE PROFILE RECORDS                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1         INITIALISE RECORD                            
         MVC   CTPKEY,APRECKEY                                                  
*                                  CHECK FOR ADD FUNCTION                       
VRADD    CLI   APACTN,ACTADD                                                    
         BNE   VRCPY                                                            
         XC    CTPREC(256),CTPREC  INITIALISE RECORD FOR ADD                    
         MVC   CTPKEY,APRECKEY                                                  
         LA    R0,CTPDATA+1-CTPREC                                              
         STCM  R0,3,CTPLEN                                                      
         B     DATAVAL                                                          
*                                  CHECK FOR COPY FUNCTION                      
VRCPY    CLI   APACTN,ACTCPY                                                    
         BNE   VRCHA                                                            
         NI    TWAMODE,X'FF'-TWAMDFR                                            
         LA    R3,CTPDATA          UPDATE RECORD FOR COPY                       
VRCPY10  CLI   0(R3),0                                                          
         BE    VRCPYX              EXIT END RECORD                              
         CLI   0(R3),X'01'                                                      
         BE    VRCPY30             DELETE ACTIVITY                              
VRCPY20  ZIC   RF,1(R3)                                                         
         AR    R3,RF               GET NEXT ELEMENT                             
         B     VRCPY10                                                          
VRCPY30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CTPREC),0,0                    
         CLI   APPARM+12,0                                                      
         BE    VRCPY10                                                          
         DC    H'00'                                                            
VRCPYX   B     VRUPD                                                            
*                                  CHANGE FUNCTION - SAVE ORIG STATUS           
VRCHA    LA    R3,CTPDATA          AND STRIP DOWN RECORD                        
VRCHA10  CLI   0(R3),0                                                          
         BE    VRCHA30                                                          
         CLI   0(R3),X'01'         ACTIVITY                                     
         BE    VRCHA20             DELETE ELEMENT                               
         CLI   0(R3),CTDSCELQ                                                   
         BE    VRCHA20                                                          
*                                  GET NEXT ELEMENT                             
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRCHA10                                                          
*                                                                               
VRCHA20  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CTPREC),0,0                    
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRCHA10                                                          
*                                                                               
VRCHA30  L     R9,AELTAB                                                        
*                                  DELETE ALL COMMON ELEMENTS FROM              
VRCHA40  CLI   0(R9),0             RECORD                                       
         BE    VRCHAX                                                           
         MVC   APBYTE,0(R9)                                                     
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),(APBYTE,(R2)),(4,LASTPRO)            
         LA    R9,4(R9)                                                         
         B     VRCHA40                                                          
*                                                                               
VRCHAX   B     DATAVAL                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD ELEMENT DATA                                     *         
***********************************************************************         
         SPACE 1                                                                
DATAVAL  EQU   *                                                                
         GOTO1 AFVAL,PROPRODH      VALIDATE PROFILE DESCRIPTION                 
         BNE   DATAV7                                                           
         SR    R4,R4                                                            
         IC    R4,FVILEN                                                        
         XC    APELEM,APELEM       BUILD A DESCRIPTION ELEMENT                  
         MVI   APELEM,CTDSCELQ                                                  
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   APELEM+2(0),FVIFLD  MOVE DESC TO ELEMENT                         
         LA    R4,3(R4)                                                         
         STC   R4,APELEM+1                                                      
         GOTO1 AADDELN,CTPREC      AND ADD TO RECORD                            
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
DATAV7   DS    0H                  BUILD ID KEY                                 
         L     R4,AIOAREA2                                                      
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         GOTO1 AFVAL,PRODSTIH         VALIDATE DEST ID                          
         BNE   DATAV7C                                                          
         CLI   FVILEN,3                                                         
         BL    EIIF                                                             
         MVC   CTIKID,FVIFLD       MOVE ID TO KEY                               
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    EIIF                                                             
         MVC   IOKEY,APRECKEY                                                   
         LA    R4,CTIDATA          LOOK FOR ID# ELEMENT (X'02')                 
         SR    R9,R9                                                            
DATAV7A  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R4),X'02'                                                      
         BE    *+14                                                             
         IC    R9,1(R4)                                                         
         AR    R4,R9                                                            
         B     DATAV7A                                                          
         MVC   APDUB(2),2(R4)      SAVE ID NUMBER                               
         XC    APELEM,APELEM                                                    
*                                    BUILD DEST CODE ELEMENT                    
         LA    R3,APELEM                                                        
         USING CTDCOD,R3                                                        
         MVI   CTDCOEL,CTDCOELQ                                                 
         MVI   CTDCOLEN,X'12'                                                   
         MVC   CTDCOTYP(4),LASTPRO                                              
         MVC   CTDCODE,FVIFLD                                                   
         MVC   CTDCNUM,APDUB                                                    
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3,R4                                                            
*                                                                               
DATAV7C  DS    0H                  VALIDATE ATTENTION TYPE                      
         GOTO1 AFVAL,PROATYPH                                                   
         BNE   DATAV8                                                           
         XC    APELEM,APELEM                                                    
*                                    BUILD ATTENTION TYPE ELEMENT               
         LA    R3,APELEM                                                        
         USING CTACOD,R3                                                        
         MVI   CTACOEL,CTACOELQ                                                 
         MVI   CTACOLEN,X'09'                                                   
         MVC   CTACOTYP(4),LASTPRO                                              
         MVC   CTACODE,FVIFLD                                                   
         GOTO1 AADDELN,CTPREC      AND ADD TO RECORD                            
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
DATAV8   DS    0H                  VALIDATE OUTPUT TYPE                         
         L     R4,AIOAREA2                                                      
         USING CTOREC,R4                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         GOTO1 AFVAL,PROOUTPH                                                   
         BNE   DATAV9                                                           
         CLI   FVILEN,1                                                         
         BL    EIIF                                                             
         MVC   CTOKID,FVIFLD       MOVE ID TO KEY                               
         MVC   IOKEY(L'CTOKEY),CTOKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    EIIF                                                             
         MVC   IOKEY,APRECKEY                                                   
         XC    APELEM,APELEM                                                    
*                                    BUILD OUTPUT TYPE ELEMENT                  
         LA    R3,APELEM                                                        
         USING CTOCOD,R3                                                        
         MVI   CTOCOEL,CTOCOELQ                                                 
         MVI   CTOCOLEN,X'10'                                                   
         MVC   CTOCOTYP(4),LASTPRO                                              
         MVC   CTOCODE,FVIFLD                                                   
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3,R4                                                            
*                                                                               
DATAV9   DS    0H                  VALIDATE OUTPUT MODE                         
         L     R4,AIOAREA2                                                      
         USING CTOREC,R4                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         GOTO1 AFVAL,PROMODPH                                                   
         BNE   DATAV9A                                                          
         CLI   FVILEN,3                                                         
         BL    EIIF                                                             
         MVC   CTOKID,FVIFLD       MOVE ID TO KEY                               
         MVC   IOKEY(L'CTOKEY),CTOKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    EIIF                                                             
         MVC   IOKEY,APRECKEY                                                   
         XC    APELEM,APELEM                                                    
*                                    BUILD OUTPUT MODE ELEMENT                  
         LA    R3,APELEM                                                        
         USING CTOCOD,R3                                                        
         MVI   CTOCOEL,X'43'                                                    
         MVI   CTOCOLEN,X'10'                                                   
         MVC   CTOCOTYP(4),LASTPRO                                              
         MVC   CTOCODE,FVIFLD                                                   
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3,R4                                                            
*        SPACE 2                                                                
DATAV9A  EQU   *                                                                
         GOTO1 AFVAL,PROPQPWH      VALIDATE PRTQUE PASSWORD                     
         BNE   DATAV9B                                                          
         OC    LIDNUM,LIDNUM       ONLY VALID IF USER SPECIFIC PROFILE          
         BZ    EIIF                                                             
         XC    APELEM,APELEM                                                    
*                                    BUILD ELEMENT                              
         LA    R3,APELEM                                                        
         USING CTPQPD,R3                                                        
         MVI   CTPQPEL,CTPQPELQ                                                 
         MVI   CTPQPLEN,X'0C'                                                   
         MVC   CTPQPTYP(4),LASTPRO                                              
         MVC   CTPQPWD,FVIFLD                                                   
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
DATAV9B  XC    DUB,DUB                                                          
         GOTO1 AFVAL,PROPQRCH      VALIDATE PRTQUE RETAIN CLASS                 
         BNE   DATAV10                                                          
         MVC   DUB(1),FVIFLD                                                    
         GOTO1 AFVAL,PROPQRSH      VALIDATE SOON RETAIN CLASS                   
         BNE   *+10                                                             
         MVC   DUB+1(1),FVIFLD                                                  
         XC    APELEM,APELEM                                                    
*                                    BUILD ELEMENT                              
         LA    R3,APELEM                                                        
         USING CTPQCD,R3                                                        
         MVI   CTPQCEL,CTPQCELQ                                                 
         MVI   CTPQCLEN,X'08'                                                   
         MVC   CTPQCTYP(4),LASTPRO                                              
         MVC   CTPQCLAS,DUB                                                     
         MVC   CTPQCLA2,DUB+1                                                   
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
         EJECT                                                                  
DATAV10  XC    APELEM,APELEM                                                    
*                                    BUILD PRIORITY CODE ELEMENT                
         LA    R3,APELEM                                                        
         USING CTPRID,R3                                                        
         MVI   CTPRIEL,CTPRIELQ                                                 
         MVI   CTPRILEN,X'09'                                                   
         MVC   CTPRITYP(4),LASTPRO                                              
         MVI   CTPRISC,C' '                                                     
         MVI   CTPRIPT,C' '                                                     
         MVI   CTPRIPO,C' '                                                     
*                                                                               
DTV10A   GOTO1 AFVAL,PROPRSCH      VALIDATE SORT CODE                           
         BE    DTV10A1                                                          
*&&US*&& CLI   IFLAG,0             REQUIRED IF MASTER PROFILE IN US             
*&&US*&& BE    EXIT                                                             
         B     DTV10B              OPTIONAL INPUT FIELD IN UK                   
DTV10A1  CLC   FVIFLD(L'PRISCSAV),PRISCSAV                                      
         BE    DTV10B                                                           
         CLI   FVIFLD,C'A'                                                      
         BL    EIIF                                                             
         CLI   FVIFLD,C'9'                                                      
         BH    EIIF                                                             
         MVC   CTPRISC,FVIFLD      SET SORT CODE                                
*                                                                               
DTV10B   XC    PROPXPT,PROPXPT     VALIDATE PROGRAM TYPE                        
         OI    PROPXPTH+6,X'80'                                                 
         GOTO1 AFVAL,PROPRPTH                                                   
         BE    DTV10B1                                                          
*&&US*&& CLI   IFLAG,0             REQUIRED IF MASTER PROFILE IN US             
*&&US*&& BE    EXIT                                                             
         B     DTV10C              OPTIONAL FIELD IN UK                         
DTV10B1  CLC   FVIFLD(L'PRIPTSAV),PRIPTSAV                                      
         BE    DTV10B4                                                          
         CLI   FVIFLD,C'A'                                                      
         BL    EIIF                                                             
         CLI   FVIFLD,C'9'                                                      
         BH    EIIF                                                             
         CLI   FVIFLD,C'Z'         Z VALID ONLY IF USER ID INPUT                
         BNE   DTV10B2                                                          
         OC    IDNUM,IDNUM                                                      
         BZ    EIIF                                                             
DTV10B2  MVC   CTPRIPT,FVIFLD                                                   
DTV10B4  BAS   RE,DPRTCODE                                                      
*                                                                               
DTV10C   XC    PROPXPO,PROPXPO     VALIDATE PROGRAM OUTPUT                      
         OI    PROPXPOH+6,X'80'                                                 
         GOTO1 AFVAL,PROPRPOH                                                   
         BNE   DTV10D                                                           
         CLC   FVIFLD(L'PRIPOSAV),PRIPOSAV                                      
         BE    DTV10C4                                                          
         CLI   FVIFLD,C'A'                                                      
         BL    EIIF                                                             
         CLI   FVIFLD,C'9'                                                      
         BH    EIIF                                                             
         CLI   FVIFLD,C'Z'         Z VALID ONLY IF USER ID INPUT                
         BNE   DTV10C2                                                          
         OC    IDNUM,IDNUM                                                      
         BZ    EIIF                                                             
DTV10C2  MVC   CTPRIPO,FVIFLD                                                   
DTV10C4  BAS   RE,DPROCODE                                                      
*                                                                               
DTV10D   CLC   CTPRISC(3),=C'   '  TEST IF ALL THREE FIELDS BLANK               
         BE    DATAV11             YES THEN OMIT ELEMENT                        
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
         EJECT                                                                  
DATAV11  GOTO1 AFVAL,PROSQLTH      VALIDATE SQL TRANSFORM FORMULA               
         BNE   DATAV12                                                          
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTSQLD,R3                                                        
         MVI   CTSQLEL,CTSQLELQ                                                 
         MVI   CTSQLLEN,X'0C'                                                   
         MVC   CTSQLTYP(4),LASTPRO                                              
         MVC   CTSQLCOD,FVIFLD                                                  
         GOTO1 AADDELN,CTPREC      AND ADD TO RECORD                            
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
DATAV12  GOTO1 AFVAL,PRORCLAH      VALIDATE READER CLASS                        
         BNE   DATAV13                                                          
         LA    R4,TABCLA                                                        
DATAV12A CLI   0(R4),X'FF'                                                      
         BE    EIIF                                                             
         CLC   0(1,R4),FVIFLD                                                   
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     DATAV12A                                                         
         XC    APELEM,APELEM                                                    
*                                    BUILD READER CLASS ELEMENT                 
         LA    R3,APELEM                                                        
         USING CTRCLD,R3                                                        
         MVI   CTRCLEL,CTRCLELQ                                                 
         MVI   CTRCLEN,X'07'                                                    
         MVC   CTRCLTYP(4),LASTPRO                                              
         MVC   CTRCLASS,FVIFLD                                                  
         GOTO1 AADDELN,CTPREC      AND ADD TO RECORD                            
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
DATAV13  GOTO1 AFVAL,PROSRTFH      VALIDATE SORT FORMULA                        
         BNE   DATAV17                                                          
         XC    APELEM,APELEM                                                    
*                                    BUILD SORT FORMULA ELEMENT                 
         LA    R3,APELEM                                                        
         USING CTSRTD,R3                                                        
         MVI   CTSRTEL,CTSRTELQ                                                 
         MVI   CTSRTLEN,X'06'                                                   
         MVC   CTSRTTYP(4),LASTPRO                                              
         GOTO1 VSCANNER,APPARM,(0,PROSRTFH),(30,BLOCK1)                         
         SR    R8,R8                                                            
         SR    R9,R9                                                            
         IC    R9,APPARM+4                                                      
         LTR   R9,R9                                                            
         BZ    EIIF                                                             
         D     R8,=F'3'            NUMBER OF LINES INPUT MUST BE A              
         LTR   R8,R8               MULTIPLE OF 3                                
         BNZ   EIIF                                                             
*                                  R9=# OF MULTIPLES OF 3                       
         LA    R4,BLOCK1           R4=A(INPUT)                                  
         LA    R8,CTSRTFRM         R8=A(OUTPUT)                                 
         MVI   FVINDX,1                                                         
*                                                                               
DATAV14  CLI   1(R4),0             VALIDATE START COLUMN                        
         BNE   EIIF                                                             
         TM    2(R4),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R4),4(R4)                                                    
         BZ    EIIF                                                             
         CLC   4(4,R4),=F'160'     WAS 80 (ONE CARD REQUESTS)                   
         BH    EIIF                                                             
         MVC   APDUB(1),7(R4)                                                   
         LA    R4,32(R4)           NEXT LINE                                    
         BAS   RE,FNDUP            INCREMENT FVINDX                             
         CLI   1(R4),0             VALIDATE LENGTH                              
         BNE   EIIF                                                             
         TM    2(R4),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R4),4(R4)                                                    
         BZ    EIIF                                                             
         CLC   4(4,R4),=F'80'                                                   
         BH    EIIF                                                             
         MVC   APDUB+1(1),7(R4)                                                 
         SR    RE,RE                                                            
         IC    RE,APDUB                                                         
         SR    R1,R1                                                            
         IC    R1,APDUB+1                                                       
         AR    R1,RE                                                            
         C     R1,=F'161'          MAX FOR 2 CARD REQUESTS                      
         BH    EIIF                                                             
         LA    R4,32(R4)           NEXT LINE                                    
         BAS   RE,FNDUP            INCREMENT FVINDX                             
         CLI   1(R4),0             VALIDATE ASCENDING/DESCENDING                
         BNE   EIIF                                                             
         CLI   0(R4),1                                                          
         BH    EFTL                                                             
         CLI   12(R4),C'A'                                                      
         BE    DATAV16                                                          
         CLI   12(R4),C'D'                                                      
         BNE   EIIF                                                             
         OI    APDUB,X'80'                                                      
DATAV16  MVC   0(2,R8),APDUB       MOVE PARAMETERS TO ELEMENT                   
         LA    R8,2(R8)                                                         
         LA    R4,32(R4)           NEXT LINE                                    
         BAS   RE,FNDUP            INCREMENT FVINDX                             
         BCT   R9,DATAV14                                                       
         SR    R8,R3               R8=L'ELEMENT                                 
         STC   R8,1(R3)                                                         
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
DATAV17  GOTO1 AFVAL,PROSJCLH      SPECIAL JCL BOOK                             
         BNE   DATAV17A                                                         
         XC    APELEM,APELEM                                                    
*                                  BUILD SPECIAL JCL ELEMENT                    
         LA    R3,APELEM                                                        
         USING CTJCLD,R3                                                        
         MVI   CTJCLEL,CTJCLELQ                                                 
         MVI   CTJCLLEN,X'10'                                                   
         MVC   CTJCLTYP(4),LASTPRO                                              
         MVC   CTJCLEX,FVIFLD                                                   
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
DATAV17A GOTO1 AFVAL,PROSPHSH      SPECIAL PHASES                               
         BNE   DATAV18                                                          
         XC    APELEM,APELEM                                                    
*                                  BUILD SPECIAL PHASE ELEMENT                  
         LA    R3,APELEM                                                        
         USING CTPHSD,R3                                                        
         MVI   CTPHSEL,CTPHSELQ                                                 
         MVI   CTPHSLEN,X'0A'                                                   
         MVC   CTPHSTYP(4),LASTPRO                                              
         MVI   CTPHS01,C' '        SET DEFAULT VALUES (SPACES)                  
         MVC   CTPHS02(3),CTPHS01                                               
*                                                                               
         GOTO1 VSCANNER,APPARM,(0,PROSPHSH),(4,BLOCK1)                          
         CLI   APPARM+4,0                                                       
         BE    EIIF                                                             
         LA    R4,BLOCK1                                                        
         MVI   FVINDX,1                                                         
         SPACE 2                                                                
DATAV17B CLC   FVINDX,APPARM+4                                                  
         BH    DATAV17C                                                         
         CLI   0(R4),1             L'PART1                                      
         BNE   EIIF                                                             
         CLI   12(R4),C'1'         V'PART1 (PHASE S/B 1-4)                      
         BL    EIIF                                                             
         CLI   12(R4),C'4'         V'PART1                                      
         BH    EIIF                                                             
         CLI   1(R4),1             L'PART2                                      
         BNE   EIIF                                                             
         CLI   22(R4),C'A'         V'PART2 (TEST LEVEL S/B A,B OR C)            
         BL    EIIF                                                             
         CLI   22(R4),C'C'         V'PART2                                      
         BH    EIIF                                                             
         SR    R1,R1                                                            
         IC    R1,7(R4)                                                         
         LA    R1,CTPHS01-1(R1)    POINT TO PHASE TEST LEVEL                    
         CLI   0(R1),C' '                                                       
         BNE   EDIF                                                             
         MVC   0(1,R1),22(R4)      SET TEST LEVEL                               
         BAS   RE,FNDUP            BUMP FVINDX                                  
         LA    R4,32(R4)           AND BLOCK POINTER                            
         B     DATAV17B                                                         
         SPACE 2                                                                
DATAV17C MVI   FVINDX,0                                                         
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
DATAV18  GOTO1 AFVAL,PROPRCAH      PROCESSING INSTRUCTIONS                      
         BNE   DATAV20                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTPRCELQ                                                  
         MVI   APELEM+1,X'06'                                                   
         LA    R4,PROPRCAH                                                      
         LA    R9,PROPRCBH                                                      
         BAS   RE,BUILDINS                                                      
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
DATAV20  GOTO1 AFVAL,PROPACAH      BREAKDOWN INSTRUCTIONS                       
         BNE   DATAV22                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTPAKELQ                                                  
         MVI   APELEM+1,X'06'                                                   
         LA    R4,PROPACAH                                                      
         LA    R9,PROPACBH                                                      
         BAS   RE,BUILDINS                                                      
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
DATAV22  GOTO1 AFVAL,PROPINAH      SHIPPING UNIT AND ROUTE                      
         BNE   DATAV24                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTSHPELQ                                                  
         MVI   APELEM+1,X'06'                                                   
         LA    R4,PROPINAH                                                      
         LA    R9,PROPINBH                                                      
         BAS   RE,BUILDINS                                                      
         CLI   FTBFLAG,1           SET IF ERROR RETURNED FROM BLDSHP            
         BE    EFTB                                                             
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         EJECT                                                                  
*                                                                               
DATAV24  GOTO1 AFVAL,PROSDSCH      REPORT SHORT DESCRIPTION                     
         BNE   DATAV26                                                          
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTSDSD,R3                                                        
         MVI   CTSDSEL,CTSDSELQ                                                 
         MVI   CTSDSLEN,CTSDSLNQ                                                
         MVC   CTSDSTYP(4),LASTPRO                                              
         MVC   CTSDSTXT,FVIFLD                                                  
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
*                                                                               
DATAV26  GOTO1 AFVAL,PROFRMCH      REPORT FORM CODE                             
         BNE   VRUPD                                                            
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTFRMD,R3                                                        
         MVI   CTFRMEL,CTFRMELQ                                                 
         MVI   CTFRMLEN,CTFRMLNQ                                                
         MVC   CTFRMTYP(4),LASTPRO                                              
         MVC   CTFRMCOD,FVIFLD                                                  
         GOTO1 AADDELN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         DROP  R3                                                               
         B     VRUPD                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
VRUPD    GOTO1 ASETACN,CTPREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         MVC   IOKEY(L'CTPKEY),CTPKEY                                           
         CLI   KLEVEL,1                                                         
         BE    VRUPD100                                                         
         MVI   APWORK,0                                                         
         LA    R3,CTPDATA          DELETE DUP ELS FROM ID LEVEL RECORD          
*                                                                               
VRUPD010 CLI   0(R3),0                                                          
         BE    VRUPD050                                                         
         CLI   0(R3),X'01'                                                      
         BE    VRUPD040                                                         
         L     R4,AIOAREA3                                                      
         LA    R4,CTPDATA-CTPREC(R4)                                            
*                                                                               
VRUPD020 CLI   0(R4),0                                                          
         BE    VRUPD040                                                         
         CLC   1(1,R3),1(R4)                                                    
         BNE   VRUPD030                                                         
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R3),2(R4)                                                    
         BNE   *+12                                                             
         MVI   0(R3),X'FF'                                                      
         OI    APWORK,X'FF'                                                     
VRUPD030 SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VRUPD020                                                         
VRUPD040 SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VRUPD010                                                         
VRUPD050 CLI   APWORK,0                                                         
         BE    VRUPD060                                                         
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),(X'FF',CTPREC),0,0                   
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
VRUPD060 CLI   KLEVEL,3                                                         
         BNE   VRUPD100                                                         
         MVI   APACTN,ACTCHA                                                    
         CLC   CTPLEN,=H'34'                                                    
         BH    VRUPD110                                                         
         B     VALRECX             ???                                          
*                                                                               
VRUPD100 CLI   APACTN,ACTCHA                                                    
         BNE   VRUPD110                                                         
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VALRECX                                                          
         EJECT                                                                  
*                                                                               
VRUPD110 GOTO1 AIO,IOADD+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VALRECX                                                          
*                                  EXIT RECORD VALIDATION AND UPDATE OK         
VALRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC              REDISPLAY RECORD                             
*                                                                               
VALRECER B     EXIT                                                             
         EJECT                                                                  
*              UPDATE FVINDX                                                    
*                                                                               
FNDUP    SR    RF,RF                                                            
         IC    RF,FVINDX                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX                                                        
         BR    RE                                                               
         SPACE 2                                                                
*              BUILD A NARRATIVE ELEMENT                                        
*                                                                               
BUILDINS NTR1                                                                   
         MVI   FTBFLAG,0           FIELD TOO BIG INDICATOR                      
         LA    R3,APELEM                                                        
         MVC   2(4,R3),LASTPRO                                                  
         CLI   APELEM,X'4C'        TEST SHIPPING INSTRUCTIONS                   
         BNE   BUILDIN1                                                         
         CLI   8(R4),C'0'          TEST SPECIAL NUMERIC SHIPPING UNIT           
         BL    BUILDIN1                                                         
         CLI   8(R4),C'9'                                                       
         BH    BUILDIN1                                                         
         ZIC   R4,FVILEN                                                        
         GOTO1 ABLDSHP,APPARM,FVIFLD,APELEM+6,(R4)                              
         CLI   8(R1),0                                                          
         BNE   *+12                                                             
         MVI   FTBFLAG,1           FLAG FIELD TOO BIG                           
         B     EXIT                                                             
         ZIC   R4,8(R1)            R4 = LENGTH OF ELEMENT DATA                  
         B     BUILDIN2                                                         
*                                  OTHERWISE MOVE IN AS IS                      
BUILDIN1 MVC   6(60,R3),FVIFLD     FIRST HALF OF NARRATIVE                      
         SR    R4,R4                                                            
         IC    R4,FVILEN                                                        
*                                                                               
BUILDIN2 LR    R1,R9               R1=A(SECOND SCREEN LINE)                     
         SR    R8,R8                                                            
         GOTO1 AFVAL                                                            
         BNE   *+8                                                              
         IC    R8,FVILEN                                                        
         LTR   R8,R8                                                            
         BZ    *+14                                                             
         LA    R4,60                                                            
         MVC   66(60,R3),FVIFLD    DO NOT CONTATENATE NARRATIVE                 
         AR    R4,R8               R4=TOTAL L'NARRATIVE                         
         LA    R4,6(R4)                                                         
         STC   R4,1(R3)                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A PROFILE RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACN,CTPREC                                                   
         BNE   DELRECX             RECORD TOO BIG                               
         OI    CTPSTAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED PROFILE RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACN,CTPREC                                                   
         BNE   RESRECX             RECORD TOO BIG                               
         NI    CTPSTAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF PROFILE RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
DISKEY   LA    R2,APRECKEY                                                      
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         LA    RE,SYSLLEN(RE)      GO PAST SERVICE SYSTEM ENTRY                 
DKEY010  CLI   SYSLNUM,0                                                        
         BE    DKEY100                                                          
         CLC   CTPKSYS,SYSLRPLT                                                 
         BE    DKEY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     DKEY010                                                          
*                                                                               
DKEY020  XC    PROSYST,PROSYST     REDISPLAYSYSTEM                              
         MVC   PROSYST,SYSLNAME                                                 
         OI    PROSYSTH+6,X'80'                                                 
*                                                                               
DKEY100  MVC   PROPRGM,CTPKPROG                                                 
         OI    PROPRGMH+6,X'80'                                                 
*                                                                               
         XC    APWORK,APWORK                                                    
         GOTO1 DISPOID,APPARM,APWORK,CTPKORIG                                   
         MVC   PROORGN,APWORK                                                   
         OI    PROORGNH+6,X'80'                                                 
*                                                                               
         MVC   PROPROT,PROSAVE                                                  
         OI    PROPROTH+6,X'80'                                                 
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PROFILE RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
DISREC   L     R2,AIOAREA1                                                      
         CLI   KLEVEL,3                                                         
         BE    DISRECX                                                          
         CLI   KLEVEL,1                                                         
         BE    *+8                                                              
         L     R2,AIOAREA3                                                      
         TWAXC PROVALSH                                                         
         LA    R1,PRODSCLH         CLEAR PROT INDICS                            
         BAS   RE,CLEARP                                                        
         BAS   RE,CLPRIVAL                                                      
         BAS   RE,LDPRISAV                                                      
         XC    VLIST,VLIST                                                      
         MVI   DISPTYPE,C'D'       SET TYPE TO DEFAULT                          
         GOTO1 EXTRACT,DLIST       EXTRACT PROFILE TYPES                        
         BAS   RE,DISPRO                                                        
         CLI   KLEVEL,1                                                         
         BE    DISPEND                                                          
         BAS   RE,LDPRISAV                                                      
         BAS   RE,CLPRIVAL                                                      
         MVI   DISPTYPE,C'O'       SET TYPE TO OVERRIDE                         
         L     R2,AIOAREA1                                                      
         GOTO1 EXTRACT,OLIST       EXTRACT PROFILE TYPES                        
         BAS   RE,DISPRO                                                        
         B     DISPEND                                                          
*                                                                               
DISPEND  MVC   LASTPRO,PROTYPE     SAVE DISPLAYED PROFILE TYPE                  
         MVC   LIDNUM,IDNUM                                                     
         BAS   RE,DISPVALS         DISPLAY PROFILE TYPES                        
*                                                                               
DISRECX  GOTO1 ADISACT,CTPREC                                                   
         B     EXIT                                                             
*                                  CLEAR PRIORITY CODE VALUES                   
CLPRIVAL XC    PRISCVAL,PRISCVAL                                                
         XC    PRIPTVAL,PRIPTVAL                                                
         XC    PRIPOVAL,PRIPOVAL                                                
         BR    RE                                                               
*                                  LOAD PRIORITY CODE SAVE VALUES               
LDPRISAV MVC   PRISCSAV,PRISCVAL                                                
         MVC   PRIPTSAV,PRIPTVAL                                                
         MVC   PRIPOSAV,PRIPOVAL                                                
         BR    RE                                                               
         EJECT                                                                  
*              DISPLAY REQUESTED PROFILE INFO                                   
*                                                                               
DISPRO   NTR1                                                                   
         LA    R3,CTPDATA          R3=A(FIRST ELEMENT)                          
DISPR2   CLI   0(R3),0             END OF RECORD                                
         BE    DISPR12                                                          
         CLI   0(R3),X'01'         IGNORE ACTIVITY ELEMENT                      
         BE    DISPR4                                                           
         CLI   0(R3),CTDSCELQ      DESCRIPTION ELEMENT                          
         BE    DISPDIS                                                          
         CLI   0(R3),X'40'         PROFILES ELEMENTS X'40' THRU X'50'           
         BL    DISPR4                                                           
         CLI   0(R3),X'50'                                                      
         BH    DISPR4                                                           
         CLC   2(1,R3),PROTYPE     FILTER ON PROFILE TYPE/DATA                  
         BE    DISPR3                                                           
         CLI   0(R3),CTPRIELQ      UNLESS PRIORITY CODE ELEMENT                 
         BNE   DISPR4                                                           
         CLI   2(R3),C'P'          AND PERMANENT TYPE ELEMENT                   
         BNE   DISPR4                                                           
         B     DISPR7              THEN GO STRAIGHT TO PROCESSOR                
*                                                                               
DISPR3   OC    PROTYPE+1(3),PROTYPE+1                                           
         BZ    DISPR6                                                           
         CLC   3(3,R3),PROTYPE+1                                                
         BE    DISPR6                                                           
DISPR4   SR    R4,R4               BUMP TO NEXT ELEMENT                         
         IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     DISPR2                                                           
DISPR6   MVC   PROTYPE+1(3),3(R3)  FILL IN PROFILE DATA IF N/I                  
DISPR7   L     R4,AELTAB           R4=A(ELEMENT TABLE)                          
DISPR8   CLC   0(1,R4),0(R3)       MATCH ELEMENT TYPE WITH TABLE                
         BE    DISPR10                                                          
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF ELEMENT NOT KNOWN                     
         LA    R4,L'ELTAB(R4)                                                   
         B     DISPR8                                                           
DISPR10  L     RF,0(R4)            A(PROCESSOR)                                 
         LA    RF,0(RF)                                                         
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     DISPR4              GET NEXT ELEMENT                             
DISPR12  XIT1                                                                   
         EJECT                                                                  
DISPDIS  DS    0H                  PROFILE DESCRIPTION                          
         XC    PROPROD,PROPROD                                                  
         SR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PROPROD(0),2(R3)                                                 
         MVC   PRODSCL+L'PRODSCL-1(1),DISPTYPE                                  
         B     DISPR4                                                           
*                                                                               
DISPDES  DS    0H                  DESTINATION ID                               
         ST    RE,RETURN                                                        
         XC    PRODSTI,PRODSTI                                                  
         USING CTDCOD,R3                                                        
         XC    APWORK,APWORK                                                    
         GOTO1 DISPOID,APPARM,APWORK,CTDCNUM                                    
         MVC   PRODSTI,APWORK      MOVE ALPHA ID TO SCREEN                      
         CLI   PRODSTI,C'#'          (OR ID# IF NOT FOUND/DELETED)              
         BE    *+14                                                             
         MVC   PRODESL+L'PRODESL-1(1),DISPTYPE                                  
         B     *+8                                                              
         MVI   PRODESL+L'PRODESL-1,C'*'                                         
         L     RE,RETURN                                                        
         BR    RE                                                               
         EJECT                                                                  
DISPATN  DS    0H                  ATTENTION TYPE                               
         XC    PROATYP,PROATYP                                                  
         USING CTACOD,R3                                                        
         MVC   PROATYP,CTACODE                                                  
         MVC   PROATTL+L'PROATTL-1(1),DISPTYPE                                  
         BR    RE                                                               
*                                                                               
DISPOUT  DS    0H                  OUTPUT ID                                    
         XC    PROOUTP,PROOUTP                                                  
         USING CTOCOD,R3                                                        
         MVC   PROOUTP,CTOCODE                                                  
         MVC   PROOUTL+L'PROOUTL-1(1),DISPTYPE                                  
         BR    RE                                                               
*                                                                               
DISPMOD  DS    0H                  OUTPUT MODE                                  
         XC    PROMODP,PROMODP                                                  
         USING CTOCOD,R3                                                        
         MVC   PROMODP,CTOCODE                                                  
         MVC   PROMODL+L'PROMODL-1(1),DISPTYPE                                  
         BR    RE                                                               
*                                                                               
DISPPQP  DS    0H                  PRTQUE PASSWORD                              
         XC    PROPQPW,PROPQPW                                                  
         USING CTPQPD,R3                                                        
         MVC   PROPQPW,CTPQPWD                                                  
         MVC   PROPQPL+L'PROPQPL-1(1),DISPTYPE                                  
         BR    RE                                                               
*                                                                               
DISPPQC  DS    0H                  PRTQUE RETAIN CLASS                          
         XC    PROPQRC,PROPQRC                                                  
         USING CTPQCD,R3                                                        
         MVC   PROPQRC,CTPQCLAS                                                 
         MVC   PROPQRS,CTPQCLA2                                                 
         MVC   PROPQRL+L'PROPQRL-1(1),DISPTYPE                                  
         BR    RE                                                               
         EJECT                                                                  
DISPPR   DS    0H                  PRIORITY CODE/PRG TYPE/PRG OUTPUT            
         USING CTPRID,R3                                                        
         ST    RE,RETURN                                                        
DPPR010  CLI   CTPRILEN,8          TEST OLD STYLE ELEMENT                       
         BNE   DPPR020             NO                                           
         CLC   2(1,R3),PROTYPE                                                  
         BNE   DPPRX                                                            
         MVC   PROPRSC,CTPRISC     YES ONLY 1ST CHR VALID                       
         MVC   PROPRTL+L'PROPRTL-1(1),DISPTYPE                                  
         B     DPPRX                                                            
*                                                                               
DPPR020  EQU   *                                                                
         CLI   CTPRISC,C' '        SORT CODE                                    
         BE    DPPR030                                                          
         CLC   2(1,R3),PROTYPE                                                  
         BE    DPPR022                                                          
         MVC   PRISCSAV,CTPRISC                                                 
         CLI   PRISCVAL,0                                                       
         BNE   DPPR030                                                          
         MVC   APBYTE,DISPTYPE                                                  
         NI    APBYTE,X'FF'-X'40'                                               
         MVC   PROPRTL+L'PROPRTL-1(1),APBYTE                                    
         B     *+10                                                             
DPPR022  MVC   PROPRTL+L'PROPRTL-1(1),DISPTYPE                                  
         MVC   PRISCVAL,CTPRISC                                                 
         MVC   PROPRSC,CTPRISC                                                  
*                                                                               
DPPR030  CLI   CTPRIPT,C' '        PROGRAM TYPE                                 
         BE    DPPR040                                                          
         CLC   2(1,R3),PROTYPE                                                  
         BE    DPPR032                                                          
         MVC   PRIPTSAV,CTPRIPT                                                 
         CLI   PRIPTVAL,0                                                       
         BNE   DPPR040                                                          
         MVC   APBYTE,DISPTYPE                                                  
         NI    APBYTE,X'FF'-X'40'                                               
         MVC   PROPPTL+L'PROPPTL-1(1),APBYTE                                    
         B     *+10                                                             
DPPR032  MVC   PROPPTL+L'PROPPTL-1(1),DISPTYPE                                  
         MVC   PRIPTVAL,CTPRIPT                                                 
         MVC   PROPRPT,CTPRIPT                                                  
         BAS   RE,DPRTCODE                                                      
*                                                                               
DPPR040  CLI   CTPRIPO,C' '        PROGRAM PROFILE                              
         BE    DPPRX                                                            
         CLC   2(1,R3),PROTYPE                                                  
         BE    DPPR042                                                          
         MVC   PRIPOSAV,CTPRIPO                                                 
         CLI   PRIPOVAL,0                                                       
         BNE   DPPRX                                                            
         MVC   APBYTE,DISPTYPE                                                  
         NI    APBYTE,X'FF'-X'40'                                               
         MVC   PROPPOL+L'PROPPOL-1(1),APBYTE                                    
         B     *+10                                                             
DPPR042  MVC   PROPPOL+L'PROPPOL-1(1),DISPTYPE                                  
         MVC   PRIPOVAL,CTPRIPO                                                 
         MVC   PROPRPO,CTPRIPO                                                  
         BAS   RE,DPROCODE                                                      
*                                                                               
DPPRX    L     RE,RETURN                                                        
         BR    RE                                                               
*                                                                               
DISPSQL  DS    0H                  SQL TRANSFORM FORMULA                        
         XC    PROSQLT,PROSQLT                                                  
         USING CTSQLD,R3                                                        
         MVC   PROSQLT,CTSQLCOD                                                 
         MVC   PROSQTL+L'PROSQTL-1(1),DISPTYPE                                  
         BR    RE                                                               
         EJECT                                                                  
DISPRCL  DS    0H                  READER CLASS                                 
         XC    PRORCLA,PRORCLA                                                  
         USING CTRCLD,R3                                                        
         MVC   PRORCLA,CTRCLASS                                                 
         MVC   PRORDCL+L'PRORDCL-1(1),DISPTYPE                                  
         BR    RE                                                               
*                                                                               
DISPSRT  NTR1                                                                   
         XC    PROSRTF,PROSRTF                                                  
         USING CTSRTD,R3           SORT FORMULA                                 
         SR    R4,R4                                                            
         IC    R4,CTSRTLEN         ELEMENT LENGTH                               
         SH    R4,=H'6'            -6                                           
         SRL   R4,1                /2=LOOP COUNT                                
         LA    R9,CTSRTFRM         R9=A(FIRST PARAM)                            
         LA    R8,PROSRTF          R8=A(SCREEN LINE)                            
DISPSR2  MVC   APDUB(2),0(R9)      GET SORT PARAM                               
         XC    DUB1,DUB1                                                        
         MVC   DUB1+3(1),APDUB      START COLUMN                                
         NI    DUB1+3,X'7F'         TURN OFF X'80' IF PRESENT                   
         MVC   DUB1+7(1),APDUB+1    LENGTH                                      
         EDIT  (B4,DUB1),(3,0(R8)),ALIGN=LEFT                                   
         AR    R8,R0                                                            
         MVI   0(R8),C','                                                       
         LA    R8,1(R8)                                                         
         EDIT  (B4,DUB1+4),(2,0(R8)),ALIGN=LEFT                                 
         AR    R8,R0                                                            
         MVI   0(R8),C','                                                       
         LA    R8,1(R8)                                                         
         MVC   0(2,R8),=C'A,'                                                   
         TM    0(R9),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(R8),C'D'                                                       
         LA    R8,2(R8)            BUMP TO NEXT OUTPUT FIELD                    
         LA    R9,2(R9)            BUMP TO NEXT INPUT FIELD                     
         BCT   R4,DISPSR2                                                       
         BCTR  R8,0                                                             
         MVI   0(R8),0                                                          
         MVC   PROSRTL+L'PROSRTL-1(1),DISPTYPE                                  
         XIT1                                                                   
         EJECT                                                                  
DISPJCL  DS    0H                  SPECIAL JCL BOOK                             
         XC    PROSJCL,PROSJCL                                                  
         USING CTJCLD,R3                                                        
         MVC   PROSJCL,CTJCLEX                                                  
         MVC   PROJCLL+L'PROJCLL-1(1),DISPTYPE                                  
         BR    RE                                                               
         SPACE 1                                                                
DISPEXP  NTR1                      SPECIAL PHASES                               
         XC    PROSPHS,PROSPHS                                                  
         USING CTPHSD,R3                                                        
         MVI   BLOCK1,C' '         CLEAR BLOCK                                  
         MVC   BLOCK1+1(79),BLOCK1                                              
         LA    R3,CTPHS01                                                       
         LA    R4,BLOCK1                                                        
         LA    R9,1                                                             
         LA    R8,4                                                             
         SR    RE,RE                                                            
DISPEXP2 CLI   0(R3),C' '                                                       
         BE    DISPEXP4                                                         
         STC   R9,0(R4)            MOVE PHASE# AND TEST LEVEL TO BLOCK          
         OI    0(R4),X'F0'                                                      
         MVC   10(1,R4),0(R3)                                                   
         LA    R4,20(R4)                                                        
         LA    RE,1(RE)            BUMP ENTRY COUNT                             
DISPEXP4 LA    R3,1(R3)            UNSCAN BLOCK INTO TWA FIELD                  
         LA    R9,1(R9)                                                         
         BCT   R8,DISPEXP2                                                      
         LR    R3,RE                                                            
         GOTO1 VUNSCAN,APPARM,((R3),BLOCK1),PROSPHSH                            
         CLI   APPARM,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF DATA WON'T FIT                        
         MVC   PROPHSL+L'PROPHSL-1(1),DISPTYPE                                  
         XIT1                                                                   
         EJECT                                                                  
DISPPRO  DS    0H                  PROCESSING INSTRS                            
         USING CTPRCD,R3                                                        
         LA    R4,PROPRCAH                                                      
         LA    R9,PROPRCBH                                                      
         MVC   PROPROL+L'PROPROL-1(1),DISPTYPE                                  
         B     DISPNAR                                                          
*                                                                               
DISPPAK  DS    0H                  PACKING INSTRS                               
         LA    R4,PROPACAH                                                      
         LA    R9,PROPACBH                                                      
         MVC   PROPACL+L'PROPACL-1(1),DISPTYPE                                  
         B     DISPNAR                                                          
*                                                                               
DISPSLI  DS    0H                  PACKING SLIP INSTRS                          
         LA    R4,PROPINAH                                                      
         LA    R9,PROPINBH                                                      
         MVC   PROSHPL+L'PROSHPL-1(1),DISPTYPE                                  
         B     DISPNAR                                                          
*                                                                               
DISPNAR  SR    R1,R1               GENERAL 2 LINE NARRATIVE HANDLER             
         XC    8(59,R4),8(R4)                                                   
         XC    8(59,R9),8(R9)                                                   
         IC    R1,1(R3)                                                         
         SH    R1,=H'6'            R3=L'NARRATIVE-1                             
         SR    R8,R8                                                            
         C     R1,=F'60'           MAX L'SCREEN LINE                            
         BNH   DISPNA2                                                          
         LR    R8,R1                                                            
         LA    R1,60               R1=L'FIRST LINE DATA                         
         SR    R8,R1               R8=L'SECOND LINE DATA                        
DISPNA2  C     R1,=F'60'           ADJUST FOR 59 CHAR FIELD                     
         BL    *+6                                                              
         BCTR  R1,0                                                             
         CLI   0(R3),X'4C'         TEST SHIPPING INSTRUCTIONS                   
         BNE   DISPNA3                                                          
         TM    6(R3),X'80'         TEST SPECIAL NUMERIC SHIPPING UNIT           
         BO    DISPNA3                                                          
         ST    R1,APPARM+8         STORE LENGTH IN PARAMETER 3                  
         ST    RE,RETURN                                                        
         GOTO1 ADISPSHP,APPARM,8(R4),6(R3) CALL SPECIAL ROUTINE                 
         L     RE,RETURN                                                        
         B     DISPNA4                                                          
*                                                                               
DISPNA3  BCTR  R1,0                OTHERWISE MOVE IN AS IS                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),6(R3)       MOVE LINE 1 NARR TO SCREEN                   
*                                                                               
DISPNA4  LA    R1,66(R3)           R1=A(NEXT CHUNK OF NARRATIVE)                
         LTR   R8,R8                                                            
         BZR   RE                                                               
         C     R8,=F'60'           ADJUST FOR 59 CHAR FIELD                     
         BL    *+6                                                              
         BCTR  R8,0                                                             
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R9),0(R1)       MOVE LINE 2 NARR TO SCREEN                   
         BR    RE                                                               
         EJECT                                                                  
DISPSDS  DS    0H                  REPORT SHORT DESCRIPTION                     
         XC    PROSDSC,PROSDSC                                                  
         USING CTSDSD,R3                                                        
         MVC   PROSDSC,CTSDSTXT                                                 
         MVC   PROSDSL+L'PROSDSL-1(1),DISPTYPE                                  
         BR    RE                                                               
         SPACE 1                                                                
DISPFRM  DS    0H                  REPORT FORM CODE                             
         XC    PROFRMC,PROFRMC                                                  
         USING CTFRMD,R3                                                        
         MVC   PROFRMC,CTFRMCOD                                                 
         MVC   PROFRML+L'PROFRML-1(1),DISPTYPE                                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
VALSEL   LA    R2,APRECKEY                                                      
         XC    SELDATA,SELDATA                                                  
*                                                                               
         LA    R4,LSTSYSTH                                                      
         GOTO1 VALPARS             GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         XC    CTPKEY,CTPKEY       BUILD AN INITIAL KEY                         
         MVI   CTPKTYP,CTPKTYPQ                                                 
         MVC   CTPKSYS,SELSYS                                                   
         MVC   CTPKPROG,SELPGM                                                  
         MVC   CTPKORIG,SELOID                                                  
*                                                                               
         XC    VLIST,VLIST                                                      
         LA    RF,DLIST                                                         
         ST    RF,VLISTPTR                                                      
*                                                                               
         MVI   GETSEQF,0           INTERNAL READ SEQUENCE FLAG                  
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
GETSEL   LA    R2,IOKEY            READ NEXT LIST RECORD                        
         MVC   CTPKEY,APRECKEY       FROM LAST SAVED KEY                        
*                                                                               
         L     RF,VLISTPTR                                                      
         OC    0(4,RF),0(RF)                                                    
         BNZ   GSEL20                                                           
*                                                                               
         TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    GSEL02                                                           
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GSEL04                                                           
GSEL02   TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    GSEL10                                                           
         NI    APINDS,X'FF'-APILRERD                                            
GSEL04   GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GSEL20                                                           
         B     GETSELN                                                          
GSEL10   TM    APINDS,APILNSEQ     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GSEL20                                                           
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         GOTO1 GETREC                                                           
         BNE   GETSELN                                                          
         B     GETSELY                                                          
GSEL20   LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 GETREC                                                           
         BNE   GETSELN             (EOF)                                        
*                                                                               
GETSELY  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTPKEY),CTPKEY                                        
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
DISSEL   EQU   *                                                                
*                                                                               
         L     R4,APPARM                                                        
         GOTO1 LINE                                                             
         L     RF,VLISTPTR                                                      
         LA    RF,4(RF)                                                         
         ST    RF,VLISTPTR                                                      
*                                                                               
DISSELX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
         SPACE 1                                                                
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO VALIDATE DELETE ACTION FROM LIST/SELECT                  *         
* ONLY ALL RIGHT IF PERSON COUNT IS 0                                 *         
***********************************************************************         
PROSEL   EQU   *                                                                
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   PROSAVE,LISTTYP                                                  
*                                                                               
PROSELX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
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
         XC    SELDATA,SELDATA                                                  
         LA    R4,REPSYSTH                                                      
         GOTO1 VALPARS             GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         XC    CTPKEY,CTPKEY       BUILD AN INITIAL KEY                         
         MVI   CTPKTYP,CTPKTYPQ                                                 
         MVC   CTPKSYS,SELSYS                                                   
         MVC   CTPKPROG,SELPGM                                                  
         MVC   CTPKORIG,SELOID                                                  
*                                                                               
         XC    VLIST,VLIST                                                      
         LA    RF,DLIST                                                         
         ST    RF,VLISTPTR                                                      
*                                                                               
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
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
* ROUTINE TO PRINT PROFILE LIST                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
PRTREP   EQU   *                                                                
         L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
         SPACE 1                                                                
         MVC   CTPKEY,APRECKEY                                                  
*                                                                               
         LA    R1,IOHI+IOCONFIL+IO1                                             
         GOTO1 GETREC                                                           
         BNE   PRTREPX                                                          
         B     PREC100                                                          
*                                                                               
PREC010  L     RF,VLISTPTR                                                      
         OC    0(4,RF),0(RF)                                                    
         BNZ   PREC040                                                          
         TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    PREC020                                                          
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     PREC030                                                          
PREC020  TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    PREC040                                                          
         NI    APINDS,X'FF'-APILRERD                                            
PREC030  LA    R2,IOKEY                                                         
         MVC   CTPKEY,APRECKEY                                                  
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    PREC040                                                          
         B     PRTREPX                                                          
*                                                                               
PREC040  LA    R1,IOSQ+IOCONFIL+IO1                                             
         GOTO1 GETREC              GO GET NEXT REC                              
         BNE   PRTREPX                                                          
*                                                                               
PREC100  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTPKEY),CTPKEY                                        
         LA    R4,REPP1-14                                                      
         GOTO1 LINE                                                             
         L     RF,VLISTPTR                                                      
         LA    RF,4(RF)                                                         
         ST    RF,VLISTPTR                                                      
*                                                                               
         GOTO1 VREPORT,REPD                                                     
         B     PREC010                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CTPREC,R2                                                        
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         MVI   0(R3),KEYSYS                                                     
         MVI   1(R3),3                                                          
         MVC   2(1,R3),CTPKSYS                                                  
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         MVI   0(R3),KEYPGM                                                     
         MVI   1(R3),4                                                          
         MVC   2(2,R3),CTPKPROG                                                 
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         MVI   0(R3),KEYUID                                                     
         MVI   1(R3),4                                                          
         MVC   2(2,R3),CTPKORIG                                                 
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         MVI   0(R3),KEYPTYP                                                    
         MVI   1(R3),6                                                          
         L     RF,VLISTPTR                                                      
         MVC   2(4,R3),0(RF)                                                    
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
PUTKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLEAR DISPLAY INDICATOR IN PROTS                       *         
***********************************************************************         
         SPACE                                                                  
CLEARP   NTR1                                                                   
         SR    R2,R2                                                            
CLEARP2  CLI   0(R1),0                                                          
         BE    CLEARPX                                                          
         TM    1(R1),X'20'                                                      
         BZ    CLEARP3                                                          
         OI    6(R1),X'80'                                                      
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         BCTR  RE,0                                                             
         AR    RE,R1                                                            
         MVI   0(RE),0                                                          
CLEARP3  IC    R2,0(R1)                                                         
         AR    R1,R2                                                            
         B     CLEARP2                                                          
CLEARPX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              EDIT PROFILE LIST AND MOVE TO TWA                      *         
***********************************************************************         
         SPACE 2                                                                
DISPVALS NTR1                                                                   
         XC    PROVALS,PROVALS                                                  
         OI    PROVALSH+6,X'80'                                                 
         XC    APWORK,APWORK                                                    
         LA    R3,APWORK           A(EDITED PROFILE LIST)                       
         MVI   DISPTYPE,C'D'                                                    
         LA    R4,DLIST                                                         
         B     DISPV4              EDIT DEFAULT PROFILE LIST                    
*                                                                               
DISPV2   MVI   DISPTYPE,C'O'                                                    
         LA    R4,OLIST            EDIT OVERRIDE PROFILE LIST                   
*                                                                               
DISPV4   MVC   0(1,R3),DISPTYPE                                                 
         MVI   1(R3),C'='                                                       
*                                                                               
DISPV6   OC    0(4,R4),0(R4)       END OF LIST                                  
         BZ    DISPV10                                                          
         CLI   1(R3),C'='          START OF LIST                                
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *+8                                                              
         MVI   0(R3),C' '          DELIMIT LAST FIELD                           
         MVC   1(1,R3),0(R4)       T/D/S/P                                      
         LA    R9,1                                                             
         OC    1(3,R4),1(R4)                                                    
         BZ    DISPV8                                                           
         MVI   2(R3),C','          D,XXX                                        
         MVC   3(3,R3),1(R4)                                                    
         LA    R9,4(R9)                                                         
         CLI   0(R4),C'D'                                                       
         BE    DISPV8                                                           
         LA    R9,4(R9)                                                         
         GOTO1 VDATCON,APPARM,(1,1(R4)),(8,3(R3))                               
         CLI   3(R3),C' '          CHECK US/UK FORMAT                           
         BNE   *+8                                                              
         OI    3(R3),X'F0'                                                      
         TM    3(R3),X'F0'                                                      
         BO    *+8                                                              
         LA    R9,1(R9)                                                         
*                                                                               
DISPV8   LA    R4,4(R4)            BUMP TO NEXT ENTRY                           
         LA    R3,1(R9,R3)         AND BUMP OUTPUT                              
         B     DISPV6                                                           
*                                                                               
DISPV10  CLI   1(R3),C'='          ANYTHING IN OUTPUT                           
         BNE   *+14                                                             
         MVC   2(4,R3),=C'NONE'                                                 
         LA    R3,7(R3)                                                         
         LA    R3,2(R3)                                                         
         CLI   DISPTYPE,C'O'       OVERRIDES DONE YET                           
         BNE   DISPV2              NO - GO AND DO THEM                          
         MVC   PROVALS,APWORK      YES - MOVE TO TWA                            
         XIT1                                                                   
         EJECT                                                                  
*        DISPLAY PRIORITY TYPE CODE                                             
*                                                                               
DPRTCODE EQU   *                                                                
         XC    PROPXPT,PROPXPT                                                  
         CLI   PROPRPT,C'A'                                                     
         BL    *+22                                                             
         CLI   PROPRPT,C'I'                                                     
         BH    *+14                                                             
         MVC   PROPXPT,=C'*1'                                                   
         B     DPRTCX                                                           
         CLI   PROPRPT,C'J'                                                     
         BL    *+22                                                             
         CLI   PROPRPT,C'R'                                                     
         BH    *+14                                                             
         MVC   PROPXPT,=C'*2'                                                   
         B     DPRTCX                                                           
         CLI   PROPRPT,C'S'                                                     
         BL    *+22                                                             
         CLI   PROPRPT,C'Y'                                                     
         BH    *+14                                                             
         MVC   PROPXPT,=C'*3'                                                   
         B     DPRTCX                                                           
         CLI   PROPRPT,C'0'                                                     
         BL    DPRTCX                                                           
         CLI   PROPRPT,C'9'                                                     
         BH    DPRTCX                                                           
         MVC   PROPXPT,=C'*4'                                                   
DPRTCX   BR    RE                                                               
         SPACE 2                                                                
*        DISPLAY PRIORITY OUTPUT CODE                                           
*                                                                               
DPROCODE EQU   *                                                                
         XC    PROPXPO,PROPXPO                                                  
         CLI   PROPRPO,C'A'                                                     
         BL    *+22                                                             
         CLI   PROPRPO,C'I'                                                     
         BH    *+14                                                             
         MVC   PROPXPO,=C'*1'                                                   
         B     DPROCX                                                           
         CLI   PROPRPO,C'J'                                                     
         BL    *+22                                                             
         CLI   PROPRPO,C'R'                                                     
         BH    *+14                                                             
         MVC   PROPXPO,=C'*2'                                                   
         B     DPROCX                                                           
         CLI   PROPRPO,C'S'                                                     
         BL    *+22                                                             
         CLI   PROPRPO,C'Y'                                                     
         BH    *+14                                                             
         MVC   PROPXPO,=C'*3'                                                   
         B     DPROCX                                                           
         CLI   PROPRPO,C'0'                                                     
         BL    DPROCX                                                           
         CLI   PROPRPO,C'9'                                                     
         BH    DPROCX                                                           
         MVC   PROPXPO,=C'*4'                                                   
DPROCX   BR    RE                                                               
         EJECT                                                                  
*                                  GETTXT MESSAGE # ERROR EXITS                 
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
EFNH     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
EIIO     MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
EDIF     MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
ERAE     MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
EIRT     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  ?? ALREADY EXISTS                            
EFNV     MVC   FVMSGNO,=AL2(CE#NOFUN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FUNCTION NOT AVAILABLE                       
ERTB     MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD TO BIG                                
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
REPDESCL DC    C'PROFILE RECORD LIST'                                           
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'PROFILE RECORD LIST'                                     
         SPEC  H2,57,C'-------------------'                                     
         SPEC  M1,1,C'     SYS/ ORIG.  PROF.      P P P C OUTP'                 
         SPEC  M2,1,C'     PGM   ID    TYPE       P T O L  TYP'                 
         SPEC  M1,41,C'UT ATTN PHASE DEST.  PQ PROGRAM NAME'                    
         SPEC  M2,41,C'E  TYPE 1234   ID    FM ------------'                    
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
*                                                                               
TABCLA   DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ012345',X'FF'                        
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    80C' '                                                           
FFILL    DC    80X'FF'                                                          
XAUTH    DC    XL2'FFFF'                                                        
YAUTH    DC    XL2'000F'                                                        
NAUTH    DC    XL2'0000'                                                        
CAPFILL  DC    (L'APWORK)X'40'                                                  
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RB,RA                                                            
*                                                                               
* ENTRY POINT FOR THE CONMMON ROUTINES                                          
* UPON ENTRY, RF HOLDS A(VCOMMON) IN ITS LOW ORDER THREE                        
* BYTES AND THE ROUTINE NUMBER IN ITS HIGH ORDER BYTE.  VCOMMON WILL            
* USE THE ROUTINE NUMBER TO BRANCH TO THE DESIRED ROUTINE.                      
*                                                                               
VCOMMON  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VCOMMON,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VCO'    INSERT NAME                                  
*                                                                               
         SRL   RF,24               BRANCH TO DESIRED ROUTINE                    
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
*                                                                               
* TABLE OF BRANCH ADDRESSES TO CONTROLLER ROUTINES                              
*                                                                               
VBRANCH  B     VVALPARS                                                         
         B     VGETREC                                                          
         B     VLINE                                                            
         B     VVALPSYS                                                         
         B     VVALPOID                                                         
         B     VVALPTYP                                                         
         B     VDISPOID                                                         
         B     VDISPTYP                                                         
         B     VEXTRACT                                                         
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
         SPACE 1                                                                
**********************************************************************          
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                            *          
* R4 = A(FIRST FIELD HEADER IN STANDARD DISPLAY)                     *          
*   APPLICABLE TO BOTH LIST AND REPORT SCREEN FIELD OFFSETS          *          
**********************************************************************          
         SPACE 1                                                                
         USING CTPREC,R2                                                        
         USING LSTSYSTH,R4                                                      
VVALPARS EQU   *                                                                
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         MVI   FVMINL,0            VALIDATE SYSTEM                              
         GOTO1 VALPSYS,APPARM,LSTSYSTH,SELSYS                                   
         BNE   VPARX                                                            
*                                                                               
         GOTO1 AFVAL,LSTPRGMH      VALIDATE PROGRAM                             
         BNE   VPAR020                                                          
         CLI   FVILEN,2            AND LENGTH OF 2                              
         BNE   VPARNO                                                           
         MVC   SELPGM,FVIFLD                                                    
*                                                                               
VPAR020  MVI   FVMINL,0                                                         
         MVI   ALLFLAG,C'Y'                                                     
         GOTO1 VALPOID,APPARM,LSTORGNH,SELOID                                   
         BNE   VPARX                                                            
         CLI   FVILEN,3                                                         
         BNE   VPAR030                                                          
         CLC   FVIFLD(3),=C'ALL'                                                
         BNE   VPAR030                                                          
         MVI   SELOIDF,X'FF'                                                    
*                                                                               
VPAR030  MVI   FVMINL,0                                                         
         GOTO1 VALPTYP,APPARM,LSTPROTH,SELTYP                                   
         BNE   VPARX                                                            
*                                  ATTENTION TYPE                               
VPAR040  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTATYPH                                                   
         BNE   VPAR050                                                          
         MVC   SELATYP,FVIFLD                                                   
*                                  DESTINATION ID                               
VPAR050  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTDSTIH                                                   
         BNE   VPAR060                                                          
         TM    FVIIND,FVINUM       TEST FOR NUMERIC INPUT                       
         BZ    VPAR052                                                          
         OC    SCFULL(4),SCFULL    NUMBER IN SCFULL                             
         BZ    VPARNO              CHECK IN RANGE                               
         OC    SCFULL(2),SCFULL                                                 
         BNZ   VPARNO                                                           
         MVC   SELDSTI,SCFULL+2    SAVE NUMBER IN FILTER                        
         B     VPAR060                                                          
*                                                                               
VPAR052  MVI   ALLFLAG,C'Y'        OR PROCESS USER ID APLHA CODE                
         GOTO1 VALPOID,APPARM,LSTDSTIH,SELDSTI                                  
         BNE   VPARX                                                            
         CLI   FVILEN,3                                                         
         BNE   VPAR060                                                          
         CLC   FVIFLD(3),=C'ALL'                                                
         BE    VPARNO                                                           
*                                  OUTPUT TYPE                                  
VPAR060  L     R3,AIOAREA2                                                      
         USING CTOREC,R3                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTOUTPH                                                   
         BNE   VPAR070                                                          
         CLI   FVILEN,1                                                         
         BL    VPARNO                                                           
         MVC   CTOKID,FVIFLD       MOVE ID TO KEY                               
         MVC   IOKEY(L'CTOKEY),CTOKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         DROP  R3                                                               
         BL    VPARNO                                                           
         BH    VPARNO                                                           
         MVC   IOKEY,APRECKEY                                                   
         MVC   SELOUTP,FVIFLD                                                   
*                                  PROGRAM SORT ORDER CODE                      
VPAR070  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTPRSCH                                                   
         BNE   VPAR080                                                          
         CLI   FVIFLD,C'A'                                                      
         BL    VPARNO                                                           
         CLI   FVIFLD,C'9'                                                      
         BH    VPARNO                                                           
         MVC   SELPRSC,FVIFLD                                                   
*                                  PROGRAM TYPE CODE                            
VPAR080  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTPRPTH                                                   
         BNE   VPAR090                                                          
         CLI   FVIFLD,C'A'                                                      
         BL    VPARNO                                                           
         CLI   FVIFLD,C'9'                                                      
         BH    VPARNO                                                           
         MVC   SELPRPT,FVIFLD                                                   
*                                  PROGRAM OUTPUT CODE                          
VPAR090  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTPRPOH                                                   
         BNE   VPAR100                                                          
         CLI   FVIFLD,C'A'                                                      
         BL    VPARNO                                                           
         CLI   FVIFLD,C'9'                                                      
         BH    VPARNO                                                           
         MVC   SELPRPO,FVIFLD                                                   
*                                  READER CLASS                                 
VPAR100  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTRCLAH                                                   
         BNE   VPAR110                                                          
         MVC   SELRCLA,FVIFLD                                                   
*                                  REPORT FORM CODE                             
VPAR110  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTFRMCH                                                   
         BNE   VPAR120                                                          
         MVC   SELFRMC,FVIFLD                                                   
*                                  TEST PHASES                                  
VPAR120  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTSPHSH                                                   
         BNE   VPAR130                                                          
         MVC   SELSPHS,FVIFLD                                                   
*                                  SPECIAL JCL                                  
VPAR130  MVI   FVMINL,0                                                         
         GOTO1 AFVAL,LSTSJCLH                                                   
         BNE   VPAR140                                                          
         MVC   SELSJCL,FVIFLD                                                   
*                                                                               
VPAR140  B     VPARX                                                            
*                                                                               
VPARNO   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VPARX                                                            
*                                                                               
VPARX    XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS       *          
**********************************************************************          
         SPACE 1                                                                
         USING CTPREC,R2                                                        
VGETREC  EQU   *                                                                
*                                                                               
         L     RF,VLISTPTR                                                      
         OC    0(4,RF),0(RF)                                                    
         BZ    GETRECIO                                                         
         B     GRTYP                                                            
*                                                                               
GETRECRD L     RF,VLISTPTR                                                      
         LA    RF,4(RF)                                                         
         ST    RF,VLISTPTR                                                      
         OC    0(4,RF),0(RF)                                                    
         BNZ   GRTYP                                                            
         TM    GETSEQF,APILRERD    READ NEXT RECORD                             
         BZ    GETRECSQ            CHECK SEQUENCE BROKEN                        
         NI    GETSEQF,X'FF'-APILRERD                                           
         MVC   IOKEY(L'CTPKEY),CTPKEY                                           
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETRECN                                                          
GETRECSQ LA    R1,IOCONFIL+IOSQ+IO1                                             
GETRECIO GOTO1 AIO                                                              
         BNE   GETRECN                                                          
         L     R2,AIOAREA1                                                      
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(CTPKSYS-CTPKEY),CTPKEY                                  
         BNE   GETRECN                                                          
*                                                                               
         XC    VLIST,VLIST                                                      
         GOTO1 EXTRACT,DLIST       EXTRACT PROFILE TYPES                        
         LA    RF,DLIST                                                         
         ST    RF,VLISTPTR                                                      
         OC    0(4,RF),0(RF)                                                    
         BNZ   GRTYP                                                            
         MVI   0(RF),C'P'                                                       
*                                                                               
GRTYP    OC    SELTYP,SELTYP      PROFILE TYPE                                  
         BZ    GRTYPX                                                           
         L     RF,VLISTPTR                                                      
         CLC   0(4,RF),SELTYP                                                   
         BNE   GETRECRD            (NO MORE RELEVENT RECORDS)                   
GRTYPX   EQU   *                                                                
*                                  * FILTER ON SELECTION CRITERIA *             
         SPACE 1                                                                
GRSYS    OC    SELSYS,SELSYS      SYSTEM                                        
         BZ    GRSYSX                                                           
         CLC   CTPKSYS,SELSYS                                                   
         BNE   GETRECN             (NO MORE RELEVENT RECORDS)                   
GRSYSX   EQU   *                                                                
*                                                                               
GRPGM    EQU   *                   FILTER ON PROGRAM                            
         OC    SELPGM,SELPGM                                                    
         BZ    GRPGMX                                                           
         CLC   CTPKPROG,SELPGM                                                  
         BNE   GETRECRD            READ NEXT RECORD                             
GRPGMX   EQU   *                                                                
*                                                                               
GROID    EQU   *                   FILTER ON ORIGIN USER ID                     
         CLI   SELOIDF,0                                                        
         BE    GROID010                                                         
         OC    CTPKORIG,CTPKORIG                                                
         BNZ   GETRECRD                                                         
         B     GROIDX                                                           
GROID010 OC    SELOID,SELOID                                                    
         BZ    GROIDX                                                           
         CLC   CTPKORIG,SELOID                                                  
         BNE   GETRECRD            READ NEXT RECORD                             
GROIDX   EQU   *                                                                
*                                                                               
GRATYP   EQU   *                   FILTER ON ATTENTION TYPE                     
         OC    SELATYP,SELATYP                                                  
         BZ    GRATYPX                                                          
         MVI   APELEM,CTACOELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTACOD,R3                                                        
         CLC   CTACODE,SELATYP                                                  
         BNE   GETRECRD                                                         
         DROP  R3                                                               
GRATYPX  EQU   *                                                                
*                                                                               
GRDSTI   EQU   *                   FILTER ON DESTINATION ID                     
         OC    SELDSTI,SELDSTI                                                  
         BZ    GRDSTIX                                                          
         MVI   APELEM,CTDCOELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTDCOD,R3                                                        
         CLC   CTDCNUM,SELDSTI                                                  
         BNE   GETRECRD                                                         
         DROP  R3                                                               
GRDSTIX  EQU   *                                                                
*                                                                               
GROUTP   EQU   *                   FILTER ON OUTPUT TYPE                        
         OC    SELOUTP,SELOUTP                                                  
         BZ    GROUTPX                                                          
         MVI   APELEM,CTOCOELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTOCOD,R3                                                        
         CLC   CTOCODE,SELOUTP                                                  
         BNE   GETRECRD                                                         
         DROP  R3                                                               
GROUTPX  EQU   *                                                                
*                                                                               
GRPRSC   EQU   *                   FILTER ON PROGRAM SORT CODE                  
         OC    SELPRSC,SELPRSC                                                  
         BZ    GRPRSCX                                                          
         MVI   APELEM,CTPRIELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTPRID,R3                                                        
         CLC   CTPRISC,SELPRSC                                                  
         BNE   GETRECRD                                                         
         DROP  R3                                                               
GRPRSCX  EQU   *                                                                
*                                                                               
GRPRPT   EQU   *                   FILTER ON PROGRAM TYPE CODE                  
         OC    SELPRPT,SELPRPT                                                  
         BZ    GRPRPTX                                                          
         MVI   APELEM,CTPRIELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTPRID,R3                                                        
         CLI   CTPRILEN,8                                                       
         BE    GETRECRD                                                         
         CLC   CTPRIPT,SELPRPT                                                  
         BNE   GETRECRD                                                         
         DROP  R3                                                               
GRPRPTX  EQU   *                                                                
*                                                                               
GRPRPO   EQU   *                   FILTER ON PROGRAM OUTPUT CODE                
         OC    SELPRPO,SELPRPO                                                  
         BZ    GRPRPOX                                                          
         MVI   APELEM,CTPRIELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTPRID,R3                                                        
         CLI   CTPRILEN,8                                                       
         BE    GETRECRD                                                         
         CLC   CTPRIPO,SELPRPO                                                  
         BNE   GETRECRD                                                         
         DROP  R3                                                               
GRPRPOX  EQU   *                                                                
*                                                                               
GRRCLA   EQU   *                   FILTER ON READER CLASS                       
         OC    SELRCLA,SELRCLA                                                  
         BZ    GRRCLAX                                                          
         MVI   APELEM,CTRCLELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTRCLD,R3                                                        
         CLC   CTRCLASS,SELRCLA                                                 
         BNE   GETRECRD                                                         
         DROP  R3                                                               
GRRCLAX  EQU   *                                                                
*                                                                               
GRFRMC   EQU   *                   FILTER ON REPORT FORM CODE                   
         OC    SELFRMC,SELFRMC                                                  
         BZ    GRFRMCX                                                          
         MVI   APELEM,CTFRMELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTFRMD,R3                                                        
         CLC   CTFRMCOD,SELFRMC                                                 
         BNE   GETRECRD                                                         
         DROP  R3                                                               
GRFRMCX  EQU   *                                                                
*                                                                               
GRSPHS   EQU   *                   FILTER ON SPECIAL PHASES                     
         OC    SELSPHS,SELSPHS                                                  
         BZ    GRSPHSX                                                          
         MVI   APELEM,CTPHSELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
         USING CTPHSD,R3                                                        
         CLC   CTPHS01,SELSPHS                                                  
         BE    GRSPHSX                                                          
         CLC   CTPHS02,SELSPHS                                                  
         BE    GRSPHSX                                                          
         CLC   CTPHS03,SELSPHS                                                  
         BE    GRSPHSX                                                          
         CLC   CTPHS04,SELSPHS                                                  
         BE    GRSPHSX                                                          
         B     GETRECRD                                                         
         DROP  R3                                                               
GRSPHSX  EQU   *                                                                
*                                                                               
GRSJCL   EQU   *                   FILTER ON SPECIAL JCL                        
         OC    SELSJCL,SELSJCL                                                  
         BZ    GRSJCLX                                                          
         MVI   APELEM,CTJCLELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD                                                         
GRSJCLX  EQU   *                                                                
*                                                                               
GETRECY  SR    RC,RC               RETURN CC EQUAL RECORD OK                    
*                                                                               
GETRECN  LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD A LINE OF PROFILE RECORD DATA                                 *         
***********************************************************************         
         SPACE                                                                  
         USING CTPREC,R2                                                        
VLINE    EQU   *                                                                
         L     R2,AIOAREA1                                                      
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
         MVC   LISTSYS,CTPKSYS     DISPLAY KEY DATA                             
         MVC   LISTPGM,CTPKPROG                                                 
         XC    APWORK,APWORK       DISPLAY ORIGIN ID                            
         GOTO1 DISPOID,APPARM,APWORK,CTPKORIG                                   
         MVC   LISTOID,APWORK                                                   
         OI    APINDS,APILRERD     FLAG READ SEQUENCE BROKEN                    
         XC    APWORK,APWORK                                                    
         L     RF,VLISTPTR         GET SUB LIST POINTER                         
         GOTO1 DISPTYP,APPARM,APWORK,(RF)                                       
         MVC   LISTTYP,APWORK                                                   
*                                  DISPLAY SHORT NAME IF FOUND                  
         USING CTSDSD,R3                                                        
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTSDSELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE100                                                          
         MVC   LISTPNAM(L'CTSDSTXT),CTSDSTXT                                    
         B     LINE110                                                          
*                                  ELSE DISPLAY START OF DESCRIPTION            
         USING CTDSCD,R3                                                        
LINE100  XC    APELEM,APELEM                                                    
         MVI   APELEM,CTDSCELQ                                                  
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE110                                                          
         SR    RF,RF                                                            
         IC    RF,CTDSCLEN                                                      
         SH    RF,=H'3'                                                         
         MVC   APWORK,VCSPACES                                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),CTDSC                                                  
         GOTO1 =V(CHOPPER),APPARM,(80,APWORK),(11,LISTPNAM),1,         *        
               RR=APRELO                                                        
*                                  DISPLAY DESTINATION ID                       
         USING CTDCOD,R3                                                        
LINE110  MVI   APELEM,CTDCOELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE120                                                          
         MVC   LISTDID,CTDCODE                                                  
         XC    APWORK,APWORK                                                    
         GOTO1 DISPOID,APPARM,APWORK,CTDCNUM                                    
         MVC   LISTDID,APWORK                                                   
         XC    APWORK,APWORK                                                    
*                                  DISPLAY ATTENTION TYPE CODE                  
         USING CTACOD,R3                                                        
LINE120  MVI   APELEM,CTACOELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE130                                                          
         MVC   LISTATYP,CTACODE                                                 
         SPACE 1                                                                
*                                  DISPLAY OUTPUT ID TYPE CODE                  
         USING CTOCOD,R3                                                        
LINE130  MVI   APELEM,CTOCOELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE140                                                          
         MVC   LISTOTYP,CTOCODE                                                 
*                                  DISPLAY PRIORITY CODE ELEMENT DATA           
         USING CTPRID,R3                                                        
LINE140  MVI   APELEM,CTPRIELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE150                                                          
         MVC   LISTPP,CTPRISC                                                   
         CLI   CTPRILEN,8                                                       
         BE    LINE150                                                          
         MVC   LISTPT,CTPRIPT                                                   
         MVC   LISTPO,CTPRIPO                                                   
*                                  DISPLAY READER CLASS                         
         USING CTRCLD,R3                                                        
LINE150  MVI   APELEM,CTRCLELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE160                                                          
         MVC   LISTCL,CTRCLASS                                                  
*                                  DISPLAY TEST PHASE NAME                      
         USING CTPHSD,R3                                                        
LINE160  MVI   APELEM,CTPHSELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE170                                                          
         MVC   LISTTPHS,CTPHS01                                                 
*                                  DISPLAY PQ FORM CODE                         
         USING CTFRMD,R3                                                        
LINE170  MVI   APELEM,CTFRMELQ                                                  
         MVI   APELEM+1,4                                                       
         L     RF,VLISTPTR                                                      
         MVC   APELEM+2(4),0(RF)                                                
         GOTO1 AGETELS,CTPREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LINE180                                                          
         MVC   LISTPQFM,CTFRMCOD                                                
*                                                                               
LINE180  B     LINEX                                                            
*                                                                               
LINEX    XIT1  ,                                                                
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROFILE SYSTEM FIELD AND RETURN INTERNAL LETTER            *         
***********************************************************************         
         SPACE                                                                  
VVALPSYS EQU   *                                                                
         LM    R3,R4,0(R1)                                                      
         GOTO1 AFVAL,(R3)                                                       
         BL    VPSYOK                                                           
         BH    VPSYNO                                                           
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         LA    RE,SYSLLEN(RE)      GO PAST SERVICE SYSTEM ENTRY                 
         ZIC   RF,FVXLEN                                                        
VPSY010  CLI   SYSLNUM,0                                                        
         BE    VPSYEIIF                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VPSY020                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VPSY010                                                          
*                                                                               
VPSY020  CLI   SYSLRPLT,C' '       MUST HAVE A KEY LETTER                       
         BE    VPSYEIIF                                                         
         MVC   0(1,R4),SYSLRPLT    MOVE SYSTEM LETTER TO KEY                    
         L     R1,FVADDR                                                        
         MVC   L'FVIHDR(L'SYSLNAME,R1),SYSLNAME                                 
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         B     VPSYOK                                                           
         DROP  RE                                                               
*                                                                               
VPSYEIIF MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VPSYNO                                                           
*                                                                               
VPSYOK   SR    RC,RC               RETURN CC EQUAL RECORD OK                    
VPSYNO   LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROFILE USERID FIELD AND RETURN ID NUMBER                  *         
***********************************************************************         
         SPACE                                                                  
VVALPOID EQU   *                                                                
         LM    R3,R4,0(R1)                                                      
         GOTO1 AFVAL,(R3)                                                       
*&&US*&& BNE   VPIDOK              OPTIONAL INPUT                               
*&&UK*&& BL    VPIDOK                                                           
*&&UK*&& BH    VPIDNO                                                           
         CLI   FVILEN,3            LENGTH NOT LESS THAN 3                       
         BL    VPIDEFTS                                                         
         BNE   VPID002                                                          
         CLI   ALLFLAG,C'Y'                                                     
         BNE   VPID002                                                          
         CLC   FVIFLD(3),=C'ALL'   TEST 'ALL' INPUT                             
         BE    VPIDOK                                                           
*                                                                               
VPID002  MVC   KEYSAVE,IOKEY       AND SAVE KEY                                 
         L     R2,AIOAREA2                                                      
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY       AND BUILD A KEY                              
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,FVIFLD       MOVE ID TO KEY                               
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    VPIDEIIO                                                         
         BH    VPIDERNF                                                         
         LA    R2,CTIDATA          LOOK FOR ID# ELEMENT                         
         SR    R1,R1                                                            
VPID010  CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'02'                                                      
         BE    *+14                                                             
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     VPID010                                                          
         MVC   0(2,R4),2(R2)                                                    
         OI    IFLAG,X'80'         SET USERID INPUT                             
         MVC   IOKEY,KEYSAVE                                                    
         B     VPIDOK                                                           
         DROP  R2                                                               
*                                                                               
VPIDEIIF MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VPIDNO                                                           
VPIDEFTS MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VPIDNO                                                           
VPIDEIIO MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     VPIDNO                                                           
VPIDERNF MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VPIDNO                                                           
*                                                                               
VPIDOK   SR    RC,RC               RETURN CC EQUAL RECORD OK                    
VPIDNO   LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROFILE TYPE FIELD                                         *         
***********************************************************************         
         SPACE                                                                  
VVALPTYP EQU   *                                                                
         LM    R3,R4,0(R1)                                                      
         GOTO1 AFVAL,(R3)          VALIDATE PROFILE TYPE                        
         BE    VPTY010                                                          
         BL    VPTYOK                                                           
         L     R1,FVADDR                                                        
         MVI   L'FVIHDR(R1),C'P'                                                
         MVI   FVILEN,1                                                         
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BASR  RE,RF                                                            
*                                                                               
VPTY010  XC    0(4,R4),0(R4)                                                    
         MVC   0(1,R4),FVIFLD                                                   
         MVI   FVINDX,1                                                         
         CLI   FVILEN,1            P/D/S/T                                      
         BE    VPTY020                                                          
         CLI   FVIFLD,C'D'         D,XXX                                        
         BE    VPTY030                                                          
         CLI   FVIFLD,C'T'         T,DDMMMYY                                    
         BE    VPTY050                                                          
         B     VPTYEIIF                                                         
VPTY020  MVI   FVINDX,2                                                         
         CLI   0(R4),C'D'          DAY                                          
         BE    VPTYEMIF                                                         
         CLI   0(R4),C'T'          TEMP                                         
         BE    VPTYEMIF                                                         
         MVI   FVINDX,1                                                         
         CLI   0(R4),C'P'          PERM                                         
         BE    VPTYOK                                                           
         CLI   0(R4),C'S'          SACRED                                       
         BE    VPTYOK                                                           
         B     VPTYEIIF                                                         
VPTY030  OI    IFLAG,X'40'         D,XXXX VALIDATION                            
         CLI   FVILEN,4                                                         
         BL    VPTYEFTS            CHECK LENGTH                                 
         SR    R1,R1               AND GET IT FOR COMPARE                       
         IC    R1,FVILEN                                                        
         SH    R1,=H'3'                                                         
         LA    R3,DAYTAB                                                        
         MVI   FVINDX,2                                                         
VPTY040  CLI   0(R3),0             END OF DAY TABLE                             
         BE    VPTYEIIF                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),FVIFLD+2    COMPARE INPUT WITH TABLE ENTRY               
         BE    *+12                                                             
         LA    R3,L'DAYTAB(R3)                                                  
         B     VPTY040                                                          
         MVC   1(3,R4),0(R3)       SET-UP PROTYPE PARAMETER                     
         B     VPTYOK                                                           
VPTY050  OI    IFLAG,X'40'                                                      
         CLI   FVILEN,7                                                         
         BE    VPTY060                                                          
         CLI   FVILEN,8            T,DDMMMYY VALIDATION                         
         BL    VPTYEFTS            CHECK LENGTH                                 
         GOTO1 VDATVAL,APPARM,(0,FVIFLD+2),APWORK                               
         OC    APPARM(4),APPARM                                                 
         MVI   FVINDX,2            INCORRECT DATE                               
         BZ    VPTYEIIF                                                         
*                                  CONVERT YYMMDD DATE TO PWOS YMD              
         GOTO1 VDATCON,APPARM,(0,APWORK),(1,1(R4))                              
         B     VPTYOK                                                           
VPTY060  CLC   FVIFLD+2(5),=C'TODAY'                                            
         BNE   VPTYEIIF                                                         
         GOTO1 VGETFACT,APPARM,0                                                
         L     RF,0(R1)            EXTRACT DATE FROM FACTSD                     
         MVC   APDUB,FADATE-FACTSD(RF)                                          
         GOTO1 VDATCON,APPARM,(4,APDUB),(1,1(R4))                               
         B     VPTYOK                                                           
*                                                                               
VPTYEIIF MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VPTYNO                                                           
VPTYEFTS MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VPTYNO                                                           
VPTYEMIF MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VPTYNO                                                           
*                                                                               
VPTYOK   SR    RC,RC               RETURN CC EQUAL RECORD OK                    
VPTYNO   LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY PROFILE USERID FIELD                                        *         
***********************************************************************         
         SPACE                                                                  
VDISPOID EQU   *                                                                
         LM    R3,R4,0(R1)                                                      
         OC    0(2,R4),0(R4)                                                    
*&&US*&& BZ    DPIDX                                                            
         BNZ   DPID010                                                          
         MVC   0(3,R3),=C'ALL'                                                  
         B     DPIDX                                                            
*                                                                               
DPID010  MVC   KEYSAVE,IOKEY                                                    
         MVI   0(R3),C'#'                                                       
         EDIT  (2,0(R4)),(4,1(R3)),FILL=0                                       
         L     R2,AIOAREA2                                                      
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY       AND BUILD A KEY                              
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,0(R4)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    DPID020                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DPID100                                                          
*                                                                               
DPID020  LA    R2,CTIDATA          LOOK FOR ID NAME ELEMENT                     
         SR    R1,R1                                                            
DPID030  CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'02'                                                      
         BE    *+14                                                             
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DPID030                                                          
         MVC   0(10,R3),2(R2)                                                   
         DROP  R2                                                               
*                                                                               
DPID100  MVC   IOKEY,KEYSAVE                                                    
*                                                                               
DPIDX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY PROFILE TYPE FIELD                                          *         
***********************************************************************         
         SPACE                                                                  
VDISPTYP EQU   *                                                                
         LM    R3,R4,0(R1)                                                      
         OC    0(4,R4),0(R4)                                                    
         BNZ   DTYP010                                                          
         MVI   0(R3),C'?'                                                       
         B     DTYPX                                                            
*                                                                               
DTYP010  MVC   0(1,R3),0(R4)                                                    
         OC    1(3,R4),1(R4)                                                    
         BZ    DTYPX                                                            
*                                                                               
DTYP020  MVI   1(R3),C','                                                       
         CLI   0(R4),C'D'                                                       
         BNE   DTYP030                                                          
         MVC   2(3,R3),1(R4)                                                    
         B     DTYPX                                                            
*                                                                               
DTYP030  CLI   0(R4),C'T'                                                       
         BNE   DTYPX                                                            
         GOTO1 VDATCON,APPARM,(1,1(R4)),(17,2(R3))                              
*                                                                               
DTYPX    XIT1  ,                                                                
         SPACE 2                                                                
***********************************************************************         
*              BUILD A LIST OF PROFILE TYPES FOR THIS REC             *         
*              R1=A(LIST)                                             *         
***********************************************************************         
         SPACE 2                                                                
         USING CTPREC,R2                                                        
VEXTRACT EQU   *                                                                
         LR    R3,R1                                                            
         LA    RE,CTPDATA                                                       
*                                                                               
EXTRAC2  CLI   0(RE),0             END OF RECORD                                
         BE    EXTRXIT                                                          
         L     RF,AELTAB                                                        
*                                                                               
EXTRAC4  CLI   0(RF),0             CHECK IF A PROFILE ELEMENT                   
         BE    EXTRAC8             NO - IGNORE                                  
         CLC   0(1,RF),0(RE)                                                    
         BE    *+12                                                             
         LA    RF,L'ELTAB(RF)                                                   
         B     EXTRAC4                                                          
         LR    R1,R3                                                            
*                                                                               
EXTRAC6  OC    0(4,R1),0(R1)       END OF PROFILE LIST                          
         BNZ   *+14                                                             
         MVC   0(4,R1),2(RE)       YES - POP IT IN                              
         B     EXTRAC8                                                          
         CLC   2(4,RE),0(R1)       ALREADY IN LIST                              
         BE    EXTRAC8             YES - IGNORE IT                              
         LA    R1,4(R1)            BUMP TO NEXT ENTRY                           
         B     EXTRAC6                                                          
*                                                                               
EXTRAC8  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     EXTRAC2                                                          
*                                                                               
EXTRXIT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              LIST OF DAYS OF WEEK                                             
*                                                                               
DAYTAB   DS    0CL9                                                             
         DC    CL9'MONDAY'                                                      
         DC    CL9'TUESDAY'                                                     
         DC    CL9'WEDNESDAY'                                                   
         DC    CL9'THURSDAY'                                                    
         DC    CL9'FRIDAY'                                                      
         DC    CL9'SATURDAY'                                                    
         DC    CL9'SUNDAY'                                                      
         DC    X'00'                                                            
*                                                                               
VCSPACES DC    80C' '                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         DS    0F                                                               
ELTAB    DS    0CL4                                                             
         DC    X'40',AL3(DISPDES)                                               
         DC    X'41',AL3(DISPATN)                                               
         DC    X'42',AL3(DISPOUT)                                               
         DC    X'43',AL3(DISPMOD)                                               
         DC    X'44',AL3(DISPPR)                                                
         DC    X'45',AL3(DISPRCL)                                               
         DC    X'46',AL3(DISPSRT)                                               
         DC    X'47',AL3(DISPPQP)                                               
         DC    X'48',AL3(DISPPRO)                                               
         DC    X'49',AL3(DISPPQC)                                               
         DC    X'4A',AL3(DISPPAK)                                               
         DC    X'4B',AL3(DISPSDS)                                               
         DC    X'4C',AL3(DISPSLI)                                               
         DC    X'4D',AL3(DISPJCL)                                               
         DC    X'4E',AL3(DISPEXP)                                               
         DC    X'4F',AL3(DISPFRM)                                               
         DC    X'50',AL3(DISPSQL)                                               
         DC    X'00',AL3(0)                                                     
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENF6D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGEND6D                                                       
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB6D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
LASTPRO  DS    CL4                                                              
LIDNUM   DS    CL2                                                              
SAVKEY   DS    XL(L'CTIKEY)        SAVE LAST RECORD KEY READ FOR COPY           
*                                                                               
PRISCSAV DS    CL1                                                              
PRIPTSAV DS    CL1                                                              
PRIPOSAV DS    CL1                                                              
SAVCLRL  EQU   *-SAVOVER                                                        
*                                                                               
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTSYS  DS    CL1                                                              
         DS    CL1                                                              
LISTPGM  DS    CL2                                                              
         DS    CL1                                                              
LISTOID  DS    CL6                                                              
         DS    CL1                                                              
LISTTYP  DS    CL10                                                             
         DS    CL1                                                              
LISTPP   DS    CL1                                                              
         DS    CL1                                                              
LISTPT   DS    CL1                                                              
         DS    CL1                                                              
LISTPO   DS    CL1                                                              
         DS    CL1                                                              
LISTCL   DS    CL1                                                              
         DS    CL1                                                              
LISTOTYP DS    CL6                                                              
         DS    CL1                                                              
LISTATYP DS    CL3                                                              
         DS    CL2                                                              
LISTTPHS DS    CL4                                                              
         DS    CL2                                                              
LISTDID  DS    CL6                                                              
         DS    CL1                                                              
LISTPQFM DS    CL2                                                              
         DS    CL1                                                              
LISTPNAM DS    CL11                                                             
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
DUB1     DS    D                                                                
RETURN   DS    F                                                                
ASYSEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
WORK     DS    XL(L'APWORK)                                                     
*                                                                               
SELDATA  DS    0XL(SELDATAL)                                                    
SELSYS   DS    CL1                 SYSTEM SEOV NUMBER                           
SELPGM   DS    CL2                 PROGRAM NUMBER                               
SELOID   DS    XL2                 ORIGIN USER ID                               
SELOIDF  DS    XL1                 ALL ORIGIN USER ID FLAG                      
SELTYP   DS    CL4                 PROFILE TYPE                                 
SELATYP  DS    CL3                 ATTENTION TYPE                               
SELDSTI  DS    XL2                 DESTINATION ID NUMBER                        
SELOUTP  DS    CL10                OUTPUT TYPE                                  
SELPRSC  DS    CL1                 PROGRAM SORT ORDER                           
SELPRPT  DS    CL1                 PROGRAM TYPE CODE                            
SELPRPO  DS    CL1                 PROGRAM OUTPUT CODE                          
SELRCLA  DS    CL1                 READER CLASS                                 
SELFRMC  DS    CL2                 REPORT FORM CODE                             
SELSPHS  DS    CL1                 SPECIAL TEST PHASE                           
SELSJCL  DS    CL1                 SPECIAL JCL                                  
SELDATAL EQU   *-SELSYS                                                         
*                                                                               
PROSAVE  DS    CL(L'LISTTYP)                                                    
PROTYPE  DS    CL4                                                              
IFLAG    DS    XL1                                                              
KLEVEL   DS    CL1                                                              
DISPTYPE DS    CL1                                                              
FTBFLAG  DS    XL1                                                              
*                                                                               
PRISCVAL DS    CL1                                                              
PRIPTVAL DS    CL1                                                              
PRIPOVAL DS    CL1                                                              
*                                                                               
FLDCNT   DS    XL1                                                              
COUNTER  DS    F                                                                
*                                                                               
SYSTEM   DS    CL1                                                              
SFLAGS   DS    CL1                                                              
PROGRAM  DS    CL1                                                              
PGNAME   DS    CL8                                                              
*                                                                               
IDNUM    DS    XL(L'CTIKNUM)                                                    
AGNUM    DS    XL(L'CTSYSAGB)                                                   
AGAID    DS    XL(L'CT5KALPH)                                                   
IDOPTS   DS    XL2                                                              
*                                                                               
ALLFLAG  DS    CL1                                                              
*                                                                               
GETSEQF  DS    XL1                                                              
IOCOUNT  DS    H                                                                
*                                                                               
SYSEL    DS    XL12                                                             
*                                                                               
AELTAB   DS    A                                                                
*                                                                               
COMMADRS DS    0A                                                               
VALPARS  DS    A                                                                
GETREC   DS    A                                                                
LINE     DS    A                                                                
VALPSYS  DS    A                                                                
VALPOID  DS    A                                                                
VALPTYP  DS    A                                                                
DISPOID  DS    A                                                                
DISPTYP  DS    A                                                                
EXTRACT  DS    A                                                                
         DS    21A                 SPARE                                        
*                                                                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLOCK3   DS    20CL32                                                           
BLOCK4   DS    20CL32                                                           
*                                                                               
DELKEY   DS    CL(L'IOKEY)                                                      
KEYSAVE  DS    CL(L'IOKEY)                                                      
*                                                                               
VLISTPTR DS    A                                                                
VLIST    DS    0CL160                                                           
DLIST    DS    CL80                                                             
OLIST    DS    CL80                                                             
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016CTGEN09X  05/01/02'                                      
         END                                                                    
