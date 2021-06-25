*          DATA SET CTGEN20    AT LEVEL 002 AS OF 08/22/00                      
*PHASE TA0B20A                                                                  
*&&      SET   NOP=N                                                            
         TITLE 'CTGEN20 - FILE MAINTENANCE - FAC RECORDS'                       
GEN20    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN20*,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CT3REC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
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
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF FACPAK RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING CT3REC,R2           R2=A(FACPAK RECORD KEY)                      
         XC    CT3KEY,CT3KEY                                                    
         MVI   CT3KTYP,CT3KTYPQ    RECORD TYPE "3"                              
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DISTYPH       VALIDATE SUB TYPE                            
         BNE   VALKEYX                                                          
         BAS   RE,VALSUB                                                        
         ICM   R1,15,APFULL        NO ENTRY (INVALID)                           
         BZ    VALKEYX                                                          
         MVC   CT3ASUB,0(R1)       OK FILL IN KEY                               
         MVC   SAVESUB,0(R1)       SAVE THIS AS WELL                            
*                                                                               
VK020    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DISNUMH       APPL/SYS NUMBER                              
         BNE   VK030                                                            
         TM    FVIIND,FVIHEX       MUST BE HEX                                  
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALKEYX                                                          
         GOTO1 VHEXIN,APPARM,FVIFLD,SAVENUM,2                                   
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
*                                                                               
         MVC   DISSYL,=C'System'   SET TEXT TO SYSTEM                           
         MVC   DISU1L,=C'Updative    '                                          
         MVC   DISU2L,=C'Applications'                                          
         MVC   DISR1L,=C'Read only   '                                          
         MVC   DISR2L,=C'Applications'                                          
*                                                                               
VK030    CLI   SAVESUB,C'A'                                                     
         BNE   VK040                                                            
*                                                                               
         MVC   DISSYL,=C'Applid'   SET TEXT TO APPLID                           
         MVC   DISU1L,=C'Appl LUID   '                                          
         MVC   DISU2L,=C'            '                                          
         MVC   DISR1L,=C'            '                                          
         MVC   DISR2L,=C'            '                                          
*                                                                               
         MVI   FVMINL,3                                                         
         GOTO1 AFVAL,DISSYSH       VALIDATE APPL ID                             
         BNE   VALKEYX                                                          
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BAS   RE,VALAPPL                                                       
         ICM   R1,15,APFULL        NO ENTRY (INVALID)                           
         BZ    VALKEYX                                                          
         MVC   SAVEAPL,4(R1)       SAVE APPL NUMBER                             
*                                                                               
VK040    CLI   SAVESUB,C'S'                                                     
         BNE   VK050                                                            
         GOTO1 AVALSE,DISSYSH      VALIDATE SE SYS                              
         BNE   VALKEYX                                                          
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         MVC   SAVESYS,APWORK      SAVE SE NUMBER                               
         B     VK052                                                            
*                                                                               
VK050    EQU   *                                                                
*                                                                               
VK051    MVC   CT3AAPPL,SAVEAPL    APPL KEY DETAIL                              
         CLI   SAVEAPL,0                                                        
         BNE   VK060                                                            
         MVC   CT3AAPPL,SAVENUM                                                 
         CLI   SAVENUM,0                                                        
         BNE   VK060                                                            
         LA    R1,DISSYSH          MISSING APPLID                               
         ST    R1,FVADDR                                                        
         B     VALKEYX                                                          
*                                                                               
VK052    MVC   CT3SSE,SAVESYS      SYSTEM KEY DETAIL                            
         CLI   SAVESYS,0                                                        
         BNE   VK060                                                            
         MVC   CT3SSE,SAVENUM                                                   
         CLI   SAVENUM,0                                                        
         BNE   VK060                                                            
         LA    R1,DISSYSH          MISSING SYSTEM ID                            
         ST    R1,FVADDR                                                        
         B     VALKEYX                                                          
         B     VK060                                                            
*                                                                               
VK060    MVC   APRECKEY(L'CT3KEY),CT3KEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
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
VK080    EQU   *                                                                
*                                                                               
VALKEYY  BAS   RE,DISPKEY                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  OI    FVOIND-FVIHDR+DISSYLH,FVOXMT                                     
         OI    FVOIND-FVIHDR+DISU1LH,FVOXMT                                     
         OI    FVOIND-FVIHDR+DISU2LH,FVOXMT                                     
         OI    FVOIND-FVIHDR+DISR1LH,FVOXMT                                     
         OI    FVOIND-FVIHDR+DISR2LH,FVOXMT                                     
         CLI   CT3ASUB,0                                                        
         BE    EXIT                                                             
         LA    R1,CT3ASUB                                                       
         BAS   RE,DISSUB           DISPLAY SUBID                                
         L     R1,APPARM                                                        
         MVC   APWORK(8),0(R1)                                                  
         GOTO1 DISPFLD,DISTYPH                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A FACPAK RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   CT3KEY,APRECKEY     GET KEY                                      
*                                                                               
         XC    CT3REC(256),CT3REC  INITIALISE RECORD                            
         MVC   CT3KEY,APRECKEY                                                  
         LA    R0,CT3DATA+1-CT3REC                                              
         STCM  R0,3,CT3LEN                                                      
*                                                                               
VR010    LA    R3,APELEM                                                        
         USING CTDSCD,R3           R3=A(DESCRIPTION ELEMENT)                    
         XC    APELEM,APELEM                                                    
         MVI   CTDSCEL,CTDSCELQ                                                 
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DISDS1H       DESCRIPTION TEXT                             
         BNE   VALRECX                                                          
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTDSC(0),FVIFLD                                                  
         LA    R1,3(R1)            ADD FIXED LEN +1 FOR EXECUTABLE              
         STC   R1,CTDSCLEN                                                      
*                                                                               
         GOTO1 AADDELS,CT3REC                                                   
*                                                                               
         CLI   CT3SSUB,CT3SYSQ                                                  
         BNE   VR040                                                            
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DISUP1H       UPDATIVE APPLICATIONS                        
         BNE   VR030                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         MVI   APBYTE,CTAPPUPQ                                                  
         BAS   RE,VALLIST                                                       
         BNE   VALRECX                                                          
*                                                                               
VR030    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DISRD1H       READ ONLY APPLICATIONS                       
         BNE   VR990                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         MVI   APBYTE,CTAPPRDQ                                                  
         BAS   RE,VALLIST                                                       
         BNE   VALRECX                                                          
         DROP  R3                                                               
         B     VR990                                                            
*                                                                               
VR040    LA    R3,APELEM                                                        
         USING CTAPLD,R3           R3=A(LUID ELEMENT)                           
         XC    APELEM,APELEM                                                    
         MVI   CTAPLEL,CTAPLELQ                                                 
         MVI   CTAPLLEN,11                                                      
         MVC   CTAPLID,SPACES                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL,DISUP1H       APPLID TEXT                                  
         BNE   VALRECX                                                          
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTAPLID(0),FVIFLD                                                
         GOTO1 AADDELS,CT3REC                                                   
*                                                                               
VR990    GOTO1 ASETACT,CT3REC      DEFINE ACTIVITY ELEMENT                      
*                                                                               
         LA    R1,IOADD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF FACPAK RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         BAS   RE,DISPKEY                                                       
         B     EXIT                                                             
*                                                                               
DISPKEY  NTR1                                                                   
*                                                                               
DISK010  LA    R1,CT3ASUB                                                       
         BAS   RE,DISSUB           DISPLAY SUBID                                
         L     R1,APPARM                                                        
         MVC   APWORK(8),0(R1)                                                  
         GOTO1 DISPFLD,DISTYPH                                                  
*                                                                               
         CLI   CT3ASUB,C'S'                                                     
         BE    DISK030                                                          
*                                                                               
DISK020  LA    R1,CT3AAPPL                                                      
         BAS   RE,DISAPPL          DISPLAY APPLID                               
         L     R1,APPARM                                                        
         MVC   APWORK(8),SPACES                                                 
         MVC   APWORK(4),0(R1)                                                  
         GOTO1 DISPFLD,DISSYSH                                                  
*                                                                               
         GOTO1 VHEXOUT,APPARM,CT3AAPPL,APWORK,2                                 
         GOTO1 DISPFLD,DISNUMH                                                  
         B     DISKEYX                                                          
*                                                                               
DISK030  GOTO1 ADISSE,CT3SSE       DISPLAY SE SYS                               
         GOTO1 DISPFLD,DISSYSH                                                  
*                                                                               
         GOTO1 VHEXOUT,APPARM,CT3SSE,APWORK,2                                   
         GOTO1 DISPFLD,DISNUMH                                                  
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY FACPAK RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC DISDS1H                                                          
*                                                                               
DREC020  XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTDSCD,R3           R3=A(DESCRIPTION ELEMENT)                    
         MVI   CTDSCEL,CTDSCELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DISRECX                                                          
*                                                                               
         MVC   APWORK(60),SPACES                                                
         SR    R1,R1               OUTPUT DESCRIPTION                           
         IC    R1,CTDSCLEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),CTDSC                                                  
         GOTO1 DISPFLD,DISDS1H                                                  
*                                                                               
         CLI   CT3ASUB,C'S'                                                     
         BE    DISR030                                                          
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTAPLD,R3           R3=A(APPL LUID ELEMENT)                      
         MVI   CTAPLEL,CTAPLELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DISRECX                                                          
*                                                                               
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(8),CTAPLID                                                
         GOTO1 DISPFLD,DISUP1H                                                  
         B     DISRECX                                                          
*                                                                               
DISR030  MVI   APBYTE,CTAPPUPQ                                                  
         BAS   RE,DISLIST                                                       
         GOTO1 DISPFLD,DISUP1H                                                  
*                                                                               
         MVI   APBYTE,CTAPPRDQ                                                  
         BAS   RE,DISLIST                                                       
         GOTO1 DISPFLD,DISRD1H                                                  
         B     DISRECX                                                          
*                                                                               
DISRECX  GOTO1 ADISACT,CT3REC      DISPLAY ACTIVITY DATE                        
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A FACPAK RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CT3REC,R2                                                        
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CT3REC                                                   
         OI    CT3STAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED FACPAK RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CT3REC                                                   
         NI    CT3STAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         XC    CT3KEY,CT3KEY                                                    
         XC    SELKEY,SELKEY                                                    
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTTYPH       VALIDATE SUB TYPE                            
         BNE   VALSELX                                                          
         BAS   RE,VALSUB                                                        
         ICM   R1,15,APFULL        NO ENTRY (INVALID)                           
         BZ    VALSELX                                                          
         MVC   SELSUB,0(R1)        OK FILL IN KEY                               
*                                                                               
VS030    MVI   FVMINL,3                                                         
         GOTO1 AFVAL,LSTAPPH       VALIDATE APPL ID                             
         BNE   VS070                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BAS   RE,VALAPPL                                                       
         ICM   R1,15,APFULL        NO ENTRY (INVALID)                           
         BZ    VALSELX                                                          
         MVC   SELAPL,4(R1)        SAVE APPL NUMBER                             
*                                                                               
VS070    EQU   *                   BUILD AN INITIAL KEY                         
*                                                                               
         MVI   CT3KTYP,CT3KTYPQ                                                 
         MVC   CT3ASUB,SELSUB                                                   
         MVI   GETFLAG,C'H'        START WITH READHI                            
*                                                                               
VALSELY  CLI   CT3ASUB,0                                                        
         BE    VALSELY1                                                         
         LA    R1,CT3ASUB                                                       
         BAS   RE,DISSUB           DISPLAY SUBID                                
         L     R1,APPARM                                                        
         MVC   APWORK(8),0(R1)                                                  
         GOTO1 DISPFLD,LSTTYPH                                                  
*                                                                               
VALSELY1 MVC   FVMSGNO,=AL2(FVFOK)                                              
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
         USING CT3REC,R2                                                        
GETSEL   LA    R2,IOKEY            READ NEXT LIST RECORD                        
         MVC   CT3KEY,APRECKEY     FROM LAST SAVED KEY                          
*                                                                               
GSEL02   TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    *+8                                                              
         MVI   GETFLAG,C'H'        FLAG FOR READHI NEXT                         
*                                                                               
         GOTO1 GETREC                                                           
         BNE   GETSELN                                                          
         B     GETSELY                                                          
*                                                                               
GETSELY  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CT3KEY),CT3KEY                                        
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
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
DIS020   CLI   CT3ASUB,C'S'                                                     
         BNE   DIS030                                                           
         MVC   LSTHDR1(50),HEADSY1                                              
         MVC   LSTHDR2(50),HEADSY2                                              
         GOTO1 ADISSE,CT3SSE       DISPLAY SE SYS                               
         MVC   LISTSYS,APWORK                                                   
*                                                                               
         GOTO1 VHEXOUT,APPARM,CT3SSE,LISTNUM,1                                  
*                                                                               
         MVI   APBYTE,CTAPPUPQ                                                  
         BAS   RE,DISLIST                                                       
         MVC   LISTUPA,APWORK                                                   
*                                                                               
         MVI   APBYTE,CTAPPRDQ                                                  
         BAS   RE,DISLIST                                                       
         MVC   LISTROA,APWORK                                                   
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTDSCD,R3           R3=A(DESCRIPTION ELEMENT)                    
         MVI   CTDSCEL,CTDSCELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DISSELX                                                          
*                                                                               
         MVC   LISTDES,SPACES                                                   
         SR    R1,R1               OUTPUT DESCRIPTION                           
         IC    R1,CTDSCLEN                                                      
         SH    R1,=H'3'                                                         
         CH    R1,=Y(L'LISTDES-1)                                               
         BNH   *+8                                                              
         LH    R1,=Y(L'LISTDES-1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTDES(0),CTDSC                                                 
         B     DISSELX                                                          
*                                                                               
DIS030   MVC   LSTHDR1(50),HEADAP1                                              
         MVC   LSTHDR2(50),HEADAP2                                              
         LA    R1,CT3AAPPL                                                      
         BAS   RE,DISAPPL          DISPLAY APPLID                               
         L     R1,APPARM                                                        
         MVC   LISTSYS(4),0(R1)                                                 
*                                                                               
         GOTO1 VHEXOUT,APPARM,CT3AAPPL,LISTNUM,1                                
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTAPLD,R3           R3=A(APPL LUID ELEMENT)                      
         MVI   CTAPLEL,CTAPLELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DIS990                                                           
*                                                                               
         MVC   APWORK(20),SPACES                                                
         MVC   APWORK(8),CTAPLID                                                
         MVC   LISTUPA,APWORK                                                   
*                                                                               
DIS990   XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTDSCD,R3           R3=A(DESCRIPTION ELEMENT)                    
         MVI   CTDSCEL,CTDSCELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DISSELX                                                          
*                                                                               
         MVC   LISTDS1,SPACES                                                   
         SR    R1,R1               OUTPUT DESCRIPTION                           
         IC    R1,CTDSCLEN                                                      
         SH    R1,=H'3'                                                         
         CH    R1,=Y(L'LISTDS1-1)                                               
         BNH   *+8                                                              
         LH    R1,=Y(L'LISTDS1-1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTDS1(0),CTDSC                                                 
                                                                                
DISSELX  EQU   *                                                                
         OI    FVOIND-FVIHDR+LSTHDR1H,FVOXMT                                    
         OI    FVOIND-FVIHDR+LSTHDR2H,FVOXMT                                    
         B     EXIT                                                             
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
VALREQ   L     R9,AREP                                                          
*                                                                               
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         XC    SELKEY,SELKEY       SELECTION CRITERION                          
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
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPTYPH       VALIDATE SUB TYPE                            
         BNE   VR060                                                            
         BAS   RE,VALSUB                                                        
         ICM   R1,15,APFULL        NO ENTRY (INVALID)                           
         BZ    VALSELX                                                          
         MVC   SELSUB,0(R1)        OK FILL IN KEY                               
*                                                                               
VR060    MVI   FVMINL,3                                                         
         GOTO1 AFVAL,REPAPPH       VALIDATE APPL ID                             
         BNE   VR070                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BAS   RE,VALAPPL                                                       
         ICM   R1,15,APFULL        NO ENTRY (INVALID)                           
         BZ    VALSELX                                                          
         MVC   SELAPL,4(R1)        SAVE APPL NUMBER                             
*                                                                               
VR070    MVC   FVMSGNO,=AL2(FVFOK) BUILD AN INITIAL KEY                         
*                                                                               
         LA    R2,APRECKEY                                                      
         MVI   CT3KTYP,CT3KTYPQ                                                 
         MVC   CT3ASUB,SELSUB                                                   
         MVI   GETFLAG,C'H'        START WITH READHI                            
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE FACPAK REPORT                                   *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   IOKEY,APRECKEY      SET INITIAL KEY VALUE                        
*                                                                               
PR010    GOTO1 GETREC              GET FIRST/NEXT RECORD                        
         BNE   PRTREPX                                                          
*                                                                               
PR020    L     R2,AIOAREA1                                                      
         LA    R1,CT3ASUB                                                       
         BAS   RE,DISSUB           DISPLAY SUBID                                
         L     R1,APPARM                                                        
         MVC   PRTTYP(8),0(R1)                                                  
         CLI   0(R1),C'S'                                                       
         BNE   PR030                                                            
*                                                                               
         CLI   SYSFIRST,C'N'       FORCE NEW PAGE ON FIRST SYSTEM               
         BE    *+8                                                              
         OI    REPHEADI,REPHFRCE                                                
         MVI   SYSFIRST,C'N'                                                    
*                                                                               
         GOTO1 ADISSE,CT3SSE       DISPLAY SE SYS                               
         MVC   PRTSYS,APWORK                                                    
         B     PR040                                                            
*                                                                               
PR030    LA    R1,CT3AAPPL                                                      
         BAS   RE,DISAPPL          DISPLAY APPLID                               
         L     R1,APPARM                                                        
         MVC   PRTSYS(4),0(R1)                                                  
*                                                                               
PR040    GOTO1 VHEXOUT,APPARM,CT3SSE,PRTNUM,1                                   
*                                                                               
         MVI   APBYTE,CTAPPUPQ                                                  
         BAS   RE,DISLIST                                                       
         MVC   PRTUPS,APWORK                                                    
*                                                                               
         MVI   APBYTE,CTAPPRDQ                                                  
         BAS   RE,DISLIST                                                       
         MVC   PRTROS,APWORK                                                    
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTAPLD,R3           R3=A(APPL LUID ELEMENT)                      
         MVI   CTAPLEL,CTAPLELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    PR050                                                            
*                                                                               
         MVC   APWORK(20),SPACES                                                
         MVC   APWORK(8),CTAPLID                                                
         MVC   PRTUPS,APWORK                                                    
*                                                                               
PR050    XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTDSCD,R3           R3=A(DESCRIPTION ELEMENT)                    
         MVI   CTDSCEL,CTDSCELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    PRTREPO                                                          
*                                                                               
         MVC   PRTDES,SPACES                                                    
         SR    R1,R1               OUTPUT DESCRIPTION                           
         IC    R1,CTDSCLEN                                                      
         SH    R1,=H'3'                                                         
         CH    R1,=Y(L'PRTDES-1)                                                
         BNH   *+8                                                              
         LH    R1,=Y(L'PRTDES-1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTDES(0),CTDSC                                                  
*                                                                               
PRTREPO  GOTO1 VREPORT,REPD        PRINT MESSAGE FIRST                          
         DROP  R3                                                               
         B     PR010                                                            
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS       *          
**********************************************************************          
         SPACE 1                                                                
         USING CT3REC,R2                                                        
GETREC   NTR1                                                                   
*                                                                               
         CLI   GETFLAG,C'S'        SEQ READ NEXT                                
         BE    GETRECSQ            CHECK SEQUENCE BROKEN                        
*                                                                               
         MVC   IOKEY(L'CT3KEY),CT3KEY                                           
         CLI   GETFLAG,C'H'                                                     
         BE    GETRECHI                                                         
*                                                                               
GETRECRD LA    R1,IOCONFIL+IORD+IO1                                             
         B     GETRECIO                                                         
*                                                                               
GETRECHI LA    R1,IOCONFIL+IOHI+IO1                                             
         B     GETRECIO                                                         
*                                                                               
GETRECSQ LA    R1,IOCONFIL+IOSQ+IO1                                             
         B     GETRECIO                                                         
*                                                                               
GETRECIO LA    R2,IOKEY                                                         
         GOTO1 AIO                                                              
         BNE   GETRECN                                                          
         MVI   GETFLAG,C'S'        SET FOR SEQ NEXT                             
         L     R2,AIOAREA1                                                      
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(1),CT3KEY                                               
         BNE   GETRECN                                                          
         CLI   IOKEYSAV+1,0                                                     
         BE    GETRECY                                                          
         CLC   IOKEYSAV+1(1),CT3KEY+1                                           
         BNE   GETRECN                                                          
*                                                                               
         CLI   SELAPL,0                                                         
         BE    GETRECY                                                          
*                                                                               
         LA    R3,APELEM                                                        
         USING CTAPPD,R3           R3=A(APPLICATION ELEMENT)                    
         MVI   CTAPPEL,CTAPPELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECSQ                                                         
*                                                                               
GETR010  TM    CTAPPFLG,CTAPPUPQ   TEST CORRECT FLAG                            
         BNO   GETR020                                                          
*                                                                               
         CLC   CTAPPNUM,SELAPL     TEST APPL                                    
         BE    GETRECY                                                          
*                                                                               
GETR020  BAS   RE,NEXTEL           DO NEXT                                      
         BNE   GETRECSQ                                                         
         BE    GETR010                                                          
*                                                                               
GETRECY  CR    RC,RC               RETURN CC EQUAL RECORD OK                    
         B     EXIT                                                             
*                                                                               
GETRECN  LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         B     EXIT                                                             
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
         BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         EJECT                                                                  
***********************************************************************         
* FIND NEXT ELEMENT OF SAME CODE WITHIN RECORD                        *         
* NTRY R3=A(CURRENT ELEMENT)                                          *         
*      APELEM = ELCODE TO FIND                                        *         
* EXIT CC EQ - FOUND - R3=A(NEW ELEMENT)                              *         
*      CC NE - NOT FOUND                                              *         
***********************************************************************         
         SPACE 1                                                                
NEXTEL   ZIC   RF,1(R3)            L'ELEMENT                                    
         AR    R3,RF               A(NEXT ELEMNT)                               
         ICM   RF,1,1(R3)          L'ELEMENT                                    
         BNZ   *+8                                                              
         LTR   RB,RB               FORCE CC NON ZERO                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM                                                   
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        DISPLAY APPL IDS ADV1,ADV2,ADV3,ADV4               *                   
*        VALUES SET TO APWORK RECORD MUST BE AT R2          *                   
*************************************************************                   
         SPACE 1                                                                
DISLIST  NTR1                                                                   
         MVC   APWORK,SPACES       CLEAR OUTPUT AREA                            
         XC    APELEM,APELEM                                                    
         LA    R9,APWORK                                                        
*                                                                               
         LA    R3,APELEM                                                        
         USING CTAPPD,R3           R3=A(APPLICATION ELEMENT)                    
         MVI   CTAPPEL,CTAPPELQ                                                 
         GOTO1 AGETELS,CT3REC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DISLISX                                                          
*                                                                               
DISL010  CLC   CTAPPFLG,APBYTE     TEST CORRECT FLAG                            
         BNE   DISL020                                                          
*                                                                               
         LA    R1,CTAPPNUM         INSERT APPLICATION NAME                      
         BAS   RE,DISAPPL                                                       
         L     R1,APPARM                                                        
         MVC   0(4,R9),0(R1)                                                    
         LA    R9,3(R9)                                                         
         CLI   0(R9),C' '                                                       
         BNH   *+8                                                              
         LA    R9,1(R9)                                                         
         MVI   0(R9),C','                                                       
         LA    R9,1(R9)                                                         
*                                                                               
DISL020  BAS   RE,NEXTEL           DO NEXT                                      
         BNE   DISLISX                                                          
         BE    DISL010                                                          
*                                                                               
DISLISX  BCTR  R9,0                                                             
         CLI   0(R9),C','                                                       
         BNE   *+8                                                              
         MVI   0(R9),C' '                                                       
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE APPL IDS ADV1,ADV2,ADV3,ADV4              *                   
*        VALUES SET BY FVAL RECORD MUST BE AT R2            *                   
*************************************************************                   
         SPACE 1                                                                
VALLIST  NTR1                                                                   
         GOTO1 VSCANNER,APPARM,FVADDR,MYBLOCK                                   
         SR    R8,R8                                                            
         ICM   R8,1,APPARM+4       R8 = NUMBER OF APPLS                         
         BZ    VALLISN                                                          
         LA    R9,MYBLOCK          R9 = A(SCAN BLOCK)                           
*                                                                               
VALL010  MVC   FVIFLD,12(R9)       PUT APPL INTO FIELD TO VALIDATE              
         BAS   RE,VALAPPL                                                       
         ICM   R1,15,APFULL                                                     
         BZ    VALLISN                                                          
*                                                                               
         LA    R3,APELEM                                                        
         USING CTAPPD,R3           R3=A(APPLICATION ELEMENT)                    
         XC    APELEM,APELEM                                                    
         MVI   CTAPPEL,CTAPPELQ                                                 
         MVI   CTAPPLEN,CTAPLLNQ                                                
         MVC   CTAPPFLG,APBYTE     SET RD OR UPDATE                             
         MVC   CTAPPNUM,4(R1)                                                   
*                                                                               
         GOTO1 AADDELS,CT3REC      ADD THIS THEN DO NEXT                        
         LA    R9,32(R9)                                                        
         BCT   R8,VALL010                                                       
         B     VALLISX                                                          
*                                                                               
VALLISN  LTR   RB,RB                                                            
         B     EXIT                                                             
VALLISX  CR    RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY APPL ID                                    *                   
*        NUMBER AT 0(R1)      EXIT APPARM=A(APPLID)         *                   
*************************************************************                   
         SPACE 1                                                                
DISAPPL  EQU   *                                                                
         XC    APPARM,APPARM       CLEAR RETURN AREA                            
         LA    RF,FACIDTAB                                                      
DISAPP1  CLC   4(1,RF),0(R1)       TEST APPL ID NUMBER                          
         BNE   *+12                                                             
         ST    RF,APPARM           SAVE A(TABLE ENT)                            
         B     DISAPPX                                                          
*                                                                               
         LA    RF,L'FACIDTAB(RF)   NEXT                                         
         CLI   0(RF),EOT                                                        
         BNE   DISAPP1                                                          
         DC    H'0'                                                             
*                                                                               
DISAPPX  BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        VALIDATE APPL ID                                   *                   
*        VALUES SET BY FVAL   EXIT APFULL=A(FACIDTAB ENTRY) *                   
*************************************************************                   
         SPACE 1                                                                
VALAPPL  EQU   *                                                                
         XC    APFULL,APFULL       CLEAR RETURN AREA                            
         LA    R1,FACIDTAB                                                      
VALAPP1  CLC   FVIFLD(4),0(R1)     TEST APPL ID NAME                            
         BNE   *+12                                                             
         ST    R1,APFULL           SAVE A(TABLE ENT)                            
         B     VALAPPX                                                          
*                                                                               
         LA    R1,L'FACIDTAB(R1)   NEXT                                         
         CLI   0(R1),EOT                                                        
         BNE   VALAPP1                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VALAPPX  BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        DISPLAY SUB TYPE                                   *                   
*        CHR AT 0(R1)      EXIT APPARM=A(TYPE)              *                   
*************************************************************                   
         SPACE 1                                                                
DISSUB   EQU   *                                                                
         LA    RF,MTYPTAB          SUB TYPE TAB                                 
DISSUB1  CLC   0(1,R1),0(RF)       TEST INPUT                                   
         BNE   *+12                                                             
         ST    RF,APPARM           SAVE A(TABLE ENT)                            
         B     DISSUBX                                                          
*                                                                               
         LA    RF,L'MTYPTAB(RF)    NEXT TABLE ENTRY                             
         CLI   0(R1),EOT                                                        
         BNE   DISSUB1                                                          
         DC    H'0'                                                             
DISSUBX  BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        VALIDATE SUB TYPE                                  *                   
*        VALUES SET BY FVAL  EXIT APFULL=A(TABLE ENTRY)     *                   
*************************************************************                   
         SPACE 1                                                                
VALSUB   EQU   *                                                                
         LA    R1,MTYPTAB          SUB TYPE TAB                                 
         ZIC   RF,FVXLEN                                                        
VALSUB1  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(R1)     TEST INPUT                                   
         BNE   *+12                                                             
         ST    R1,APFULL           SAVE A(TABLE ENT)                            
         B     VALSUBX                                                          
*                                                                               
         LA    R1,L'MTYPTAB(R1)    NEXT TABLE ENTRY                             
         CLI   0(R1),EOT                                                        
         BNE   VALSUB1                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VALSUBX  BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS TABLES & LITERAL POOL                    *                   
*************************************************************                   
         SPACE 1                                                                
MTYPTAB  DS    0CL8                TABLE OF SUB TYPES                           
         DC    CL8'SELIST'                                                      
         DC    CL8'APPLID'                                                      
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
       ++INCLUDE FACIDTAB          TABLE OF APPL IDS                            
         SPACE 1                                                                
SPACES   DC    CL80' '                                                          
*                                                                               
HEADSY1  DC    CL50'Act System  No Appls R/W Appls R/O Description    '         
HEADSY2  DC    CL50'--- ------- -- --------- --------- ---------------'         
HEADAP1  DC    CL50'Act Applid  No Appl LUID Description              '         
HEADAP2  DC    CL50'--- ------- -- --------- -------------------------'         
         SPACE 1                                                                
         EJECT                                                                  
REPDESCL DC    C'FACPAK LIST'                                                   
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'Facpak record list'                                      
         SPEC  H2,57,C'------------------'                                      
         SPEC  M1,1,C'Record  Appl/      Appl LUID/'                            
         SPEC  M2,1,C'Type    System  No Updative systems Read only sysX        
               tems Description '                                               
         SPEC  M3,1,C'------- ------- -- ---------------- -------------X        
               ---- ------------'                                               
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* FAGETTXTD                                                                     
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENA7D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB7D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENBBD                                                       
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL74                                                            
LISTSYS  DS    CL7                 SYSTEM                                       
         DS    CL1                                                              
LISTNUM  DS    CL2                 NUMBER                                       
         DS    CL1                                                              
LISTUPA  DS    CL9                 UPDATIVE                                     
         DS    CL1                                                              
LISTDS1  DS    0CL53                                                            
*                                                                               
LISTROA  DS    CL9                 READ ONLY                                    
         DS    CL1                                                              
LISTDES  DS    CL43                DESCRIPTION                                  
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)                                                     
PRTTYP   DS    CL7                                                              
         DS    CL1                                                              
PRTSYS   DS    CL7                                                              
         DS    CL1                                                              
PRTNUM   DS    CL2                                                              
         DS    CL1                                                              
PRTUPS   DS    CL16                                                             
         DS    CL1                                                              
PRTROS   DS    CL17                                                             
         DS    CL1                                                              
PRTDES   DS    CL(L'REPP1-(PRTDES-REPP1))                                       
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
WORK     DS    XL32                                                             
*                                                                               
SELKEY   DS    0XL25                                                            
SELSUB   DS    XL1                 FAC RECORD TYPE                              
SELNUM   DS    XL1                 APPL/SYS NUMBER                              
SELSYS   DS    XL1                 SE SYSTEM                                    
SELAPL   DS    XL2                 APPLICATION                                  
         ORG   SELKEY+L'SELKEY                                                  
*                                                                               
SAVESUB  DS    X                                                                
SAVESYS  DS    X                                                                
SAVEAPL  DS    X                                                                
SAVENUM  DS    X                                                                
*                                                                               
MYBLOCK  DS    CL512               USE FOR SCANBLOCK                            
*                                                                               
GETFLAG  DS    X                                                                
SYSFIRST DS    X                                                                
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTGEN20   08/22/00'                                      
         END                                                                    
