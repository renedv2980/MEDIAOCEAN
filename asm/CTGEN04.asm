*          DATA SET CTGEN04    AT LEVEL 048 AS OF 09/02/03                      
*PHASE TA0B04A                                                                  
*                                                                               
         TITLE 'CTGEN04 - EASY LINK STATION RECORD MAINTENANCE'                 
GEN04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN4**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CT3KEY,R2           R2=A(RECORD KEY)                             
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
         B     EXIT                APMPROC                                      
         B     EXIT                APMFSCR                                      
         B     LSTSCR              APMLSCR                                      
         B     VALREQ              APMVALQ                                      
         B     PRTREP              APMREPP                                      
         B     SETTWA              APMSETT                                      
         B     EXIT                APMPUTK                                      
         B     VALREC              APMNEWK                                      
         B     EXIT                APMFRP                                       
         B     EXIT                APMDISS2                                     
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
         SPACE 1                                                                
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF EASY LINK STATION RECORD                *          
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    CT3KEY,CT3KEY                                                    
         MVI   CT3KTYP,CT3KTYPQ                                                 
         MVI   CT3ESUB,CT3EASIQ                                                 
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,EASEMEDH                                                   
         BNE   VALKEYX                                                          
         MVC   CT3EMED,EASEMED                                                  
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,EASESTAH                                                   
         BNE   VALKEYX                                                          
*                                                                               
         OC    ASTAVAL,ASTAVAL                                                  
         BNZ   VKEY010                                                          
         GOTO1 VCOLY,APPARM,0,X'D9000A68'                                       
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ASTAVAL                                                       
*                                                                               
         USING STABLKD,R4                                                       
VKEY010  XC    STABLK,STABLK                                                    
         LA    R4,STABLK                                                        
         LA    RF,EASESTAH                                                      
         ST    RF,STBADDR                                                       
         MVC   STBMED,EASEMED                                                   
         MVI   STBCTRY,C'U'                                                     
         MVC   STBACOM,ACPARM+16                                                
*                                                                               
         GOTO1 ASTAVAL,APPARM,STABLK                                            
         CLI   STBERR,0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
         OC    STBSTA,STBSTA                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
         MVC   CT3ESTAT,STBSTA                                                  
         DROP  R4                                                               
*                                                                               
VKEY020  MVC   APRECKEY(L'CT3KEY),CT3KEY                                        
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
* ROUTINE TO ADD OR CHANGE AN EASY LINK STATION RECORD                *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         XC    CT3REC(256),CT3REC  CLEAR KEY ETC.                               
         MVC   CT3KEY,APRECKEY                                                  
         LA    R0,CT3DATA+1-CT3REC                                              
         STCM  R0,3,CT3LEN                                                      
         XC    APELEM,APELEM                                                    
*                                  UPDATE RECORD                                
         GOTO1 ASETACT,CT3REC                                                   
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         CLI   APACTN,ACTADD                                                    
         BNE   *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
VALRECY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF EASY LINK STATION RECORD                  *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   EASEMED,CT3EMED                                                  
         MVC   EASESTA(4),CT3ESTAT                                              
         LA    RF,EASESTA+3                                                     
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         MVC   0(1,RF),CT3ESTAT+4                                               
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY EASY LINK STATION RECORD DATA                    *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC EASTENDH                                                         
*                                                                               
DISPX    GOTO1 ADISACT,CT3REC      DISPLAY ACTIVITY DATE                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN EASY LINK STATION RECORD DATA                  *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CT3REC                                                   
         OI    CT3STAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE AN EASY LINK STATION RECORD DATA                 *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CT3REC                                                   
         NI    CT3STAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS FOR AN EASY LINK STAT. RECORD *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         SET UP FIRST LIST RECORD KEY                 
         XC    CT3KEY,CT3KEY                                                    
         MVI   CT3KTYP,CT3KTYPQ                                                 
         MVI   CT3ESUB,CT3EASIQ                                                 
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTEMEDH                                                   
         BNE   VSEL020                                                          
         MVC   CT3EMED,LSTEMED                                                  
*                                                                               
         GOTO1 AFVAL,LSTESTAH                                                   
         BNE   VSEL020                                                          
*                                                                               
         OC    ASTAVAL,ASTAVAL                                                  
         BNZ   VSEL010                                                          
         GOTO1 VCOLY,APPARM,0,X'D9000A68'                                       
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ASTAVAL                                                       
*                                                                               
         USING STABLKD,R4                                                       
VSEL010  XC    STABLK,STABLK                                                    
         LA    R4,STABLK                                                        
         LA    RF,LSTESTAH                                                      
         ST    RF,STBADDR                                                       
         MVC   STBMED,EASEMED                                                   
         MVI   STBCTRY,C'U'                                                     
         MVC   STBACOM,ACPARM+16                                                
*                                                                               
         GOTO1 ASTAVAL,APPARM,STABLK                                            
         CLI   STBERR,0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
         MVC   CT3ESTAT,STBSTA                                                  
*                                                                               
VSEL020  XC    SELKEY,SELKEY       CLEAR SELECT DATA FIELDS                     
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  XC    APPARM(8),APPARM                                                 
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST-SELECT RECORD FOR EASY LINK STATION RECORDS           *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         MVC   CT3KEY,APRECKEY                                                  
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
         CLI   CT3KTYP,CT3KTYPQ                                                 
         BNE   GETSELN                                                          
         CLI   CT3ESUB,CT3EASIQ                                                 
         BNE   GETSELN                                                          
         CLC   CT3EMED,LSTEMED                                                  
         BNE   GETSELN                                                          
*                                                                               
GETSELK  DS    0H                  RECORD SELECTED                              
*                                                                               
GETSELY  MVC   APRECKEY(L'CT3KEY),CT3KEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE FOR EASY LINK STATION RECORDS              *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTSTAT(4),CT3ESTAT                                             
         LA    RF,LISTSTAT+3                                                    
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         MVC   0(1,RF),CT3ESTAT+4                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN FOR EASI RECORDS          *         
***********************************************************************         
         SPACE 1                                                                
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
         LA    R2,APRECKEY         SET UP INITIAL  KEY                          
         XC    CT3KEY,CT3KEY                                                    
         MVI   CT3KTYP,CT3KTYPQ                                                 
         MVI   CT3ESUB,CT3EASIQ                                                 
*                                                                               
         GOTO1 AFVAL,REPESTAH                                                   
         BNE   VREQ100                                                          
         OC    ASTAVAL,ASTAVAL                                                  
         BNZ   VREQ010                                                          
         GOTO1 VCOLY,APPARM,0,X'D9000A68'                                       
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ASTAVAL                                                       
*                                                                               
         USING STABLKD,R4                                                       
VREQ010  XC    STABLK,STABLK                                                    
         LA    R4,STABLK                                                        
         LA    RF,REPESTAH                                                      
         ST    RF,STBADDR                                                       
         MVC   STBMED,EASEMED                                                   
         MVI   STBCTRY,C'U'                                                     
         MVC   STBACOM,ACPARM+16                                                
*                                                                               
         GOTO1 ASTAVAL,APPARM,STABLK                                            
         CLI   STBERR,0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREQX                                                          
         MVC   CT3ESTAT,STBSTA                                                  
*                                                                               
VREQ100  MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT EASY LINK STATION RECORD REPORT                    *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
         MVC   CT3KEY,APRECKEY     SET INITIAL KEY VALUE                        
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     PRTREP2                                                          
*                                  GET RECORD (SEQUENCE IS BROKEN)              
PRTREP1  LA    R2,IOKEY                                                         
         MVC   CT3KEY,APRECKEY                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PRTREPX                                                          
         LA    R1,IOSQ+IOCONFIL+IO1                                             
PRTREP2  GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
         L     R2,AIOAREA1                                                      
         CLI   CT3KTYP,CT3KTYPQ    TEST IF AN EASI TYPE RECORD                  
         BNE   PRTREPX                                                          
         CLI   CT3ESUB,CT3EASIQ                                                 
         BNE   PRTREPX                                                          
         MVC   APRECKEY(L'CT3KEY),CT3KEY                                        
         MVC   LINESTAT(4),CT3ESTAT                                             
         LA    RF,LINESTAT+3                                                    
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         MVC   0(1,RF),CT3ESTAT+4                                               
*                                  PRINT REPORT LINE                            
         GOTO1 VREPORT,REPD                                                     
         B     PRTREP1                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MEDSEQ   EQU   X'04'               MEDIA SYSTEM CODE NUMBER                     
ACCSEQ   EQU   X'06'               ACCOUNT SYSTEM CODE NUMBER                   
*                                                                               
CTFILE   DC    CL8'CTFILE  '                                                    
*                                                                               
REPDESCL DC    C'EASY LINK STATION LIST'                                        
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'EASY LINK STATION LIST'                                  
         SPEC  H2,57,C'----------------------'                                  
         SPEC  M1,1,C'STATION'                                                  
         SPEC  M2,1,C'-------'                                                  
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
* SPSTABLK                                                                      
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENF3D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGEND3D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB3D                                                       
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL(L'LSTACT1)       ACTION FIELD                                 
LISTLINH DS    XL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTSTAT DS    CL7                 EASY LINK STATION CODE                       
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
LINESTAT DS    CL7                 EASY LINK STATION CODE                       
         DS    CL2                                                              
TWAD     DSECT                                                                  
         ORG   SAVOVER                                                          
         EJECT                                                                  
         DS    0D                                                               
ASTAVAL  DS    A                                                                
         SPACE 1                                                                
SAVCLRL  EQU   *-SAVOVER                                                        
         SPACE 1                                                                
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
PARMS    DS    8F                                                               
SELKEY   DS    0XL6                SELECT KEY PARAMETERS                        
SELSTAT  DS    CL6                 EASY LINK STATION CODE                       
         ORG   SELKEY+L'SELKEY                                                  
VSCINKEY DS    V                   UNSCAN ROUTINE ADDRESS                       
DDSACC   DS    XL1                 DDS ACCESS LEVEL FLAG                        
FLDCNT   DS    X                   SUB-FIELD COUNT                              
LISTBUFF DS    CL256               SYSTEM INFO DISPLAY BUFFER                   
STABLK   DS    CL(STBLNQ)          STABLK AREA                                  
*                                                                               
OVSYS    DS    XL256                                                            
BLOCKI   DS    20CL32              SCAN BLOCK                                   
         ORG   BLOCKI                                                           
BLOCKO   DS    20CL20              UNSCAN BLOCK                                 
         ORG                                                                    
*                                                                               
LOCALX   EQU   *                   END OF LOCAL WORKING STORAGE                 
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048CTGEN04   09/02/03'                                      
         END                                                                    
