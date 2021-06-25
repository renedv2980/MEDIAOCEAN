*          DATA SET CTGEN11    AT LEVEL 006 AS OF 09/29/05                      
*PHASE TA0B11B                                                                  
*INCLUDE SCINKEY                                                                
         TITLE 'CTGEN11 - AUTHORIZATION RECORD MAINTENANCE'                     
GEN11    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN11*,RA,R9,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CT0REC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         L     R1,=V(SCINKEY)                                                   
         AR    R1,RE                                                            
         ST    R1,VSCINKEY                                                      
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
* ROUTINE TO VALIDATE KEY OF AUTHORIZATION RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TWAAGY      DEFAULT AGY                                  
*                                                                               
         GOTO1 AFVAL,AUTAGYH                                                    
         BNE   VK010                                                            
         CLI   FVILEN,2            TWO CHR AGY                                  
         BNE   VK005                                                            
         MVC   CT0KAGY,FVIFLD                                                   
         B     VK010                                                            
*                                                                               
VK005    BAS   RE,GETAGY           OR USERID                                    
         BNE   VALKEYX                                                          
         MVC   CT0KAGY,APWORK                                                   
         GOTO1 DISPFLD,AUTAGYH                                                  
*                                                                               
VK010    GOTO1 AFVAL,AUTTYPH       VALIDATE TYPE CODE                           
         BNE   *+10                                                             
         MVC   CT0KOFFC,FVIFLD                                                  
         GOTO1 AFVAL,AUTNAMH       LAST NAME                                    
         BNE   *+10                                                             
         MVC   CT0KLAST,FVIFLD                                                  
         GOTO1 AFVAL,AUTINITH      INITIALS                                     
         BNE   *+10                                                             
         MVC   CT0KFI(2),FVIFLD                                                 
         OC    CT0KEYS,CT0KEYS     TEST ANY INPUT                               
         BNZ   VK030                                                            
*                                                                               
VK020    GOTO1 AFVAL,AUTPWDH       MUST BE PASSWORD                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     VALKEYX             ENTER KEY                                    
         MVC   CT0KCODE,FVIFLD                                                  
*                                                                               
VK030    GOTO1 AFVAL,AUTSYSH                                                    
         BNE   VK035                                                            
         GOTO1 AVALSYS,AUTSYSH     VALIDATE SYSTEM                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     VALKEYX             INVALID SYSTEM                               
         MVC   SYSTEM,APWORK       HOLD FOR LATER USE                           
*                                                                               
VK035    CLI   SYSTEM,0            IF NO SYSTEM INPUT                           
         BNE   *+10                                                             
         MVC   SYSTEM,OPTSYS       TRY OPTSYS                                   
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK040                                                            
         TM    IOERR,IOEDEL                                                     
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD                                                  
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VK040    EQU   *                                                                
         L     R2,AIOAREA1                                                      
         TM    CT0STAT,X'40'       TEST IF NEW PERSON PASSWORD                  
         BZ    VK042                                                            
         LA    R1,AUTPWDH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
*                                                                               
VK042    BAS   RE,GETKEYS          BUILD PASSIVE POINTERS                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,AUTKEY3          DISPLAY PASSWORD KEY                         
         MVC   APWORK(10),CT0KCODE                                              
         GOTO1 DISPFLD,AUTPWDH                                                  
         LA    R2,AUTKEY2          DISPLAY NAME KEY                             
         MVC   APWORK(2),CT0KOFFC                                               
         GOTO1 DISPFLD,AUTTYPH                                                  
         MVC   APWORK(18),CT0KLAST                                              
         GOTO1 DISPFLD,AUTNAMH                                                  
         MVC   APWORK(2),CT0KFI                                                 
         GOTO1 DISPFLD,AUTINITH                                                 
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN AUTHORIZATION RECORD                    *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF AUTHORIZATION RECORD                      *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   EQU   *                                                                
         LA    R2,APRECKEY                                                      
         CLI   CT0KLAST,0          PASSWORD OR NAME ?                           
         BE    DISKEY2                                                          
         MVC   APWORK(2),CT0KAGY                                                
         GOTO1 DISPFLD,AUTAGYH                                                  
         MVC   APWORK(2),CT0KOFFC  NAME KEY                                     
         GOTO1 DISPFLD,AUTTYPH                                                  
         MVC   APWORK(18),CT0KLAST                                              
         GOTO1 DISPFLD,AUTNAMH                                                  
         MVC   APWORK(2),CT0KFI                                                 
         GOTO1 DISPFLD,AUTINITH                                                 
         B     DISKEYX                                                          
*                                                                               
DISKEY2  MVC   APWORK(2),CT0KAGY                                                
         GOTO1 DISPFLD,AUTAGYH                                                  
         MVC   APWORK(10),CT0KCODE PASSWORD KEY                                 
         GOTO1 DISPFLD,AUTPWDH                                                  
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY AUTHORIZATION RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC AUTCMTSH                                                         
         XC    XXCNT(XXCNTL),XXCNT                                              
         XC    ASYSEL,ASYSEL                                                    
         MVI   SYSNUMSC,0                                                       
         CLI   SYSTEM,0                                                         
         BE    DRREC10                                                          
         GOTO1 ADISSE,SYSTEM       GET A(SELIST) ENTRY INTO ASE                 
         OC    APPARM,APPARM                                                    
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   ASE,APPARM                                                       
DRREC10  LA    R3,CT0DATA                                                       
*                                                                               
DRDAT10  CLI   0(R3),0                                                          
         BE    DISPEND                                                          
         CLI   0(R3),X'02'         DESCRIPTION ELEMENT                          
         BE    DRDES                                                            
         CLI   0(R3),X'20'         ID                                           
         BE    DISPID                                                           
         CLI   0(R3),X'21'         SYSTEM                                       
         BE    DISPSS                                                           
*                                                                               
DRDAT20  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DRDAT10                                                          
         EJECT                                                                  
*                                  DESCRIPTION ELEMENT                          
DRDES    MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'APWORK-1),APWORK                                      
         SR    R1,R1                                                            
         IC    R1,1(R3)            L'COMMENT ELEMENT                            
         SH    R1,=H'3'            SUB 2 FOR COMMENT ONLY                       
         EX    R1,*+8              SUB 1 FOR EXECUTE                            
         B     *+10                                                             
         MVC   APWORK(0),2(R3)                                                  
         GOTO1 DISPFLD,AUTCMTSH                                                 
         B     DRDAT20                                                          
*                                                                               
* ADD AN ID ELEMENT TO ID BLOCK (BLOCK1)                                        
*                                                                               
DISPID   SR    R1,R1                                                            
         IC    R1,IDCNT            BUMP BLOCK COUNT                             
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IDCNT                                                         
         MH    R4,=H'20'                                                        
         LA    R4,BLOCK1(R4)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R4),C' '          AND CLEAR IT                                 
         MVC   1(19,R4),0(R4)                                                   
         IC    R1,1(R3)                                                         
         OC    2(2,R3),2(R3)                                                    
         BNZ   *+10                                                             
         MVC   2(2,R3),=C'L='                                                   
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R3)       MOVE IN ID                                   
         B     DRDAT20                                                          
*                                                                               
* SAVE A(SYSTEM ELEMENT) IF IT'S THE ONE REQUESTED                              
*                                                                               
DISPSS   CLI   SYSTEM,0            IS A SYSTEM BEING DISPLAYED                  
         BE    DISPSS1             NO                                           
         USING CTSYSD,R3                                                        
         CLC   CTSYSNUM,SYSTEM                                                  
         BNE   *+8                                                              
         ST    R3,ASYSEL           SAVE A(SYSTEM ELEMENT)                       
DISPSS1  MVC   SYSNUMS,CTSYSNUM                                                 
         L     RF,=A(GETSEN)       GET SE NAME AND ADD TO LIST                  
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     DRDAT20                                                          
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* PUT BLOCKS TO TWA                                                             
*                                                                               
DISPEND  CLI   IDCNT,0             VALID ID'S                                   
         BE    DISPEND2                                                         
         ZIC   R0,IDCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(1,AUTUSR1H),(20,BLOCK1),(R0),       *        
               RR=APRELO                                                        
*                                  SYSTEM INFORMATION                           
DISPEND2 OC    ASYSEL,ASYSEL       LIMIT ACCESS                                 
         BZ    DISPEND4                                                         
         USING CTSYSD,R3                                                        
         L     R3,ASYSEL                                                        
         GOTO1 ADISLACC,APPARM,(CTSYSNUM,CTSYSLMT),CT0KAGY                      
         GOTO1 DISPFLD,AUTACCSH                                                 
         L     RF,=A(DISPSYS)      PROGRAM ACCESS                               
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         ZIC   R0,PGCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(2,AUTPRG1H),(20,BLOCK1),(R0),       *        
               RR=APRELO                                                        
*                                                                               
DISPEND4 MVI   AUTSHD,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   AUTSHD+1(L'AUTSHD-1),AUTSHD                                      
         CLI   SYSNUMSC,0                                                       
         BE    DISPEND6                                                         
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DISPEND6                                                         
         LA    R1,L'AUTSHD                                                      
         SR    R1,RF                                                            
         BNP   DISPEND6                                                         
         SRL   R1,1                                                             
         LA    RE,AUTSHD(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DISPEND6 OI    AUTSHDH+6,X'80'                                                  
*                                                                               
DISRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN AUTHORIZATION RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED AUTHORIZATION RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         OI    APINDS,APILFLST     SET FIRST TIME FLAG                          
         XC    SELKEY1,SELKEY1                                                  
         MVI   APFLAG,0                                                         
*                                                                               
         MVC   SELAGY,TWAAGY       SET DEFAULT AGY                              
         GOTO1 AFVAL,LSTAGYH                                                    
         BNE   VS010                                                            
         CLI   FVILEN,2            TEST FOR 2 CHR AGY                           
         BE    VS005                                                            
         CLC   FVIFLD(3),=C'ALL'   TEST FOR ALL AGYENCYS                        
         BNE   VS006                                                            
         XC    SELAGY,SELAGY                                                    
         OI    APFLAG,APAGALL      FLAG ALL AGYENCYS                            
         B     VS010                                                            
VS005    MVC   SELAGY,FVIFLD                                                    
         B     VS010                                                            
*                                                                               
VS006    BAS   RE,GETAGY           FIND AGY FROM USERID                         
         BNE   VALSELX                                                          
         MVC   SELAGY,APWORK                                                    
         GOTO1 DISPFLD,LSTAGYH     DISPLAY 2 CHR AGY                            
*                                                                               
VS010    GOTO1 AFVAL,LSTTYPH       2 CHR TYPE CODE                              
         BNE   VS020                                                            
         CLI   FVILEN,3                                                         
         BNE   VS015                                                            
         CLC   FVIFLD(3),=C'ALL'   OR ALL TYPE CODES                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX             INVALID INPUT                                
         OI    APFLAG,APOFALL                                                   
         B     VS020                                                            
VS015    MVC   SELOFFC,FVIFLD                                                   
*                                                                               
VS020    GOTO1 AFVAL,LSTNAMEH      LAST NAME                                    
         BNE   VS030                                                            
         MVC   SELLAST,FVIFLD                                                   
         MVC   SELK1CL,FVXLEN                                                   
*                                                                               
VS030    GOTO1 AFVAL,LSTPSWDH                                                   
         BNE   VS040                                                            
         LA    R1,ERRPWD           SET PASSWORD MUST BE ALONE                   
         BAS   RE,SETERR                                                        
         OC    SELKEY1S,SELKEY1S   TEST PASSWORD IS ONLY INPUT                  
         BNZ   VALSELX                                                          
         OI    APFLAG,APPWKEY      LIST BY PASSWORD                             
         MVC   SELPSWD,FVIFLD                                                   
*                                                                               
VS040    OC    SELKEY1S,SELKEY1S   TEST SOME KEY IS INPUT                       
         BNZ   VS045                                                            
         MVI   SELLAST,X'01'       READ FIRST NAME RECORD                       
*                                                                               
VS045    XC    CT0KEY,CT0KEY       BUILD INITIAL KEY                            
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,SELAGY                                                   
         OC    SELPSWD,SELPSWD                                                  
         BZ    VS050                                                            
         MVC   CT0KCODE,SELPSWD                                                 
         B     VALSELY                                                          
VS050    MVC   CT0KOFFC,SELOFFC                                                 
         MVC   CT0KLAST,SELLAST                                                 
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
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
GETSEL   LA    R2,IOKEY                                                         
         MVC   CT0KEY,APRECKEY                                                  
         TM    APINDS,APILFLST                                                  
         BNO   *+12                                                             
         NI    APINDS,255-APILFLST                                              
         B     GETSEL6                                                          
GETSEL2  TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
*                                                                               
         L     R2,AIOAREA1                                                      
         CLI   CT0KTYP,CT0KTEQU    CHECK STILL AUTH RECORD                      
         BNE   GETSELN                                                          
         CLC   CT0KAGY,SELAGY      CHECK SAME AGENCY                            
         BE    GETSEL9                                                          
         TM    APFLAG,APAGALL      IS AGY = ALL                                 
         BNO   GETSELN             NO SO END OF RECORDS                         
         MVC   SELAGY,CT0KAGY      YES SET CURRENT AGY                          
*                                                                               
GETSEL9  TM    APFLAG,APPWKEY      TEST PASSWORD INPUT                          
         BNO   GETSEL10                                                         
         CLI   CT0KLAST,0          TEST PASSWORD KEY                            
         BE    GETSELF                                                          
         B     GETSELN                                                          
*                                                                               
GETSEL10 CLI   CT0KLAST,0          TEST FOR PASSIVE POINTER                     
         BNE   GETSEL11                                                         
         LA    R2,IOKEY                                                         
         XC    CT0KEYS,CT0KEYS                                                  
         MVC   CT0KAGY,SELAGY      RESET AGENCY CODE AND                        
         MVC   CT0KOFFC,SELOFFC                                                 
         MVC   CT0KLAST,SELLAST    READ HI FOR NAME KEY                         
         B     GETSEL6                                                          
*                                                                               
GETSEL11 TM    APFLAG,APOFALL      IS TYPE CODE ALL                             
         BNO   GETSELF                                                          
         CLI   SELLAST,0                                                        
         BE    PRTREPF                                                          
         SR    R1,R1               YES SO SCAN NAME FIELD                       
         IC    R1,SELK1CL                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELLAST(0),CT0KLAST                                              
         BE    GETSELF                                                          
*                                                                               
         ICM   R1,3,CT0KOFFC       NO MATCH ON NAME                             
         LA    R2,IOKEY                                                         
         LA    R1,1(R1)            SO BUMP OFFICE CODE                          
         STCM  R1,3,CT0KOFFC                                                    
         MVC   CT0KLAST,SELLAST    RESET NAME                                   
         B     GETSEL6             READ HI                                      
*                                                                               
GETSELF  MVC   APRECKEY(L'CT0KEY),CT0KEY                                        
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         OI    APINDS,APILNSEQ     SET READ SEQ                                 
         BAS   RE,FILTER           TEST FILTERS AGAINST RECORD                  
         BNE   GETSEL              NO MATCH SO GET NEXT                         
*                                                                               
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BNO   GETSELY                                                          
         LA    R2,IOKEY                                                         
         MVC   CT0KEY,APRECKEY                                                  
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         L     R2,AIOAREA1                                                      
*                                                                               
GETSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
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
DISSEL   EQU   *                                                                
         L     R4,APPARM                                                        
         USING LISTD,R4                                                         
         BAS   RE,GETKEYS          BUILD KEYS FROM POINTERS                     
*                                                                               
         LA    R2,AUTKEY2          NAME KEY                                     
         MVC   LISTTYPE(2),CT0KAGY                                              
         MVC   LISTTYPE+3(2),CT0KOFFC                                           
         MVC   LISTNAME(18),CT0KLAST                                            
         LA    R1,LISTNAME+17                                                   
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(1,R1),CT0KFI                                                   
         MVI   3(R1),C' '                                                       
         MVC   4(1,R1),CT0KMI                                                   
*                                                                               
         LA    R2,AUTKEY3          PASSWORD KEY                                 
         MVC   LISTPSWD,CT0KCODE                                                
*                                                                               
         L     R2,AIOAREA1         ACTUAL RECORD                                
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'01'        GET ACTIVITY ELEMENT                         
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    DISSELX             NOT FOUND                                    
         USING CTACTD,R3                                                        
***??                                                                           
*        GOTO1 VSWITCH,APPARM,X'00FFFFFF'                                       
*        L     R1,0(R1)                                                         
*        EDIT  (B2,102(R1)),(3,LISTACTY)                                        
***??                                                                           
         GOTO1 VDATCON,APPARM,(3,CTACTDT),(8,LISTACTY)                          
         DROP  R3                                                               
*                                                                               
         MVI   APBYTE,C'L'         GET LONG SYSTEM NAMES FIRST                  
*                                                                               
DSEL001  LA    R8,WORK                                                          
         MVC   WORK,SPACES                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'21'        GET SYSTEM ELEMENTS                          
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    DISSELX                                                          
         USING CTSYSD,R3                                                        
DSEL010  GOTO1 ADISSYS,CTSYSNUM                                                 
         CLI   APBYTE,C'S'         TEST SHORT NAMES REQUIRED                    
         BNE   DSEL015                                                          
         L     R1,APPARM                                                        
         MVC   APWORK(7),SPACES    MOVE SHORT NAME TO APWORK                    
         MVC   APWORK(3),9(R1)                                                  
DSEL015  MVC   0(7,R8),APWORK      INSERT NAME                                  
         LA    R8,6(R8)                                                         
         CLI   0(R8),C' '          SQUASH OUT SPACES                            
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         LA    R8,2(R8)                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DSEL010                                                          
         LA    R1,WORK+28          SEE IF LIST IS TOO LONG                      
         CR    R8,R1                                                            
         BH    *+14                YES SO RERUN FOR SHORT NAMES                 
         MVC   LISTSYST,WORK                                                    
         B     DISSELX                                                          
         MVI   APBYTE,C'S'                                                      
         B     DSEL001                                                          
*                                                                               
DISSELX  B     EXIT                                                             
*                                                                               
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
VALREQ   L     R8,AREP                                                          
         USING REPD,R8             R8=A(REPORT WORK AREA)                       
*                                                                               
         XC    SELKEY1,SELKEY1                                                  
         MVI   APFLAG,0                                                         
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
         MVC   SELAGY,TWAAGY       SET DEFAULT AGY                              
         GOTO1 AFVAL,REPAGYH                                                    
         BNE   VR010                                                            
         CLI   FVILEN,2            2 CHR AGY                                    
         BE    VR005                                                            
         CLC   FVIFLD(3),=C'ALL'   ALL AGENCYS                                  
         BNE   VR006                                                            
         XC    SELAGY,SELAGY                                                    
         OI    APFLAG,APAGALL                                                   
         B     VR010                                                            
VR005    MVC   SELAGY,FVIFLD                                                    
         B     VR010                                                            
*                                                                               
VR006    BAS   RE,GETAGY           GET AGY FROM USERID                          
         BNE   VALREQX                                                          
         MVC   SELAGY,APWORK                                                    
         GOTO1 DISPFLD,REPAGYH                                                  
*                                                                               
VR010    GOTO1 AFVAL,REPTYPH       GET TYPE CODE                                
         BNE   VR020                                                            
         CLI   FVILEN,3            MUST BE 2 CHR                                
         BNE   VR015                                                            
         CLC   FVIFLD(3),=C'ALL'   OR ALL                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALREQX             INVALID INPUT                                
         OI    APFLAG,APOFALL                                                   
         B     VR020                                                            
VR015    MVC   SELOFFC,FVIFLD                                                   
*                                                                               
VR020    GOTO1 AFVAL,REPNAMH       GET NAME FIELD                               
         BNE   VR030                                                            
         MVC   SELLAST,FVIFLD                                                   
         MVC   SELK1CL,FVXLEN                                                   
*                                                                               
VR030    MVI   RPTFMT,0                                                         
         GOTO1 AFVAL,REPSYSH       SEE IF SYSTEM REQUESTED                      
         BNE   VR040                                                            
         CLC   FVIFLD(3),=C'ALL'                                                
         BNE   *+12                                                             
         MVI   RPTFMT,X'FF'        FLAG SYSTEM ALL                              
         B     VR040                                                            
         GOTO1 AVALSYS,REPSYSH     VALIDATE SYSTEM                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     VALREQX             INVALID SYSTEM                               
         MVC   RPTFMT,APWORK       SET SYSTEM SELECT                            
         MVC   OPTSYS,APWORK       IMPLIED SYS= OPTION                          
*                                                                               
VR040    OC    SELKEY1S,SELKEY1S   TEST SOME KEY IS INPUT                       
         BNZ   VR045                                                            
         MVI   SELLAST,X'01'       READ FIRST NAME RECORD                       
*                                                                               
*                                                                               
VR045    LA    R2,APRECKEY                                                      
         XC    CT0KEY,CT0KEY       BUILD INITIAL KEY                            
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,SELAGY                                                   
         MVC   CT0KOFFC,SELOFFC                                                 
         MVC   CT0KLAST,SELLAST                                                 
*                                                                               
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVC   REPPSWD,=C'ACCESS'                                               
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPFOOTN,0                                                       
         MVI   REPMIDSI,REPMCLRA                                                
         LA    R0,REPSPEC1                                                      
         CLI   RPTFMT,0                                                         
         BNE   VR050                                                            
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         LA    R0,REPSPEC0                                                      
VR050    ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT AUTHORIZATION LIST                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   EQU   *                                                                
         L     R8,AREP                                                          
         LA    R2,IOKEY                                                         
         MVC   CT0KEY,APRECKEY                                                  
PRTREP1  LA    R1,IOCONFIL+IOHI+IO1                                             
         GOTO1 AIO                 READ INITIAL RECORD                          
         BNE   PRTREPX                                                          
         B     PRTREP4                                                          
*                                                                               
PRTREP2  LA    R2,IOKEY                                                         
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BNO   PRTREP3                                                          
*                                                                               
         NI    APINDS,255-APILRERD                                              
         MVC   CT0KEY,APRECKEY     RESET READ SEQUENCE                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
*                                                                               
PRTREP3  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                 READ SEQUENTIAL                              
*                                                                               
PRTREP4  L     R2,AIOAREA1                                                      
         CLI   CT0KTYP,CT0KTEQU    CHECK STILL AUTH RECORD                      
         BNE   PRTREPX                                                          
         CLC   CT0KAGY,SELAGY      CHECK SAME AGENCY                            
         BE    PRTREP9                                                          
         TM    APFLAG,APAGALL      IS AGY = ALL                                 
         BNO   PRTREPX             NO SO END OF RECORDS                         
         MVC   SELAGY,CT0KAGY      YES SO SET CURRENT AGY                       
*                                                                               
PRTREP9  CLI   CT0KLAST,0          IS RECORD A PASSIVE POINTER                  
         BNE   PRTREP10                                                         
         LA    R2,IOKEY            YES SO                                       
         XC    CT0KEYS,CT0KEYS                                                  
         MVC   CT0KAGY,SELAGY      RESET AGENCY CODE AND                        
         MVC   CT0KLAST,SELLAST    READ HI FOR NAME KEY                         
         B     PRTREP1             READ HI                                      
*                                                                               
PRTREP10 TM    APFLAG,APOFALL      IS TYPE CODE ALL                             
         BNO   PRTREPF                                                          
         CLI   SELLAST,0                                                        
         BE    PRTREPF                                                          
         SR    R1,R1               YES SO SCAN NAME FIELD                       
         IC    R1,SELK1CL                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELLAST(0),CT0KLAST                                              
         BE    PRTREPF                                                          
*                                                                               
         ICM   R1,3,CT0KOFFC       NO MATCH ON NAME SO                          
         LA    R2,IOKEY                                                         
         LA    R1,1(R1)            BUMP OFFICE CODE                             
         STCM  R1,3,CT0KOFFC                                                    
         MVC   CT0KLAST,SELLAST    RESET NAME                                   
         B     PRTREP1             READ HI                                      
*                                                                               
PRTREPF  MVC   APRECKEY(L'CT0KEY),CT0KEY                                        
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         BAS   RE,FILTER           TEST FILTERS ON RECORD                       
         BNE   PRTREP2             NO MATCH SO READ SEQ                         
*                                                                               
         TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BNO   PRTREPY                                                          
         NI    APINDS,255-APILRERD                                              
         LA    R2,IOKEY                                                         
         MVC   CT0KEY,APRECKEY     RESET READ SEQUENCE                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         L     R2,AIOAREA1                                                      
*                                                                               
PRTREPY  BAS   RE,GETKEYS          EXPAND ALL KEYS                              
         LA    R2,AUTKEY2          NAME KEY                                     
*                                                                               
         MVC   RPTAGY,CT0KAGY      AGENCY                                       
         MVC   RPTOFFC,CT0KOFFC    TYPE CODE                                    
*                                                                               
         MVC   RPTNAME(18),CT0KLAST                                             
         LA    R1,RPTNAME+17       NAME + INITIALS                              
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(1,R1),CT0KFI                                                   
         MVI   3(R1),C' '                                                       
         MVC   4(1,R1),CT0KMI                                                   
*                                                                               
         LA    R2,AUTKEY3          PASSWORD KEY                                 
         MVC   RPTCODE,CT0KCODE                                                 
*                                                                               
         L     R2,AIOAREA1         ACTUAL RECORD                                
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'01'        GET ACTIVITY ELEMENT                         
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    PRTREP11            NOT FOUND                                    
         USING CTACTD,R3                                                        
         GOTO1 VDATCON,APPARM,(3,CTACTDT),(8,RPTDATE)                           
*                                                                               
PRTREP11 LA    R4,WORK             EXPAND SYSTEM NAMES                          
         MVC   WORK,SPACES                                                      
         MVI   SYSCNT,0            COUNT NUM OF SYSTEMS                         
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'21'        GET SYSTEM ELEMENTS                          
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    PRTREP13            NOT FOUND                                    
         USING CTSYSD,R3                                                        
PRTREP12 ZIC   RF,SYSCNT                                                        
         LA    RF,1(RF)                                                         
         STC   RF,SYSCNT                                                        
         GOTO1 ADISSYS,CTSYSNUM    INSERT SYSTEM NAME                           
         MVC   0(7,R4),APWORK                                                   
         LA    R4,6(R4)                                                         
         CLI   0(R4),C' '          SQUASH OUT SPACES                            
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    PRTREP12                                                         
         MVC   RPTSYS,WORK        MOVE LIST TO RPTSYS                           
*                                                                               
PRTREP13 CLI   RPTFMT,0           TEST SYSTEM LIST REQUIRED                     
         BNE   PRT010                                                           
*                                                                               
         MVC   PRTAGY,RPTAGY      MOVE FIELDS INTO LIST LINE                    
         MVC   PRTOFFC,RPTOFFC                                                  
         MVC   PRTNAME,RPTNAME                                                  
         MVC   PRTCODE,RPTCODE                                                  
         MVC   PRTDATE,RPTDATE                                                  
         MVC   PRTSYS,RPTSYS                                                    
         B     PRTREPS            AND PRINT IT                                  
*                                                                               
PRT010   CLI   RPTFMT,X'FF'       TEST ALL SYSTEMS                              
         BE    *+8                                                              
         MVI   SYSCNT,1           SET TO 1 SYSTEM                               
*                                                                               
         ZIC   RF,SYSCNT          CALCULATE SIZE OF ENTRY                       
         SLL   RF,2                                                             
         LA    RF,7(RF)                                                         
         SR    R1,R1                                                            
         IC    R1,REPLINE         ADD TO CURRENT LINE POS                       
         AR    RF,R1                                                            
         CLM   RF,1,REPMAXL       WILL IT FIT ON THE PAGE                       
         BNH   *+8                                                              
         OI    REPHEADI,REPHFRCE  NO SO FORCE NEW PAGE                          
*                                                                               
         MVC   PRLAGY,RPTAGY      MOVE FIELDS INTO REPORT                       
         MVC   PRLOFFC,RPTOFFC                                                  
         MVC   PRLNAME,RPTNAME                                                  
         MVC   PRLCODE,RPTCODE                                                  
         MVC   PRLDATE,RPTDATE                                                  
*                                                                               
         MVCDD PRLAGYD,CT#AGY     MOVE IN DICTIONARY LABLES                     
         MVCDD PRLOFFCD,CT#TYPCD                                                
         MVCDD PRLNAMED,CT#LASTN                                                
         MVCDD PRLCODED,CT#PSWD                                                 
         MVCDD PRLDATED,CT#LCHNG                                                
         MVCDD PRLCMTD,CT#CMTS                                                  
         MVCDD PRLUSRD,CT#USRIS                                                 
*                                                                               
         MVC   PRLLIN2,SPACES      PAD WITH BLANK LINES                         
         MVC   PRLLIN4,SPACES                                                   
*                                                                               
         MVC   PRLCMT,SPACES       DISPLAY COMMENT LINE                         
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'        GET DESCRIPTION ELEMENT                      
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    PRT020              NOT FOUND NEXT                               
         SR    R1,R1                                                            
         IC    R1,1(R3)            L'COMMENT ELEMENT                            
         SH    R1,=H'3'            SUB 2 FOR COMMENT ONLY                       
         EX    R1,*+8              SUB 1 FOR EXECUTE                            
         B     *+10                                                             
         MVC   PRLCMT(0),2(R3)                                                  
*                                                                               
PRT020   BAS   RE,DISIDS           DISPLAY USERID LINE                          
         LA    RE,BLOCK1                                                        
         LA    RF,PRLUSR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,IDCNT                                                       
         BZ    PRT030                                                           
PRT025   MVC   0(10,RF),0(RE)      INSERT USERID                                
         LA    RF,9(RF)                                                         
         CLI   0(RF),C' '          SQUASH OUT SPACES                            
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C','          INSERT , BETWEEN IDS                         
         LA    RF,2(RF)                                                         
         LA    RE,10(RE)                                                        
         BCT   R1,PRT025           GET NEXT USERID                              
         BCTR  RF,0                                                             
         MVI   0(RF),C' '          REMOVE , AFTER LAST ID                       
*                                                                               
PRT030   GOTO1 VREPORT,REPD        PRINT THIS BLOCK                             
*                                                                               
         LA    R4,PRLLIN1          SET UP FOR SYSTEM ACCESS LIST                
         USING PRLLINS,R4                                                       
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'21'        GET SYSTEM ELEMENTS                          
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    PRTREPS                                                          
         USING CTSYSD,R3                                                        
PRT032   CLI   RPTFMT,X'FF'        IS SYSTEM ALL                                
         BE    *+14                                                             
         CLC   RPTFMT,CTSYSNUM     IS THIS SELECTED SYSTEM                      
         BNE   PRT036                                                           
*                                                                               
         MVI   PRLSYS1,C' '        START WITH BLANK LINE                        
         GOTO1 ADISSYS,CTSYSNUM                                                 
         MVC   PRLSYS(7),APWORK    SYSTEM NAME                                  
         MVCDD PRLSYSD,CT#SYS                                                   
*                                GET LIMIT ACCESS                               
         GOTO1 ADISLACC,APPARM,(CTSYSNUM,CTSYSLMT),CT0KAGY                      
         MVC   PRLLIM,APWORK                                                    
         OC    PRLLIM,PRLLIM       TEST IF ANY THERE                            
         BZ    *+10                                                             
         MVCDD PRLLIMD,CT#LIMAC    ONLY PRINT HEADER IF NEEDED                  
*                                                                               
         MVC   PRLSYS2,SPACES      ANOTHER BLANK LINE                           
         L     RF,=A(DISPSYS)      PROGRAM ACCESS                               
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         LA    RE,BLOCK1                                                        
         LA    RF,PRLSYS3                                                       
         SR    R1,R1                                                            
         IC    R1,PGCNT                                                         
PRT035   MVC   0(10,RF),0(RE)      INSERT PROG= ENTRY                           
         LA    RF,9(RF)                                                         
         CLI   0(RF),C' '          SQUASH OUT SPACES                            
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C','          INSERT , BETWEEN ENTRYS                      
         LA    RF,2(RF)                                                         
         LA    RE,10(RE)                                                        
         BCT   R1,PRT035           GET NEXT PROG= ENTRY                         
         BCTR  RF,0                                                             
         MVI   0(RF),C' '          REMOVE , AFTER LAST                          
         GOTO1 VREPORT,REPD        PRINT THIS ONE                               
         LA    R4,PRLLIN1          RESET FOR NEXT                               
*                                                                               
PRT036   BAS   RE,NEXTEL           GET NEXT SYSTEM ELEMENT                      
         BNE   PRTREPS                                                          
         B     PRT032                                                           
*                                                                               
PRTREPS  MVC   PRLSYS1,ASTS        BUILD LINE OF ASTS                           
         GOTO1 VREPORT,REPD        PRINT LAST ENTRY                             
*                                                                               
         B     PRTREP2             NEXT RECORD                                  
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   BUILD ID LIST INTO BLOCK1                                         *         
*   R2 = RECORD                                                       *         
***********************************************************************         
DISIDS   NTR1                                                                   
         MVI   IDCNT,0                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'20'        GET ID ELEMENT                               
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    DISIDX              NOT FOUND END                                
         LA    R4,BLOCK1                                                        
DISID01  MVC   0(10,R4),CTID-CTIDD(R3)                                          
         OC    0(2,R4),0(R4)       TEST ID LIST                                 
         BNZ   *+10                                                             
         MVC   0(2,R4),=C'L='                                                   
         LA    R4,10(R4)                                                        
         SR    R1,R1                                                            
         IC    R1,IDCNT                                                         
         LA    R1,1(R1)            BUMP COUNTER                                 
         STC   R1,IDCNT                                                         
         BAS   RE,NEXTEL           GET NEXT ID                                  
         BE    DISID01                                                          
*                                                                               
DISIDX   B     EXIT                                                             
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
*        BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO CONSTRUCT 3 AUTHORIZATION KEYS FROM POINTER ELEMENTS    *         
***********************************************************************         
         SPACE 1                                                                
GETKEYS  ST    RE,APFULL                                                        
         L     R2,AIOAREA1                                                      
         MVC   AUTKEY1(25),CT0KEY  DUPLICATE KEYS                               
         MVC   AUTKEY2(25),CT0KEY                                               
         MVC   AUTKEY3(25),CT0KEY                                               
         XC    APELEM,APELEM                                                    
         MVI   APBYTE,1                                                         
         MVI   APELEM,X'03'        GET PASSIVE POINTER ELEMENTS                 
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    GKEYSX              NOT FOUND EXIT                               
*                                                                               
GKEYS1   CLI   1(R3),X'04'         KEY1 NUMBER POINTER                          
         BNE   *+16                                                             
         XC    AUTKEY1+3(22),AUTKEY1+3                                          
         MVC   AUTKEY1+CT0KNUM-CT0KEY(2),2(R3)                                  
*                                                                               
         CLI   1(R3),X'18'         KEY2 NAME POINTER                            
         BNE   *+16                                                             
         XC    AUTKEY2+3(22),AUTKEY2+3                                          
         MVC   AUTKEY2+CT0KOFFC-CT0KEY(22),2(R3)                                
*                                                                               
         CLI   1(R3),X'0C'         KEY3 PASSWORD POINTER                        
         BNE   *+16                                                             
         XC    AUTKEY3+3(22),AUTKEY3+3                                          
         MVC   AUTKEY3+CT0KCODE-CT0KEY(10),2(R3)                                
*                                                                               
         CLI   APBYTE,2            WAS THIS SECOND PASS                         
         BE    GKEYSY                                                           
         MVI   APBYTE,2            IF NOT DO SECOND PASS                        
         BAS   RE,NEXTEL                                                        
         BE    GKEYS1                                                           
*                                                                               
GKEYSX   LTR   RB,RB               ERROR EXIT                                   
         L     RE,APFULL                                                        
         BNER  RE                                                               
GKEYSY   CR    RB,RB               OK EXIT                                      
         L     RE,APFULL                                                        
         BER   RE                                                               
         EJECT                                                                  
****************************************************************                
*   TEST RECORD AGAINST FILTERS                                *                
*   CC EQU = RECORD IS SELECTED                                *                
*   CC NEQ = RECORD IS NOT SELECTED                            *                
****************************************************************                
         SPACE 1                                                                
FILTER   NTR1                                                                   
         CLI   OPTUSR,0            TEST USER FILTER                             
         BE    FILT010                                                          
         OI    APINDS,APILRERD     SET SEQUENCE BROKEN                          
         GOTO1 VGETIDS,APPARM,(C'C',(R2)),0,VDMGR                               
         L     RF,4(R1)                                                         
         SR    RE,RE                                                            
         CLI   0(R1),X'FF'                                                      
         BE    FILTXX              ERROR                                        
         ICM   RE,1,0(R1)                                                       
         BZ    FILTXX                                                           
         OC    OPTUSR,SPACES       CONVERT TO UPPER                             
FILT005  OC    0(10,RF),SPACES                                                  
         CLC   OPTUSR,0(RF)                                                     
         BE    FILT010             USERID MATCHED                               
         LA    RF,12(RF)                                                        
         BCT   RE,FILT005                                                       
         B     FILTXX              USERID NOT MATCHED                           
*                                                                               
FILT010  CLI   OPTSYS,0            TEST SYSTEM FILTER                           
         BE    FILT020                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'21'        GET SYSTEM ELEMENTS                          
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    FILTXX                                                           
FILT015  CLC   2(1,R3),OPTSYS      IS IT FILTER SYSTEM                          
         BE    FILT020                                                          
*                                                                               
         BAS   RE,NEXTEL           TRY NEXT ONE                                 
         BE    FILT015                                                          
         B     FILTXX              SYSTEM NOT MATCHED                           
*                                                                               
FILT020  OC    OPTRNG,OPTRNG       TEST DATE FILTER                             
         BZ    FILT030                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'01'        GET ACTIVITY ELEMENT                         
         GOTO1 AGETELS,CT0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    FILTXX              NOT FOUND                                    
*                                                                               
         CLC   2(3,R3),OPTRNG+0    MUST BE > OR = START                         
         BL    FILTXX                                                           
         CLC   2(3,R3),OPTRNG+3    MUST BE < OR = END                           
         BH    FILTXX                                                           
*                                                                               
FILT030  EQU   *                                                                
*                                                                               
FILTYX   CR    RB,RB               RECORD MATCHED                               
         B     EXIT                                                             
FILTXX   LTR   RB,RB               RECORD NOT MATCHED                           
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*    GET 2 CHR AGY ID FROM USERID IN FVIFLD                    *                
*    CC = EQU  OK  AGYID IN APWORK                             *                
*    CC = NEQ  ERR GETTXT SET FOR ERROR EXIT                   *                
****************************************************************                
         SPACE 1                                                                
GETAGY   NTR1                                                                   
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FVIFLD                                                    
         LA    R1,IORD+IOCONFIL+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETAGYX                                                          
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'06'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    GETAGYX                                                          
         MVC   APWORK(2),CTAGYID-CTAGYD(R3)                                     
*                                                                               
GETAGYY  CR    RB,RB               OK EXIT                                      
         B     EXIT                                                             
GETAGYX  LA    R1,ERRUSR           SET INVALID USER ID                          
         BAS   RE,SETERR                                                        
         LTR   RB,RB               ERROR EXIT                                   
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***************************************                                         
*    SET UP GETTXT ERROR EXIT         *                                         
*    R1=MSG NUMBER                    *                                         
***************************************                                         
SETERR   XC    APPARM,APPARM       SET UP GETTXT BLOCK                          
         LA    RF,APPARM                                                        
         MVI   8(RF),C'E'          MESSAGE TYPE                                 
         STH   R1,2(RF)            MESSAGE NUMBER                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
SPACES   DC    132C' '                                                          
ASTS     DC    C'+',130C'-',C'+'                                                
*                                                                               
APOFALL  EQU   X'80'               OFFICE CODE 'ALL'                            
APAGALL  EQU   X'40'               AGENCY CODE 'ALL' DDS ONLY                   
APPWKEY  EQU   X'20'               KEY IS BY PASSWORD                           
*                                                                               
ERRUSR   EQU   154                 INVALID USER ID                              
ERRPWD   EQU   155                 PASSWORD IS INVALID WITH TYP NAME            
*                                                                               
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
*                                                                               
REPDESCL DC    C'AUTH REPORT'                                                   
*                                                                               
REPSPEC0 DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#AULST,18,L                                              
         SPEC  H2,57,CT#AULST,18,LU                                             
         SPEC  M1,1,CT#AUT03,71,L                                               
         SPEC  M2,1,CT#AUT03,71,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         SPACE 2                                                                
REPSPEC1 DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#AULST,18,L                                              
         SPEC  H2,57,CT#AULST,18,LU                                             
         SPEC  M1,1,132C'*'                                                     
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
         LTORG                                                                  
HELPID   DC    X'0131FF01010000000000'                                          
TEXT     DC    CL75'TEST HELP'                                                  
         DC    CL75'TEST HELP'                                                  
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
*                                                                               
         DROP  RB,RA                                                            
*                                                                               
GETSEN   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETSEN,RB                                                        
*                                                                               
GETSEN1  CLI   SYSNUMSC,0          TEST FIRST CALL - NUMBER OF ITEMS            
         BNE   GETSEN2                                                          
         MVI   SYSNAMS,C' '                                                     
         MVC   SYSNAMS+1(L'SYSNAMS-1),SYSNAMS                                   
         MVC   SYSNAMS(7),=C'SYSTEMS'                                           
         NC    SYSNAMS+1(6),=8X'BF'                                             
         MVC   SYSNAMS+8(8),=C'ASSIGNED'                                        
         NC    SYSNAMS+9(7),=8X'BF'                                             
         LA    RE,SYSNAMS+17       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
*                                                                               
GETSEN2  L     R3,ASYSFACS         SEARCH SE LIST FOR SE NUM AT SYSNUMS         
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R3                                                       
         CLC   SYSNUMS,SEOVSYS                                                  
         BE    GETSEN3                                                          
         BXLE  R3,RE,*-10                                                       
         LA    R3,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
GETSEN3  L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYSNUMSC       TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYSNUMSC         BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    GETSENX                                                          
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
*                                                                               
GETSENX  XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* BUILD PROGRAM ACCESS LIST INTO BLOCK1 AND SET PGCNT TO NUMBER OF              
* ENTRIES IN BLOCK.                                                             
*                                                                               
DISPSYS  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DISPSYS,RB                                                       
         L     R3,ASYSEL           GET A(PGMS) INTO APGM                        
         USING CTSYSEL,R3                                                       
         XC    APGM,APGM                                                        
         XC    ASE,ASE                                                          
         GOTO1 ADISSE,CTSYSNUM                                                  
         OC    APPARM,APPARM                                                    
         BZ    DISPSYS1                                                         
         MVC   ASE,APPARM                                                       
         L     RE,ASE                                                           
         MVC   APGM,SEPGMS-SELISTD(RE)                                          
DISPSYS1 LA    R4,CTSYSPGM         R4=A(AUTHS)                                  
         ZIC   R9,CTSYSLEN         R9=PGM NUMBER                                
         CLI   CTSYSLEN,16         CHECK FOR ALL= VALUE ONLY                    
         BE    DISPSYS8                                                         
*                                                                               
DISPSYS2 CH    R9,=H'16'                                                        
         BNH   DISPSYS8                                                         
         MVC   PROGRAM,0(R4)                                                    
         LA    R4,1(R4)                                                         
         BAS   RE,GETPGAN          GET PROGRAM NAME                             
         CLI   PROGRAM,X'FF'       SET TO X'FF' IF N/F                          
         BE    DISPSYS6                                                         
*                                                                               
DISPSYS4 SR    R1,R1                                                            
         IC    R1,PGCNT            BUMP BLOCK COUNT                             
         LA    RE,1(R1)                                                         
         STC   RE,PGCNT                                                         
         MH    R1,=H'20'                                                        
         LA    R1,BLOCK1(R1)       GET A(BLOCK ENTRY)                           
         MVI   0(R1),C' '                                                       
         MVC   1(19,R1),0(R1)                                                   
         LR    R8,R1                                                            
         MVC   0(4,R8),PGNAME                                                   
         LA    R8,4(R8)                                                         
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C'='                                                       
         MVI   2(R8),C'Y'          AUTH IS Y N OR XXXX                          
         CLC   0(2,R4),=X'000F'                                                 
         BE    DISPSYS6                                                         
         MVI   2(R8),C'N'                                                       
         CLC   0(2,R4),=X'0000'                                                 
         BE    DISPSYS6                                                         
         GOTO1 VHEXOUT,APPARM,(R4),2(R8),2,=C'TOG'                              
*                                                                               
DISPSYS6 LA    R8,CTSYSALL         EXIT IF ALL=VALUE JUST DONE                  
         CR    R8,R4                                                            
         BE    DISPSYSX                                                         
         LA    R4,2(R4)                                                         
         SH    R9,=H'3'                                                         
         B     DISPSYS2                                                         
*                                                                               
DISPSYS8 LA    R4,CTSYSALL                                                      
         MVC   PGNAME,=CL8'ALL'                                                 
         B     DISPSYS4                                                         
*                                                                               
DISPSYSX XIT1                                                                   
         DROP  R3                                                               
*                                                                               
* SET PROGRAM NAME FROM PROGRAM ACCESS NUMBER                                   
*                                                                               
GETPGAN  NTR1                                                                   
         MVC   PGNAME,=C'N/A'                                                   
         ICM   R3,15,APGM                                                       
         BZ    GETPGANX                                                         
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GETPGAN2 CLC   PGMNUM,PROGRAM      MATCH ON PROGRAM NUMBER                      
         BE    GETPGAN4                                                         
         CLI   PGMALNUM,0                                                       
         BE    *+14                                                             
         CLC   PGMALNUM,PROGRAM    OR ACCESS OVERRIDE (IF SET)                  
         BE    GETPGAN4                                                         
         BXLE  R3,RE,GETPGAN2                                                   
         B     GETPGANN                                                         
GETPGAN4 SR    RF,RF               HERE IF PROGRAM FOUND                        
         ICM   RF,7,PGMAGYLA       TEST FOR RESTRICTED AGENCY                   
         BZ    GETPGANY            ACCESS LIST                                  
GETPGAN6 CLC   0(2,RF),=CL2'  '                                                 
         BE    GETPGANN            END OF LIST                                  
         CLC   0(2,RF),TWAAGY                                                   
         BE    GETPGANY            CONNECTED AGENCY IN LIST                     
         LA    RF,2(RF)                                                         
         B     GETPGAN6            BUMP TO NEXT AGENCY IN LIST                  
GETPGANN MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGANX                                                         
GETPGANY MVC   PGNAME,PGMNAME                                                   
GETPGANX XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
* CONVERT THE X'21' ELEMENT TO THE NEW FORMAT                                   
*                                                                               
CNVELM21 CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING CNVELM21,RB                                                      
         MVC   BLOCK1(L'APELEM),APELEM  COPY THE ELEMENT OVER                   
         LA    R9,BLOCK1           POINT FROM WHERE TO COPY                     
         USING CTSYSD,R9                                                        
         LA    R5,APELEM+CTSYSPGM-CTSYSD  WHERE TO COPY                         
         LA    R6,1                FIRST PROGRAM                                
         LA    R0,16               LENGTH IS HEADER FIRST                       
         LA    R4,CTSYSPGM                                                      
CNV21LP  CLC   0(2,R4),CTSYSALL    DEFAULT?                                     
         BE    CNV21NX             YES, SKIP TO NEXT ONE                        
         STC   R6,0(R5)            STORE THE PROGRAM NUMBER                     
         MVC   1(2,R5),0(R4)       AND ITS AUTHORIZATION CODE                   
         AH    R0,=H'3'            LENGTH IS CHANGED BY 3                       
         LA    R5,3(R5)            NEXT POSTION FOR NEXT PROGRAM                
CNV21NX  LA    R6,1(R6)            NEXT PROGRAM NUMBER                          
         LA    R4,2(R4)            NEXT AUTHORIZATION CODE                      
         CH    R6,=H'64'           DID WE DO ALL 64 PROGRAMS?                   
         BNH   CNV21LP             NO, CONTINUE UNTIL WE'RE DONE                
         LA    R9,APELEM           STORE THE NEW LENGTH OF THE ELEMENT          
         STC   R0,CTSYSLEN                                                      
         XIT1                                                                   
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENEED                                                       
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENCED                                                       
         ORG                                                                    
         ORG   GENTABH                                                          
       ++INCLUDE CTGENAED                                                       
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTTYPE DS    CL5                                                              
         DS    CL1                                                              
LISTNAME DS    CL22                                                             
         DS    CL1                                                              
LISTPSWD DS    CL10                                                             
         DS    CL1                                                              
LISTACTY DS    CL7                                                              
         DS    CL2                                                              
LISTSYST DS    CL26                                                             
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
REPD     DSECT                                                                  
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)        DSECT FOR SHORT REPORT                       
PRTAGY   DS    CL2                                                              
         DS    CL4                                                              
PRTOFFC  DS    CL2                                                              
         DS    CL3                                                              
PRTNAME  DS    CL22                                                             
         DS    CL1                                                              
PRTCODE  DS    CL10                                                             
         DS    CL2                                                              
PRTDATE  DS    CL9                                                              
         DS    CL1                                                              
PRTSYS   DS    CL40                                                             
         DS    CL35                                                             
*                                                                               
         ORG   REPP1                                                            
PRLLIN1  DS    0CL(L'REPP1)        DSECT FOR FULL REPORT                        
PRLAGYD  DS    CL9                                                              
         DS    CL1                                                              
PRLAGY   DS    CL2                                                              
         DS    CL3                                                              
PRLOFFCD DS    CL11                                                             
         DS    CL1                                                              
PRLOFFC  DS    CL2                                                              
         DS    CL3                                                              
PRLNAMED DS    CL9                                                              
         DS    CL1                                                              
PRLNAME  DS    CL22                                                             
         DS    CL3                                                              
PRLCODED DS    CL8                                                              
         DS    CL1                                                              
PRLCODE  DS    CL10                                                             
         DS    CL3                                                              
PRLDATED DS    CL12                                                             
         DS    CL1                                                              
PRLDATE  DS    CL8                                                              
         ORG   REPP2                                                            
PRLLIN2  DS    0CL(L'REPP2)                                                     
         DS    CL132                                                            
         ORG   REPP3                                                            
PRLLIN3  DS    0CL(L'REPP3)                                                     
PRLCMTD  DS    CL9                                                              
         DS    CL1                                                              
PRLCMT   DS    CL60                                                             
         ORG   REPP4                                                            
PRLLIN4  DS    0CL(L'REPP4)                                                     
         DS    CL132                                                            
         ORG   REPP5                                                            
PRLLIN5  DS    0CL(L'REPP5)                                                     
PRLUSRD  DS    CL9                                                              
         DS    CL1                                                              
PRLUSR   DS    CL120                                                            
         ORG   REPP6                                                            
PRLLIN6  DS    0CL(L'REPP6)                                                     
         DS    CL132                                                            
*                                                                               
PRLLINS  DSECT                                                                  
PRLSYS1  DS    CL132                                                            
PRLSYSD  DS    CL9                                                              
         DS    CL1                                                              
PRLSYS   DS    CL8                                                              
         DS    CL4                                                              
PRLLIMD  DS    CL12                                                             
         DS    CL1                                                              
PRLLIM   DS    CL12                                                             
         DS    CL85                                                             
PRLSYS2  DS    CL132                                                            
PRLSYS3  DS    CL132                                                            
PRLSYSN  DS    CL132                                                            
*                                                                               
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
VSCINKEY DS    A                                                                
FIELD    DS    F                                                                
WORK     DS    CL80                                                             
*                                                                               
AUTKEY1  DS    CL25                KEY1 AUTHORIZATION NUMBER                    
AUTKEY2  DS    CL25                KEY2 TYPE NAME INITIALS                      
AUTKEY3  DS    CL25                KEY3 PASSWORD                                
NEWKEY2  DS    CL25                KEY2 NEW TYPE NAME INITS                     
NEWKEY3  DS    CL25                KEY3 NEW PASSWORD                            
*                                                                               
SELKEY1  DS    0XL32                                                            
SELAGY   DS    CL2                 AGENCY                                       
SELKEY1S DS    0CL30                                                            
SELPSWD  DS    CL10                PASSWORD                                     
SELOFFC  DS    CL2                 OFFICE CODE                                  
SELLAST  DS    CL18                LAST NAME                                    
         ORG   SELKEY1+L'SELKEY1                                                
SELK1CL  DS    XL1                 KEY COMPARE LENGTH                           
*                                                                               
RPTWORK  DS    0CL86               REPORT WORK                                  
RPTAGY   DS    CL2                                                              
RPTOFFC  DS    CL2                                                              
RPTNAME  DS    CL22                                                             
RPTCODE  DS    CL10                                                             
RPTDATE  DS    CL9                                                              
RPTSYS   DS    CL40                                                             
RPTFMT   DS    CL1                                                              
*                                                                               
RETURN   DS    F                                                                
PROGRAM  DS    CL1                                                              
SYSTEM   DS    CL1                                                              
AUTALL   DS    H                                                                
SYSCNT   DS    XL1                                                              
ASYSEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
PGNAME   DS    CL8                                                              
*                                                                               
XXCNT    DS    0C                                                               
IDCNT    DS    CL1                                                              
EXCNT    DS    CL1                                                              
PGCNT    DS    CL1                                                              
APCNT    DS    CL1                                                              
ATCNT    DS    CL1                                                              
XXCNTL   EQU   *-XXCNT                                                          
*                                                                               
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
SYSNUMSC DS    XL1                                                              
         DS    XL1                                                              
*                                                                               
TABLE    DS    8XL20                                                            
*                                                                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLOCK3   DS    20CL32                                                           
BLOCK4   DS    20CL32                                                           
*                                                                               
LOCALX   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTGEN11   09/29/05'                                      
         END                                                                    
