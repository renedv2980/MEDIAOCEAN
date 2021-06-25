*          DATA SET CTGEN12    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA0B12A                                                                  
         TITLE 'CTGEN12 - FEES MAINTENANCE - N.I.C. RECORDS'                    
GEN12    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GENC**,RA,RR=RE                                              
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
         CLI   APMODE,APMVALK      WHAT IS THE MODE ?                           
         BE    VALKEY                                                           
         CLI   APMODE,APMVALR                                                   
         BE    VALREC                                                           
         CLI   APMODE,APMDISR                                                   
         BE    DISREC                                                           
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD THE KEY OF THE NIC RECORD (OLDNIC FOR DISP ONLY)   *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   CLI   INREC,RECONIC                                                    
         BNE   VK020                                                            
         CLI   APACTN,ACTDIS                                                    
         BNE   OLDKEY                                                           
VK020    LA    R2,IOKEY                                                         
         USING GFEED,R2            R2=A(NIC RECORD KEY)                         
         XC    GFKEY,GFKEY                                                      
         MVI   GFKMIN,GFKMINQ      MINOR SYSTEM                                 
         MVI   GFKREC,GFKNICQ      NIC RECORD TYPE=N                            
         CLI   INREC,RECONIC                                                    
         BNE   *+8                                                              
         MVI   GFKONIC,GFKONICQ    OLD NIC RECORD (X'01')                       
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
*                                                                               
VK070    TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
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
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD THE KEYS OF THE NIC/OLDNIC RECORDS FOR OLDNIC REPL *         
*     GET NIC RECORD IN IO1                                           *         
*     GET OLDNIC RECORD IN IO2                                        *         
***********************************************************************         
         SPACE 1                                                                
OLDKEY   LA    R2,IOKEY                                                         
         USING GFEED,R2            R2=A(NIC RECORD KEY)                         
         XC    GFKEY,GFKEY                                                      
         MVI   GFKMIN,GFKMINQ      MINOR SYSTEM                                 
         MVI   GFKREC,GFKNICQ      NIC RECORD TYPE=N                            
*                                                                               
         LA    R1,IORDD+IOGENDIR+IO1                                            
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                MUST BE NIC TO REPL OLDNIC WITH              
         LA    R1,IOGET+IOGENFIL+IO1                                            
         GOTO1 AIO                 GET NIC RECORD IN IO1                        
         TM    IOERR,IOERRS        NO ERROR ALLOWED - MUST HAVE NIC REC         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* NOW GET OLDNIC RECORD                                                         
*                                                                               
         XC    GFKEY,GFKEY                                                      
         MVI   GFKMIN,GFKMINQ      MINOR SYSTEM                                 
         MVI   GFKREC,GFKNICQ      NIC RECORD TYPE=N                            
         MVI   GFKONIC,GFKONICQ    OLD NIC RECORD (X'01')                       
         LA    R1,IORDD+IOGENDIR+IO2                                            
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BNE   OLDKEY50            NO OLD NIC RECORD YET                        
         LA    R1,IOGET+IOGENFIL+IO2                                            
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET OLD NIC RECORD                           
         TM    IOERR,IOERRS        NO ERROR ALLOWED                             
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   APRECDA,IODA        SAVE DISK ADDRESS ????????                   
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     OLDKEYY                                                          
*                                                                               
OLDKEY50 MVI   APINDS,APIOKDIS+APIOKADD+APIOKCHA                                
         B     OLDKEYY                                                          
*                                                                               
OLDKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
OLDKEYX  B     EXIT                                                             
*                                                                               
OLDKEYZ  MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE THE NIC RECORD                           *           
***********************************************************************         
         SPACE 1                                                                
*                                                                               
VALREC   CLI   APACTN,ACTREPL      REPLACING OLD NIC                            
         BE    OLDREP                                                           
         GOTO1 AFVAL,NICVATH       VAT PERCENTAGE                               
         BH    VRBADFLD            BAD INPUT                                    
         BL    VRNOINP             NO INPUT                                     
         ZIC   RF,FVILEN                                                        
         GOTO1 VCASHVAL,APDUB,(2,FVIFLD),(RF)                                   
         CLI   0(R1),0                                                          
         BNE   VRBADFLD                                                         
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         CLI   APDUB+4,0                                                        
         BNE   VRBADFLD            IT'S TOO BIG                                 
         C     RE,=F'5000'                                                      
         BH    VRBADFLD                                                         
         STCM  RE,15,SVVAT         STORE RESULT                                 
*                                                                               
*                                  PREPARE LOOP START                           
         SR    R8,R8               USE R8 AS INDEX ON SCREEN FIELDS             
VR010    XC    SAVLINES,SAVLINES   CLEAR SAVE TABLE                             
         LA    R4,MAXLINQ          R4 = 6 LINES                                 
         LA    R9,SAVLINES                                                      
         USING SAVLIND,R9                                                       
*                                                                               
*                                  START LINE VALIDATION LOOP                   
VR020    GOTO1 AFVAL,NICWEL1H(R8)  WEEKLY EARNINGS LIMIT                        
         BH    VRBADFLD            BAD INPUT                                    
         BL    VR030               NO INPUT                                     
         OC    FVIFLD,SPACES                                                    
         ZIC   RF,FVILEN                                                        
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         BNE   VR025                                                            
         CLC   FVIFLD(0),UNLIMITU  MAY BE UPPER LIMIT                           
         MVC   SVWEL,HIGHVAL                                                    
         B     VR030                                                            
*                                                                               
VR025    GOTO1 VCASHVAL,APDUB,(2,FVIFLD),(RF)                                   
         CLI   0(R1),0                                                          
         BNE   VRBADFLD                                                         
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         CLI   APDUB+4,0                                                        
         BNE   VRBADFLD            IT'S TOO BIG                                 
         C     RE,=F'999999999'                                                 
         BH    VRBADFLD                                                         
         STCM  RE,15,SVWEL         STORE RESULT IN TABLE                        
*                                                                               
VR030    GOTO1 AFVAL,NICMEL1H(R8)  MONTHLY EARNINGS LIMIT                       
         BH    VRBADFLD            BAD INPUT                                    
         BL    VR040               NO INPUT                                     
         OC    FVIFLD,SPACES                                                    
         ZIC   RF,FVILEN                                                        
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         BNE   VR035                                                            
         CLC   FVIFLD(0),UNLIMITU  MAY BE UPPER LIMIT                           
         MVC   SVMEL,HIGHVAL                                                    
         B     VR040                                                            
*                                                                               
VR035    GOTO1 VCASHVAL,APDUB,(2,FVIFLD),(RF)                                   
         CLI   0(R1),0                                                          
         BNE   VRBADFLD                                                         
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         CLI   APDUB+4,0                                                        
         BNE   VRBADFLD            IT'S TOO BIG                                 
         C     RE,=F'999999999'                                                 
         BH    VRBADFLD                                                         
         STCM  RE,15,SVMEL         STORE RESULT IN TABLE                        
*                                                                               
*                                                                               
VR040    GOTO1 AFVAL,NICEEA1H(R8)  A TYPE  PERCENTAGE                           
         BH    VRBADFLD            BAD INPUT                                    
         BL    VR050               NO INPUT                                     
         ZIC   RF,FVILEN                                                        
         GOTO1 VCASHVAL,APDUB,(2,FVIFLD),(RF)                                   
         CLI   0(R1),0                                                          
         BNE   VRBADFLD                                                         
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         CLI   APDUB+4,0                                                        
         BNE   VRBADFLD            IT'S TOO BIG                                 
         C     RE,=F'1999'                                                      
         BH    VRBADFLD                                                         
         STCM  RE,3,SVEAS         STORE RESULT IN TABLE                         
*                                                                               
VR050    GOTO1 AFVAL,NICEEB1H(R8)  B TYPE  PERCENTAGE                           
         BH    VRBADFLD            BAD INPUT                                    
         BL    VR060               NO INPUT                                     
         ZIC   RF,FVILEN                                                        
         GOTO1 VCASHVAL,APDUB,(2,FVIFLD),(RF)                                   
         CLI   0(R1),0                                                          
         BNE   VRBADFLD                                                         
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         CLI   APDUB+4,0                                                        
         BNE   VRBADFLD            IT'S TOO BIG                                 
         C     RE,=F'1999'                                                      
         BH    VRBADFLD                                                         
         STCM  RE,3,SVEBS          STORE RESULT IN TABLE                        
*                                                                               
VR060    GOTO1 AFVAL,NICEEC1H(R8)  C TYPE  PERCENTAGE                           
         BH    VRBADFLD            BAD INPUT                                    
         BL    VR080               NO INPUT                                     
         ZIC   RF,FVILEN                                                        
         GOTO1 VCASHVAL,APDUB,(2,FVIFLD),(RF)                                   
         CLI   0(R1),0                                                          
         BNE   VRBADFLD                                                         
         ICM   RE,15,APDUB+4       PICK UP VALUE FROM VCASHVAL                  
         CLI   APDUB+4,0                                                        
         BNE   VRBADFLD            IT'S TOO BIG                                 
         C     RE,=F'1999'                                                      
         BH    VRBADFLD                                                         
         STCM  RE,3,SVECS          STORE RESULT IN TABLE                        
*                                                                               
*                                                                               
*                                  BUMP TO NEXT LINE                            
VR080    LA    R8,NICWEL2H-NICWEL1H(R8)                                         
         LA    R9,SVLENQ(R9)       BUMP TO NEXT TABLE ENTRY                     
         BCT   R4,VR020            LOOP BACK FOR NEXT LINE                      
         SPACE 1                                                                
*                                                                               
***********************************************************                     
* CHECK WEEKLY/MONTHLY AMOUNTS ASCENDING * FROM SAVLINES  *                     
***********************************************************                     
*                                                                               
         LA    R4,MAXLINQ                                                       
         LA    R9,SAVLINES                                                      
         CLI   EESDONE,C'Y'        CHECKING EES/ERS                             
         BE    *+12                                                             
         LA    R1,NICWEL1H         EES                                          
         B     *+8                                                              
         LA    R1,NICWEL3H         ERS                                          
VR100    ST    R1,APCURSOR         CURSOR TO LINE WE ARE CHECKING               
         ICM   RE,15,SVWEL         PICK UP WKLY    LIMIT                        
         BNZ   VR110               ITS > ZERO                                   
         ICM   RF,15,SVMEL         IF WEEKLY ZERO SO MUST MONTHLY               
         BNZ   VRBADVAL            WOOPS                                        
         OC    SVEAS(8),SVEAS      IF BOTH ZERO ALL %AGES MUST BE               
         BNZ   VRBADVAL            WOOPS THEY ARE NOT                           
         B     VR120               IF ALL ZERO THIS LINE OK                     
VR110    ICM   RF,15,SVMEL         PICK UP MONTHLY LIMIT                        
         BZ    VRBADVAL            WOOPS CANT BE 0 IF WKLY WAS >0               
         CLC   SVMEL,HIGHVAL                                                    
         BNE   VR115                                                            
         CLC   SVWEL,HIGHVAL       BOTH 'UNLIMITED'                             
         BE    *+10                                                             
VR115    CR    RE,RF               WEEKLY < MONTHLY                             
         BNL   VRBADVAL            WOOPS THIS CANT BE RIGHT                     
         CH    R4,=Y(MAXLINQ)      IF 1ST LOOP                                  
         BE    VR120               THEN JUMP                                    
         CLC   SVWEL,0(R8)         ASCENDANCY CHECK - WEEKLY                    
         BNH   VRBADVAL                                                         
         CLC   SVMEL,4(R8)         ASCENDANCY CHECK - MONTHLY                   
         BNH   VRBADVAL                                                         
VR120    LR    R8,R9               SAVE ADDRESS OF LAST LINE                    
         LA    R9,SVLENQ(R9)       BUMP TO NEXT TABLE ENTRY                     
         LA    R1,NICWEL2H-NICWEL1H(R1) & CURSOR TO NEXT LINE                   
         BCT   R4,VR100            LOOP BACK FOR TABLE ENTRY                    
*                                                                               
*                                  END OF EMPLOYEE INPUT                        
*                                  REPEAT PROCESS FOR EMPLOYERS                 
         CLI   EESDONE,C'Y'                                                     
         BE    VR200               EES + ERS DONE                               
         MVI   EESDONE,C'Y'                                                     
         MVC   SAVLINS2,SAVLINES   HOLD ON TO EES SAVLINES                      
         LA    R8,NICWEL3H-NICWEL1H  SET R8 TO FIRST ERS-EES DIFF               
         B     VR010               REPEAT FOR ERS                               
*                                                                               
******************************************                                      
* BUILD NIC ELEMENTS                     *                                      
******************************************                                      
*                                                                               
VR200    L     R2,AIOAREA1                                                      
         MVC   GFKEY,APRECKEY                                                   
         CLI   APACTN,ACTADD                                                    
         BE    VR210               NO ELEMENT TO REMOVE ON ADD                  
         MVI   APELEM,GFNICELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GFEED       DELETE EXISTING NIC ELEMENTS                 
         MVI   APELEM,GFVATELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GFEED       DELETE EXISTING VAT ELEMENT                  
*                                  BUILD VAT                                    
VR210    LA    R3,APELEM                                                        
         USING GFVATD,R3           R3=A(NIC ELEMENT)                            
         XC    APELEM,APELEM                                                    
         MVI   GFVATEL,GFVATELQ                                                 
         MVI   GFVATELL,GFVATLNQ                                                
         MVC   GFVATP,SVVAT        SET VAT PERCENTAGE                           
         GOTO1 AADDELS,GFEED                                                    
*                                  BUILD NIC                                    
         LA    R3,APELEM                                                        
         USING GFNICD,R3           R3=A(NIC ELEMENT)                            
         XC    APELEM,APELEM                                                    
         MVI   GFNICEL,GFNICELQ                                                 
         MVI   GFNICELL,GFNICLNQ                                                
         MVI   GFNICTYP,C'E'       SET AS EES DATA                              
         LA    R4,MAXLINQ                                                       
         LA    R9,SAVLINS2         POINT TO SAVED EES VALUES                    
VR240    OC    SVLINE,SVLINE       IF TABLE ENTRY EMPTY                         
         BZ    VR250               BYPASS BUILD                                 
         MVC   GFNICWHI(SVLENQ),SVLINE                                          
         GOTO1 AADDELS,GFEED                                                    
VR250    LA    R9,SVLENQ(R9)       BUMP DOWN TABLE                              
         BCT   R4,VR240            LOOP BACK                                    
*                                                                               
*                                  REPEAT THE PROCESS FOR ERS DATA              
*                                                                               
         MVI   GFNICTYP,C'R'       SET AS EES DATA                              
         LA    R4,MAXLINQ                                                       
         LA    R9,SAVLINES         POINT TO SAVED EES VALUES                    
VR260    OC    SVLINE,SVLINE       IF TABLE ENTRY EMPTY                         
         BZ    VR270               BYPASS BUILD                                 
         MVC   GFNICWHI(SVLENQ),SVLINE                                          
         GOTO1 AADDELS,GFEED                                                    
VR270    LA    R9,SVLENQ(R9)       BUMP DOWN TABLE                              
         BCT   R4,VR260            LOOP BACK                                    
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
         B     VALXXXOK            DO IT OURSELF ON CHANGE IF REQD              
*                                  ERROR RETURNS                                
VRBADFLD MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALXXBAD                                                         
VRBADVAL MVC   FVMSGNO,=AL2(CTILLOG)                                            
         B     VALXXBAD                                                         
VRNOINP  MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VALXXBAD                                                         
*                                                                               
VALXXXOK MVC   FVMSGNO,=AL2(FVFOK) GOOD RETURN                                  
         LA    R1,GENACTH          CURSOR TO NEXT ACTION                        
         ST    R1,APCURSOR                                                      
         B     DISREC              REDISPLAY THE RECORD                         
VALXXBAD B     EXIT                                                             
*                                                                               
         DROP  R3,R9                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO REPLACE/ADD OLD NIC RECORD                              *          
**********************************************************************          
         SPACE 2                                                                
OLDREP   L     R2,AIOAREA1         R2=A(NIC RECORD)                             
         USING GFEED,R2                                                         
         L     R3,AIOAREA2         R3=A(OLD NIC RECORD)                         
         LA    R0,GFFIRST(R3)                                                   
         LA    RE,GFFIRST(R2)                                                   
         SR    R1,R1                                                            
         ICM   R1,3,GFFLEN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE NIC RECORD OVER OLD NIC                 
         DROP  R2                                                               
*                                                                               
         USING GFEED,R3                                                         
         MVC   GFKEY(GFKEYL+L'GFFLEN),0(R2)    SET OLD NIC KEY                  
         MVI   GFKONIC,GFKONICQ                                                 
         TM    APINDS,APIOKADD                                                  
         BNZ   OLDREP10                                                         
         LA    R1,IOPUT+IOGENFIL+IO2    REPLACE OLD NIC                         
         B     OLDREPX                                                          
         DROP  R3                                                               
*                                                                               
OLDREP10 LA    R1,IOADD+IOGENFIL+IO2    ADD FIRST OLD NIC                       
*                                                                               
OLDREPX  GOTO1 AIO                                                              
         L     R2,AIOAREA2                                                      
         B     DISOLD                   RE-DISPLAY OLD NIC RECORD               
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO DISPLAY NIC RECORD                                      *          
**********************************************************************          
*                                                                               
DISREC   L     R2,AIOAREA1                                                      
         USING GFEED,R2                                                         
DISOLD   TWAXC NICWEL1H                                                         
*                                  DISPLAY VAT PERCENTAGE                       
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GFVATELQ     FIND VAT ELEMENT                             
         GOTO1 AGETELS,GFEED                                                    
         ICM   R3,15,APPARM        R3=A(VAT ELEMENT)                            
         USING GFVATD,R3                                                        
         EDIT  (B4,GFVATP),(10,APWORK),2,WRK=OVWORK,DUB=APDUB                   
         GOTO1 DISPFLD,NICVATH                                                  
*                                                                               
         SR    R8,R8                                                            
         MVI   EESDONE,0                                                        
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GFNICELQ     FIND NIC ELEMENT                             
DREC02   GOTO1 AGETELS,GFEED                                                    
         ICM   R3,15,APPARM        R3=A(NIC ELEMENT)                            
         USING GFNICD,R3                                                        
         BZ    DISRECX                                                          
DREC05   LA    R9,NICWEL1(R8)                                                   
         CLC   GFNICWHI,HIGHVAL                                                 
         BNE   DREC05A                                                          
         MVC   0(L'UNLIMIT,R9),UNLIMIT                                          
         B     DREC05B                                                          
*                                                                               
DREC05A  EDIT  (B4,GFNICWHI),(10,(R9)),2,WRK=APWORK,DUB=APDUB                   
*                                                                               
DREC05B  LA    R9,NICMEL1(R8)                                                   
         CLC   GFNICMHI,HIGHVAL                                                 
         BNE   DREC05C                                                          
         MVC   0(L'UNLIMIT,R9),UNLIMIT                                          
         B     DREC05D                                                          
*                                                                               
DREC05C  EDIT  (B4,GFNICMHI),(10,(R9)),2,WRK=APWORK,DUB=APDUB                   
*                                                                               
DREC05D  LA    R9,NICEEA1(R8)                                                   
         EDIT  (B2,GFNICAPC),(5,(R9)),2,WRK=APWORK,DUB=APDUB                    
         OI    1(R9),X'F0'         SHOW .00 AS 0.00                             
         LA    R9,NICEEB1(R8)                                                   
         EDIT  (B2,GFNICBPC),(5,(R9)),2,WRK=APWORK,DUB=APDUB                    
         OI    1(R9),X'F0'         SHOW .00 AS 0.00                             
         LA    R9,NICEEC1(R8)                                                   
         EDIT  (B2,GFNICCPC),(5,(R9)),2,WRK=APWORK,DUB=APDUB                    
         OI    1(R9),X'F0'         SHOW .00 AS 0.00                             
*                                                                               
         LA    R9,NICEEL1H(R8)     POINT TO INPUT LITERALS                      
         GOTO1 AFVAL,(R9)                                                       
         BE    DREC10              LITERAL ALREADY THERE                        
         CLI   GFNICTYP,C'E'       EES DATA                                     
         BE    DREC07                                                           
         MVCDD APWORK(10),CT#LESS,R                                             
         B     DREC08                                                           
DREC07   MVCDD APWORK(10),CT#BLNCE,R                                            
DREC08   LR    R1,R9                                                            
         GOTO1 DISPFLD                                                          
*                                                                               
DREC10   BAS   RE,NEXTEL           GET NEXT ELEMENT R3=A(CURRENT EL)            
*                                  BUMP TO NEXT LINE                            
         BNE   DISRECX                                                          
         CLI   GFNICTYP,C'E'       EES DATA                                     
         BE    DREC20                                                           
         CLI   EESDONE,C'Y'        DOING 1ST ERS                                
         BE    DREC20              NO                                           
         MVI   EESDONE,C'Y'        SET ON FIRST ERS                             
         LA    R8,NICWEL3H-NICWEL1H      JUMP TO ERS LINES                      
         B     DREC05                                                           
DREC20   LA    R8,NICWEL2H-NICWEL1H(R8)                                         
         BE    DREC05                                                           
*                                                                               
DISRECX  GOTO1 ADISACT,GFEED       DISPLAY ACTIVITY DATE                        
         CLI   APACTN,ACTREPL      EXIT IF DISPLAYING NIC                       
         BNE   EXIT                                                             
         LA    R1,GENACTH                                                       
         ST    R1,APCURSOR                                                      
         MVI   FVOMTYP,GTMINF                                                   
         MVI   APMODE,APMFMOK                                                   
         C     R2,AIOAREA2                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CURNICDQ)  CURR NIC DISP - NTR TO REPL OLD          
         B     EXIT                                                             
         MVC   FVMSGNO,=AL2(OLDNICRQ)  OLD NIC REPLACED                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
         SPACE 1                                                                
****************************************************************                
* DISPFLD DISPLAYS FROM APWORK INTO SCREEN FIELD POINTED BY R1 *                
****************************************************************                
*                                                                               
DISPFLD  ZIC   R4,0(R1)                                                         
         SH    R4,=H'9'                                                         
         TM    1(R1),X'02'         IF ITS AN EXTENDED FIELD                     
         BZ    *+8                                                              
         SH    R4,=H'8'            THEN REDUCE LENGTH BY ANOTHER 8              
         EX    R4,DSPCLC           IS DATA ON SCREEN ALREADY                    
         BER   RE                                                               
         EX    R4,DSPMVC           NO - SO MOVE IT THERE                        
         OI    6(R1),X'80'              AND TRANSMIT                            
         BR    RE                                                               
         SPACE 1                                                                
DSPCLC   CLC   8(0,R1),APWORK                                                   
DSPMVC   MVC   8(0,R1),APWORK                                                   
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
UNLIMIT  DC    C'Unlimited'                                                     
UNLIMITU DC    C'UNLIMITED'                                                     
*                                                                               
HIGHVAL  DC    XL4'7FFFFFFF'                                                    
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* DDFLDHDR                                                                      
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
* GEGENMSG                                                                      
       ++INCLUDE GEGENFEE                                                       
* CTDDEQUS                                                                      
       ++INCLUDE CTDDEQUS                                                       
* FAGETTXTD                                                                     
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENEDD                                                       
         ORG                                                                    
         EJECT                                                                  
SAVLIND  DSECT                     ** DSECT TO COVER SAVED LINE *               
SVLINE   DS    0XL(SVLENQ)                                                      
SVWEL    DS    XL4     B           WEEKLY EARNINGS LIMIT                        
SVMEL    DS    XL4     B           MONTHLY EARNINGS LIMIT                       
SVEAS    DS    XL2     B           A - %AGE                                     
SVEBS    DS    XL2     B           B - %AGE                                     
SVECS    DS    XL2     B           C - %AGE                                     
SVLENQ   EQU   *-SVWEL             SIZE OF SAVLINE ENTRY                        
         SPACE 1                                                                
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
*                                                                               
SVVAT    DS    XL4                                                              
OVWORK   DS    XL20                                                             
EESDONE  DS    XL1                 Y =  ERS PASS 0 = EES PASS                   
MAXLINQ  EQU   6                   6 LINES OF EES / 6 LINES OF ERS              
SAVLINES DS    XL(MAXLINQ*SVLENQ)  ** BINARY VALUES OF INPUT LINES **           
SAVLINS2 DS    XL(MAXLINQ*SVLENQ)  ** BINARY VALUES OF INPUT LINES **           
         ORG   SAVLINES+L'SAVLINES+L'SAVLINS2                                   
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTGEN12   05/01/02'                                      
         END                                                                    
