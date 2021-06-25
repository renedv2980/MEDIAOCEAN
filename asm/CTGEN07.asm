*          DATA SET CTGEN07    AT LEVEL 010 AS OF 05/01/02                      
*PHASE TA0B07A                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'CTGEN07 - USER ID ATTENTION DATA MAINTENANCE'                   
GEN07    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN7**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(RECORD KEY)                             
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
         B     VALKEY              APMVALK                                      
         B     VALREC              APMVALR                                      
         B     DISKEY              APMDISK                                      
         B     DISREC              APMDISR                                      
         B     EXIT                APMDELR                                      
         B     EXIT                APMRESR                                      
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
         B     EXIT                APMSETT                                      
         B     EXIT                APMPUTK                                      
         B     VALREC              APMNEWK                                      
         B     EXIT                APMFRP                                       
         B     EXIT                APMDISS2                                     
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF USER ID RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         GOTO1 AFVAL,IDAIDH        VALIDATE USER ID                             
         BNE   VALKEYX                                                          
         TM    FVIIND,FVINUM       CHECK FOR NUMERIC INPUT                      
         BO    VKUSR10                                                          
         CLI   FVILEN,3              ELSE USER ID CHARACTERS                    
         BL    EFTS                                                             
         MVC   CTIKID,FVIFLD                                                    
         B     VKUSRX                                                           
*                                  BUILD KEY OF NUMERIC REC & READ              
VKUSR10  OC    SCFULL(4),SCFULL    NUMBER IN SCFULL FROM AFVAL                  
         BZ    EIIF                CHECK NUMBER IN RANGE                        
         OC    SCFULL(2),SCFULL                                                 
         BNZ   EFTB                CHECK NUMBER IN RANGE                        
         MVC   CTIKNUM,SCFULL+2                                                 
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                 READ ID RECORD                               
         BNE   ERNF                RECORD MUST BE PRESENT & CORRECT             
         L     R2,AIOAREA2                                                      
         LA    R3,CTIDATA                                                       
         SR    RF,RF                                                            
VKUSR20  CLI   0(R3),0             FIND PASSIVE POINTER                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VKUSR20                                                          
         MVC   IDAID,2(R3)                                                      
         MVI   IDAIDH+5,8                                                       
         NI    IDAIDH+4,X'F7'                                                   
         OI    IDAIDH+6,X'80'                                                   
         B     VALKEY                                                           
VKUSRX   EQU   *                                                                
* VALIDATE THIS/LAST ACTIONS                                                    
*                                                                               
VKIO     MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
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
VALKEYY  CLI   APACTN,ACTCPY                                                    
         BNE   VALKEYY1                                                         
         TM    ACLFMIND,ACLFMIFK   FIRST VALKEY                                 
         BNZ   VALKEYY1                                                         
         MVC   IOKEY,SAVKEY                                                     
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO1                                    
         BE    *+6                                                              
         DC    H'00'                                                            
VALKEYY1 MVC   SAVKEY,APRECKEY                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE USER ID RECORDS                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1         INITIALISE RECORD                            
         MVC   CTIKEY,APRECKEY                                                  
*                                  CHANGE FUNCTION - SAVE ORIG STATUS           
VRINI    CLI   APACTN,ACTCHA                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   APFLAG,0                                                         
         LA    R3,CTIDATA          AND STRIP DOWN RECORD                        
VRINI10  CLI   0(R3),0                                                          
         BE    VRINIX                                                           
         CLI   0(R3),X'01'         ACTIVITY                                     
         BE    VRINI30             DELETE ELEMENT                               
         CLI   0(R3),X'31'                                                      
         BE    VRINI30                                                          
         CLI   0(R3),X'02'                                                      
         BNE   VRINI20                                                          
         MVC   IDNUM,2(R3)                                                      
         MVI   APFLAG,1                                                         
VRINI20  ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRINI10                                                          
*                                                                               
VRINI30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CTIREC),0,0                    
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRINI10                                                          
*                                                                               
VRINIX   CLI   APFLAG,1                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   IDALP,FVIFLD                                                     
         B     VRATD                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE LINES OF ATTENTION DETAIL ELEMENT DATA                     *         
***********************************************************************         
         SPACE 1                                                                
         USING LINED,R4                                                         
VRATD    LA    R4,IDAATTAH         R4=A(FIRST TWA INPUT FIELD)                  
*                                                                               
VRATD10  CLI   0(R4),10            END OF TWA                                   
         BL    VRATDX                                                           
         LA    R1,LITYPEH                                                       
         GOTO1 AFVAL                                                            
         BE    *+16                                                             
         CLI   LINAMEH+5,0                                                      
         BNE   EMIF                                                             
         B     VRATD40                                                          
         MVC   APWORK(3),FVIFLD                                                 
         LA    R1,LINAMEH                                                       
         GOTO1 AFVAL                                                            
         BNE   EXIT                                                             
         LA    R1,LITYPEH                                                       
         ST    R1,FVADDR                                                        
         XC    APELEM,APELEM       BUILD ATTENTION DETAIL ELEMENT               
         MVI   APELEM,X'31'                                                     
         MVI   APELEM+1,X'05'                                                   
         LA    R3,APELEM                                                        
         USING CTATTND,R3                                                       
         MVC   CTATTTYP,APWORK                                                  
         MVC   CTATTDET,FVIFLD                                                  
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         LA    R1,5(R1)                                                         
         STC   R1,CTATTLEN                                                      
*                                                                               
         LA    R8,CTIDATA          SEE IF DUPLICATE ATTN TYPE ON REC            
         SR    R1,R1                                                            
*                                                                               
VRATD20  CLI   0(R8),0                                                          
         BE    VRATD30                                                          
         CLI   0(R8),X'31'                                                      
         BNE   *+14                                                             
         CLC   2(3,R8),CTATTTYP                                                 
         BE    EDIF                DUPLICATE INPUT FIELD (TYPE)                 
         IC    R1,1(R8)                                                         
         AR    R8,R1                                                            
         B     VRATD20                                                          
         DROP  R3                                                               
*                                                                               
VRATD30  GOTO1 AADDELN,CTIREC      ADD ELEMENT                                  
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
VRATD40  LA    R4,LINEXT(R4)       BUMP TO NEXT TWA LINE                        
         B     VRATD10                                                          
         DROP  R4                                                               
*                                                                               
VRATDX   B     VRUPD                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
VRUPD    GOTO1 ASETACN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
         LA    R1,GENACTH          SET CURSOR IN CASE OF ERROR                  
         ST    R1,FVADDR                                                        
         L     RF,=A(CHKBIG)       CHECK RECORD WILL BE TOO BIG                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   VALRECER            EXIT WITH ERROR MESSAGE                      
*                                  WRITE ID RECORD                              
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         MVC   KEYSAVE,IOKEY                                                    
*                                  WRITE ID RECORD                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,IDNUM                                                    
*                                  WRITE ID# PASSIVE RECORD                     
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTIREC                                                   
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,X'0C'                                                   
         MVC   APELEM+2(10),KEYSAVE+CTIKID-CTIKEY                               
         GOTO1 AADDELN,CTIREC                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R2,AIOAREA1                                                      
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VALRECX                                                          
*                                  EXIT RECORD VALIDATION AND UPDATE OK         
VALRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECER B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF USER ID RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
DISKEY   LA    R2,APRECKEY                                                      
         MVC   IDAID,CTIKID                                                     
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY USER ID RECORD ATTENTION DATA                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
DISREC   L     R2,AIOAREA1                                                      
         TWAXC IDAATTAH                                                         
         LA    R4,IDAATTAH                                                      
         USING LINED,R4            R5=A(TWA LINE)                               
         LA    R3,CTIDATA          R3=A(FIRST ELEMENT)                          
*                                                                               
DREC10   CLI   0(R3),0             END OF RECORD                                
         BE    DRECX                                                            
         CLI   0(R3),X'31'         ATTENTION DETAIL                             
         BE    DREC30                                                           
*                                                                               
DREC20   SR    R1,R1               BUMP TO NEXT ELEMENT                         
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DREC10                                                           
*                                                                               
DREC30   DS    0H                  DISPLAY ATTN DETAIL ELEMENT                  
         USING CTATTND,R3                                                       
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         SH    R1,=H'6'            R1=L'NAME                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINAME(0),CTATTDET                                               
         MVC   LITYPE,CTATTTYP                                                  
         LA    R4,LINEXT(R4)                                                    
         B     DREC20                                                           
         DROP  R3                                                               
*                                                                               
DRECX    GOTO1 ADISACT,CTIREC                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
VALSEL   LA    R2,APRECKEY                                                      
         XC    SELDATA,SELDATA                                                  
*                                                                               
         LA    R4,LSTIDH                                                        
         L     RF,=A(VALPARS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF               GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         XC    CTIKEY,CTIKEY       BUILD AN INITIAL KEY                         
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SELID                                                     
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
         USING CTIREC,R2                                                        
GETSEL   LA    R2,IOKEY            READ NEXT LIST RECORD                        
         MVC   CTIKEY,APRECKEY       FROM LAST SAVED KEY                        
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
*                                  READ PAST PASSIVE # RECORDS                  
         OC    CTIKID(L'CTIKID-L'CTIKNUM),CTIKID                                
         BNZ   *+8                                                              
         MVI   CTIKID+L'CTIKID-L'CTIKNUM-1,1                                    
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         L     RF,=A(GETREC)       GO SELECT NEXT RECORD                        
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETSELN                                                          
         B     GETSELY                                                          
GSEL20   LA    R1,IOCONFIL+IOSQ+IO1                                             
         L     RF,=A(GETREC)       GO SELECT NEXT RECORD                        
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETSELN             (EOF)                                        
*                                                                               
GETSELY  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
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
         USING CTIREC,R2                                                        
DISSEL   EQU   *                                                                
*                                                                               
         L     R4,APPARM                                                        
         L     RF,=A(LINE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
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
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
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
         LA    R4,REPIDH                                                        
         L     RF,=A(VALPARS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF               GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
*                                                                               
         LA    R2,APRECKEY         BUILD AN INITIAL KEY                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SELID                                                     
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
* ROUTINE TO PRINT USER ID LIST                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
PRTREP   EQU   *                                                                
         L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
         MVC   CTIKEY,APRECKEY                                                  
*                                  READ PAST PASSIVE # RECORDS                  
         OC    CTIKID(L'CTIKID-L'CTIKNUM),CTIKID                                
         BNZ   *+8                                                              
         MVI   CTIKID+L'CTIKID-L'CTIKNUM-1,1                                    
*                                                                               
         LA    R1,IOHI+IOCONFIL+IO1                                             
         L     RF,=A(GETREC)         GO GET REC WIV                             
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   PRTREPX                                                          
         B     PREC100                                                          
*                                                                               
PREC010  TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    PREC020                                                          
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     PREC030                                                          
PREC020  TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    PREC040                                                          
         NI    APINDS,X'FF'-APILRERD                                            
PREC030  LA    R2,IOKEY                                                         
         MVC   CTIKEY,APRECKEY                                                  
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    PREC040                                                          
         B     PRTREPX                                                          
*                                                                               
PREC040  LA    R1,IOSQ+IOCONFIL+IO1                                             
         L     RF,=A(GETREC)       GO GET NEXT REC                              
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   PRTREPX                                                          
*                                                                               
PREC100  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
         LA    R4,REPP1-14                                                      
         L     RF,=A(LINE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF               GO BUILD A PRINT LINE                        
*                                                                               
         GOTO1 VREPORT,REPD                                                     
         B     PREC010                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
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
ERTB     MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD TOO BIG                               
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
REPDESCL DC    C'USER ID LIST'                                                  
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'USER ID LIST'                                            
         SPEC  H2,57,C'------------'                                            
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
*                                  HEADINGS FOR REPORT SCREEN                   
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
         SPACE 1                                                                
**********************************************************************          
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                            *          
* R4 = A(FIRST FIELD HEADER IN STANDARD DISPLAY)                     *          
*   APPLICABLE TO BOTH LIST AND REPORT SCREEN FIELD OFFSETS          *          
**********************************************************************          
         SPACE 1                                                                
         USING CTIREC,R2                                                        
         USING LSTIDH,R4                                                        
VALPARS  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALPARS,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VPA'    INSERT NAME                                  
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VPID     EQU   *                                                                
         GOTO1 AFVAL,LSTIDH        STORE USER ID                                
         BNE   VPIDX               (IF ENTERED)                                 
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VPID1    CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VPID2               FOR KEY COMPARE IN GETREC                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VPID1                                                         
VPID2    STC   RE,SELKEYCL                                                      
         MVC   SELID,FVIFLD                                                     
         MVC   SELIDL,FVILEN                                                    
         MVC   SELIDSP,0(RF)                                                    
VPIDX    EQU   *                                                                
*                                                                               
VPSYS    EQU   *                   VALIDATE SYSTEM                              
         GOTO1 AFVAL,LSTSYSH                                                    
         BNE   VPSYSX                                                           
         L     RF,ASYSLST          LOOK UP NAME IN SYSLST                       
         LA    RF,6(,RF)                                                        
         USING SYSLSTD,RF                                                       
         ZIC   RE,FVILEN                                                        
         BCTR  RE,0                                                             
VPSYS3   CLI   SYSLNUM,0                                                        
         BE    VPSYS1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD                                               
         BE    VPSYS4                                                           
         LA    RF,SYSLLEN(,RF)                                                  
         B     VPSYS3                                                           
VPSYS4   MVC   SELSYS,SYSLNUM                                                   
         B     VPSYSX                                                           
VPSYS1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPSYSX   EQU   *                                                                
*                                                                               
VPPGM    EQU   *                   VALIDATE PROGRAM                             
         GOTO1 AFVAL,LSTPGMH                                                    
         BNE   VPPGMX                                                           
         OC    SELSYS,SELSYS                                                    
         BNZ   VPPGM2                                                           
         MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VALPARSX            PROGRAM NAME INVALID WITHOUT SYS             
VPPGM2   GOTO1 AVALPGM,APPARM,(SELSYS,LSTPGMH)                                  
         BNE   VPPGM1                                                           
         MVC   SELPGM,APWORK                                                    
         B     VPPGMX                                                           
VPPGM1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPPGMX   EQU   *                                                                
*                                                                               
VPAGY    EQU   *                   VALIDATE AGENCY ALPHA ID                     
         GOTO1 AFVAL,LSTAGYAH                                                   
         BNE   VPAGYX                                                           
         USING CT5REC,R1                                                        
         LA    R1,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    VPAGY2                                                           
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALPARSX            ACCESS RECORD NOT FOUND                      
VPAGY2   MVC   SELAGY,FVIFLD                                                    
         B     VPAGYX                                                           
VPAGY1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPAGYX   EQU   *                                                                
*                                                                               
VPPWD    EQU   *                   VALIDATE PASSWORD REQUIRED                   
         GOTO1 AFVAL,LSTPWDH                                                    
         BNE   VPPWDX                                                           
         CLI   FVIFLD,C'Y'                                                      
         BE    VPPWD2                                                           
         CLI   FVIFLD,C'N'                                                      
         BNE   VPPWD1                                                           
VPPWD2   MVC   SELPWD,FVIFLD                                                    
         B     VPPWDX                                                           
VPPWD1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPPWDX   EQU   *                                                                
*                                                                               
VPCID    EQU   *                   VALIDATE COMPATIBLE USER ID                  
         GOTO1 AFVAL,LSTCIDH                                                    
         BNE   VPCIDX                                                           
         OC    SELAGY,SELAGY                                                    
         BNZ   VPCID2                                                           
         MVC   FVMSGNO,=AL2(CE#CIDAG)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VALPARSX            PROGRAM NAME INVALID WITHOUT SYS             
VPCID2   L     R2,AIOAREA2         SWITCH IO AREAS                              
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,FVIFLD                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   VPCID1                                                           
         MVC   IOKEY,APRECKEY                                                   
         MVC   SELCID,CTIKID                                                    
         B     VPCIDX                                                           
VPCID1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*        B     VALPARSX                                                         
VPCIDX   EQU   *                                                                
*                                                                               
VALPARSX XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS       *          
**********************************************************************          
         SPACE 1                                                                
         USING CTIREC,R2                                                        
GETREC   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETREC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GRE'    INSERT NAME                                  
*                                                                               
         B     GETRECIO            PRESERVE VALUE OF R1 ON ENTRY                
GETRECRD TM    GETSEQF,APILRERD    READ NEXT RECORD                             
         BZ    GETRECSQ            CHECK SEQUENCE BROKEN                        
         NI    GETSEQF,X'FF'-APILRERD                                           
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETRECN                                                          
GETRECSQ LA    R1,IOCONFIL+IOSQ+IO1                                             
GETRECIO GOTO1 AIO                                                              
         BNE   GETRECN                                                          
         L     R2,AIOAREA1                                                      
         OC    CTIKID-CTIKEY(10,R2),CTIKID-CTIKEY(R2)                           
         BZ    GETRECN                                                          
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(CTIKID-CTIKEY),CTIKEY                                   
         BNE   GETRECN                                                          
         SPACE 1                                                                
*                                  * FILTER ON SELECTION CRITERIA *             
         SPACE 1                                                                
GRID     CLI   SELIDSP,C' '       USER ID - FILTER ONLY IF IT                   
         BNH   GRIDX                 CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GRID1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTIKID(0),SELID                                                  
         BH    GETRECN             (NO MORE RELEVENT RECORDS)                   
GRID1    GOTO1 ATXTFLT,APPARM,(SELIDL,SELID),(8,CTIKID)                         
         BNE   GETRECRD            READ NEXT RECORD                             
GRIDX    EQU   *                                                                
*                                                                               
GRSYS    EQU   *                   FILTER ON SYSTEM                             
         OC    SELSYS,SELSYS                                                    
         BZ    GRPGMX                                                           
         LR    R3,R2                                                            
         MVI   APELEM,X'21'        GET SYSTEM ELEMS                             
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         B     GRSYS3                                                           
GRSYS2   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECRD            READ NEXT RECORD                             
         CLI   0(R3),X'21'                                                      
         BNE   GRSYS2                                                           
         USING CTSYSD,R3                                                        
GRSYS3   CLC   CTSYSNUM,SELSYS                                                  
         BNE   GRSYS2                                                           
GRSYSX   EQU   *                                                                
*                                                                               
GRPGM    EQU   *                   FILTER ON PROGRAM                            
         OC    SELPGM,SELPGM                                                    
         BZ    GRPGMX                                                           
         MVC   PROGRAM,SELPGM                                                   
         LA    R1,CTSYSPGM         POINT TO SYSTEM ELEMENT                      
         ZIC   RE,CTSYSLEN                                                      
*                                  FIND PROGRAM IN ELEMENT                      
GRPGM10  CH    RE,=Y(CTSYSL1Q)                                                  
         BNH   GRPGM30             END OF ELEMENT                               
         CLC   SELPGM,0(R1)                                                     
         BE    GRPGM20             PROGRAM FOUND                                
         LA    R1,L'CTSYSPGM(R1)   GET NEXT PROGRAM                             
         SH    RE,=Y(L'CTSYSPGM)                                                
         B     GRPGM10                                                          
GRPGM20  OC    1(2,R1),1(R1)       CHECK PROGRAM ACCESS=N                       
         BZ    GETRECRD                                                         
         B     GRPGMX                                                           
GRPGM30  OC    CTSYSALL,CTSYSALL   CHECK ALL ACCESS=N                           
         BZ    GETRECRD                                                         
         B     GRPGMX                                                           
GRPGMX   EQU   *                                                                
*                                                                               
GRAGY    EQU   *                   FILTER ON AGENCY ALPHA ID                    
         OC    SELAGY,SELAGY                                                    
         BZ    GRAGYX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ     GET AGENCY ELEMENT                           
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         USING CTAGYD,R3                                                        
         CLC   SELAGY,CTAGYID                                                   
         BNE   GETRECRD                                                         
GRAGYX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GRPWD    EQU   *                   FILTER ON PASSWORD REQUIRED                  
         OC    SELPWD,SELPWD                                                    
         BZ    GRPWDX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'07'        GET IDOPTS ELEMENT                           
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         CLI   SELPWD,C'Y'                                                      
         BNE   GRPWD10                                                          
         TM    2(R3),X'80'                                                      
         BZ    GETRECRD                                                         
         B     GRPWDX                                                           
GRPWD10  TM    2(R3),X'80'                                                      
         BNZ   GETRECRD                                                         
GRPWDX   EQU   *                                                                
*                                                                               
GRCID    EQU   *                   FILTER ON COMPATIBLE USER ID                 
         OC    SELCID,SELCID                                                    
         BZ    GRCIDX                                                           
*                                  BUILD COMPATIBLE ID TABLE                    
         OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         GOTO1 VGETIDS,APPARM,(C'C',(R2)),0,VDMGR                               
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               IO ERROR                                     
         CLI   0(R1),0                                                          
         BE    GETRECRD            NULL TABLE                                   
         L     R1,4(R1)                                                         
GRCID1   CLI   0(R1),X'FF'         END OF TABLE ?                               
         BE    GETRECRD            NO MATCH, READ NEXT RECORD                   
         CLC   0(10,R1),=CL10'ALL'                                              
         BNE   GRCID2                                                           
         TM    CUSTAT,CUSDDS       ONLY VALID FOR DDS TERMINALS                 
         BZ    GRCID3                                                           
         B     GRCIDX                                                           
GRCID2   CLC   0(10,R1),SELCID                                                  
         BE    GRCIDX                                                           
GRCID3   LA    R1,12(R1)           GET NEXT TABLE ENTRY                         
         B     GRCID1                                                           
GRCIDX   EQU   *                                                                
*                                                                               
GETRECY  SR    RC,RC               RETURN CC EQUAL RECORD OK                    
GETRECN  LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD A LINE OF USER ID RECORD DATA                                 *         
***********************************************************************         
         SPACE                                                                  
         USING CTIREC,R2                                                        
LINE     CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING LINE,RB                                                          
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+LIN'    INSERT NAME                                  
*                                                                               
         L     R2,AIOAREA1                                                      
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
         MVC   LISTID,CTIKID                                                    
*                                                                               
LNTYP    EQU   *                   LIST OF ATTENTION TYPES                      
         XC    APWORK,APWORK                                                    
         LA    R8,APWORK                                                        
         LA    R3,CTIDATA                                                       
LNTYP10  CLI   0(R3),0             END OF RECORD                                
         BE    LNTYP40                                                          
         CLI   0(R3),X'31'         ATTENTION DETAIL                             
         BE    LNTYP30                                                          
         SR    R1,R1               BUMP TO NEXT ELEMENT                         
LNTYP20  IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     LNTYP10                                                          
*                                                                               
         USING CTATTND,R3                                                       
LNTYP30  MVC   0(3,R8),CTATTTYP                                                 
         LA    R8,4(R8)                                                         
         B     LNTYP20                                                          
         DROP  R3                                                               
*                                                                               
LNTYP40  GOTO1 =V(SQUASHER),APPARM,APWORK,L'APWORK,RR=RB                        
         GOTO1 =V(CHOPPER),APPARM,(L'APWORK,APWORK),                   *        
               (L'LISTATYP,LISTATYP),1,RR=RB                                    
LNTYPX   EQU   *                                                                
*                                                                               
LINEX    XIT1  ,                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK ID RECORD AND ITS PASSIVE WILL NOT BE TOO BIG BEFORE UPDATE   *         
* R2=A(USER ID RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
CHKBIG   NTR1  BASE=*                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CTILEN                                                      
         L     RE,AIOAREA2                                                      
         L     R2,AIOAREA1                                                      
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
         L     R2,AIOAREA2         SUE AIOAREA 2 AS TEMPORARY WORK AREA         
         MVI   APELEM,X'02'        ADD LONGEST PASSIVE POINTER ELEMENT          
         MVI   APELEM+1,X'0C'                                                   
         MVC   APELEM+2(10),=CL10' '                                            
         GOTO1 AADDELN,CTIREC                                                   
         BNE   CBIGNO              HERE IF RECORD WILL BE TOO BIG               
         B     CBIGOK                                                           
*                                                                               
CBIGOK   SR    RC,RC                                                            
CBIGNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENF8D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGEND8D                                                       
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB8D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 LAST SYSTEM DISPLAYED                        
SAVKEY   DS    XL(L'CTIKEY)        SAVE LAST RECORD KEY READ FOR COPY           
*                                                                               
*                                  ** DSECT TO COVER TWA LINE **                
LINED    DSECT                     ** ON DISPLAY RECORD SCREEN**                
LITYPEH  DS    XL8                                                              
LITYPE   DS    CL3                                                              
LINAMEH  DS    CL8                                                              
LINAME   DS    CL33                                                             
LINEXT   EQU   *-LINED                                                          
*                                                                               
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTID   DS    CL10                                                             
         DS    CL1                                                              
LISTATYP DS    CL60                                                             
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
RETURN   DS    F                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
WORK     DS    CL(L'APWORK)                                                     
*                                                                               
SELDATA  DS    0XL(SELDATAL)                                                    
SELID    DS    CL8                 USER ID                                      
SELIDSP  DS    CL1                 1ST SPECIAL CHAR                             
SELIDL   DS    CL1                 (L'DATA ENTERED)                             
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELSYS   DS    XL1                 SYSTEM SEOV NUMBER                           
SELPGM   DS    XL1                 PROGRAM NUMBER                               
SELAGY   DS    XL2                 AGENCY ALPHA ID                              
SELPWD   DS    XL1                 PASSWORD REQUIRED                            
SELCID   DS    XL10                COMPATIBLE ID                                
SELDATAL EQU   *-SELID                                                          
*                                                                               
FLDCNT   DS    XL1                                                              
*                                                                               
GETSEQF  DS    XL1                                                              
FLAG     DS    XL1                                                              
SYSTEM   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
*                                                                               
IDNUM    DS    CL2                                                              
IDALP    DS    CL10                                                             
KEYSAVE  DS    CL(L'IOKEY)                                                      
REC      DS    1024C                                                            
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010CTGEN07   05/01/02'                                      
         END                                                                    
