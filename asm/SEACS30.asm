*          DATA SET SEACS30    AT LEVEL 002 AS OF 01/30/20                      
*PHASE TA0D30A                                                                  
*                                                                               
         TITLE 'SEACS30 -SECURITY ACCESS - TRAFFIC BUYING AGENCY'               
*                                                                               
ACS30    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**S30**,RA,RR=RE                                               
         USING WORKD,R7                 R7=A(GLOBAL W/S)                        
         USING TWAD,R5                  R5=A(TWA)                               
         USING SAVAREA,R6               R6=A(GLOBAL SAVE AREA)                  
         LA    R2,IOKEY                                                         
         USING BAGRECD,R2               R2=A(RECORD KEY)                        
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC               RC=A(SYSTEM + LOCAL W/S)                
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                   01 - APMVALK                            
         B     VALREC                   02 - APMVALR                            
         B     DISKEY                   03 - APMDISK                            
         B     DISREC                   04 - APMDISR                            
         B     DELREC                   05 - APMDELR                            
         B     RESREC                   06 - APMRESR                            
         B     VALSEL                   07 - APMVALP                            
         B     GETSEL                   08 - APMGETS                            
         B     DISSEL                   09 - APMDISS                            
         B     XIT                      10 - APMVALS                            
         B     FSTLST                   11 - APMFLST                            
         B     XIT                      12 - APMPROC                            
         B     XIT                      13 - APMFSCR                            
         B     LSTSCR                   14 - APMLSCR                            
         B     VALREQ                   15 - APMVALQ                            
         B     PRTREP                   16 - APMREPP                            
         B     XIT                      17 - APMSETT                            
         B     XIT                      18 - APMPUTK                            
         B     XIT                      19 - APMNEWK                            
         B     XIT                      20 - APMFRP                             
         B     XIT                      21 - APMDISS2                           
*                                                                               
EXIT     OI    ACSSRVH+FHOID,FHOITR+FHOIMO AVOID NO DATA ENTERED                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
XIT      XIT1                                                                   
*                                                                               
EXITNE   LTR   RB,RB                                                            
         J     XIT                                                              
EXITEQ   CR    RB,RB                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY                                                       
***********************************************************************         
VALKEY   LA    R2,APRECKEY                                                      
*                                                                               
         XC    BAGKEY,BAGKEY                                                    
         MVI   BAGKMIN,BAGKMINQ        C'T' TRAFFIC                             
         MVI   BAGKTYP,BAGKTYPQ        C'B' BUYING AGENCY                       
         MVC   BAGKAGY,CUAALF          AGENCY                                   
                                                                                
*--------------------------------------                                         
* SYSTEM                                                                        
*--------------------------------------                                         
VK020    DS    0H                                                               
*                                                                               
*        MVC   BAGAGY,CUAALF           AGENCY (DISPLAY ONLY)                    
*        OI    FVOIND,FVOIND                                                    
*                                                                               
*        MVI   FVMINL,1                AGENCY (EDITABLE)                        
*        GOTO1 AFVAL,BAGAGYH                                                    
*        BNE   VALKEYX                                                          
*        MVC   BAGKAGY,BAGAGY                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BAGBAGH                                                    
         BNE   VALKEYX                                                          
         LA    R1,FVIFLD                                                        
         ICM   R1,B'1000',FVILEN                                                
         BAS   RE,VALAN                  MUST BE ALPHA NUMERIC                  
         BNE   SAEIIF                                                           
         MVC   BAGKBAGY,BAGBAG                                                  
                                                                                
*--------------------------------------                                         
* READ FOR RECORD                                                               
*--------------------------------------                                         
         LA    R2,IOKEY                                                         
         MVC   BAGKEY,APRECKEY         SET KEY                                  
*                                                                               
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                   I/O ERROR EXIT                         
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK100                                                            
         TM    IOERR,IOEDEL              TEST RECORD IS DELETED                 
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VK100                                                            
         MVI   APINDS,APIOKADD                                                  
         B     VALKEYY                                                          
*                                                                               
VK100    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                   I/O ERROR EXIT                         
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A RECORD                                             
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
*                                                                               
         CLI   APACTN,ACTADD           ADD ACTION                               
         BNE   VR010                                                            
         XC    BAGKEY(BAGKLEN+1),BAGKEY                                         
         MVC   BAGKEY,APRECKEY                                                  
         LHI   R1,BAGFIRST+1                                                    
         STCM  R1,3,BAGFLEN                                                     
*                                                                               
VR010    LA    R3,APELEM                                                        
         USING BANAMED,R3                                                       
         MVI   BANAMEL,BANAMELQ          REMOVE NAME ELEMENT                    
         MVI   BANAMLN,0                                                        
         GOTO1 ADELELS,BAGRECD                                                  
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BAGNAMH                                                    
         BNE   VALRECX                                                          
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   BANAMEL,BANAMELQ          REMOVE NAME ELEMENT                    
         LLC   R1,FVILEN                                                        
         AHI   R1,BANAMLNQ                                                      
         STC   R1,BANAMLN                                                       
         LLC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BANAME(0),BAGNAM                                                 
         GOTO1 AADDELS,BAGRECD                                                  
         DROP  R3                                                               
*                                                                               
         GOTO1 ASETACT,BAGRECD           SET ACTIVITY ELEMENT                   
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         JNE   *+2                                                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY                                                        
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
*        MVC   BAGAGY,BAGKAGY                                                   
*        OI    BAGAGYH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         MVC   BAGBAG,BAGKBAGY                                                  
         OI    BAGBAGH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DISKEYX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY                                                            
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         MVC   BAGNAM,SPACES                                                    
         OI    BAGNAMH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         LA    R3,BAGFIRST(R2)                                                  
DR010    CLI   0(R3),0                                                          
         BE    DISRECX                                                          
         CLI   0(R3),BANAMELQ      X'0A' NAME ELEMENT                           
         BE    DR030                                                            
DR020    LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR010                                                            
*                                                                               
         USING BANAMED,R3                                                       
DR030    LLC   R1,BANAMLN                                                       
         AHI   R1,-BANAMLNQ-1                                                   
         BM    DISRECX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BAGNAM(0),BANAME                                                 
         OI    BAGNAMH+(FVOIND-FVIHDR),FVOXMT                                   
         B     DR020                                                            
*                                                                               
DISRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* DELETE A RECORD                                                               
***********************************************************************         
DELREC   LA    R2,IOKEY                                                         
         OI    BAGKSTAT,X'80'      SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         JNE   *+2                                                              
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,BAGRECD                                                  
         OI    BAGFSTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         JNE   *+2                                                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* RESTORE A DELETED RECORD                                                      
***********************************************************************         
RESREC   LA    R2,IOKEY                                                         
         NI    BAGKSTAT,X'FF'-X'80'      TURN DELETE BIT OFF IN KEY             
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         JNE   *+2                                                              
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,BAGRECD                                                  
         NI    BAGFSTAT,X'FF'-X'80'      TURN DELETE BIT OFF IN RECORD          
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         JNE   *+2                                                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)              
***********************************************************************         
FSTLST   MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                                         
***********************************************************************         
VALSEL   LA    R2,APRECKEY               BUILD FIRST RECORD KEY                 
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    BAGKEY(BAGKLEN),BAGKEY                                           
         MVI   BAGKMIN,BAGKMINQ          C'T' TRAFFIC                           
         MVI   BAGKTYP,BAGKTYPQ          C'B' BUY AGENCY RECORDS                
         MVC   BAGKAGY,CUAALF                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTBAGH                                                    
         BNE   VS010                                                            
         LLC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BAGKBAGY(0),LSTBAG                                               
         OC    BAGKBAGY,SPACES                                                  
*                                                                               
VS010    LA    R0,LSTACT1H               SET ADDRESS OF FIRST LIST LINE         
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H               SET LIST LINE LENGTH                   
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VALSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                                   
***********************************************************************         
GETSEL   LA    R2,IOKEY                  READ NEXT RECORD                       
         MVC   BAGKEY,APRECKEY           FROM LAST KEY                          
         TM    APINDS,APILNSEQ           FIRST LINE IN LIST SEQUENCE            
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
         LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOGENDIR+IOSQ+IO1      ELSE NEXT LIST LINE                    
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
*                                                                               
         MVC   APRECKEY(BAGKLEN),BAGKEY  SAVE LAST RECORD KEY                   
         CLI   BAGKMIN,BAGKMINQ                                                 
         BNE   GSEND                                                            
         CLI   BAGKTYP,BAGKTYPQ                                                 
         BNE   GSEND                                                            
         CLC   BAGKAGY,CUAALF                                                   
         BNE   GSEND                                                            
         GOTO1 AIO,IOGET+IOGENFIL+IO1                                           
         BNE   GSEND                     I/O ERROR EXIT                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GSEND    MVI   APMODE,APMEOFS            SET NO MORE RECORDS TO COME            
GETSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                                      
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*        MVC   LISTAGY,BAGKAGY     DISPLAY KEY DATA                             
         MVC   LISTBAG,BAGKBAGY    DISPLAY KEY DATA                             
*                                                                               
         LA    R3,BAGFIRST(R2)     GET ELEMENT DATA                             
DS030    CLI   0(R3),0                                                          
         BE    DISSELX             END OF RECORD                                
         CLI   0(R3),BANAMELQ                                                   
         BE    DS050                                                            
DS040    LLC   R0,1(R3)            GET NEXT ELEMENT                             
         AR    R3,R0                                                            
         B     DS030                                                            
*                                                                               
         USING BANAMED,R3                                                       
DS050    LLC   R1,BANAMLN                                                       
         AHI   R1,-BANAMLNQ-1                                                   
         BM    DISSELX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTNAM(0),BANAME                                                
         B     DS040                                                            
*                                                                               
DISSELX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)              
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
LSTSCRX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                                     
***********************************************************************         
VALREQ   L     R4,AREP                                                          
*                                                                               
         USING REPD,R4             R4=A(REPORT WORK AREA)                       
         XC    APRECKEY,APRECKEY                                                
         XC    SELVALS(SELVALSQ),SELVALS                                        
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
*        GOTO1 AFVAL,REPSYSH       VALIDATE SYSTEM                              
*        BNE   VQ040                                                            
*        GOTO1 AVALSYS,REPSYSH                                                  
*        BNE   SAEIIF                                                           
*                                                                               
*        LA    R1,MOLSYSL                                                       
*Q020    CLI   0(R1),0             MAKE SURE VALID SYSTEM FOR                   
*        BE    SAESYE              MEDIA OFFICE RECORDS                         
*        CLC   SYSTEM,0(R1)                                                     
*        BE    VQ030                                                            
*        LA    R1,1(,R1)                                                        
*        B     VQ020                                                            
*Q030    L     R1,APPARM           SYSLST ENTRY                                 
*        MVC   SELSYS,12(R1)       SYSTEM LETTER                                
*                                                                               
*Q040    GOTO1 AFVAL,REPOFFLH      VALIDATE OFFICE LIST CODE                    
*        BNE   VQ050                                                            
*                                                                               
*Q100    LA    R2,APRECKEY         BUILD FIRST RECORD KEY                       
*        XC    CTUKEY,CTUKEY                                                    
*        MVI   CTUKTYP,CTUKTYPQ                                                 
*        MVC   CTUKSYS,SELSYS                                                   
*        CLI   CTUKSYS,0                                                        
*        BNE   *+8                                                              
*        MVI   CTUKSYS,C'P'        START WITH PRINT                             
*        MVI   CTUKPROG+1,C'$'                                                  
*        MVC   CTUKPROG+2(1),SELOFFL                                            
*                                                                               
*        MVCDD REPDESC,CT#OFFL     SET REPORT DESCRIPTION                       
*        GOTO1 VDICTAT,APPARM,C'SL  ',REPDESC                                   
*        MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
*        MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
*        MVI   REPFOOTN,0                                                       
*        LA    R0,REPSPEC                                                       
*        ST    R0,REPAPHS                                                       
*        OI    REPIND2,REPILOW                                                  
*        MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO GENERATE REPORT                                                    
***********************************************************************         
PRTREP   L     R4,AREP                                                          
*                                                                               
*        CLC   =C'DOWN',REPOTYP                                                 
*        BNE   PR060                                                            
*        CLC   =C'NOW',REPWHEN     ONLY PRINT THIS FOR NOW REQUEST              
*        BNE   PR040                                                            
*        XC    REPAPHS,REPAPHS     CLEAR SPECS                                  
*        MVC   REPP1(18),=C'Office List Report'                                 
*        MVI   REPLINE,1                                                        
*        GOTO1 VREPORT,REPD                                                     
*        MVI   REPLINE,99                                                       
*        GOTO1 VREPORT,REPD                                                     
*        B     PR050                                                            
*                                                                               
*R040    XC    REPAPHS,REPAPHS     CLEAR SPECS                                  
*        GOTO1 VREPORT,REPD                                                     
*                                                                               
*R050    LA    R3,WORKD                                                         
*        AHI   R3,DLCB-WORKD                                                    
*        USING DLCBD,R3                                                         
*        XC    DLCBD(DLCBL),DLCBD                                               
*        MVI   DLCBACT,DLCBINIT      DOWNLOAD ACTION IS START                   
*        LARL  RF,DLHOOK                                                        
*        ST    RF,DLCBAPR                                                       
*        LA    RF,REPP1                                                         
*        ST    RF,DLCBAPL                                                       
*        LA    RF,L'REPP1                                                       
*        STH   RF,DLCXMAXL                                                      
*        MVI   DLCXDELC,C' '         DELIMITER                                  
*        MVI   DLCXEOTC,C'"'         TEXT DELIMITER                             
*        MVI   DLCXEOLC,X'5E'        SEMI-COLON, END-OF-LINE                    
*        MVI   DLCXEORC,C':'         END-OF-REPORT                              
*        GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
*        MVC   DLCBFLD,SPACES        MUST CLEAR FIRST TIME                      
*        MVI   DLCBFLX,C' '                                                     
*        MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*        DROP  R3                                                               
*                                                                               
*R060    LA    R2,IOKEY                                                         
*        MVC   CTUKEY,APRECKEY         SET INITIAL KEY VALUE                    
*R070    LA    R1,IOHI+IOCONFIL+IO1                                             
*        B     PR100                   GET FIRST RECORD                         
*                                      GET NEXT RECORD(SEQUENCE BROKEN)         
*R080    LA    R2,IOKEY                                                         
*        MVC   CTUKEY,APRECKEY                                                  
*        GOTO1 AIO,IORD+IOCONFIL+IO1                                            
*        BNE   PREND                                                            
*                                      GET NEXT RECORD (IN SEQUENCE)            
*R090    LA    R1,IOSQ+IOCONFIL+IO1                                             
*R100    GOTO1 AIO                                                              
*        BNE   PREND                                                            
*        L     R2,AIOAREA1                                                      
*                                                                               
*                                                                               
*        LA    R8,1(,R8)                                                        
*        BCT   R9,PR210                                                         
*                                                                               
*                                                                               
*R350    LA    RF,L'REPP1(,RF)                                                  
*        B     PR320                                                            
*                                                                               
*R400    GOTO1 VREPORT,REPD        PRINT LINE                                   
*        B     PR080               READ SEQUENCE IS BROKEN                      
*                                                                               
*REND    CLC   =C'DOWN',REPOTYP                                                 
*        BNE   PREND02                                                          
*                                                                               
*        LA    R3,WORKD                                                         
*        AHI   R3,DLCB-WORKD                                                    
*        USING DLCBD,R3                                                         
*        MVI   DLCBACT,DLCBEOR     END OF REPORT                                
*        GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
PRENDX   MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* VALIDATE ALPHANUMERIC FIELD                                                   
*   NTRY: 0(R1) = FIELD                                                         
*         4(R1) = LENGTH                                                        
*   EXIT: CC=ZERO IF OK                                                         
***********************************************************************         
VALAN    NTR1                                                                   
         STCM  R1,B'1000',APBYTE                                                
         LLC   RF,APBYTE                                                        
         LTR   RF,RF                                                            
         BZ    VAANNO                                                           
*                                                                               
VAAN10   CLI   0(R1),C' '                                                       
         BE    VAAN20                                                           
         CLI   0(R1),C'9'                                                       
         BH    VAANNO                                                           
         CLI   0(R1),C'0'                                                       
         BNL   VAAN20                                                           
         CLI   0(R1),C'Z'                                                       
         BH    VAANNO                                                           
         CLI   0(R1),C'A'                                                       
         BL    VAANNO                                                           
VAAN20   LA    R1,1(R1)                                                         
         BCT   RF,VAAN10                                                        
         B     VAANOK                                                           
*                                                                               
VAANNO   LTR   RB,RB                                                            
         J     EXIT                                                             
VAANOK   CR    RB,RB                                                            
         J     EXIT                                                             
                                                                                
***********************************************************************         
* ERROR EXITS & CONSTANTS                                                       
***********************************************************************         
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     EXIT                    INPUT FIELD ERROR                        
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         J     EXIT                    MISSING FIELD                            
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         J     EXIT                    I/O ERROR                                
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXIT                    RECORD NOT FOUND                         
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         J     EXIT                    ALREADY EXISTS                           
SAEDUP   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         J     EXIT                    DUPLICATED INPUT FIELD                   
SAESAF   MVC   FVMSGNO,=AL2(FVFTSAF)                                            
         J     EXIT                    STORAGE ALLOCATION FAILURE               
SAESYE   MVC   FVMSGNO,=AL2(CE#INCSS)                                           
         J     EXIT                    INVALID SYSTEM                           
*                                                                               
SPACES   DC    128C' '                                                          
DASHES   DC    32C'-'                                                           
         LTORG                                                                  
*                                                                               
REPSPEC  DS    0X                      REPORT HEADING SPECIFICATIONS            
         SPEC  H1,4,RUN                                                         
         SPEC  H1,30,CT#OFLR,30,C                                               
         SPEC  H2,30,CT#OFLR,30,CU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,4,CT#SYS,10,L                                                 
         SPEC  M2,4,CT#SYS,10,LU                                                
         SPEC  M1,14,CT#OFFL,8,L                                                
         SPEC  M2,14,CT#OFFL,8,LU                                               
         SPEC  M1,24,CT#DESC,12,L                                               
         SPEC  M2,24,CT#DESC,12,LU                                              
         SPEC  M1,45,CT#OFFS,12,L                                               
         SPEC  M2,45,CT#OFFS,12,LU                                              
         SPEC  END                                                              
                                                                                
***********************************************************************         
* BUILD DOWNLOAD LINES                                                          
***********************************************************************         
DLLINE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        LA    R3,WORKD                                                         
*        AHI   R3,DLCB-WORKD                                                    
*        USING DLCBD,R3                                                         
*                                                                               
*        CLI   SENTCOLS,C'Y'                                                    
*        BE    DLLIN02                                                          
*                                                                               
*        MVC   DLCBFLD(6),=C'System'                                            
*        BRAS  RE,DLOUT                                                         
*        MVC   DLCBFLD(11),=C'Office list'                                      
*        BRAS  RE,DLOUT                                                         
*        MVC   DLCBFLD(11),=C'Description'                                      
*        BRAS  RE,DLOUT                                                         
*        MVC   DLCBFLD(7),=C'Offices'                                           
*        BRAS  RE,DLOUT                                                         
*        MVI   SENTCOLS,C'Y'                                                    
*        MVI   DLCBACT,DLCBEOL         END OF LINE                              
*        GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
*LLIN02  MVC   DLCBFLD(L'PRSYS),PRSYS                                           
*        BRAS  RE,DLOUT                                                         
*                                                                               
*        MVC   DLCBFLD(L'PROFL),PROFL                                           
*        BRAS  RE,DLOUT                                                         
*                                                                               
*        MVC   DLCBFLD(L'PRDSC),PRDSC                                           
*        BRAS  RE,DLOUT                                                         
*        MVC   DLCBFLD,SPACES                                                   
*                                                                               
*        CLI   PROFF,C' '              ANY OFFICES?                             
*        BNH   DLLIN50                 NO                                       
*                                                                               
*        LA    R1,PROFF                                                         
*        LHI   R0,L'PROFF                                                       
*LLIN20  CLI   0(R1),C'"'              SEARCH FOR DELIMINTER                    
*        BNE   *+8                                                              
*        MVI   0(R1),C'?'              REPLACE WITH INDECISION                  
*        LA    R1,1(,R1)                                                        
*        BCT   R0,DLLIN20                                                       
*                                                                               
*        MVI   DLCXEOTC,C' '           REMOVE TEXT DELIMITER                    
*        MVI   DLCBFLX,C'"'            ADD MY OWN                               
*                                                                               
*        MVC   DLCBFLX+1(128),PROFF                                             
*        OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
*        BRAS  RE,DLOUT                                                         
*                                                                               
*        CLI   PROFF+(1*128),C' '      ANY MORE OFFICES                         
*        BNH   DLLIN40                 NO                                       
*                                                                               
*        MVC   DLCBFLX(128),PROFF+(1*128)                                       
*        OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
*        BRAS  RE,DLOUT                                                         
*                                                                               
*        CLI   PROFF+(2*128),C' '      ANY MORE OFFICES                         
**       BNH   DLLIN40                 NO                                       
**                                                                              
**       MVC   DLCBFLX(128),PROFF+(2*128)                                       
*        OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
*        BRAS  RE,DLOUT                                                         
*                                                                               
*        CLI   PROFF+(3*128),C' '      ANY MORE OFFICES                         
*        BNH   DLLIN40                 NO                                       
*                                                                               
*        MVC   DLCBFLX(128),PROFF+(3*128)                                       
*        OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
*        BRAS  RE,DLOUT                                                         
*                                                                               
*LLIN40  MVI   DLCBFLX,C'"'            END OF TEXT DELIMETER                    
*LLIN50  OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
*        BRAS  RE,DLOUT                                                         
*                                                                               
*        MVI   DLCXEOTC,C'"'           TEXT DELIMITER                           
*        NI    DLCBFLG1,X'FF'-DLCBFXFL USE EXTENDED FOR TEXT                    
*        MVC   DLCBFLD,SPACES                                                   
*        MVI   DLCBFLX,C' '                                                     
*        MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
*        MVI   DLCBACT,DLCBEOL         END OF LINE                              
*        GOTO1 VDLFLD,DLCBD                                                     
*        J     EXIT                                                             
*                                                                               
*----------------------------------------------------------------------         
* DOWNLOAD HOOK ROUTINE                                                         
*----------------------------------------------------------------------         
*LOUT    NTR1  BASE=*,LABEL=*                                                   
*        MVI   DLCBACT,DLCBPUT         ACTION IS PUT                            
*        MVI   DLCBTYP,DLCBTXT         TYPE IS TEXT                             
*        GOTO1 VDLFLD,DLCBD                                                     
*        J     EXIT                                                             
*        DROP  R3                                                               
*                                                                               
*----------------------------------------------------------------------         
* DOWNLOAD HOOK ROUTINE                                                         
*----------------------------------------------------------------------         
*LHOOK   NTR1  BASE=*,LABEL=*                                                   
*        MVI   REPLINE,1           SUPPRESS PAGING                              
*        MVI   REPHEADI,0          SUPPRESS ALL FORCE PAGES                     
*        MVI   REPMIDSI,0                                                       
*        MVI   REPFOOTI,0                                                       
*        GOTO1 VREPORT,REPD                                                     
*        J     EXIT                                                             
*                                                                               
***********************************************************************         
* SECURITY ACCESS WORK DSECT                                                    
***********************************************************************         
       ++INCLUDE SEACSWRK                                                       
                                                                                
***********************************************************************         
* TWA                                                                           
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACS9DD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACS9ED                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACS9FD                                                       
         ORG   SAVOVER                                                          
*----------------------------------------------------------------------         
* SAVED STORAGE                                                                 
*----------------------------------------------------------------------         
SYSTEM   DS    X                       SYSTEM NUMBER                            
SVPAGE   DS    X                       PAGE NUMBER                              
*                                                                               
SELVALS  DS    0C                      *SELECTED VALUES*                        
SELAGY   DS    C                       AGENCY ALPHA                             
SELBAG   DS    X                       BUY AGENCY                               
SELDOWN  DS    C                       DOWNLOAD                                 
SELVALSQ EQU   *-SELVALS                                                        
                                                                                
***********************************************************************         
* LIST/SELECT LINE LAYOUT                                                       
***********************************************************************         
LISTD    DSECT                                                                  
LISTSELH DS    XL8                                                              
LISTSEL  DS    CL3                     ACTION FIELD                             
LISTLINH DS    CL8                                                              
LISTLINX DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
         DS    CL2                                                              
*ISTAGY  DS    CL2                     AGENCY ALPHA                             
*        DS    CL2                                                              
LISTBAG  DS    CL3                     BUYING AGENCY                            
         DS    CL2                                                              
LISTNAM  DS    CL33                    BUYING AGENCY FULL NAME                  
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
                                                                                
***********************************************************************         
* REPORT LINE DSECT                                                             
***********************************************************************         
REPD     DSECT                                                                  
         ORG   REPP1                                                            
         DS    CL3                                                              
RLAGY    DS    CL2                     AGENCY ALPHA                             
         DS    CL3                                                              
RLBAG    DS    CL3                     BUYING AGENCY                            
         DS    CL3                                                              
RLBAN    DS    CL33                    BUYING AGENCY FULL NAME                  
                                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                                         
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
VDLFLD   DS    V                                                                
DUB      DS    D                                                                
WORK     DS    CL64                                                             
BYTE     DS    X                                                                
FLAG     DS    X                                                                
*                                                                               
SENTCOLS DS    C                                                                
*                                                                               
LOCALX   EQU   *                                                                
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
       ++INCLUDE GEGENBAG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SEACS30   01/30/20'                                      
         END                                                                    
