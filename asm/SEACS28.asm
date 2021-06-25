*          DATA SET SEACS28    AT LEVEL 007 AS OF 01/30/19                      
*PHASE TA0D28B                                                                  
         TITLE 'SEACS28 - TOKEN AUTHORIZATION RECORDS'                          
*                                                                               
ACS28    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACS28*,RR=RE                                                  
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING TOKRECD,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
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
         B     DELREC              05 - APMDELR                                 
         B     RESREC              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     EXIT                15 - APMVALQ                                 
         B     EXIT                16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     EXIT                18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXITCC   CLC   FVMSGNO,=AL2(FVFOK)                                              
EXIT     XIT1  ,                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF RECORD                                             
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVC   SAGY,CUAALF         SECURITY AGENCY                              
         MVC   AAGY,CUAALF         AGENCY ALPHA                                 
                                                                                
*----------------------------------                                             
* SETUP AGENCY                                                                  
*----------------------------------                                             
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,TOKAGYH                                                    
         BE    VK010                                                            
         MVC   TOKAGY,AAGY                                                      
         OI    TOKAGYH+FHOID,FHOITR                                             
         B     *+10                                                             
VK010    MVC   AAGY,FVIFLD         SET AGENCY ALPHA                             
*                                                                               
         BRAS  RE,CHKAGYS          CHECK AGENCY VALUES                          
         BNE   VALKEYX             NE: SOMETHING NOT COMPATIBLE                 
*                                                                               
         BRAS  RE,GETUID                                                        
         MVC   TOKCOD,PUIDCODE                                                  
         OI    TOKCODH+FHOID,FHOITR                                             
         MVC   TOKNAM,PUIDNAME                                                  
         OI    TOKNAMH+FHOID,FHOITR                                             
                                                                                
*----------------------------------                                             
* SYSTEM                                                                        
*----------------------------------                                             
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,TOKSYSH                                                    
         BNE   EXIT                                                             
         GOTO1 AVALSYS,TOKSYSH                                                  
         BNE   EXIT                                                             
         MVC   SYSTEM,APWORK                                                    
                                                                                
*----------------------------------                                             
* READ FOR RECORD                                                               
*----------------------------------                                             
VK030    XC    TOKKEY,TOKKEY                                                    
         MVI   TOKKMAJ,0                                                        
         MVI   TOKKMIN,TOKKMINQ    TOKEN AUTHORIZATION RECORDS                  
         MVI   TOKKTYP,TOKKRTRK    RENTRAK RECORD                               
         MVC   TOKKSAGY,SAGY       SECURITY AGENCY                              
         MVC   TOKKAAGY,AAGY       SETUP ALPHA AGENCY                           
         MVC   TOKKSYS,SYSTEM                                                   
         MVC   APRECKEY(TOKKLEN),TOKKEY  SAVE KEY                               
*                                                                               
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK100                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         MVI   APINDS,APIOKADD                                                  
         B     VALKEYY                                                          
*                                                                               
VK100    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
*                                                                               
VALKEYY  B     EXITOK                                                           
VALKEYX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A RECORD                                             
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
*                                                                               
         CLI   APACTN,ACTADD       ADD ACTION                                   
         BNE   VR020                                                            
         XC    TOKKEY(256),TOKKEY                                               
         MVC   TOKKEY,APRECKEY                                                  
*                                                                               
         LHI   R1,TOKFIRST+1                                                    
         STCM  R1,3,TOKFLEN                                                     
                                                                                
*----------------------------------                                             
* UPDATE/BUILD ELEMENTS                                                         
*----------------------------------                                             
VR020    LA    R3,APELEM                                                        
                                                                                
*----------------------------------                                             
* RENTRAK AUTHORIZATION ELEM X'0A'                                              
*----------------------------------                                             
         MVI   FVMINL,32                                                        
         GOTO1 AFVAL,TOKUTOH       RENTRAK USER ID TOKEN                        
         BNE   VALKEYX                                                          
*                                                                               
         MVI   APBYTE,L'TOKUTO     VERIFY CHARACTER SET                         
         GOTOR VALAN,TOKUTO                                                     
         BNE   SAEIIF              INVALID CHARACTER IN STRING                  
*                                                                               
*AW      MVI   FVMINL,32                                                        
*        GOTO1 AFVAL,TOKSTOH       RENTRAK SECRET TOKEN                         
*        BNE   VALKEYX                                                          
*                                                                               
*        MVI   APBYTE,L'TOKSTO     VERIFY CHARACTER SET                         
*        GOTOR VALAN,TOKSTO                                                     
*        BNE   SAEIIF              INVALID CHARACTER IN STRING                  
*                                                                               
         USING RTAUTHD,R3                                                       
         MVI   RTAUTEL,RTAUTELQ    REMOVE RENTRAK AUTHORIZATION ELEM            
         MVI   RTAUTLN,0                                                        
         GOTO1 ADELELS,TOKRECD                                                  
*                                                                               
         XC    RTAUTHD(RTAUTLNQ),RTAUTHD                                        
         MVI   RTAUTEL,RTAUTELQ    ADD NEW TOKEN ELEMENT                        
         MVI   RTAUTLN,RTAUTLNQ                                                 
         MVC   RTAUTID,TOKUTO      RENTRAK USER ID TOKEN                        
*AW      MVC   RTAUTSEC,TOKSTO     RENTRAK SECRET CODE TOKEN                    
*                                                                               
         GOTO1 AADDELS,TOKRECD                                                  
                                                                                
*----------------------------------                                             
* GENFIL ACTIVITY ELEMENT X'FE'                                                 
*----------------------------------                                             
         GOTO1 ASETACT,TOKRECD     SET ACTIVITY ELEMENT                         
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         JNE   *+2                                                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF RECORD                                              
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
*                                                                               
         MVC   SAGY,TOKKSAGY                                                    
         MVC   AAGY,TOKKAAGY                                                    
*                                                                               
         MVC   TOKAGY,TOKKAAGY      SETUP AGENCY                                
         OI    TOKAGYH+FHOID,FHOITR                                             
*                                                                               
         GOTOR GETAGY,TOKAGY                                                    
         BNE   SAERNF                                                           
         MVC   PUID,APWORK+2       GET PRINCIPAL USER ID                        
         BRAS  RE,GETUID                                                        
         MVC   TOKCOD,PUIDCODE                                                  
         OI    TOKCODH+FHOID,FHOITR                                             
         MVC   TOKNAM,PUIDNAME                                                  
         OI    TOKNAMH+FHOID,FHOITR                                             
*                                                                               
         GOTO1 ADISSYS,TOKKSYS      SYSTEM                                      
         MVC   TOKSYS,APWORK                                                    
         OI    TOKSYSH+FHOID,FHOITR                                             
*                                                                               
         MVC   IOKEY,APRECKEY                                                   
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY A RECORD                                                   
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         MVC   TOKUTO,SPACES                                                    
         OI    TOKUTOH+FHOID,FHOITR                                             
**AW     MVC   TOKSTO,SPACES                                                    
*        OI    TOKSTOH+FHOID,FHOITR                                             
*                                                                               
         LA    R3,TOKFIRST(R2)                                                  
DR010    CLI   0(R3),0                                                          
         BE    DISRECX                                                          
         CLI   0(R3),RTAUTELQ      X'0A' RENTRAK AUTHORIZATION ELEM             
         BE    DR030                                                            
DR020    LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR010                                                            
*                                                                               
         USING RTAUTHD,R3                                                       
DR030    MVC   TOKUTO,RTAUTID                                                   
**AW     MVC   TOKSTO,RTAUTSEC                                                  
         B     DR020                                                            
*                                                                               
DISRECX  B     EXITOK                                                           
                                                                                
***********************************************************************         
* DELETE A RECORD                                                               
***********************************************************************         
DELREC   LA    R2,IOKEY                                                         
         OI    TOKKSTAT,X'80'      SET DELETE BIT IN DIRECTORY                  
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         JNE   *+2                                                              
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,TOKRECD                                                  
         OI    TOKFSTAT,X'80'      SET DELETE BIT IN RECORD                     
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         JNE   *+2                                                              
*                                                                               
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* RESTORE A DELETED RECORD                                                      
***********************************************************************         
RESREC   LA    R2,IOKEY                                                         
*                                                                               
         GOTO1 AIO,IORDUPD+IOGENDIR                                             
         BL    VALKEYX               I/O ERROR EXIT                             
         NI    TOKKSTAT,X'FF'-X'80'  TURN DELETE BIT OFF IN KEY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         JNE   *+2                                                              
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AIO,IOGETRUP+IOGENFIL+IO1                                        
         BL    VALKEYX               I/O ERROR EXIT                             
         GOTO1 ASETACT,TOKRECD                                                  
         NI    TOKFSTAT,X'FF'-X'80'  TURN DELETE BIT OFF IN RECORD              
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         JNE   *+2                                                              
         B     DISREC                                                           
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)              
***********************************************************************         
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                                         
***********************************************************************         
VALSEL   LA    R2,APRECKEY         BUILD FIRST RECORD KEY                       
*                                                                               
         XC    TOKKEY(TOKKLEN),TOKKEY                                           
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
*                                                                               
         GOTOR GETAGY,CUAALF                                                    
         MVC   SAGY,APWORK                                                      
         MVC   TOKKSAGY,SAGY       SECURITY AGENCY                              
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTAGYH                                                    
         BNE   VS010                                                            
         LLC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TOKKAAGY(0),LSTAGY  SETUP AGENCY                                 
*                                                                               
VS010    XC    SELSYS,SELSYS                                                    
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTSYSH                                                    
         BNE   VS020                                                            
         GOTO1 AVALSYS,LSTSYSH                                                  
         BNE   SAEIIF                                                           
         MVC   TOKKSYS,APWORK                                                   
         MVC   SELSYS,APWORK                                                    
*                                                                               
VS020    LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                                   
***********************************************************************         
GETSEL   LA    R2,IOKEY            READ NEXT RECORD                             
         MVC   TOKKEY,APRECKEY     FROM LAST KEY                                
         TM    APINDS,APILNSEQ     FIRST LINE IN LIST SEQUENCE                  
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
         LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOGENDIR+IOSQ+IO1  ELSE NEXT LIST LINE                        
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
*                                                                               
         MVC   APRECKEY(TOKKLEN),TOKKEY  SAVE LAST RECORD KEY                   
         CLI   TOKKMIN,TOKKMINQ                                                 
         BNE   GSEND                                                            
         CLI   TOKKTYP,TOKKRTRK                                                 
         BNE   GSEND                                                            
*                                                                               
         CLC   TOKKAAGY,SAGY       SETUP AGY IS SECURITY AGENCY?                
         BE    GS022               YES: WE CAN SHOW THIS                        
         CLC   CUAALF,SAGY         LOGGED IN W/ SEC AGY?                        
         BE    GS022               YES: WE CAN SHOW EVERYTING                   
         CLC   CUAALF,TOKKAAGY     LOGGED IN W/ SETUP AGY?                      
         BNE   GS020               NO: DON'T HAVE ACCESS                        
GS022    CLC   TOKKSAGY,SAGY                                                    
         BNE   GSEND                                                            
*                                                                               
         CLI   SELSYS,0                                                         
         BE    GS030                                                            
         CLC   TOKKSYS,SELSYS                                                   
         BNE   GS020                                                            
GS030    GOTO1 AIO,IOGET+IOGENFIL+IO1                                           
         BNE   GSEND               I/O ERROR EXIT                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GSEND    MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
GETSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                                      
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTAGY,TOKKAAGY    DISPLAY KEY DATA                             
*                                                                               
         GOTO1 ADISSYS,TOKKSYS     SYSTEM                                       
         MVC   LISTSYS,APWORK                                                   
*                                                                               
         LA    R3,TOKFIRST(R2)     GET ELEMENT DATA                             
DS030    CLI   0(R3),0                                                          
         BE    DISSELX             END OF RECORD                                
         CLI   0(R3),RTAUTELQ                                                   
         BE    DS050                                                            
DS040    LLC   R0,1(R3)            GET NEXT ELEMENT                             
         AR    R3,R0                                                            
         B     DS030                                                            
*                                                                               
         USING RTAUTHD,R3                                                       
DS050    MVC   LISTUTO,RTAUTID                                                  
         B     DS040                                                            
*                                                                               
DISSELX  B     EXITOK                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)              
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
                                                                                
***********************************************************************         
* VALIDATE ALPHANUMERIC TOKEN                                                   
*  ENTRY:  APBYTE=L'FIELD, R1=A(FIELD) /  EXIT:  CC=ZERO IF OK                  
***********************************************************************         
VALAN    NTR1                                                                   
*                                                                               
         LLC   RF,APBYTE                                                        
         LTR   RF,RF                                                            
         BZ    VAANNO                                                           
*                                                                               
VAAN10   CLI   0(R1),C'9'          NUMBERS                                      
         BH    VAANNO                                                           
         CLI   0(R1),C'0'                                                       
         BNL   VAAN20                                                           
*                                                                               
         CLI   0(R1),C'Z'          UPPER CASE LETTERS                           
         BH    VAANNO                                                           
         CLI   0(R1),C'A'                                                       
         BNL   VAAN20                                                           
*                                                                               
         CLI   0(R1),C'z'          LOWER CASE LETTERS                           
         BH    VAANNO                                                           
         CLI   0(R1),C'a'                                                       
         BL    VAANNO                                                           
*                                                                               
VAAN20   LA    R1,1(R1)                                                         
         BCT   RF,VAAN10                                                        
         B     VAANOK                                                           
*                                                                               
VAANNO   LTR   RB,RB                                                            
         J     EXIT                                                             
VAANOK   CR    RB,RB                                                            
         J     EXIT                                                             
                                                                                
***********************************************************************         
* CHECK SECURITY AND SIGNON AGENCY FOR COMPATIBILITY                            
***********************************************************************         
CHKAGYS  NTR1                                                                   
*                                                                               
         GOTOR GETAGY,CUAALF       LOGON AGENCY DETAILS                         
         BNE   EXIT                ERROR SET BY GETAGY                          
         MVC   SAGY,APWORK                                                      
*                                                                               
         GOTOR GETAGY,AAGY         SETUP AGENCY DETAILS                         
         BNE   EXIT                ERROR SET BY GETAGY                          
         MVC   PUID,APWORK+2       SET PRINCIPAL USER ID                        
*                                                                               
         CLC   CUAALF,SAGY         LOGGED IN W/ SECURITY AGYENCY?               
         BNE   CA020               NO                                           
         CLC   SAGY,APWORK         SETUP AGY HAS SAME SECURITY AGY?             
         BNE   SAEIIF              NO: NO ACCESS TO SETUP AGENCY                
         B     EXITOK              YES: ALLOWED                                 
*                                                                               
CA020    CLC   CUAALF,AAGY         LOGGED IN W/ SETUP AGENCY?                   
         BE    EXITOK              YES: ALLOWED                                 
         CLC   SAGY,AAGY           SECURITY AGENCY SAME AS SETUP AGY?           
         BE    EXITOK              YES: ALLOWING THIS TOO                       
         B     SAEIIF              NO: OTHER COMBINATIONS NO ACCESS             
                                                                                
***********************************************************************         
* GET AGENCY DETAILS                                                            
***********************************************************************         
GETAGY   NTR1                                                                   
*                                                                               
         XC    APWORK,APWORK                                                    
         MVC   APWORK(2),0(R1)     SET SECURITY AGENCY DEFAULT                  
*                                                                               
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R1)      AGENCY                                       
         L     R2,AIOAREA3                                                      
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNE   SAEIIF                                                           
*                                                                               
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
GA010    CLI   0(R3),0             TEST EOR                                     
         BE    GAOKX                                                            
         CLI   0(R3),CTDSCELQ      X'02' CONTROL PRINCIPAL USER ID              
         BE    GA030                                                            
         CLI   0(R3),CTSEAELQ      X'B8' SECURITY AGENCY ALPHA ID               
         BE    GA040                                                            
GA020    LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GA010                                                            
*                                                                               
         USING CTDSCD,R3                                                        
GA030    MVC   APWORK+2(2),CTDSC   PRINCIPAL ID                                 
         B     GA020                                                            
         DROP  R3                                                               
*                                                                               
         USING CTSEAD,R3                                                        
GA040    MVC   APWORK(2),CTSEAAID  SECURITY AGENCY ALPHA ID                     
         B     GA020                                                            
         DROP  R3                                                               
*                                                                               
GAOKX    B     EXITOK                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* GET PRINCIPAL USER ID INFORMATION                                             
***********************************************************************         
GETUID   NTR1                                                                   
*                                                                               
         MVC   PUIDCODE,SPACES                                                  
         MVC   PUIDNAME,SPACES                                                  
*                                                                               
         OC    PUID,PUID                                                        
         BZ    SAERNF                                                           
*                                                                               
         LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(ID RECORD)                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PUID        PRINCIPAL USER                               
         L     R2,AIOAREA3                                                      
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNE   GUOKX                                                            
*                                                                               
         LA    R3,CTIDATA          R3=A(FIRST ELEMENT)                          
GU010    CLI   0(R3),0             TEST EOR                                     
         BE    GUOKX                                                            
         CLI   0(R3),CTDSCELQ      X'02' DESCRIPTION                            
         BE    GU030                                                            
         CLI   0(R3),CTORGELQ      X'36' ORIGIN DETAILS ELEMENT                 
         BE    GU040                                                            
GU020    LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GU010                                                            
*                                                                               
         USING CTDSCD,R3                                                        
GU030    MVC   PUIDCODE,CTDSC       ORIGIN ID                                   
         B     GU020                                                            
         DROP  R3                                                               
*                                                                               
         USING CTORGD,R3                                                        
GU040    MVC   PUIDNAME,CTORGNAM    ORIGIN NAME                                 
         B     GU020                                                            
         DROP  R3                                                               
*                                                                               
GUOKX    B     EXITOK                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* EXIT AND ERROR EXITS                                                          
***********************************************************************         
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     EXITCC              INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         J     EXITCC              MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         J     EXITCC              I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXITCC              RECORD NOT FOUND                             
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         J     EXITCC              ALREADY EXISTS                               
SAESYE   MVC   FVMSGNO,=AL2(CE#INCSS)                                           
         B     EXITCC              INVALID SYSTEM                               
*                                                                               
EXITOK   MVC   FVMSGNO,=AL2(FVFOK)                                              
         J     EXITCC              EXIT OK                                      
*                                                                               
         LTORG                                                                  
*                                                                               
FFS      DC    32X'FF'                                                          
SPACES   DC    132C' '                                                          
                                                                                
***********************************************************************         
* SEACSWRK                                                                      
***********************************************************************         
       ++INCLUDE SEACSWRK                                                       
                                                                                
***********************************************************************         
* TWA                                                                           
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB2D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB3D                                                       
         ORG   SAVOVER                                                          
*----------------------------------------------------------------------         
* SAVED STORAGE (PUT IT HERE)                                                   
*----------------------------------------------------------------------         
SAGY     DS    CL2                                                              
AAGY     DS    CL2                                                              
PUID     DS    XL2                                                              
PUIDCODE DS    CL10                                                             
PUIDNAME DS    CL33                                                             
SYSTEM   DS    X                                                                
SELSYS   DS    X                                                                
*                                                                               
***********************************************************************         
* LIST/SELECT LINE LAYOUT                                                       
***********************************************************************         
LISTD    DSECT                                                                  
LISTSELH DS    XL8                                                              
LISTSEL  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLINX DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTAGY  DS    CL2                 SETUP AGENCY                                 
         DS    CL2                                                              
LISTSYS  DS    CL7                 SYSTEM                                       
         DS    CL2                                                              
LISTUTO  DS    CL32                RENTRAK USER TOKEN                           
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
                                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                                         
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
LOCALX   EQU   *                                                                
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
       ++INCLUDE GEGENTOK                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SEACS28   01/30/19'                                      
         END                                                                    
