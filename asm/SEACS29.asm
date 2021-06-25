*          DATA SET SEACS29    AT LEVEL 009 AS OF 04/23/20                      
*PHASE TA0D29A                                                                  
*INCLUDE DLFLD                                                                  
*                                                                               
         TITLE 'SEACS29 -SECURITY ACCESS - MEDIA OFFICE LISTS'                  
*                                                                               
ACS29    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**S29**,RA,RR=RE                                               
         USING WORKD,R7                 R7=A(GLOBAL W/S)                        
         USING TWAD,R5                  R5=A(TWA)                               
         USING SAVAREA,R6               R6=A(GLOBAL SAVE AREA)                  
         LA    R2,IOKEY                                                         
         USING CTUREC,R2                R2=A(RECORD KEY)                        
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC               RC=A(SYSTEM + LOCAL W/S)                
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         MVC   VDLFLD,CDLFLD                                                    
         XC    DUB(8),DUB                                                       
         MVC   DUB+4(3),=X'D9000A'                                              
         MVI   DUB+7,QOFFICER          GET OFFICER ADDRESS                      
         GOTO1 CCALLOV,DUB                                                      
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   VOFFICER,0(R1)                                                   
         DROP  RF                                                               
         USING OFFICED,OFFBLK                                                   
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
         B     SETSCR                   17 - APMSETT                            
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
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ        C'U' USER PROFILE RECORD                 
         MVI   CTUKPROG+1,C'$'                                                  
         MVC   CTUKAGY,CUAALF          AGENCY                                   
*--------------------------------------                                         
* SYSTEM                                                                        
*--------------------------------------                                         
VK020    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,MOLSYSH                                                    
         BNE   VALKEYX                                                          
*                                                                               
         GOTO1 AVALSYS,MOLSYSH                                                  
         BNE   VALKEYX                                                          
*&&US                                                                           
         CLI   APWORK,X'03'            NETWORK USES SPOT LISTS                  
         BNE   *+8                                                              
         MVI   APWORK,2                FORCE SPOT FOR NETWORK                   
*&&                                                                             
         CLC   SYSTEM,APWORK                                                    
         BE    VK021                                                            
         MVI   SVPAGE,0                RESET PAGE FOR NEW SYSTEM                
         MVC   SYSTEM,APWORK           SAVE SYSTEM NUMBER                       
*                                                                               
VK021    LA    R1,MOLSYSL                                                       
VK022    CLI   0(R1),0                 MAKE SURE VALID SYSTEM FOR               
         BE    SAESYE                  MEDIA OFFICE RECORDS                     
         CLC   SYSTEM,0(R1)                                                     
         BE    VK024                                                            
         LA    R1,1(,R1)                                                        
         B     VK022                                                            
*                                                                               
VK024    L     R1,APPARM               SYSLST ENTRY                             
         MVC   CTUKSYS,12(R1)          SYSTEM LETTER                            
*--------------------------------------                                         
* OFFICE LIST CODE                                                              
*--------------------------------------                                         
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,MOLCODH                                                    
         BNE   VALKEYX                                                          
*                                                                               
         XC    OFFBLK,OFFBLK                                                    
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCMOL2,FVIFLD          TWO CHARACTER OFFICE LIST CODE           
         OI    OFCINDS,OFCIMOLC        OFFICE LIST CONVERSION                   
*                                                                               
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
         BNE   SAEIIF                                                           
         MVC   CTUKPROG+2(1),OFCMOL                                             
*                                                                               
         CLC   OFFLISTC,FVIFLD         HAS THE LIST CODE CHANGED                
         BE    VK100                                                            
         MVI   SVPAGE,0                YES: RESET THE PAGE                      
         MVC   OFFLISTC,FVIFLD                                                  
*--------------------------------------                                         
* READ FOR RECORD                                                               
*--------------------------------------                                         
VK100    LA    R2,IOKEY                                                         
         MVC   CTUKEY,APRECKEY         SET KEY                                  
*                                                                               
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                 I/O ERROR EXIT                           
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL            TEST RECORD IS DELETED                   
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         MVI   APINDS,APIOKADD                                                  
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
         XC    CTUKEY((CTUDATA+1)-CTUREC),CTUKEY                                
         MVC   CTUKEY,APRECKEY                                                  
         LHI   R1,((CTUDATA+1)-CTUREC)                                          
         STCM  R1,3,CTULEN                                                      
         MVI   SVPAGE,0                                                         
         XC    OFFLIST,OFFLIST                                                  
*                                                                               
VR010    LLC   R0,SVPAGE               DISPLAY PAGE                             
         AHI   R0,1                                                             
         EDIT  (R0),(1,MOLPAGE)                                                 
         OI    MOLPAGEH+(FVOIND-FVIHDR),FVOXMT                                  
*--------------------------------------                                         
* OFFICE LIST DESCRIPTION                                                       
*--------------------------------------                                         
         USING CTDSCD,R3                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   CTDSCEL,CTDSCELQ        REMOVE ELEMENT                           
         GOTO1 ADELELS,CTUREC                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,MOLDESH                                                    
         BNE   VR100                                                            
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   CTDSCEL,CTDSCELQ                                                 
         LLC   R1,FVILEN                                                        
         AHI   R1,CTDSC-CTDSCD                                                  
         STC   R1,CTDSCLEN                                                      
         LLC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTDSC(0),MOLDES                                                  
*                                                                               
         GOTO1 AADDELS,CTUREC                                                   
*--------------------------------------                                         
* OFFICE CODES                                                                  
*--------------------------------------                                         
         USING CTOFD,R3                                                         
VR100    LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTPVELQ          REMOVE ELEMENT                           
         GOTO1 ADELELS,CTUREC                                                   
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTOFELQ          REMOVE ELEMENT                           
         GOTO1 ADELELS,CTUREC                                                   
*                                                                               
         MVI   OFFCNT,0                                                         
*                                                                               
         LA    R3,MOLOF11H             FIRST OFFICE ON THE SCREEN               
         LA    R4,OFFLIST              LIST OFF OFFICES FROM RECORD             
         LLC   R1,SVPAGE               PAGE NUMBER (STARTING AT 0)              
         MHI   R1,PAGECOLS*PAGEROWS                                             
         AR    R4,R1                   START FROM CORRECT PAGE                  
*                                                                               
         LA    R8,PAGEROWS                                                      
VR160    LHI   R9,PAGECOLS                                                      
*                                                                               
VR170    MVI   0(R4),0                 CLEAR OFFICE                             
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,0(R3)             ANY OFFICE IN FIELD?                     
         BNE   VR180                   NO                                       
*                                                                               
         XC    OFFBLK,OFFBLK                                                    
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCOFC2,FVIFLD                                                   
*                                                                               
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
*                                                                               
         TM    OFCINDS,OFCIOINV        INVALID OFFICE                           
         BO    SAEIIF                                                           
         MVC   0(1,R4),OFCOFC          STORE OFFICE IN OFFLIST                  
         LA    R4,1(,R4)               BUMP TO NEXT IN OFFLIST                  
         LLC   R0,OFFCNT                                                        
         AHI   R0,1                                                             
         STC   R0,OFFCNT                                                        
*                                                                               
VR180    LA    R1,OFFLIST+L'OFFLIST                                             
         CR    R4,R1                   REACHED MAX OFFICES                      
         BNL   VR210                   YES: THEN DONE                           
*                                                                               
         AHI   R3,MOLOF12H-MOLOF11H    BUMP TO NEXT COLUMN                      
         BCT   R9,VR170                                                         
*                                                                               
         AHI   R3,-(MOLOF12H-MOLOF11H) BACK UP TO LAST FIELD                    
         AHI   R3,MOLOF21H-MOLOF14H    NEXT ROW                                 
         BCT   R8,VR160                                                         
*                                                                               
VR210    BRAS  RE,COMPLIST             COMPRESS THE LIST                        
         CLI   APPFKEY,PFK09           SORT?                                    
         BNE   *+8                                                              
         BRAS  RE,SORTLIST                                                      
         XC    APELEM,APELEM                                                    
*                                                                               
         USING CTOFD,R3                                                         
         LA    R3,APELEM                                                        
         MVI   CTOFEL,CTOFELQ                                                   
         LLC   R1,OFFCNT                                                        
         AHI   R1,-1                                                            
         BM    VR220                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTOFOFFS(0),OFFLIST                                              
*                                                                               
         LLC   R1,OFFCNT                                                        
         AHI   R1,CTOFOFFS-CTOFD                                                
         STC   R1,CTOFLEN                                                       
*                                                                               
         GOTO1 AADDELS,CTUREC                                                   
*--------------------------------------                                         
* ADD / WRITE UPDATED RECORD                                                    
*--------------------------------------                                         
VR220    LA    R1,IOADD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
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
         MVI   SVPAGE,0                                                         
         XC    OFFLIST,OFFLIST                                                  
*                                                                               
         LA    R1,SYSLST+6                                                      
DK010    CLI   0(R1),0                 END OF TABLE                             
         JE    *+2                     SHOULD NEVER HAPPEN                      
         CLC   CTUKSYS,12(R1)          MATCH ON SYSTEM LETTER                   
         BE    DK020                                                            
         LA    R1,L'SYSLST(,R1)                                                 
         B     DK010                                                            
DK020    MVC   MOLSYS(7),2(R1)                                                  
         OI    MOLSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   SYSTEM,0(R1)                                                     
*                                                                               
         OI    MOLCODH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         XC    OFFBLK,OFFBLK                                                    
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCMOL,CTUKPROG+2       ONE BYTE OFFICE VALUE                    
         OI    OFCINDS,OFCIMOLC        OFFICE LIST CONVERSION                   
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
         BE    DK030                   CONVERTED SUCCESSFULLY                   
         MVC   MOLCOD,OFCMOL           SHOW BAD CHARACTER                       
         B     SAEIIF                                                           
*                                                                               
DK030    MVC   MOLCOD,OFCMOL2          TWO CHARACTER OFFICE LIST CODE           
         MVC   OFFLISTC,MOLCOD         SAVE THE OFFICE LIST CODE                
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY                                                            
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
DR010    CLI   APPFKEY,PFK07           PAGE UP                                  
         BNE   DR012                                                            
         CLI   SVPAGE,0                SAVED PAGE AT ZERO?                      
         BE    DR014                   YES: CAN'T SCROLL UP                     
         LLC   R0,SVPAGE                                                        
         AHI   R0,-1                                                            
         STC   R0,SVPAGE                                                        
*                                                                               
DR012    CLI   APPFKEY,PFK08           PAGE DOWN                                
         BNE   DR014                                                            
         CLI   SVPAGE,2                SAVED PAGE AT 2?                         
         BNL   DR014                   YES: CAN'T SCROLL DOWN                   
         LLC   R0,SVPAGE                                                        
         AHI   R0,1                                                             
         STC   R0,SVPAGE                                                        
*                                                                               
DR014    LLC   R0,SVPAGE               DISPLAY PAGE                             
         AHI   R0,1                                                             
         EDIT  (R0),(1,MOLPAGE)                                                 
         OI    MOLPAGEH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         MVC   MOLDES,SPACES           INIT DESCRIPTION                         
         OI    MOLDESH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         LA    R3,CTUDATA                                                       
DR020    CLI   0(R3),0                                                          
         BE    DR150                                                            
         CLI   0(R3),CTDSCELQ          X'02' DESCRIPTION                        
         BE    DR100                                                            
DR030    LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR020                                                            
*                                                                               
         USING CTDSCD,R3                                                        
DR100    LLC   R1,CTDSCLEN             SHOW DESCRIPTION                         
         AHI   R1,-((CTDSC-CTDSCD)+1)                                           
         BM    DR030                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MOLDES(0),CTDSC                                                  
         B     DR030                                                            
*                                                                               
DR150    BRAS  RE,BLDLIST              BUILD OFFICE LIST IN BUFFER              
         BNE   EXIT                                                             
         BRAS  RE,COMPLIST             COMPRESS THE LIST                        
*                                                                               
         LA    R3,MOLOF11H             FIRST OFFICE ON THE SCREEN               
         LA    R4,OFFLIST              LIST OFF OFFICES FROM RECORD             
         LLC   R1,SVPAGE               PAGE NUMBER (STARTING AT 0)              
         MHI   R1,PAGECOLS*PAGEROWS                                             
         AR    R4,R1                   START FROM CORRECT PAGE                  
*                                                                               
         LA    R8,PAGEROWS                                                      
DR160    LHI   R9,PAGECOLS                                                      
*                                                                               
DR170    MVC   8(L'MOLOF11,R3),SPACES  CLEAR FIELD                              
         OI    6(R3),FVOXMT            AND TRANSMIT                             
         LA    R1,MOLOF1NH-MOLOF11H(,R3)                                        
         MVC   8(L'MOLOF1N,R1),SPACES  CLEAR FIELD                              
         OI    6(R1),FVOXMT            AND TRANSMIT                             
*                                                                               
         CLI   0(R4),0                 ANY MORE OFFICES?                        
         BE    DR180                   NO: ONLY CLEAR FIELDS                    
         LA    R1,OFFLIST+L'OFFLIST                                             
         CR    R4,R1                   REACHED MAX OFFICES                      
         BL    DR190                   NOT YET                                  
*                                                                               
DR180    AHI   R3,MOLOF12H-MOLOF11H    BUMP TO NEXT FIELD                       
         B     DR200                                                            
*                                                                               
DR190    XC    OFFBLK,OFFBLK                                                    
         MVC   SHORTNM,SPACES                                                   
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCOFC,0(R4)                                                     
*                                                                               
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(1,ACOM),(C'S',SHORTNM)            
*                                                                               
         MVC   8(2,R3),OFCOFC2         TWO BYTE OFFICE                          
         LA    R1,MOLOF1NH-MOLOF11H(,R3)                                        
         MVC   8(L'SHORTNM,R1),SHORTNM SHORT NAME                               
*                                                                               
         AHI   R3,MOLOF12H-MOLOF11H    BUMP TO NEXT OFFICE FIELD                
         LA    R4,1(,R4)               NEXT OFFICE IN LIST                      
*                                                                               
DR200    BCT   R9,DR170                PUT ON NEXT COLUMN                       
         AHI   R3,-(MOLOF12H-MOLOF11H) BACK UP TO LAST FIELD                    
         AHI   R3,MOLOF21H-MOLOF14H    NEXT ROW                                 
         BCT   R8,DR160                                                         
*                                                                               
DISRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                    EXIT EQUAL                               
                                                                                
***********************************************************************         
* DELETE A RECORD                                                               
***********************************************************************         
DELREC   L     R2,AIOAREA1                                                      
         OI    CTUSTAT,X'80'           SET DELETE FLAG                          
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         JNE   *+2                                                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* RESTORE A DELETED RECORD                                                      
***********************************************************************         
RESREC   L     R2,AIOAREA1                                                      
         NI    CTUSTAT,X'FF'-X'80'     TURN DELETE BIT OFF                      
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
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
VALSEL   LA    R2,APRECKEY             BUILD FIRST RECORD KEY                   
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVI   CTUKSYS,C'P'            START WITH PRINT                         
         MVI   CTUKPROG+1,C'$'                                                  
*                                                                               
         XC    SELSYS,SELSYS           SYSTEM LETTER                            
         MVI   FVMINL,1                SYSTEM                                   
         GOTO1 AFVAL,LSTSYSH                                                    
         BNE   VS050                                                            
*                                                                               
         GOTO1 AVALSYS,MOLSYSH                                                  
         BNE   SAEIIF                                                           
*&&US                                                                           
         CLI   APWORK,X'03'            NETWORK USES SPOT LISTS                  
         BNE   *+8                                                              
         MVI   APWORK,2                FORCE SPOT FOR NETWORK                   
*&&                                                                             
         MVC   SYSTEM,APWORK                                                    
         LA    R1,MOLSYSL                                                       
VS020    CLI   0(R1),0                 MAKE SURE VALID SYSTEM FOR               
         BE    SAESYE                  MEDIA OFFICE RECORDS                     
         CLC   SYSTEM,0(R1)                                                     
         BE    VS030                                                            
         LA    R1,1(,R1)                                                        
         B     VS020                                                            
VS030    L     R1,APPARM               SYSLST ENTRY                             
         MVC   SELSYS,12(R1)           SYSTEM LETTER                            
*                                                                               
VS050    LA    R0,LSTACT1H             SET ADDRESS OF FIRST LIST LINE           
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H             SET LIST LINE LENGTH                     
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                                   
***********************************************************************         
GETSEL   LA    R2,IOKEY                READ NEXT RECORD                         
         MVC   CTUKEY,APRECKEY         FROM LAST KEY                            
*                                                                               
         TM    APINDS,APILNSEQ         FIRST LINE IN LIST SEQUENCE              
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
GS010    LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
         L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTUKEY),CTUKEY                                        
*                                                                               
         CLI   CTUKTYP,CTUKTYPQ        USER PROFILE                             
         BNE   GSEND                   NO: DONE                                 
*                                                                               
         CLI   CTUKSYS,C'P'            LOOKING FOR PRINT OFFICE LISTS?          
         BNE   GS030                   NO: CHECK SPOT                           
         CLI   CTUKPROG+1,C'$'         IS THIS AN OFFICE LIST?                  
         BNE   GS040                   NO: MOVE ON                              
         B     GS050                   YES: CONTINUE VERIFICATION               
*                                                                               
GS030    CLI   CTUKSYS,C'S'            LOOKING FOR SPOT OFFICE LISTS?           
         BL    GS040                   NOT YET: GO JUMP TO SPOT LISTS           
         BH    GSEND                   DONE WITH SPOT, THEN THAT'S IT           
         CLI   CTUKPROG+1,C'$'         STILL READING OFFICE LISTS               
         BH    GSEND                   NO: THEN WE'RE FINISHED                  
         BE    GS050                   YES: CONTINUE VERIFYING RECORD           
*                                                                               
GS040    LA    R2,IOKEY                FORCE READ FOR SPOT OFFICE LISTS         
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVI   CTUKSYS,C'S'                                                     
         MVI   CTUKPROG+1,C'$'                                                  
         B     GS010                                                            
*                                                                               
GS050    CLC   CTUKAGY,CUAALF          IS THIS THE AGENCY WE WANT?              
         BNE   GS020                   NO: GET NEXT                             
         OC    CTUKMED(CTULEN-CTUKMED),CTUKMED THE RESET SHOULD BE ZERO         
         BNZ   GS020                           NO: GET NEXT                     
         CLI   SELSYS,0                IS THERE A SYSTEM FILTER?                
         BE    GS090                   NO: DISPLAY THIS RECORD                  
         CLC   CTUKSYS,SELSYS          IS THIS THE SYSTEM WE WANT?              
         BNE   GS020                   NO: GET NEXT                             
*                                                                               
GS090    MVC   FVMSGNO,=AL2(FVFOK)     EVERYTHING CHECKS OUT                    
         B     GETSELX                 WE WANT THIS ONE                         
*                                                                               
GSEND    MVI   APMODE,APMEOFS          SET NO MORE RECORDS TO COME              
GETSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                                      
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4                R4=A(LIST/SELECT LINE)                   
         MVI   FLAG,0                  CLEAR FLAG FOR OFFICE DISPLAYING         
*                                                                               
         LA    R1,SYSLST+6                                                      
DS010    CLI   0(R1),0                 END OF TABLE                             
         JE    *+2                     SHOULD NEVER HAPPEN                      
         CLC   CTUKSYS,12(R1)          MATCH ON SYSTEM LETTER                   
         BE    DS020                                                            
         LA    R1,L'SYSLST(,R1)                                                 
         B     DS010                                                            
DS020    MVC   LISTSYS(7),2(R1)                                                 
*                                                                               
         XC    OFFBLK,OFFBLK                                                    
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCMOL,CTUKPROG+2                                                
         OI    OFCINDS,OFCIMOLC        OFFICE LIST CONVERSION                   
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
         MVC   LISTCOD,OFCMOL2         OFFICE LIST                              
*                                                                               
         LA    R3,CTUDATA                                                       
DS022    CLI   0(R3),0                                                          
         BE    DS050                                                            
         CLI   0(R3),CTDSCELQ          X'02' DESCRIPTION                        
         BE    DS030                                                            
         CLI   0(R3),CTPVELQ           X'72' PROFILE VALUE ELEMENT              
         BE    DS040                                                            
         CLI   0(R3),CTOFELQ           X'75' MEDIA OFFICE ELEMENT               
         BE    DS040                                                            
DS025    LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DS022                                                            
*                                                                               
         USING CTDSCD,R3                                                        
DS030    LLC   R1,CTDSCLEN             SHOW DESCRIPTION                         
         AHI   R1,-((CTDSC-CTDSCD)+1)                                           
         BM    DS025                                                            
         CHI   R1,L'LISTDSC                                                     
         BL    *+8                                                              
         LHI   R1,L'LISTDSC-1                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTDSC(0),CTDSC                                                 
         B     DS025                                                            
*                                                                               
         USING CTOFD,R3                                                         
DS040    LA    R1,LISTOFF                                                       
         ST    R1,APDUB                                                         
*                                                                               
         LA    R8,CTOFOFFS                                                      
         LLC   R9,CTOFLEN                                                       
         AHI   R9,-(CTOFOFFS-CTOFEL)                                            
         BNP   DS025                                                            
         CHI   R9,9                    SHOW UP TO 9 OFFICES                     
         BNH   DS046                                                            
         LHI   R9,9                                                             
         CLI   0(R3),CTPVELQ           NO ELIPSES FOR OLD STYLE ELEM            
         BE    DS046                                                            
         MVC   LISTOFF+L'LISTOFF-4(3),=C'...'                                   
         B     DS046                                                            
*                                                                               
DS044    TM    FLAG,FOSKIPQ            SKIPPING THIS OFFICE                     
         BO    DS045                                                            
         MVI   0(R1),C','              SEPARATE OFFICES WITH COMMA              
         LA    R1,1(,R1)                                                        
         ST    R1,APDUB                                                         
DS045    NI    FLAG,X'FF'-FOSKIPQ      RESET SKIP FLAG                          
*                                                                               
DS046    CLI   0(R8),C'0'              OLD PROFILES HAD A DEFAULT C'0'          
         BNE   DS047                   ONLY DISPLAY ONE C'0'                    
         TM    FLAG,FOZEROQ            FLAG FOR OFFICE C'0'                     
         BO    DS048                   NOT YET                                  
         OI    FLAG,FOZEROQ                                                     
*                                                                               
DS047    XC    OFFBLK,OFFBLK           OFFICER FOR PRINTABLE OFFICE             
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCOFC,0(R8)                                                     
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
         BE    DS047A                  OFFICE LIST                              
         MVC   OFCOFC2,=C'??'                                                   
DS047A   L     R1,APDUB                                                         
         MVC   0(2,R1),OFCOFC2                                                  
         LA    R1,2(,R1)                                                        
         B     DS049                                                            
*                                                                               
DS048    OI    FLAG,FOSKIPQ            SKIPPING THIS OFFICE                     
*                                                                               
DS049    LA    R8,1(,R8)                                                        
         BCT   R9,DS044                                                         
*                                                                               
         AHI   R1,-1                                                            
         CLI   0(R1),C','                                                       
         BNE   DS025                                                            
         MVI   0(R1),C' '                                                       
         B     DS025                                                            
         DROP  R3                                                               
*                                                                               
DS050    MVC   IOKEY,0(R2)                                                      
         GOTO1 AIO,IORDD+IOCONFIL+IO1                                           
         BE    DISSELX                                                          
         TM    IOERR,IOEDEL            TEST RECORD IS DELETED                   
         BO    DISSELX                                                          
         DC    H'0'                                                             
*                                                                               
DISSELX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* FIRST SCREEN INIT                                                             
***********************************************************************         
SETSCR   TM    TWASWPST,TWAFIRST                                                
         BZ    SETSCRX                                                          
         MVI   SVPAGE,0                                                         
         XC    OFFLIST,OFFLIST                                                  
         NI    TWASWPST,X'FF'-TWAFIRST TURN OFF FIRST FOR RECORD                
SETSCRX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
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
         GOTO1 AFVAL,REPSYSH       VALIDATE SYSTEM                              
         BNE   VQ040                                                            
         GOTO1 AVALSYS,REPSYSH                                                  
         BNE   SAEIIF                                                           
*&&US                                                                           
         CLI   APWORK,X'03'        NETWORK USES SPOT LISTS                      
         BNE   *+8                                                              
         MVI   APWORK,X'02'        FORCE SPOT FOR NETWORK                       
*&&                                                                             
         MVC   SYSTEM,APWORK                                                    
         LA    R1,MOLSYSL                                                       
VQ020    CLI   0(R1),0             MAKE SURE VALID SYSTEM FOR                   
         BE    SAESYE              MEDIA OFFICE RECORDS                         
         CLC   SYSTEM,0(R1)                                                     
         BE    VQ030                                                            
         LA    R1,1(,R1)                                                        
         B     VQ020                                                            
VQ030    L     R1,APPARM           SYSLST ENTRY                                 
         MVC   SELSYS,12(R1)       SYSTEM LETTER                                
*                                                                               
VQ040    GOTO1 AFVAL,REPOFFLH      VALIDATE OFFICE LIST CODE                    
         BNE   VQ050                                                            
*                                                                               
         XC    OFFBLK,OFFBLK                                                    
         MVI   OFCSYS,C'P'         PRINT/SPOT HAVE SAME 1 BYTE VALUE            
         MVC   OFCAGY,CUAALF                                                    
         MVC   OFCMOL2,FVIFLD                                                   
         OI    OFCINDS,OFCIMOLC    OFFICE LIST CONVERSION                       
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
         BNE   SAEIIF                                                           
         MVC   SELOFFL,OFCMOL      ONE BYTE OFFICE LIST VALUE                   
*                                                                               
VQ050    GOTO1 AFVAL,REPOFFCH      VALIDATE OFFICE CODE                         
         BNE   VQ100                                                            
         MVC   SELOFF,FVIFLD                                                    
*                                                                               
VQ100    LA    R2,APRECKEY         BUILD FIRST RECORD KEY                       
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVC   CTUKSYS,SELSYS                                                   
         CLI   CTUKSYS,0                                                        
         BNE   *+8                                                              
         MVI   CTUKSYS,C'P'        START WITH PRINT                             
         MVI   CTUKPROG+1,C'$'                                                  
         MVC   CTUKPROG+2(1),SELOFFL                                            
*                                                                               
         MVCDD REPDESC,CT#OFFL     SET REPORT DESCRIPTION                       
         GOTO1 VDICTAT,APPARM,C'SL  ',REPDESC                                   
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO GENERATE REPORT                                                    
***********************************************************************         
PRTREP   L     R4,AREP                                                          
*                                                                               
         CLC   =C'DOWN',REPOTYP                                                 
         BNE   PR060                                                            
         MVI   REPLINE,1                                                        
         XC    REPAPHS,REPAPHS         Clear specs                              
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         CLC   =C'NOW',REPWHEN         Only print this for NOW request          
         BNE   PR050                                                            
         BRAS  RE,PRTFILT              Output filters for NOW report            
         MVI   REPLINE,99                                                       
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PR050    LA    R3,WORKD                                                         
         AHI   R3,DLCB-WORKD                                                    
         USING DLCBD,R3                                                         
         XC    DLCBD(DLCBL),DLCBD                                               
         MVI   DLCBACT,DLCBINIT        Download action is start                 
         LARL  RF,DLHOOK                                                        
         ST    RF,DLCBAPR                                                       
         LA    RF,REPP1                                                         
         ST    RF,DLCBAPL                                                       
         LA    RF,L'REPP1                                                       
         STH   RF,DLCXMAXL                                                      
         MVI   DLCXDELC,C' '           Delimiter                                
         MVI   DLCXEOTC,C'"'           Text delimiter                           
         MVI   DLCXEOLC,X'5E'          Semi-colon, Ene-of-line                  
         MVI   DLCXEORC,C':'           End-of-report                            
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
         MVC   DLCBFLD,SPACES          Must clear first time                    
         MVI   DLCBFLX,C' '                                                     
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
         DROP  R3                                                               
*                                                                               
PR060    CLC   =C'DOWN',REPOTYP        Download                                 
         BE    PR065                   Yes                                      
         CLC   =C'NOW',REPWHEN         No: NOW request                          
         BNE   PR065                   No                                       
         BRAS  RE,PRTFILT              Output filters for NOW request           
*                                                                               
PR065    LA    R2,IOKEY                                                         
         MVC   CTUKEY,APRECKEY         Set initial key value                    
PR070    LA    R1,IOHI+IOCONFIL+IO1                                             
         B     PR100                   GET FIRST RECORD                         
*                                      GET NEXT RECORD(SEQUENCE BROKEN)         
PR080    LA    R2,IOKEY                                                         
         MVC   CTUKEY,APRECKEY                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PREND                                                            
*                                      GET NEXT RECORD (IN SEQUENCE)            
PR090    LA    R1,IOSQ+IOCONFIL+IO1                                             
PR100    GOTO1 AIO                                                              
         BNE   PREND                                                            
         L     R2,AIOAREA1                                                      
*                                                                               
         MVI   FLAG,0                  CLEAR FLAG FOR OFFICE DISPLAYING         
         MVI   PRFLDS,C' '             CLEAR THE PRINT FIELDS                   
         MVC   PRFLDS+1(255),PRFLDS                                             
         MVC   PRFLDS+256(236),PRFLDS                                           
*                                                                               
         CLI   CTUKTYP,CTUKTYPQ        USER PROFILE                             
         BNE   PREND                   NO: DONE                                 
         CLI   CTUKSYS,C'P'            LOOKING FOR PRINT OFFICE LISTS?          
         BNE   PR110                   NO: CHECK SPOT                           
         CLI   CTUKPROG+1,C'$'         IS THIS AN OFFICE LIST?                  
         BNE   PR120                   NO: MOVE ON                              
         B     PR130                   YES: CONTINUE VERIFICATION               
PR110    CLI   CTUKSYS,C'S'            LOOKING FOR SPOT OFFICE LISTS?           
         BL    PR120                   NOT YET: GO JUMP TO SPOT LISTS           
         BH    PREND                   DONE WITH SPOT, THEN THAT'S IT           
         CLI   CTUKPROG+1,C'$'         STILL READING OFFICE LISTS               
         BH    PREND                   NO: THEN WE'RE FINISHED                  
         BE    PR130                   YES: CONTINUE VERIFYING RECORD           
PR120    LA    R2,IOKEY                FORCE READ FOR SPOT OFFICE LISTS         
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVI   CTUKSYS,C'S'                                                     
         MVI   CTUKPROG+1,C'$'                                                  
         B     PR070                                                            
*                                                                               
PR130    CLC   CTUKAGY,CUAALF          IS THIS THE AGENCY WE WANT?              
         BNE   PR090                   NO: GET NEXT                             
         OC    CTUKMED(CTULEN-CTUKMED),CTUKMED THE REST SHOULD BE ZERO          
         BNZ   PR090                           NO: GET NEXT                     
*                                                                               
         CLI   SELSYS,0                IS THERE A SYSTEM FILTER?                
         BE    PR140                   NO: DISPLAY THIS RECORD                  
         CLC   SELSYS,CTUKSYS          IS THIS THE SYSTEM WE WANT?              
         BNE   PR090                   NO: GET NEXT                             
*                                                                               
PR140    CLI   SELOFFL,0           Did we have an office list filter?           
         BE    PR145               No                                           
         CLC   SELOFFL,CTUKPROG+2  Yes: is it the one we want?                  
         BNE   PR090               No: get next                                 
*                                                                               
PR145    MVC   APRECKEY,CTUKEY     FOUND A GOOD RECORD, SAVE IT                 
*                                                                               
         LA    R1,SYSLST+6                                                      
PR150    CLI   0(R1),0             END OF TABLE                                 
         JE    *+2                 SHOULD NEVER HAPPEN                          
         CLC   CTUKSYS,12(R1)      MATCH ON SYSTEM LETTER                       
         BE    PR160                                                            
         LA    R1,L'SYSLST(,R1)                                                 
         B     PR150                                                            
PR160    MVC   PRSYS(7),2(R1)                                                   
*                                                                               
         XC    OFFBLK,OFFBLK                                                    
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCMOL,CTUKPROG+2                                                
         OI    OFCINDS,OFCIMOLC    OFFICE LIST CONVERSION                       
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
         MVC   PROFL,OFCMOL2       OFFICE LIST                                  
*                                                                               
         MVC   PRDSC(10),DASHES                                                 
*                                                                               
         LA    R3,CTUDATA                                                       
PR170    CLI   0(R3),0                                                          
         BE    PR200                                                            
         CLI   0(R3),CTDSCELQ      X'02' DESCRIPTION                            
         BE    PR190                                                            
PR180    LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     PR170                                                            
*                                                                               
         USING CTDSCD,R3                                                        
PR190    LLC   R1,CTDSCLEN         SHOW DESCRIPTION                             
         AHI   R1,-((CTDSC-CTDSCD)+1)                                           
         BM    PR180                                                            
         MVC   PRDSC,SPACES                                                     
         CHI   R1,L'PRDSC                                                       
         BL    *+8                                                              
         LHI   R1,L'PRDSC-1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRDSC(0),CTDSC                                                   
         B     PR180                                                            
         DROP  R3                                                               
*                                                                               
PR200    BRAS  RE,BLDLIST          BUILD OFFICE LIST IN BUFFER                  
         JNE   *+2                                                              
         BRAS  RE,COMPLIST         COMPRESS THE LIST                            
*                                                                               
         NI    FLAG,X'FF'-FOFILTQ  RESET OFFICE FOUND FLAG                      
*                                                                               
         LA    R3,PROFF            OFFICE PRINT AREA                            
         LA    R8,OFFLIST          OFFICES IN LIST                              
         LLC   R9,OFFCNT           OFFICE COUNT                                 
         LTR   R9,R9               MAKE SURE THERE IS ONE OFFICE                
         BZ    PR250                                                            
         B     PR220                                                            
*                                                                               
PR210    CLI   0(R8),0             ANY MORE OFFICES                             
         BE    PR250               NO                                           
*                                                                               
         AHI   R3,-1                                                            
         CLI   0(R3),C' '                                                       
         BE    *+8                                                              
         AHI   R3,1                                                             
         MVI   0(R3),C','          SEPARATE OFFICES WITH COMMA                  
         LA    R3,1(,R3)                                                        
*                                                                               
PR220    XC    OFFBLK,OFFBLK       OFFICER FOR PRINTABLE OFFICE                 
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCOFC,0(R8)                                                     
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
         BE    *+10                OFFICE LIST                                  
         MVC   OFCOFC2,=C'??'                                                   
         MVC   0(L'OFCOFC2,R3),OFCOFC2                                          
         LA    R3,L'OFCOFC2(,R3)                                                
*                                                                               
         CLI   SELOFF,0            IS THERE AN OFFICE FILTER?                   
         BE    *+14                NO                                           
         CLC   SELOFF,OFCOFC2      IS THIS THE OFFICE WE WANT?                  
         BNE   *+8                 NO                                           
         OI    FLAG,FOFILTQ        RESET OFFICE FOUND FLAG                      
*                                                                               
         LA    R8,1(,R8)                                                        
         BCT   R9,PR210                                                         
                                                                                
*----------------------------------------------------------------------         
* PRINT OFFICE LIST                                                             
*----------------------------------------------------------------------         
PR250    CLI   SELOFF,0            IS THERE AN OFFICE FILTER?                   
         BE    PR300               NO                                           
         TM    FLAG,FOFILTQ        DID WE FIND THE OFFICE IN LIST?              
         BO    PR300               YES                                          
         B     PR080               NO: SKIP THIS LIST                           
*                                                                               
PR300    CLC   =C'DOWN',REPOTYP                                                 
         BNE   PR310                                                            
         BRAS  RE,DLLINE           FORMAT AND PRINT DOWNLOAD LINE               
         B     PR080                                                            
*                                                                               
PR310    MVC   RLSYS,PRSYS                                                      
         MVC   RLOFL,PROFL                                                      
         MVC   RLDSC,PRDSC                                                      
*                                                                               
         LA    R1,PROFF                                                         
         LA    RF,RLOFF                                                         
PR320    MVC   0(L'RLOFF,RF),0(R1)                                              
         LA    R1,L'RLOFF(,R1)                                                  
         LA    RE,L'RLOFF-1(,RF)                                                
*                                                                               
         CLI   0(R1),C' '                                                       
         BNH   PR400                                                            
         CLI   0(R1),C','                                                       
         BE    PR350                                                            
         AHI   R1,-1                                                            
*                                                                               
PR330    CLI   0(R1),C','                                                       
         BE    PR340                                                            
         MVI   0(RE),C' '                                                       
         AHI   R1,-1                                                            
         AHI   RE,-1                                                            
         B     PR330                                                            
PR340    LA    R1,1(,R1)                                                        
*                                                                               
PR350    LA    RF,L'REPP1(,RF)                                                  
         B     PR320                                                            
*                                                                               
PR400    GOTO1 VREPORT,REPD        PRINT LINE                                   
         B     PR080               READ SEQUENCE IS BROKEN                      
*                                                                               
PREND    CLC   =C'DOWN',REPOTYP                                                 
         BNE   PREND02                                                          
*                                                                               
         LA    R3,WORKD                                                         
         AHI   R3,DLCB-WORKD                                                    
         USING DLCBD,R3                                                         
         MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
PREND02  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* Output filters for NOW report                                                 
***********************************************************************         
PRTFILT  NTR1                                                                   
*                                                                               
         LA    R3,REPH5+3                                                       
         CLC   =C'DOWN',REPOTYP                                                 
         BNE   PF005                                                            
         MVI   REPLINE,1                                                        
         MVC   REPP1+3(18),=C'Office List Report'                               
         MVC   REPP2+3(18),=C'------------------'                               
         LA    R3,REPP3+3                                                       
*                                                                               
PF005    CLC   REPSYS,SPACES                                                    
         BNH   PF010                                                            
         LR    R1,R3                                                            
         MVC   0(L'REPSYSN,R1),REPSYSN                                          
         LA    R1,L'REPSYSN-1(,R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         AHI   R1,-1                                                            
         B     *-12                                                             
         MVI   1(R1),C'='                                                       
         MVC   2(L'REPSYS,R1),REPSYS                                            
         LA    R3,L'REPP1(,R3)                                                  
*                                                                               
PF010    CLC   REPOFFL,SPACES                                                   
         BNH   PF020                                                            
         LR    R1,R3                                                            
         MVC   0(L'REPOFLN,R1),REPOFLN                                          
         LA    R1,L'REPOFLN-1(,R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         AHI   R1,-1                                                            
         B     *-12                                                             
         MVI   1(R1),C'='                                                       
         MVC   2(L'REPOFFL,R1),REPOFFL                                          
         LA    R3,L'REPP1(,R3)                                                  
*                                                                               
PF020    CLC   REPOFFC,SPACES                                                   
         BNH   PF030                                                            
         LR    R1,R3                                                            
         MVC   0(L'REPOFCN,R1),REPOFCN                                          
         LA    R1,L'REPOFCN-1(,R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         AHI   R1,-1                                                            
         B     *-12                                                             
         MVI   1(R1),C'='                                                       
         MVC   2(L'REPOFFC,R1),REPOFFC                                          
*                                                                               
PF030    GOTO1 VREPORT,REPD                                                     
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* BUILD OFFICE LIST IN BUFFER (OFFLIST)                                         
***********************************************************************         
BLDLIST  NTR1                                                                   
*                                                                               
         L     R2,AIOAREA1             R2=A(OFFICE LIST RECORD)                 
         LA    R4,OFFLIST              R4=A(OFFICE LIST BUFFER)                 
         XC    OFFLIST,OFFLIST                                                  
         MVI   BYTE,C'N'               ASSUME NEW STYLE                         
*                                                                               
BL020    LA    R3,CTUDATA                                                       
BL030    CLI   0(R3),0                 END OF RECORD?                           
         BE    BL100                                                            
         CLI   0(R3),CTPVELQ           PROFILE VALUE ELEMENT                    
         BE    BL050                                                            
         CLI   0(R3),CTOFELQ           MEDIA OFFICE LIST ELEMENT                
         BE    BL060                                                            
BL040    LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BL030                                                            
*                                                                               
         USING CTPVD,R3                                                         
BL050    MVC   0(16,R4),CTPVALUE       OFFICES                                  
         LA    R4,16(,R4)              GET READY FOR NEXT PAGE                  
         MVI   BYTE,C'O'               OLD STYLE OFFICE PROFILE                 
         B     BL040                                                            
*                                                                               
         USING CTOFD,R3                                                         
BL060    LLC   R1,CTOFLEN                                                       
         AHI   R1,-((CTOFOFFS-CTOFD)+1) OVERHEAD PLUS 1 FOR EXECUTE             
         BM    BLXNE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFLIST(0),CTOFOFFS     COPY IN ALL OFFICES                      
         B     BL040                                                            
         DROP  R3                                                               
*                                                                               
BL100    CLI   BYTE,C'O'               IS THIS AN OLD STYLE PROFILE             
         BNE   BL200                   NO: THEN WE'RE ALL DONE                  
*                                                                               
         MVC   IOKEY,0(R2)             OLD STYLE OFFICE LISTS MAY               
         LA    R2,IOKEY                HAVE OTHER RECORDS                       
         CLI   CTUKPROG,C'$'                                                    
         BE    BL110                                                            
         MVC   CTUKPROG(2),CTUKPROG+1                                           
         MVI   CTUKPROG+2,C'A'                                                  
         B     BL120                                                            
BL110    CLI   CTUKPROG+2,C'D'         C'D' IS LAST RECORD IN LIST              
         BNL   BL200                   WE'VE REACHED THE LIMIT, MOVE ON         
         LLC   R0,CTUKPROG+2                                                    
         AHI   R0,1                                                             
         STC   R0,CTUKPROG+2           BUMP TO NEXT PAGE IN LIST                
BL120    L     R2,AIOAREA2             R2=A(NEXT OFFICE LIST RECORD)            
         GOTO1 AIO,IORDD+IOCONFIL+IO2                                           
         BE    BL020                                                            
         TM    IOERR,IOEDEL            TEST RECORD IS DELETED                   
         BO    BL020                                                            
*                                                                               
BL200    DS    0H                                                               
*                                                                               
BLXEQ    B     EXITEQ                                                           
BLXNE    B     EXITNE                                                           
                                                                                
***********************************************************************         
* COMPRESS OFFICE LIST BY REMOVING DUPLICATE AND BLANK ENTRIES                  
***********************************************************************         
COMPLIST NTR1                                                                   
         XC    TEMPLIST,TEMPLIST                                                
*                                                                               
         XR    R0,R0                                                            
         LA    R3,OFFLIST                                                       
         LA    RF,L'OFFLIST                                                     
*                                                                               
CL010    LA    R4,TEMPLIST             START OF TEMPLIST                        
         LA    RE,L'OFFLIST            MAX 1 BYTE OFFICE ENTRIES                
*                                                                               
CL012    CLI   0(R4),0                 IS THIS AN EMPTY SLOT                    
         BE    CL020                   YES: ADD OFFICE HERE                     
         CLC   0(1,R4),0(R3)           IS OFFICE ALREADY IN THE LIST?           
         BE    CL022                   YES: SKIP IT                             
CL014    LA    R4,1(,R4)                                                        
         BCT   RE,CL012                                                         
         DC    H'0'                    SHOULD NEVER GET HERE                    
*                                                                               
CL020    MVC   0(1,R4),0(R3)           ADD OFFICE TO TEMPLIST                   
         CLI   0(R4),0                 ZERO VALUE                               
         BE    CL022                   DON'T COUNT IT                           
         AHI   R0,1                    COUNT NUMBER OF OFFICES                  
CL022    LA    R3,1(,R3)               BUMP TO NEXT OFFICE                      
         BCT   RF,CL010                                                         
*                                                                               
         MVC   OFFLIST,TEMPLIST                                                 
         STC   R0,OFFCNT                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* SORT OFFICE LIST BY TWO CHARACTER OFFICE CODE                                 
***********************************************************************         
SORTLIST NTR1                                                                   
*                                                                               
         LA    R3,OFFLIST              OFFICE LIST                              
         LA    R4,TEMPLIST             START OF TEMPLIST                        
         LA    R8,L'OFFLIST                                                     
         XC    TEMPLIST,TEMPLIST                                                
         XC    TEMPLIS2,TEMPLIS2                                                
         XC    TEMPLIS3,TEMPLIS3                                                
*                                                                               
SL010    CLI   0(R3),0                 START OF COMPRESSED OFFICE LIST          
         BE    SL020                   ZERO MEANS WE'RE DONE                    
*                                                                               
         XC    OFFBLK,OFFBLK                                                    
         MVC   OFCSYS,CTUKSYS                                                   
         MVC   OFCAGY,CTUKAGY                                                   
         MVC   OFCOFC,0(R3)                                                     
*                                                                               
         GOTO1 VOFFICER,APPARM,(C'2',OFFBLK),(0,ACOM)                           
         MVC   0(2,R4),OFCOFC2                                                  
         MVC   2(1,R4),OFCOFC                                                   
*                                                                               
         LA    R3,1(,R3)               NEXT OFFICE                              
         LA    R4,3(,R4)                                                        
         BCT   R8,SL010                                                         
*                                                                               
SL020    LA    R1,OFFLIST                                                       
         SR    R3,R1                   NUMBER OF ENTRIES                        
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CXSORT-COMFACSD(RF)                                           
         GOTO1 (RF),APPARM,(0,TEMPLIST),(R3),3,2,0,0                            
*                                                                               
         LA    R3,OFFLIST                                                       
         LA    R4,TEMPLIST             START OF TEMPLIST                        
         LA    R8,L'OFFLIST                                                     
SL040    MVC   0(1,R3),2(R4)                                                    
         LA    R3,1(,R3)                                                        
         LA    R4,3(,R4)                                                        
         BCT   R8,SL040                                                         
         B     EXIT                                                             
                                                                                
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
       ++INCLUDE FASYSLST                                                       
*                                                                               
MOLSYSL  DC    X'02'                   ONLY SPOT AND                            
         DC    X'04'                   PRINT VALID FOR MEDIA OFFICES            
         DC    X'00'                                                            
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
         LA    R3,WORKD                                                         
         AHI   R3,DLCB-WORKD                                                    
         USING DLCBD,R3                                                         
*                                                                               
         CLI   SENTCOLS,C'Y'                                                    
         BE    DLLIN02                                                          
*                                                                               
         MVC   DLCBFLD(6),=C'System'                                            
         BRAS  RE,DLOUT                                                         
         MVC   DLCBFLD(11),=C'Office list'                                      
         BRAS  RE,DLOUT                                                         
         MVC   DLCBFLD(11),=C'Description'                                      
         BRAS  RE,DLOUT                                                         
         MVC   DLCBFLD(7),=C'Offices'                                           
         BRAS  RE,DLOUT                                                         
         MVI   SENTCOLS,C'Y'                                                    
         MVI   DLCBACT,DLCBEOL         END OF LINE                              
         GOTO1 VDLFLD,DLCBD                                                     
                                                                                
DLLIN02  MVC   DLCBFLD(L'PRSYS),PRSYS                                           
         BRAS  RE,DLOUT                                                         
*                                                                               
         MVC   DLCBFLD(L'PROFL),PROFL                                           
         BRAS  RE,DLOUT                                                         
*                                                                               
         MVC   DLCBFLD(L'PRDSC),PRDSC                                           
         BRAS  RE,DLOUT                                                         
         MVC   DLCBFLD,SPACES                                                   
*                                                                               
         CLI   PROFF,C' '              ANY OFFICES?                             
         BNH   DLLIN50                 NO                                       
*                                                                               
         LA    R1,PROFF                                                         
         LHI   R0,L'PROFF                                                       
DLLIN20  CLI   0(R1),C'"'              SEARCH FOR DELIMINTER                    
         BNE   *+8                                                              
         MVI   0(R1),C'?'              REPLACE WITH INDECISION                  
         LA    R1,1(,R1)                                                        
         BCT   R0,DLLIN20                                                       
*                                                                               
         MVI   DLCXEOTC,C' '           REMOVE TEXT DELIMITER                    
         MVI   DLCBFLX,C'"'            ADD MY OWN                               
*                                                                               
         MVC   DLCBFLX+1(128),PROFF                                             
         OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
         BRAS  RE,DLOUT                                                         
*                                                                               
         CLI   PROFF+(1*128),C' '      ANY MORE OFFICES                         
         BNH   DLLIN40                 NO                                       
*                                                                               
         MVC   DLCBFLX(128),PROFF+(1*128)                                       
         OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
         BRAS  RE,DLOUT                                                         
*                                                                               
         CLI   PROFF+(2*128),C' '      ANY MORE OFFICES                         
         BNH   DLLIN40                 NO                                       
*                                                                               
         MVC   DLCBFLX(128),PROFF+(2*128)                                       
         OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
         BRAS  RE,DLOUT                                                         
*                                                                               
         CLI   PROFF+(3*128),C' '      ANY MORE OFFICES                         
         BNH   DLLIN40                 NO                                       
*                                                                               
         MVC   DLCBFLX(128),PROFF+(3*128)                                       
         OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
         BRAS  RE,DLOUT                                                         
*                                                                               
DLLIN40  MVI   DLCBFLX,C'"'            END OF TEXT DELIMETER                    
DLLIN50  OI    DLCBFLG1,DLCBFXFL       USE EXTENDED FOR TEXT                    
         BRAS  RE,DLOUT                                                         
*                                                                               
         MVI   DLCXEOTC,C'"'           TEXT DELIMITER                           
         NI    DLCBFLG1,X'FF'-DLCBFXFL USE EXTENDED FOR TEXT                    
         MVC   DLCBFLD,SPACES                                                   
         MVI   DLCBFLX,C' '                                                     
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         MVI   DLCBACT,DLCBEOL         END OF LINE                              
         GOTO1 VDLFLD,DLCBD                                                     
         J     EXIT                                                             
                                                                                
*----------------------------------------------------------------------         
* DOWNLOAD HOOK ROUTINE                                                         
*----------------------------------------------------------------------         
DLOUT    NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBPUT         ACTION IS PUT                            
         MVI   DLCBTYP,DLCBTXT         TYPE IS TEXT                             
         GOTO1 VDLFLD,DLCBD                                                     
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
*----------------------------------------------------------------------         
* DOWNLOAD HOOK ROUTINE                                                         
*----------------------------------------------------------------------         
DLHOOK   NTR1  BASE=*,LABEL=*                                                   
         MVI   REPLINE,1           SUPPRESS PAGING                              
         MVI   REPHEADI,0          SUPPRESS ALL FORCE PAGES                     
         MVI   REPMIDSI,0                                                       
         MVI   REPFOOTI,0                                                       
         GOTO1 VREPORT,REPD                                                     
         J     EXIT                                                             
                                                                                
***********************************************************************         
* SECURITY ACCESS WORK DSECT                                                    
***********************************************************************         
       ++INCLUDE SEACSWRK                                                       
                                                                                
***********************************************************************         
* TWA                                                                           
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSA9D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSA1D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSA0D                                                       
         ORG   SAVOVER                                                          
*----------------------------------------------------------------------         
* SAVED STORAGE                                                                 
*----------------------------------------------------------------------         
SYSTEM   DS    X                       SYSTEM NUMBER                            
SVPAGE   DS    X                       PAGE NUMBER                              
OFFLISTC DS    CL2                     OFFICE LIST CODE                         
OFFLIST  DS    XL201                   OFFICE LIST                              
OFFCNT   DS    X                       OFFICE COUNT                             
*                                                                               
SELVALS  DS    0C                      *SELECTED VALUES*                        
SELSYS   DS    C                       SYSTEM                                   
SELOFFL  DS    X                       OFFICE LIST CODE                         
SELOFF   DS    CL2                     OFFICE CODE                              
SELDOWN  DS    C                       DOWNLOAD                                 
SELVALSQ EQU   *-SELVALS                                                        
*                                                                               
PAGEMAX  EQU   (L'OFFLIST/(PAGEROWS*PAGECOLS))+1                                
PAGEROWS EQU   16                                                               
PAGECOLS EQU   4                                                                
                                                                                
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
LISTSYS  DS    CL7                     SYSTEM                                   
         DS    CL1                                                              
LISTCOD  DS    CL2                     OFFICE LIST CODE                         
         DS    CL4                                                              
LISTDSC  DS    CL14                    DESCRIPTION                              
         DS    CL1                                                              
LISTOFF  DS    CL30                                                             
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
                                                                                
***********************************************************************         
* REPORT LINE DSECT                                                             
***********************************************************************         
REPD     DSECT                                                                  
         ORG   REPP1                                                            
         DS    CL3                                                              
RLSYS    DS    CL7                     SYSTEM                                   
         DS    CL3                                                              
RLOFL    DS    CL2                     OFFICE LIST                              
         DS    CL8                                                              
RLDSC    DS    CL20                    DESCRIPTION                              
         DS    CL1                                                              
RLOFF    DS    CL50                                                             
                                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                                         
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
VOFFICER DS    V                                                                
VDLFLD   DS    V                                                                
DUB      DS    D                                                                
WORK     DS    CL64                                                             
BYTE     DS    X                                                                
FLAG     DS    X                                                                
FOZEROQ  EQU   X'80'                   . OFFICE ZERO                            
FOSKIPQ  EQU   X'40'                   . SKIP OFFICE                            
FOFILTQ  EQU   X'01'                   . OFFICE FILTER FOUND                    
*                                                                               
SHORTNM  DS    CL8                     OFFICE SHORT NAME                        
TEMPLIST DS    XL(L'OFFLIST)                                                    
TEMPLIS2 DS    XL(L'OFFLIST)                                                    
TEMPLIS3 DS    XL(L'OFFLIST)                                                    
OFFBLK   DS    XL(OFCLENQ)                                                      
*                                                                               
SENTCOLS DS    C                                                                
*                                                                               
PRFLDS   DS    0X                                                               
PRSYS    DS    CL7                     SYSTEM                                   
PROFL    DS    CL2                     OFFICE LIST                              
PRDSC    DS    CL20                    DESCRIPTION                              
PROFF    DS    CL512                                                            
PRFLDSQ  EQU   *-PRFLDS                                                         
*                                                                               
LOCALX   EQU   *                                                                
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SEACS29   04/23/20'                                      
         END                                                                    
