*          DATA SET CTESS09    AT LEVEL 071 AS OF 06/18/02                      
*PHASE TA0E09A                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*                                                                               
         TITLE 'CTESS09 - FILE MAINTENANCE - EXTRACT BDE RECORDS'               
ESS09    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ES08**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GBDRD,R2            R2=A(RECORD KEY)                             
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
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF BDE RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING GBDRD,R2            R2=A(BDE RECORD KEY)                         
         XC    GBDRKEY,GBDRKEY                                                  
         MVI   GBDRKREC,GBDRRECQ   BDE ID RECORD TYPE                           
*                                  VALIDATE AGENCY ALPHA ID                     
         MVI   FVMINL,2                                                         
         GOTO1 VALAGY,BDEAGYH                                                   
         BE    VK010                                                            
         CLC   FVMSGNO,=AL2(FVFNONE)                                            
         BNE   VALKEYX                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VK020                                                            
*                                                                               
VK010    MVC   GBDRAGY,FVIFLD                                                   
*                                  VALIDATE BDE ID                              
VK020    MVI   FVMINL,1                                                         
         MVI   FVMAXL,L'GBDEID                                                  
         GOTO1 AFVAL,BDEIDH                                                     
         BNE   VALKEYX                                                          
         MVC   GBDEID,FVIFLD                                                    
*                                  READ RECORD                                  
VKREAD   MVC   APRECKEY(GBDRKEYL),GBDRKEY                                       
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VKR010                                                           
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL BEFORE NO REC                        
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VKR010   LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
VALKEYY  EQU  *                                                                 
*                                  UPDATE SCREEN KEY FIELDS                     
         OI    BDEIDH+(FVOIND-FVIHDR),FVOXMT                                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A REFORM RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   GBDRKEY,APRECKEY                                                 
         CLI   APACTN,ACTADD                                                    
         BNE   VRCHG                                                            
*                                                                               
VRADD    MVC   GBDRKEY,APRECKEY                                                 
         MVC   GBFLEN,=AL2(GBFIRST)                                             
         XC    GBFSTAT(GBFIRST-GBDRKEYL),GBFSTAT                                
VRADDX   B     VRDATA                                                           
*                                                                               
VRCHG    EQU   *                                                                
         USING GBDEEL,R3                                                        
         LA    R3,APELEM                                                        
         MVI   GBDEEL,GBDEELQ                                                   
         MVI   GBDEELL,0                                                        
         GOTO1 ADELELS,GBDRD       DELETE EXISTING REF ELEMENT                  
         DROP  R3                                                               
VRCHGX   B     VRDATA                                                           
         EJECT                                                                  
*                                                                               
VRDATA   EQU   *                                                                
*                                  PROCESS BDE EXTRACT CONTROL ELEMENT          
         USING GBDEEL,R3                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   GBDEEL,GBDEELQ                                                   
         MVI   GBDEELL,GBDEELLQ                                                 
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BDEEIDH                                                    
         BNE   VALKEYX                                                          
         MVC   GBDEEDI,FVIFLD                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BDEPQUH                                                    
         BNE   VALKEYX                                                          
         MVC   UIDNAME,FVIFLD                                                   
         L     RF,=A(GETUIN)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   EIIF                                                             
         MVC   GBDEUIN,UIDNUM                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BDEFILH                                                    
         BNE   VALKEYX                                                          
         MVC   GBDEFIL,FVIFLD                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,BDESUBH                                                    
         BNE   VALKEYX                                                          
         MVC   GBDESUB,FVIFLD                                                   
*                                                                               
VRD100   EQU   *                                                                
         GOTO1 AADDELS,GBDRD       ADD BDE ELEMENT                              
         DROP  R3                                                               
*                                  UPDATE RECORD                                
VRUPD    GOTO1 ASETACT,GBDRD       DEFINE ACTIVITY ELEMENT                      
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   APACTN,ACTADD       DON'T UPDATE GENDIR ON ADD                   
         BE    VALRECOK                                                         
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRECOK MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF REFORM RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         XC    BDEAGY,BDEAGY                                                    
         OC    GBDRAGY,GBDRAGY                                                  
         BZ    *+10                                                             
         MVC   BDEAGY,GBDRAGY                                                   
         MVC   BDEID,GBDEID                                                     
         OI    BDEIDH+(FVOIND-FVIHDR),FVOXMT                                    
         SPACE 1                                                                
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY REFORM RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC BDEEIDH                                                          
*                                                                               
         LA    R3,APELEM                                                        
         USING GBDEEL,R3                                                        
         MVI   GBDEEL,GBDEELQ                                                   
         MVI   GBDEELL,0                                                        
         GOTO1 AGETELS,GBDRD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    DREC100                                                          
         MVC   BDEEID,GBDEEDI                                                   
         MVC   BDEFIL,GBDEFIL                                                   
         MVC   BDESUB,GBDESUB                                                   
         MVC   UIDNUM,GBDEUIN                                                   
         L     RF,=A(GETUID)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   DISRECX                                                          
         MVC   BDEPQU,UIDNAME                                                   
         DROP  R3                                                               
*                                                                               
DREC100  EQU   *                                                                
         GOTO1 ADISACT,GBDRD       DISPLAY ACTIVITY DATE                        
*                                                                               
DISRECX  EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN REFORM RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
         OI    GBDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GBDRD                                                    
         OI    GBFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED REFORM RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GBDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GBDRD                                                    
         NI    GBFSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         XC    GBDRKEY,GBDRKEY                                                  
         MVI   GBDRKMAJ,X'FF'      FLAG FOR FIRST PASS                          
         MVI   GBDRKREC,GBDRRECQ                                                
         XC    SELKEY,SELKEY                                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VSELAGY  EQU   *                                                                
         GOTO1 AFVAL,LSTAGYH                                                    
         BNE   VSAGYX                                                           
         MVC   SELAGY,FVIFLD                                                    
         MVC   GBDRAGY,SELAGY                                                   
VSAGYX   EQU   *                                                                
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
         MVC   GBDRKEY,APRECKEY                                                 
         CLI   GBDRKMAJ,X'FF'      TEST FIRST TIME FLAG                         
         BNE   GETSEL2                                                          
         MVI   GBDRKMAJ,0                                                       
         B     GETSEL6             READ HIGH                                    
GETSEL2  TM    APINDS,2            TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,1            TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOGENDIR+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         LA    R2,IOKEY                                                         
         CLI   GBDRKREC,GBDRRECQ                                                
         BNE   GETSELN                                                          
         SPACE 1                                                                
*                                                                               
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
*                                                                               
GSAGY    EQU   *                                                                
         OC    SELAGY,SELAGY                                                    
         BZ    GSAGYX                                                           
         CLC   SELAGY,GBDRAGY                                                   
         BNE   GETSEL8                                                          
GSAGYX   EQU   *                                                                
*                                                                               
GSELEM   EQU   *                   GET ELEMENT DATA                             
         LA    R3,GBFIRST(R2)                                                   
         SR    RF,RF                                                            
         USING GBDEEL,R3                                                        
GSEM010  CLI   GBDEEL,0                                                         
         BE    GSEM030                                                          
         CLI   GBDEEL,GBDEELQ                                                   
         BE    GSEM030                                                          
GSEM020  IC    RF,GBDEELL                                                       
         AR    R3,RF                                                            
         B     GSEM010                                                          
*                                                                               
GSEM030  EQU   *                                                                
         DROP  R3                                                               
GSEMX    EQU   *                                                                
*                                                                               
GETSELY  MVC   APRECKEY(L'GBDRKEY),GBDRKEY                                      
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
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                  GET DATA FROM RECORD KEY                     
         OC    GBDRAGY,GBDRAGY                                                  
         BZ    *+10                                                             
         MVC   LISTAGY,GBDRAGY                                                  
         MVC   LISTBDE,GBDEID                                                   
*                                  GET DATA FROM ELEMENTS                       
         LA    R3,GBFIRST(R2)                                                   
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),GBDEELQ                                                    
         BE    DSBDEEL                                                          
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                                                               
         USING GBDEEL,R3                                                        
DSBDEEL  EQU   *                                                                
         B     DSLP1A                                                           
         DROP  R3                                                               
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
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AGENCY ALPHA ID                                 *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
VALAGY   NTR1                                                                   
         MVI   FVMAXL,L'CT5KALPH                                                
         GOTO1 AFVAL                                                            
         BNE   VAGYNO                                                           
         MVC   IOKEYSV(L'IOKEY),IOKEY                                           
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY       BUILD KEY                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNL   *+14                TEST AIO RETURN CONDITION                    
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     VAGYNO                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VAGYNO                                                           
*                                                                               
         B     VAGYOK                                                           
*                                                                               
VAGYNO   B     NO                                                               
VAGYOK   EQU   *                                                                
         MVC   IOKEY(L'IOKEY),IOKEYSV                                           
         B     YES                                                              
         DROP  R2                                                               
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
*                                  ERROR EXITS                                  
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
ESYS     MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     NO                  SYSTEM NAME ERROR                            
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE EXCEEDS MAXIMUM                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RB,RA                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO READ USER ID NAME RECORD INTO IOARE3 AND GET INFO        *         
*                                                                     *         
* ENTRY - UIDNAME= USER ID NAME                                       *         
* EXIT  - UIDNUM = USER ID NUMBER                                     *         
***********************************************************************         
         SPACE 1                                                                
GETUIN   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETUIN,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GAC'    INSERT NAME                                  
*                                                                               
         MVC   KEYSAVE,IOKEY                                                    
         L     R3,AIOAREA3                                                      
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,UIDNAME                                                   
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         DROP  R3                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO3                                          
         MVC   IOKEY,KEYSAVE                                                    
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GUINNO                                                           
         L     R3,AIOAREA3                                                      
         LA    R3,CTIDATA-CTIREC(R3)                                            
*                                  SAVE AGENCY COUNTRY CODE                     
GUIN010  CLI   0(R3),0                                                          
         BE    GUINOK                                                           
         CLI   0(R3),X'02'                                                      
         BE    GUIN020                                                          
GUIN012  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GUIN010                                                          
*                                                                               
GUIN020  CLI   1(R3),4                                                          
         BNE   GUINNO                                                           
         MVC   UIDNUM,2(R3)                                                     
         B     GUINOK                                                           
*                                                                               
GUINOK   SR    RC,RC               RETURN CC EQUAL                              
GUINNO   LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO READ USER ID NUMBER RECORD INTO IOARE3 AND GET INFO      *         
*                                                                     *         
* ENTRY - UIDNUM = USER ID NUMBER                                     *         
* EXIT  - UIDNAME= USER ID NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
GETUID   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETUID,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GAC'    INSERT NAME                                  
*                                                                               
         MVC   KEYSAVE,IOKEY                                                    
         L     R3,AIOAREA3                                                      
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID+8(2),UIDNUM                                               
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         DROP  R3                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO3                                          
         MVC   IOKEY,KEYSAVE                                                    
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GUIDNO                                                           
         L     R3,AIOAREA3                                                      
         LA    R3,CTIDATA-CTIREC(R3)                                            
*                                  SAVE AGENCY COUNTRY CODE                     
GUID010  CLI   0(R3),0                                                          
         BE    GUIDOK                                                           
         CLI   0(R3),X'02'                                                      
         BE    GUID020                                                          
GUID012  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GUID010                                                          
*                                                                               
GUID020  CLI   1(R3),12                                                         
         BNE   GUIDNO                                                           
         MVC   UIDNAME,2(R3)                                                    
         B     GUIDOK                                                           
*                                                                               
GUIDOK   SR    RC,RC               RETURN CC EQUAL                              
GUIDNO   LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* CTESSWRK                                                                      
       ++INCLUDE CTESSWRK                                                       
         SPACE 1                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSF6D                                                       
         ORG   ESSTABH                                                          
       ++INCLUDE CTESSD6D                                                       
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT DISPLAYED SYSTEM                
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTAGY  DS    CL2                                                              
         DS    CL1                                                              
LISTBDE  DS    CL8                                                              
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
FULL     DS    F                                                                
WORK     DS    XL64                                                             
IOKEYSV  DS    XL(L'IOKEY)                                                      
ASE      DS    A                                                                
APGM     DS    A                                                                
*                                                                               
UIDNAME  DS    CL10                                                             
UIDNUM   DS    XL2                                                              
LASTSYS  DS    XL1                 CONTROL TOF ON CHANGE OF SYSTEM              
SYSTEM   DS    CL1                 SYSTEM SE NUMBER SAVE                        
SYCNT    DS    CL1                                                              
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
*                                                                               
SELKEY   DS    0XL32                                                            
SELAGY   DS    XL2                                                              
         ORG   SELKEY+L'SELKEY                                                  
KEYSAVE  DS    CL(L'IOKEY)                                                      
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071CTESS09   06/18/02'                                      
         END                                                                    
