*          DATA SET CTGEN05    AT LEVEL 007 AS OF 02/01/18                      
*PHASE TA0B05B                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE NUMVAL                                                                 
*INCLUDE DLFLD                                                                  
         TITLE 'CTGEN05 - USER ID SYSTEMS MAINTENANCE'                          
GEN05    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN5**,RA,R8,RR=RE                                           
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
         LLC   RF,APMODE                                                        
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
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF USER ID RECORD                           *         
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         GOTO1 AFVAL,IDSIDH        VALIDATE USER ID                             
         BNE   VALKEYX                                                          
         CLI   FVIFLD,X'80'        1ST CHR CAN'T BE A SPECIAL CHR               
         BL    EIIF                                                             
*&&US*&& CLC   FVIFLD(2),=C'OX'    OX... USER IDS NOT ALLOWED                   
*&&US*&& BE    EIIF                                                             
         TM    FVIIND,FVINUM       CHECK FOR NUMERIC INPUT                      
         BO    VKUSR10                                                          
         CLI   FVILEN,3            USER ID TOO SHORT                            
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
         JE    *+2                                                              
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VKUSR20                                                          
         MVC   IDSID,2(R3)                                                      
         MVI   IDSIDH+5,8                                                       
         NI    IDSIDH+4,X'F7'                                                   
         OI    IDSIDH+6,X'80'                                                   
         B     VALKEY                                                           
VKUSRX   EQU   *                                                                
*                                                                               
VKSYS    MVI   SYSTEM,0            SET SYSTEM NOT INPUT                         
         GOTO1 AFVAL,IDSSYSH                                                    
         BNE   VKSYS12                                                          
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         LA    RE,SYSLLEN(RE)      GO PAST SERVICE SYSTEM ENTRY                 
         LLC   RF,FVXLEN                                                        
VKSYS10  CLI   SYSLNUM,0                                                        
         BE    EIIF                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VKSYS20                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VKSYS10                                                          
*                                  DEFAULT TO LIST SYSTEM FILTER OR             
VKSYS12  MVC   SYSTEM,SELSYS       FIRST VALID SYSTEM IN RECORD                 
         CLI   SYSTEM,0                                                         
         BE    VKSYSX                                                           
         GOTO1 ADISSYS,SYSTEM      GET SYSTEM LIST ENTRY FROM NUMBER            
         ICM   RE,15,APPARM                                                     
         BZ    VKSYSX                                                           
*                                                                               
VKSYS20  MVC   SYSTEM,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         MVC   SFLAGS,SYSLIND1     AND SET INPUT FLAGS                          
         CLI   SYSLRPLT,C'C'       NO AGENCY BINARY FOR CONTROL SYSTEM          
         BNE   *+8                                                              
         MVI   SFLAGS,0                                                         
         MVC   IDSSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    IDSSYSH+6,X'80'                                                  
VKSYSX   EQU   *                                                                
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THIS/LAST ACTIONS                                          *         
***********************************************************************         
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
         OC    SAVKEY,SAVKEY                                                    
         BZ    VALKEYY1                                                         
         MVC   IOKEY,SAVKEY                                                     
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO1                                    
         BL    VALKEYX             I/O ERROR EXIT                               
         BE    VALKEYY1            COPY RECORD REREAD OK                        
         MVC   FVADDR,AINKHDR                                                   
         TM    IOERR,IOEDEL        RECORD IS DELETED/NOT FOUND                  
         BZ    *+20                                                             
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         MVC   SAVKEY,APRECKEY                                                  
         B     VALKEYX                                                          
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         MVC   SAVKEY,APRECKEY                                                  
         B     VALKEYX                                                          
VALKEYY1 MVC   SAVKEY,APRECKEY                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE USER ID RECORDS                            *         
***********************************************************************         
VALREC   L     R2,AIOAREA1         INITIALISE RECORD                            
         MVC   CTIKEY,APRECKEY                                                  
         XC    IDOPTS,IDOPTS                                                    
         XC    PQGID,PQGID                                                      
         XC    SYNNUM,SYNNUM                                                    
         XC    SYNID,SYNID                                                      
         XC    LASTIDOS,LASTIDOS                                                
         XC    LASTSYN,LASTSYN                                                  
         XC    LASTSID,LASTSID                                                  
         CLI   APACTN,ACTADD                                                    
         BE    *+14                                                             
         CLC   SYSTEM,SAVSYS                                                    
         BNE   DISREC                                                           
*                                  CHECK FOR ADD FUNCTION                       
VRADD    CLI   APACTN,ACTADD                                                    
         BNE   VRCPY                                                            
         XC    CTIREC(256),CTIREC  INITIALISE RECORD FOR ADD                    
         MVC   CTIKEY,APRECKEY                                                  
         LA    R0,CTIDATA+1-CTIREC                                              
         STCM  R0,3,CTILEN                                                      
         B     VRPID                                                            
*                                  CHECK FOR COPY FUNCTION                      
VRCPY    CLI   APACTN,ACTCPY                                                    
         BNE   VRCHA                                                            
         NI    TWAMODE,X'FF'-TWAMDFR                                            
         NI    CTISTAT,X'80'       CLEAR LAST USED MONTH                        
         LA    R3,CTIDATA          UPDATE RECORD FOR COPY                       
VRCPY10  CLI   0(R3),0                                                          
         BE    VRCPYX              EXIT END RECORD                              
         CLI   0(R3),X'01'                                                      
         BE    VRCPY30             DELETE ACTIVITY                              
         CLI   0(R3),X'02'                                                      
         BE    VRCPY30             DELETE PASSIVE POINTER                       
         CLI   0(R3),X'06'                                                      
         BE    VRCPY40             MERGE ACCESS SYSTEM DATA                     
         CLI   0(R3),X'07'                                                      
         BE    VRCPY50             PROCESS ID OPTIONS ELEMENT                   
VRCPY20  LLC   RF,1(R3)                                                         
         AR    R3,RF               GET NEXT ELEMENT                             
         B     VRCPY10                                                          
VRCPY30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CTIREC),0,0                    
         CLI   APPARM+12,0                                                      
         BE    VRCPY10                                                          
         DC    H'0'                                                             
VRCPY40  MVC   AGAID,CTAGYID-CTAGYD(R3)                                         
*                                  GET ACCESS RECORD INTO IOAREA3               
         L     RF,=A(GETACC)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   ERNF                                                             
         BAS   RE,MRGSYS           MERGE AGENCY ACCESS SYSTEM DATA              
         B     VRCPY20                                                          
*                                  PROCESS ID OPTIONS ELEMENT                   
         USING CTIDOD,R3                                                        
VRCPY50  CLI   CTIDOLEN,CTIDOLNQ                                                
         BNE   VRCPY20             CHECK NEW STYLE IDO ELEMENT                  
         TM    CTIDOFL1,CTIDOFSN                                                
         BNZ   EIIF                CAN NOT COPY SYNONYM USER ID                 
         NI    CTIDOFL1,X'FF'-CTIDOFSP                                          
         XC    CTIDOSNU,CTIDOSNU   CLEAR ANY SYNONYM PASSIVE POINTER            
         XC    CTIDOSID,CTIDOSID                                                
         B     VRCPY20                                                          
         DROP  R3                                                               
*                                  CHANGE FUNCTION - SAVE ORIG STATUS           
VRCPYX   B     VRUPD                                                            
*                                  CHANGE FUNCTION - SAVE ORIG STATUS           
VRCHA    LA    R3,CTIDATA          AND STRIP DOWN RECORD                        
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTIDOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    VRCHA08                                                          
         USING CTIDOEL,R3                                                       
         CLI   CTIDOLEN,CTIDOLNQ                                                
         BNE   VRCHA04                                                          
         TM    CTIDOFL1,CTIDOFSP                                                
         BZ    VRCHA04                                                          
         DROP  R2                                                               
         L     R4,AIOAREA2         CHECK SYNONYM ID RECORD                      
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,CTIDOSID                                                  
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BE    VRCHA02                                                          
*                                  REMOVE REFERENCE IF DELETED                  
         NI    CTIDOFL1,X'FF'-CTIDOFSP                                          
         XC    CTIDOSNU,CTIDOSNU                                                
         XC    CTIDOSID,CTIDOSID                                                
*                                                                               
VRCHA02  MVC   IOKEY,APRECKEY                                                   
         DROP  R4                                                               
         USING CTIREC,R2                                                        
*                                                                               
VRCHA04  MVC   IDOPTS(1),CTIDOFL1                                               
         MVC   IDOPTS+1(1),CTIDOFL2                                             
         MVC   LASTIDOS,IDOPTS                                                  
         CLI   CTIDOLEN,CTIDOLNQ                                                
         BNE   VRCHA08                                                          
         TM    IDOPTS,CTIDOFPI                                                  
         BZ    *+10                                                             
         MVC   PQGID,CTIDOPQI                                                   
         TM    IDOPTS,CTIDOFSN+CTIDOFSP                                         
         BZ    VRCHA08                                                          
         MVC   SYNNUM,CTIDOSNU                                                  
         MVC   SYNID,CTIDOSID                                                   
         MVC   LASTSYN,SYNNUM                                                   
         MVC   LASTSID,SYNID                                                    
         DROP  R3                                                               
*                                                                               
VRCHA08  LA    R3,CTIDATA                                                       
VRCHA10  CLI   0(R3),0                                                          
         BE    VRCHAX                                                           
         CLI   0(R3),X'02'         PASSIVE POINTER                              
         BNE   VRCHA20                                                          
         MVC   IDNUM,2(R3)         SAVE ID NUMBER PASSIVE                       
         B     VRCHA30                                                          
VRCHA20  CLI   0(R3),X'01'         ACTIVITY                                     
         BE    VRCHA30             DELETE ELEMENT                               
         CLI   0(R3),X'03'         PRINCIPLE ID                                 
         BE    VRCHA30                                                          
         CLI   0(R3),X'06'         AGENCY ID                                    
         BE    VRCHA30                                                          
         CLI   0(R3),X'07'         ID OPTIONS                                   
         BE    VRCHA30                                                          
         CLI   0(R3),CTTOUELQ      TIME OUT ELEMENT                             
         BE    VRCHA30                                                          
*&&UK                                                                           
         CLI   0(R3),CTLNIELQ      LOTUS NOTES ADMIN USER ID ELEMENT            
         BE    VRCHA30                                                          
*&&                                                                             
         CLI   0(R3),CTAFPELQ      AFP USER ID ELEMENT                          
         BE    VRCHA30                                                          
         CLI   0(R3),X'20'         COMPATIBLE ID                                
         BE    VRCHA30                                                          
         CLI   0(R3),X'23'         PROGRAM EXCEPTION                            
         BE    VRCHA40                                                          
         B     VRCHA50                                                          
*                                                                               
VRCHA30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),CTIREC),0,0                    
         CLI   APPARM+12,0                                                      
         JNE   *+2                                                              
         B     VRCHA10                                                          
*                                                                               
VRCHA40  GOTO1 VHEXOUT,APPARM,SYSTEM,APWORK,1,=C'TOG'                           
         MVI   APWORK,C'T'                                                      
         SR    R0,R0                                                            
         ICM   R0,1,0(R3)                                                       
         GOTO1 ,APPARM,(C'D',CTFILE),((R0),CTIREC),(2,APWORK)                   
         CLI   SYSTEM,0                                                         
         BNE   *+10                                                             
         XC    APPARM+8(4),APPARM+8                                             
         GOTO1 VHELLO,APPARM       DELETE PROGRAM EXCEPTION ELEMENTS            
         CLI   APPARM+12,0                                                      
         JNE   *+2                                                              
*                                                                               
VRCHA50  LLC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     VRCHA10                                                          
*                                                                               
VRCHAX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRINCIPAL ID AND SET FLAG IN IDOPTS SAVED FOR CTIDOEL      *         
***********************************************************************         
VRPID    MVI   APFLAG,0            SET FLAGS FOR WHAT WAS INPUT                 
         NI    IDOPTS,X'FF'-CTIDOFGN-CTIDOFSN                                   
         GOTO1 AFVAL,IDSPIDH                                                    
         BNE   VRPIDX              PRINCIPAL ID NOT INPUT                       
*                                                                               
         CLI   FVIFLD,C'='         TEST FOR =SYNONYM USER ID                    
         BE    VRPID10                                                          
         CLI   FVILEN,3                                                         
         BL    EFTS                                                             
         CLI   FVILEN,7                                                         
         BH    EFTL                                                             
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8              TEST IF GENERIC USER-ID                      
         B     *+10                                                             
         CLC   FVIFLD(0),=C'GENERIC'                                            
         BNE   VRPID20                                                          
*                                  YES - SET THIS IS A GENERIC                  
         OI    IDOPTS,CTIDOFGN           USERID OR PRINTERQ LOGIC               
         B     VRPIDX                                                           
*                                                                               
VRPID10  TM    IDOPTS,CTIDOFSP     =SYNONYM USER ID                             
         BNZ   EIIF                                                             
         CLI   FVILEN,4                                                         
         BL    EFTS                                                             
         CLI   FVILEN,9                                                         
         BH    EFTL                                                             
         L     R3,AIOAREA2         READ PRINCIPLE ID RECORD                     
         DROP  R2                                                               
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,SPACES                                                    
         MVC   CTIKID(L'IDSPID-1),FVIFLD+1                                      
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    EIRT                                                             
         MVC   SYNNUM,2(R1)                                                     
         MVC   SYNID,CTIKID                                                     
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTIDOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    VRPID12                                                          
         USING CTIDOD,R1                                                        
         TM    CTIDOFL1,CTIDOFSN   CANT BE ANOTHER SYNONYM ID                   
         BNZ   EIIF                                                             
         DROP  R1,R3                                                            
*                                  SET IDOPTS TO INDICATE THAT A                
VRPID12  OI    IDOPTS,CTIDOFSN           USERID SYNONYM ENETERED                
         B     VRPIDX                                                           
         USING CTIREC,R2           R2=A(RECORD KEY)                             
*                                                                               
VRPID20  L     R3,AIOAREA2         READ PRINCIPLE ID RECORD                     
         DROP  R2                                                               
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FVIFLD                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    EIRT                                                             
         DROP  R3                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'03'        BUILD PRINCIPAL ID ELEMENT                   
         MVI   APELEM+1,X'04'                                                   
         MVC   APELEM+2(2),2(R1)                                                
         USING CTIREC,R2           R2=A(RECORD KEY)                             
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
VRPIDX   EQU   *                                                                
                                                                                
***********************************************************************         
* VALIDATE ACCESS REQUIRED                                            *         
* USER ID CONNECT PASSWORD AND PQ ACCESS PASSWORD                     *         
* AND SET FLAG IN IDOPTS SAVED FOR CTIDOEL                            *         
***********************************************************************         
VRACC    OI    IDOPTS,CTIDOFPW         SET CONNECT PWD REQUIRED                 
         NI    IDOPTS,X'FF'-CTIDOFPP   RESET PQ ACCESS PWD NOT REQUIRED         
         GOTO1 AFVAL,IDSACCRH                                                   
         BNE   VRACCX                                                           
*&&DO                                                                           
         NI    IDOPTS,X'FF'-CTIDOFPW-CTIDOFPP                                   
         GOTO1 AFVAL,IDSACCRH                                                   
         BNE   VRACCX                                                           
         CLI   FVIFLD,C'N'                                                      
         BE    VRAC010                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   EIIF                                                             
         OI    IDOPTS,CTIDOFPW     SET CONNECT PASSWORD REQUIRED                
*&&                                                                             
VRAC010  CLI   FVIFLD+1,C'N'                                                    
         BE    VRACCX                                                           
         CLI   FVIFLD+1,C'Y'                                                    
         BNE   EIIF                                                             
*NOP*    TM    IDOPTS,CTIDOFPW     MUST HAVE CONNECT PASSWORD REQUIRED          
*NOP*    BZ    EIIF                                                             
         OI    IDOPTS,CTIDOFPP     SET PQ ACCESS PASSWORD REQUIRED              
VRACCX   EQU   *                                                                
                                                                                
***********************************************************************         
* VALIDATE COMPATIBLE ID WILD CARD FILTER ALLOWED                     *         
* AND SET FLAG IN IDOPTS SAVED FOR CTIDOEL                            *         
***********************************************************************         
VRCWC    NI    IDOPTS,X'FF'-CTIDOFWC                                            
         GOTO1 AFVAL,IDSCWCH                                                    
         BNE   VRCWCX                                                           
         CLI   FVIFLD,C'N'                                                      
         BE    VRCWCX                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EIIF                                                             
         OI    IDOPTS,CTIDOFWC     SET WILD CARD FILTERING ALLOWED              
VRCWCX   EQU   *                                                                
                                                                                
***********************************************************************         
* VALIDATE PRIVILEGED PQ USER                                         *         
* AND SET FLAG IN IDOPTS SAVED FOR CTIDOEL                            *         
***********************************************************************         
VRPPU    NI    IDOPTS,X'FF'-CTIDOPPU                                            
         GOTO1 AFVAL,IDSPPUH                                                    
         BNE   VRPPUX                                                           
         CLI   FVIFLD,C'N'                                                      
         BE    VRPPUX                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EIIF                                                             
         OI    IDOPTS,CTIDOPPU     SET PRIVILEGED PQ USER                       
VRPPUX   EQU   *                                                                
                                                                                
***********************************************************************         
* VALIDATE MASTER PQ GROUP USER ID                                    *         
* AND SET ID NUMBER IN PQGID SAVED FOR CTIDOEL                        *         
***********************************************************************         
VRPQI    NI    IDOPTS,X'FF'-CTIDOFPI                                            
         GOTO1 AFVAL,IDSPQIH                                                    
         BNE   VRPQIX                                                           
*                                                                               
         CLI   FVILEN,3                                                         
         BL    EFTS                                                             
         CLI   FVILEN,7                                                         
         BH    EFTL                                                             
*                                                                               
VRPQI10  L     R3,AIOAREA2         READ MASTER PQ GROUP ID RECORD               
         DROP  R2                                                               
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FVIFLD                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    EIRT                                                             
         DROP  R3                                                               
         USING CTIREC,R2           R2=A(RECORD KEY)                             
         MVC   PQGID,2(R1)                                                      
*                                  SET FLAG IN IDOPTS TO INDICATE               
         OI    IDOPTS,CTIDOFPI           MASTER PQ GROUP ID ENTERED             
         B     VRPQIX                                                           
*                                                                               
VRPQIX   EQU   *                                                                
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VALIDATE MASTER/PARTNER FLAG                                        *         
* AND SET FLAG IN IDOPTS SAVED FOR CTIDOEL                            *         
***********************************************************************         
VRMPF    NI    IDOPTS+1,X'FF'-(CTIDOFMA+CTIDOFPA)                               
         GOTO1 AFVAL,IDSMPFH                                                    
         BNE   VRMPFX                                                           
         CLI   FVIFLD,C'M'                                                      
         BE    VRMP010                                                          
         CLI   FVIFLD,C'P'                                                      
         BE    VRMP020                                                          
         CLI   FVIFLD,C'B'                                                      
         BE    VRMP030                                                          
         B     EIIF                                                             
VRMP010  OI    IDOPTS+1,CTIDOFMA   SET MASTER FLAG                              
         B     VRMPFX                                                           
VRMP020  OI    IDOPTS+1,CTIDOFPA   SET PARTNER FLAG                             
         B     VRMPFX                                                           
VRMP030  OI    IDOPTS+1,CTIDOFMA+CTIDOFPA  SET BOTH                             
         B     VRMPFX                                                           
VRMPFX   EQU   *                                                                
*&&                                                                             
                                                                                
***********************************************************************         
* ADD USERID OPTIONS ELEMENT - CTIDOEL                                *         
***********************************************************************         
VROPT    XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDOEL,R3                                                       
         MVI   CTIDOEL,CTIDOELQ                                                 
         MVI   CTIDOLEN,CTIDOLNQ                                                
         MVC   CTIDOFL1,IDOPTS                                                  
         MVC   CTIDOFL2,IDOPTS+1                                                
         MVC   CTIDOPQI,PQGID                                                   
         MVC   CTIDOSNU,SYNNUM                                                  
         MVC   CTIDOSID,SYNID                                                   
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
VROPTX   TM    IDOPTS,CTIDOFSN                                                  
         BNZ   VRUPD               SYNONYM ID NEEDS NOTHING ELSE                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD AGENCY ID ELEMENT                                *         
***********************************************************************         
VRAID    GOTO1 AFVAL,IDSAGYAH                                                   
         BNE   EMIF                                                             
         CLC   FVIFLD(2),=C'??'                                                 
         BE    EIIF                                                             
         CLI   FVILEN,2                                                         
         BL    EFTS                                                             
*                                                                               
         XC    SYSEL,SYSEL                                                      
         MVC   AGAID,FVIFLD                                                     
         MVC   SECAID,FVIFLD                                                    
*                                  GET ACCESS RECORD INTO IOAREA3               
         L     RF,=A(GETACC)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   ERNF                                                             
         BAS   RE,MRGSYS           MERGE ACCESS SYSTEM DATA                     
*                                  GET ACCESS RECORD INTO IOAREA3               
         L     R1,AIOAREA3         LOCATE SYSEL AND SECAGY ON ACCS REC          
         LA    R1,CT5DATA-CT5REC(R1)                                            
VRAID10  CLI   0(R1),0             TEST E-O-R                                   
         BE    VRAID60                                                          
         CLI   0(R1),CTSYSELQ      X'21' SYSTEM ELEMENT                         
         BE    VRAID20                                                          
         CLI   0(R1),CTAGDELQ      X'B4' AGENCY GROUP DETAILS ELEMENT           
         BE    VRAID40                                                          
         CLI   0(R1),CTSEAELQ      X'B8' SECAGY ELEMENT                         
         BE    VRAID50                                                          
VRAID12  LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     VRAID10                                                          
*                                                                               
         USING CTSYSD,R1                                                        
VRAID20  CLI   SYSTEM,0                                                         
         BE    VRAID12                                                          
         CLC   SYSTEM,CTSYSNUM                                                  
         BNE   VRAID12                                                          
         MVC   SYSEL,0(R1)                                                      
         B     VRAID12                                                          
*                                                                               
         USING CTAGDD,R1                                                        
VRAID40  LA    RE,IDSID+L'IDSID-1  POINT TO POSSIBLE UAT TEXT                   
         LHI   RF,L'IDSID-2        NEED AT LEAST THREE FOR UAT TEXT             
VRAID42  CLI   0(RE),C' '          FIND LAST LETTER OF ID                       
         BH    VRAID44                                                          
         AHI   RE,-1               BACKUP ONE CHARACTER                         
         BCT   RF,VRAID42                                                       
         B     VRAID46                                                          
VRAID44  AHI   RE,-2               LOOK AT THE LAST THREE LETTERS               
         CLC   =C'UAT',0(RE)       DOES USER ID END IN UAT?                     
         BNE   VRAID46             NO: SEE IF ITS NEEDED                        
         TM    CTAGOPTS,CTAGUAT    YES: MUST BE UAT AGENCY                      
         BZ    EUAT                ITS NOT, THEN NO GOOD                        
         B     VRAID12             IT IS, THEN WE ARE GOOD                      
VRAID46  TM    CTAGOPTS,CTAGUAT    UAT AGENCY, MUST END IN "UAT"                
         BO    EUAM                IT DOESNT, THEN NO GOOD                      
         B     VRAID12             IT DOES, THEN WE ARE GOOD                    
*                                                                               
         USING CTSEAD,R1                                                        
VRAID50  MVC   SECAID,CTSEAAID                                                  
         B     VRAID12                                                          
         DROP  R1                                                               
*                                                                               
VRAID60  CLI   SYSTEM,0                                                         
         BE    VRAID80                                                          
         CLI   SYSEL,0                                                          
         BNE   VRAID80                                                          
         LA    R1,IDSSYSH          SYSTEM IS INVALID                            
         ST    R1,FVADDR                                                        
         B     EIIF                                                             
*                                                                               
VRAID80  XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTAGYD,R3                                                        
         MVI   CTAGYEL,CTAGYELQ                                                 
         MVI   CTAGYLEN,CTAGYLNQ                                                
         MVC   CTAGYID,FVIFLD                                                   
*                                                                               
         GOTO1 AVALLNG,IDSLANGH    VALIDATE LANGUAGE                            
         BNE   EXIT                                                             
         MVC   CTAGYLNG,APWORK     SET LANGUAGE CODE                            
*                                                                               
VRIDT    GOTO1 AFVAL,IDSIDTH       VALIDATE ID TYPE CODE                        
         BNE   VRIDTX                                                           
         CLI   FVILEN,1                                                         
         BL    EFTS                                                             
         CLI   FVILEN,L'TYPTAB-1                                                
         BH    EFTL                                                             
*                                                                               
VRIDT10  LARL  RE,TYPTAB                                                        
VRIDT20  CLI   0(RE),0                                                          
         BE    EIIF                                                             
         LLC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),1(RE)                                                  
         BE    VRIDT40                                                          
         LA    RE,L'TYPTAB(RE)                                                  
         B     VRIDT20                                                          
*                                                                               
VRIDT40  MVC   IDSIDT(L'TYPTAB-1),1(RE)                                         
         OI    IDSIDTH+6,X'80'                                                  
         MVC   CTAGYIDT,0(RE)      SET ID TYPE CODE IN AGENCY ELEMENT           
*                                                                               
VRIDTX   GOTO1 AADDELN,CTIREC      ADD AGENCY ALPHA ELEMENT                     
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
VRAIDX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD TIME OUT ELEMENT                                 *         
***********************************************************************         
VRTOU    GOTO1 AFVAL,IDSATOUH                                                   
         BNE   VRTOUX                                                           
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTTOUD,R3                                                        
         MVI   CTTOUEL,CTTOUELQ                                                 
         MVI   CTTOULEN,CTTOULNQ                                                
         GOTO1 =V(NUMVAL),APPARM,IDSATOU,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    EATO                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    EATO                                                             
         C     R1,=F'1275'                                                      
         BH    EATO                                                             
         STCM  R1,3,CTTOUADV                                                    
         GOTO1 AADDELN,CTIREC      ADD TIME OUT ELEMENT                         
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
VRTOUX   EQU   *                                                                
         DROP  R3                                                               
*&&UK                                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE LOTUS NOTES ADMIN USER ID ELEMENT                          *         
***********************************************************************         
VRLNI    GOTO1 AFVAL,IDSLNIH                                                    
         BNE   VRLNIX                                                           
         CLI   FVILEN,3                                                         
         BL    EFTS                                                             
         CLI   FVILEN,7                                                         
         BH    EFTL                                                             
*                                                                               
VRLNI10  L     R3,AIOAREA2         READ LOTUS NOTES USER ID RECORD              
         DROP  R2                                                               
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FVIFLD                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    EIRT                                                             
         DROP  R3                                                               
         USING CTIREC,R2           R2=A(RECORD KEY)                             
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTLNID,R3                                                        
         MVI   CTLNIEL,CTLNIELQ                                                 
         MVI   CTLNILEN,CTLNILNQ                                                
         MVC   CTLNINUM,2(R1)                                                   
*                                  SET FLAG IN IDOPTS TO INDICATE               
         GOTO1 AADDELN,CTIREC      ADD ELEMENT                                  
         BNE   VALRECER            RECORD TOO BIG                               
         B     VRLNIX                                                           
*                                                                               
VRLNIX   EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE AFP USER ID ELEMENT                                        *         
***********************************************************************         
VRAFP    GOTO1 AFVAL,IDSAFPH                                                    
         BNE   VRAFPX                                                           
         CLI   FVILEN,3                                                         
         BL    EFTS                                                             
         CLI   FVILEN,10                                                        
         BH    EFTL                                                             
*                                                                               
VRAFP10  L     R3,AIOAREA2         READ AFP USER ID RECORD                      
         DROP  R2                                                               
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FVIFLD                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    EIRT                                                             
         DROP  R3                                                               
         USING CTIREC,R2           R2=A(RECORD KEY)                             
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTAFPD,R3                                                        
         MVI   CTAFPEL,CTAFPELQ                                                 
         MVI   CTAFPLEN,CTAFPLNQ                                                
         MVC   CTAFPNUM,2(R1)                                                   
*                                                                               
         GOTO1 AADDELN,CTIREC      ADD ELEMENT                                  
         BNE   VALRECER            RECORD TOO BIG                               
         B     VRAFPX                                                           
*                                                                               
VRAFPX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD COMPATIBLE ID ELEMENTS                           *         
***********************************************************************         
VRCID    LA    R9,IDSID1H                                                       
         LA    RF,3                                                             
         STC   RF,FCOUNT                                                        
         MVI   LIDFNDX,0           CLEAR DUPLIVATE ID INDEX SAVE AND            
         MVC   AIDLIST,ATIA        SAVE ADDRESS START OF ID SAVE LIST           
*                                                                               
VRCID10  TM    1(R9),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         LLC   R1,0(R9)                                                         
         AR    R9,R1                                                            
         LR    R1,R9                                                            
         GOTO1 AFVAL                                                            
         BNE   VRCID90                                                          
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK1)                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R4,BLOCK1           R4=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FVINDX,1                                                         
*                                                                               
VRCID20  CLC   FVINDX,FLDCNT                                                    
         BH    VRCID90                                                          
         CLI   1(R4),0                                                          
         BNE   VRCID25                                                          
         CLI   0(R4),3             VALIDATE USER-ID                             
         BL    EFTS                                                             
         CLI   0(R4),10                                                         
         BH    EFTL                                                             
         BAS   RE,WILDCARD         TEST FOR WILD CARD STYLE USERID              
         BE    VRCID22                                                          
         L     R3,AIOAREA2          SWITCH I/O AREAS                            
         DROP  R2                                                               
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY       BUILD ID KEY                                 
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R4)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
*                                                                               
VRCID22  MVC   LIDSAVE,12(R4)                                                   
         BAS   RE,ADDIDLST         ADD ID TO SAVE LIST                          
         DROP  R3                                                               
         USING CTIREC,R2                                                        
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD ID ELEMENT                             
         MVI   CTIDLEN,X'0C'                                                    
         MVC   CTID,12(R4)                                                      
         B     VRCID80                                                          
         DROP  R3                                                               
*                                                                               
VRCID25  CLI   0(R4),0             VALIDATE LIST-ID                             
         BE    EIIF                                                             
         LLC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'LIST'   CHECK FOR VALID KEYWORD 'LIST'               
         BE    VRCID40                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'AGY'    CHECK FOR VALID KEYWORD 'AGY'                
         BNE   EIIF                                                             
         CLI   1(R4),2                                                          
         BNE   EIIF                MUST BE 2 CHARS                              
*                                                                               
         MVC   APWORK(L'IOKEY),IOKEY    SAVE THE ID KEY AROUND                  
*                                  AGY=  AGY-UID LIST                           
         L     R3,AIOAREA2         SWITCH I/O AREAS                             
         USING CT9BREC,R3                                                       
         XC    CT9BKEY,CT9BKEY     BUILD LIST KEY                               
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01                                                
         MVC   CT9BKAGY,22(R4)                                                  
         MVC   IOKEY(L'CT9BKEY),CT9BKEY                                         
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BL    EIIO                                                             
         CLC   CT9BKAGY,22(R4)     SAME AGENCY?                                 
         BNE   ERNF                NO - NOT FOUND                               
*                                                                               
VRCID33  LA    R1,CT9BDATA                                                      
VRCID34  CLI   0(R1),0                                                          
         JE    *+2                 MUST HAVE X'02' ELEMENT                      
         CLI   0(R1),X'02'                                                      
         BE    VRCID35                                                          
         LLC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VRCID34                                                          
*                                                                               
VRCID35  MVC   LIDSAVE,2(R1)       SAVE THIS USER ID                            
         BAS   RE,ADDIDLST                                                      
*                                                                               
         GOTO1 AIO,IOSQ+IOCONFIL+IO2                                            
         CLC   CT9BKAGY,22(R4)     STILL THE SAME AGENCY?                       
         BE    VRCID33                                                          
         DROP  R3                                                               
*                                                                               
         MVC   IOKEY,APWORK        RESTORE THE ID REC KEY                       
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD THE ELEMENT                            
         MVI   CTIDLEN,X'06'                                                    
         MVC   CTID(2),=X'0001'    AGY= SUBCODE                                 
         MVC   CTIDAGY(2),22(R4)                                                
         B     VRCID80                                                          
         DROP  R3                                                               
*                                                                               
*                                  LIST=  USER ID LIST                          
VRCID40  CLI   1(R4),6                                                          
         BH    EFTL                                                             
         L     R3,AIOAREA2         SWITCH I/O AREAS                             
         USING CTWREC,R3                                                        
         XC    CTWKEY,CTWKEY       BUILD LIST KEY                               
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'I'                                                     
         MVC   CTWKID,22(R4)                                                    
         MVC   IOKEY(L'CTWKEY),CTWKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BL    EIIO                                                             
         BH    ERNF                                                             
         MVC   IOKEY,APRECKEY                                                   
         LA    R3,CTWDATA          ADD LIST IDS TO SAVE LIST                    
VRCID41  CLI   0(R3),0                                                          
         BE    VRCID50                                                          
         CLI   0(R3),X'A4'                                                      
         BNE   VRCID42                                                          
         MVC   LIDSAVE,3(R3)                                                    
         BAS   RE,ADDIDLST                                                      
VRCID42  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRCID41                                                          
         DROP  R3                                                               
*                                                                               
VRCID50  XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD ID ELEMENT                             
         MVI   CTIDLEN,X'0C'                                                    
         XC    CTID(2),CTID        LIST= SUBCODE                                
         MVC   CTID+2(8),22(R4)                                                 
         DROP  R3                                                               
*                                  ADD ID ELEMENT TO THE REC                    
VRCID80  LA    R3,APELEM                                                        
         GOTO1 VHELLO,APPARM,(C'P',CTFILE),(R2),(R3),=C'ADD=CODE'               
         CLI   12(R1),0                                                         
         BNE   ERTB                CHECK RECORD TOO BIG                         
*                                                                               
         LLC   R1,FVINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,32(R4)                                                        
         B     VRCID20                                                          
*                                                                               
VRCID90  LLC   R1,0(R9)            BUMP TO NEXT TWA LINE                        
         AR    R9,R1                                                            
         LLC   RF,FCOUNT                                                        
         BCT   RF,*+8                                                           
         B     VRCIDX                                                           
         STC   RF,FCOUNT                                                        
         B     VRCID10                                                          
VRCIDX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD PROGRAM EXCEPTION ELEMENTS                       *         
***********************************************************************         
VRPEX    LA    R9,IDSPE1H                                                       
         LA    RF,2                                                             
         STC   RF,FCOUNT                                                        
*                                                                               
VRPEX10  TM    1(R9),FVAPROT       SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         LLC   R1,0(R9)                                                         
         AR    R9,R1                                                            
         LR    R1,R9                                                            
         GOTO1 AFVAL                                                            
         BNE   VRPEX50                                                          
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK1)                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R4,BLOCK1           R4=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FVINDX,1                                                         
*                                                                               
VRPEX20  CLC   FVINDX,FLDCNT                                                    
         BH    VRPEX50                                                          
         CLI   0(R4),1             L'PART1                                      
         BL    EMIF                                                             
         CLI   0(R4),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   1(R4),1             L'PART2                                      
         BH    EFTL                                                             
         BL    EIIF                                                             
         CLI   0(R4),4             L'PART1                                      
         BNE   VRPEX30                                                          
         CLI   12(R4),C'T'         'T' MEANS ONLINE                             
         BNE   VRPEX30                                                          
         GOTO1 VHEXIN,APPARM,13(R4),APDUB,3,0                                   
         OC    APPARM+12(4),APPARM+12                                           
         BNZ   VRPEX40                                                          
*                                                                               
VRPEX30  CLI   SYSTEM,0            CAN'T INPUT PGMNAME IF NOT IN                
         BE    EIIF                SYSTEM MODE                                  
         BAS   RE,GETSOV           GET SELIST ENTRY FRO OVERLAY SYS#            
         BAS   RE,GETPGN           GET PROGRAM NUMBER                           
         BNE   EIIF                                                             
         MVC   APDUB(1),SYSTEM                                                  
         MVC   APDUB+1(1),PROGRAM                                               
         GOTO1 VHEXOUT,APPARM,APDUB,12(R4),2,=C'TOG'                            
         MVI   12(R4),C'T'         NAME CONVERTED TO TSPP                       
*                                                                               
VRPEX40  CLI   22(R4),C'A'         TEST LEVEL S/B A,B OR C                      
         BL    EIIF                                                             
         CLI   22(R4),C'C'                                                      
         BH    EIIF                                                             
         XC    APELEM,APELEM       BUILD PROGRAM EXCEPTION ELEMENT              
         LA    R3,APELEM                                                        
         USING CTPRGD,R3                                                        
         MVI   CTPRGEL,CTPRGELQ                                                 
         MVI   CTPRGLEN,X'07'                                                   
         MVC   CTPRGRAM,12(R4)                                                  
         MVC   CTPRGTST,22(R4)                                                  
         GOTO1 AADDELN,CTIREC                                                   
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
         LLC   R1,FVINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,32(R4)                                                        
         B     VRPEX20                                                          
*                                                                               
VRPEX50  LLC   R1,0(R9)            BUMP TO NEXT TWA LINE                        
         AR    R9,R1                                                            
         LLC   RF,FCOUNT                                                        
         BCT   RF,*+8                                                           
         B     VRPEXX                                                           
         STC   RF,FCOUNT                                                        
         B     VRPEX10                                                          
VRPEXX   EQU   *                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND BUILD A SYSTEM ELEMENT                                 *         
***********************************************************************         
VRSYS    CLI   SYSTEM,0                                                         
         BE    VRSPWD                                                           
         OC    SYSEL,SYSEL                                                      
         JZ    *+2                                                              
*                                                                               
         L     R3,ASYSFACS         GET SELIST ENTRY FOR SE#                     
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
*                                                                               
         CLC   SESYS,SYSEL+(CTSYSSE-CTSYSD)                                     
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
         ST    R3,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         DROP  R3                                                               
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTSYSD,R3                                                        
         MVI   CTSYSEL,CTSYSELQ                                                 
         MVI   CTSYSLEN,CTSYSL1Q                                                
         MVC   CTSYSNUM,SYSTEM                                                  
         MVC   CTSYSALL,XAUTH      PRESET ALL & PROGRAM VALUES                  
         MVC   CTSYSPGM,XAUTH                                                   
         MVC   CTSYSPGM+2(126),CTSYSPGM                                         
         MVC   CTSYSSE,SYSEL+(CTSYSSE-CTSYSD)                                   
         MVC   CTSYSAGB,SYSEL+(CTSYSAGB-CTSYSD)                                 
         MVC   AGNUM,CTSYSAGB                                                   
         BAS   RE,DISPSEAG                                                      
*                                  VALIDATE SYSTEM PASSWORD                     
VRSPWD   GOTO1 AFVAL,IDSSPWDH                                                   
         BNE   VRSLAC                                                           
         CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
         CLI   FVIFLD,C'N'                                                      
         BNE   VRSPWD10                                                         
         OI    CTSYSIND,CTSYSINP                                                
         B     VRSPWDX                                                          
VRSPWD10 CLI   FVIFLD,C'Y'                                                      
         BNE   EIIF                                                             
         OI    CTSYSIND,CTSYSIYP                                                
VRSPWDX  EQU   *                                                                
*                                  VALIDATE SYSTEM LIMIT ACCESS                 
VRSLAC   GOTO1 AFVAL,IDSACCSH                                                   
         BNE   VRSLACX                                                          
         CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
*&&UK*&& GOTO1 AVALLACC,APPARM,(SYSTEM,CTSYSLMT),SECAID                         
*&&US*&& GOTO1 AVALLACC,APPARM,(SYSTEM,CTSYSLMT),AGAID                          
         BNE   EXIT                                                             
VRSLACX  EQU   *                                                                
*                                  VALIDATE SYSTEM PRIORITY                     
VRSPRI   GOTO1 AFVAL,IDSPRIH                                                    
         BNE   VRSPRIX                                                          
         CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
         TM    FVIIND,FVINUM       CHECK NUMERIC                                
         BZ    EFNN                                                             
         LLC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,FVIFLD(0)                                                  
         CVB   R1,APDUB                                                         
         STCM  R1,3,CTSYSPRI                                                    
VRSPRIX  EQU   *                                                                
                                                                                
***********************************************************************         
* VALIDATE PROGRAM AUTHORISATIONS                                     *         
***********************************************************************         
VRSPGM   GOTO1 AFVAL,IDSPA1H                                                    
         BE    VRSPGM10                                                         
         CLI   SYSTEM,0                                                         
         BE    VRSPGMX                                                          
         B     EMIF                                                             
VRSPGM10 CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
VRSPGM20 CLI   FVILEN,6            TEST FOR DELETE                              
         BNE   VRSPGM30                                                         
         CLC   FVIFLD(6),=C'DELETE'                                             
         BNE   VRSPGM30                                                         
         CLI   APACTN,ACTADD       CANNOT DELETE ELEMENT IF ACTION=ADD          
         BE    EIIF                                                             
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),(X'21',(R2)),(1,SYSTEM)              
         B     VRSPGMX                                                          
*                                                                               
VRSPGM30 LA    RE,BLOCK2           CLEAR ELEMENT WORK AREA                      
         ICM   RF,15,=AL4(20*32)                                                
         LA    R0,*                                                             
         L     R1,=X'FF000000'                                                  
         MVCL  RE,R0                                                            
         MVC   BLOCK2(CTSYSPGM-CTSYSD),CTSYSD                                   
         LA    R3,BLOCK2                                                        
*                                                                               
         LA    R9,IDSPA1H                                                       
         LA    RF,3                                                             
         STC   RF,FCOUNT                                                        
*                                                                               
VRSPGM40 TM    1(R9),FVAPROT       SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         LLC   R1,0(R9)                                                         
         AR    R9,R1                                                            
         LR    R1,R9                                                            
         GOTO1 AFVAL                                                            
         BNE   VRSPGM52                                                         
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK1)                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
*                                                                               
         LA    R4,BLOCK1           R9=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FVINDX,1                                                         
*                                                                               
VRSPGM42 CLC   FVINDX,FLDCNT                                                    
         BH    VRSPGM52                                                         
         CLI   0(R4),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   0(R4),1             L'PART1                                      
         BL    EFTS                                                             
         CLI   1(R4),4             L'PART2                                      
         BH    EFTL                                                             
         BE    VRSPGM44                                                         
         CLI   1(R4),1             L'PART2                                      
         BNE   EIIF                                                             
         MVC   APDUB(2),YAUTH      ONE CHR INPUT S/B Y OR N                     
         CLI   22(R4),C'Y'                                                      
         BE    VRSPGM46                                                         
         MVC   APDUB(2),NAUTH                                                   
         CLI   22(R4),C'N'                                                      
         BE    VRSPGM46                                                         
         B     EIIF                                                             
*                                                                               
VRSPGM44 TM    3(R4),X'20'         IF NOT Y OR N MUST BE VALID HEX              
         BZ    EFNH                                                             
         GOTO1 VHEXIN,APPARM,22(R4),APDUB,4                                     
         OC    APPARM+12(4),APPARM+12  DOUBLE CHECK FOR VALID HEX               
         BZ    EFNH                                                             
*                                                                               
VRSPGM46 CLI   0(R4),3                                                          
         BNE   VRSPGM48                                                         
         CLC   12(3,R4),=C'ALL'                                                 
         BNE   VRSPGM48                                                         
         CLC   CTSYSALL,XAUTH      ALL VALUES ALREADY INPUT ?                   
         BNE   EDIF                                                             
         MVC   CTSYSALL,APDUB                                                   
         B     VRSPGM50                                                         
*                                  GET PROGRAM # FROM PROGRAMS LIST             
VRSPGM48 BAS   RE,GETPAN             WITH ACCESS OVERIDE                        
         BNE   EIIF                                                             
         LLC   R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,CTSYSPGM(R1)     POINT TO PROGRAM AUTH                        
         CLC   0(2,R1),XAUTH       PRGM VALUE PREVIOUSLY INPUT ?                
         BNE   EDIF                                                             
         MVC   0(2,R1),APDUB                                                    
*                                                                               
VRSPGM50 LLC   R1,FVINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         LA    R4,32(R4)                                                        
         B     VRSPGM42                                                         
*                                                                               
VRSPGM52 LLC   R1,0(R9)            BUMP TO NEXT TWA LINE                        
         AR    R9,R1                                                            
         LLC   RF,FCOUNT                                                        
         BCT   RF,*+8                                                           
         B     VRSPGM53                                                         
         STC   RF,FCOUNT                                                        
         B     VRSPGM40                                                         
*                                                                               
VRSPGM53 CLC   CTSYSALL,XAUTH      SET ALL VALUE TO N IF N/I                    
         BNE   *+10                                                             
         MVC   CTSYSALL,NAUTH                                                   
         LA    R1,CTSYSPGM         SET PRG VALUES TO ALL VALUE IF N/I           
         LA    RE,255                                                           
*                                                                               
VRSPGM54 MVI   AUTHCHK,0                                                        
*                                                                               
VRSPGM55 CLC   0(2,R1),XAUTH                                                    
         BNE   *+14                                                             
         MVC   0(2,R1),CTSYSALL                                                 
         B     *+18                                                             
         CLC   0(2,R1),CTSYSALL                                                 
         BE    *+8                                                              
         MVI   AUTHCHK,1                                                        
         LA    R1,2(R1)                                                         
         BCT   RE,VRSPGM55                                                      
*                                                                               
         OI    CTSYSEL+5,X'80'                                                  
         CLI   AUTHCHK,0                                                        
         BE    VRSPGM56                                                         
         MVC   APELEM(CTSYSPGM-CTSYSD),BLOCK2                                   
         BAS   RE,CNVELM21                                                      
         B     VRSPGM58                                                         
*                                                                               
VRSPGM56 MVI   CTSYSLEN,16         SET SHORT IF NO PRGM OVERRIDES               
         MVC   APELEM(CTSYSPGM-CTSYSD),BLOCK2                                   
*                                                                               
VRSPGM58 LA    R3,APELEM                                                        
         CLI   APACTN,ACTADD                                                    
         BE    VRSPGM60                                                         
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),(X'21',(R2)),(1,SYSTEM)              
*                                                                               
VRSPGM60 GOTO1 AADDELN,CTIREC      DELETE OLD AND ADD NEW SYSTEM EL             
         BNE   VALRECER            RECORD TOO BIG                               
*                                                                               
VRSPGMX  EQU   *                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD                                                       *         
***********************************************************************         
VRUPD    MVC   KEYSAVE,IOKEY                                                    
         TM    IDOPTS,CTIDOFSN                                                  
         BZ    VRUPDC                                                           
         L     R4,AIOAREA2         UPDATE CURRENT SYNONYM ID RECORD             
         DROP  R2                                                               
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,SYNID                                                     
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDOEL,R3                                                       
         MVI   APELEM,CTIDOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    VRUPDA                                                           
         MVC   WORK(CTIDOLNQ),0(R3)                                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTIDOELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTIREC                                                   
         MVC   APELEM(CTIDOLNQ),WORK                                            
VRUPDA   LA    R3,APELEM                                                        
         MVI   CTIDOEL,CTIDOELQ                                                 
         MVI   CTIDOLEN,CTIDOLNQ                                                
         OI    CTIDOFL1,CTIDOFSP                                                
         NI    CTIDOFL1,X'FF'-CTIDOFSN                                          
         MVC   CTIDOSNU,IDNUM                                                   
         MVC   CTIDOSID,KEYSAVE+CTIKID-CTIKEY                                   
         GOTO1 AADDELN,CTIREC                                                   
         JNE   *+2                                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         DROP  R3                                                               
         XC    CTIKEY,CTIKEY      UPDATE SYNONYM ID NUMBER RECORD               
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,SYNNUM                                                   
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDOEL,R3                                                       
         MVI   APELEM,CTIDOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    VRUPDB                                                           
         MVC   WORK(CTIDOLNQ),0(R3)                                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTIDOELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTIREC                                                   
         MVC   APELEM(CTIDOLNQ),WORK                                            
*                                                                               
VRUPDB   LA    R3,APELEM                                                        
         MVI   CTIDOEL,CTIDOELQ                                                 
         MVI   CTIDOLEN,CTIDOLNQ                                                
         OI    CTIDOFL1,CTIDOFSP                                                
         NI    CTIDOFL1,X'FF'-CTIDOFSN                                          
         MVC   CTIDOSNU,IDNUM                                                   
         MVC   CTIDOSID,KEYSAVE+CTIKID-CTIKEY                                   
         GOTO1 AADDELN,CTIREC                                                   
         JNE   *+2                                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         DROP  R3,R4                                                            
*                                                                               
VRUPDC   TM    LASTIDOS,CTIDOFSN                                                
         BZ    VRUPDID                                                          
         CLC   LASTSID,SYNID                                                    
         BE    VRUPDID                                                          
         L     R4,AIOAREA2         UPDATE LAST SYNONYM ID RECORD                
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,LASTSID                                                   
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDOEL,R3                                                       
         MVI   APELEM,CTIDOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    VRUPDD                                                           
         MVC   WORK(CTIDOLNQ),0(R3)                                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTIDOELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTIREC                                                   
         MVC   APELEM(CTIDOLNQ),WORK                                            
VRUPDD   LA    R3,APELEM                                                        
         MVI   CTIDOEL,CTIDOELQ                                                 
         MVI   CTIDOLEN,CTIDOLNQ                                                
         NI    CTIDOFL1,X'FF'-CTIDOFSP                                          
         XC    CTIDOSNU,CTIDOSNU                                                
         XC    CTIDOSID,CTIDOSID                                                
         GOTO1 AADDELN,CTIREC                                                   
         JNE   *+2                                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         DROP  R3                                                               
         XC    CTIKEY,CTIKEY      UPDATE SYNONYM ID NUMBER RECORD               
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,LASTSYN                                                  
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTIDOEL,R3                                                       
         MVI   APELEM,CTIDOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    VRUPDE                                                           
         MVC   WORK(CTIDOLNQ),0(R3)                                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTIDOELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTIREC                                                   
         MVC   APELEM(CTIDOLNQ),WORK                                            
VRUPDE   LA    R3,APELEM                                                        
         MVI   CTIDOEL,CTIDOELQ                                                 
         MVI   CTIDOLEN,CTIDOLNQ                                                
         NI    CTIDOFL1,X'FF'-CTIDOFSP                                          
         XC    CTIDOSNU,CTIDOSNU                                                
         XC    CTIDOSID,CTIDOSID                                                
         GOTO1 AADDELN,CTIREC                                                   
         JNE   *+2                                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE REGULAR ID RECORDS                                           *         
***********************************************************************         
         USING CTIREC,R2                                                        
VRUPDID  LA    R1,GENACTH          SET CURSOR IN CASE OF ERROR                  
         ST    R1,FVADDR                                                        
         MVC   IOKEY,KEYSAVE                                                    
         GOTO1 ASETACN,CTIREC                                                   
         BE    *+14                                                             
         TM    IDOPTS,CTIDOFSN                                                  
         BZ    VALRECER            EXIT WITH MESSAGE RECORD TOO BIG             
         DC    H'0'                OR DIE TO BACK OUT SYNONYM UPDATES           
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         L     RF,=A(CHKBIG)       CHECK RECORD WILL BE TOO BIG                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    *+14                                                             
         TM    IDOPTS,CTIDOFSN                                                  
         BZ    VALRECER            EXIT WITH MESSAGE RECORD TOO BIG             
         DC    H'0'                OR DIE TO BACK OUT SYNONYM UPDATES           
         CLI   APACTN,ACTCHA                                                    
         BNE   VRUPD10                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,X'04'                                                   
         MVC   APELEM+2(2),IDNUM                                                
         GOTO1 AADDELN,CTIREC                                                   
         JNE   *+2                                                              
         MVC   KEYSAVE,IOKEY                                                    
*                                  WRITE ID RECORD                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         JNE   *+2                                                              
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
         JNE   *+2                                                              
*                                                                               
         XC    SAVNAGY,SAVNAGY                                                  
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    *+10                                                             
         USING CTAGYD,R1                                                        
         MVC   SAVNAGY,CTAGYID     SAVE THE NEW AGENCY ALPHA-ID                 
         DROP  R1                                                               
*                                                                               
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JNE   *+2                                                              
*                                                                               
         XC    SAVOAGY,SAVOAGY                                                  
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    *+10                                                             
         USING CTAGYD,R1                                                        
         MVC   SAVOAGY,CTAGYID     SAVE THE OLD AGENCY ALPHA-ID                 
         DROP  R1                                                               
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         JNE   *+2                                                              
         DROP  R2                                                               
*                                                                               
         CLC   SAVOAGY,SAVNAGY     AGENCY CHANGED?                              
         BE    VALRECX             NO - DON'T ADD/CHG AG-UID# PASS REC          
         OC    SAVNAGY,SAVNAGY     HAVE A NEW AGENCY?                           
         BZ    VRUPD04             NO - JUST DEL OLD AG-UID# PASS REC           
*                                                                               
         L     R3,AIOAREA3         WRITE AG-UID# PASSIVE RECORD                 
         USING CT9BREC,R3          COPY ONLY 01 AND 02 ELEMENTS                 
         MVC   0(CT9BDATA+1-CT9BKEY+X'05'+X'0C',R3),0(R2)                       
         MVI   CT9BDATA-CT9BKEY+X'05'+X'0C'(R3),X'00'                           
         MVI   CT9BLEN,X'00'                                                    
         MVI   CT9BLEN+1,CT9BDATA+1-CT9BKEY+X'05'+X'0C'                         
*                                                                               
         XC    CT9BKEY,CT9BKEY                                                  
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01   SUB TYPE FOR AG-UID# PASSIVE RECORD          
         MVC   CT9BKNUM,IDNUM                                                   
         MVC   CT9BKAGY,SAVNAGY    AGENCY ALPHA-ID                              
*                                                                               
         MVC   IOKEY(L'CT9BKEY),CT9BKEY                                         
         DROP  R3                                                               
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JL    *+2                                                              
         TM    IOERR,IOEDEL                                                     
         BNZ   VRUPD02             DELETED, WRITE NEW AG-UID# PASS REC          
         TM    IOERR,IOERNF                                                     
         BNZ   VRUPD03             NOT FOUND, ADD NEW AG-UID# PASS REC          
         DC    H'0'                                                             
*                                                                               
VRUPD02  L     R2,AIOAREA3                                                      
         GOTO1 AIO,IOWRITE+IOCONFIL+IO3                                         
         BE    VRUPD04                                                          
         DC    H'0'                                                             
*                                                                               
VRUPD03  L     R2,AIOAREA3                                                      
         GOTO1 AIO,IOADD+IOCONFIL+IO3                                           
         BE    VRUPD04                                                          
         DC    H'0'                                                             
*                                                                               
VRUPD04  OC    SAVOAGY,SAVOAGY     HAVE AN OLD AGENCY?                          
         BZ    VALRECX             NO                                           
*                                  YES - DEL THE OLD AG-UID# PASS REC           
         L     R3,AIOAREA2                                                      
         USING CT9BREC,R3                                                       
         XC    CT9BKEY,CT9BKEY                                                  
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01   SUB TYPE FOR AG-UID# PASSIVE RECORD          
         MVC   CT9BKNUM,IDNUM                                                   
         MVC   CT9BKAGY,SAVOAGY    AGENCY ALPHA-ID                              
*                                                                               
         MVC   IOKEY(L'CT9BKEY),CT9BKEY                                         
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JL    *+2                                                              
         TM    IOERR,IOERNF                                                     
         BNZ   VALRECX     NOT FOUND, IGNORE THIS FOR NOW, 6/20/03              
         GOTO1 ASETACN,CT9BREC                                                  
         BNE   VALRECX             RECORD TOO BIG                               
         OI    CT9BSTAT,X'80'                                                   
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         DROP  R3                                                               
*                                                                               
         B     VALRECX                                                          
         EJECT                                                                  
***********************************************************************         
* ADD NEW ID RECORDS                                                  *         
***********************************************************************         
VRUPD10  L     R2,AIOAREA2         ALLOCATE NEW ID# FROM MASTER ID REC          
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   KEYSAVE,IOKEY                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         JZ    *+2                                                              
         MVC   IDNUM,2(R3)         UPDATE POINTER NUMBER                        
         L     R2,AIOAREA3                                                      
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
*                                                                               
         GOTO1 AFVAL,IDSIDNH       TEST FOR ID NUMBER OVERRIDE                  
         BNE   VRUPD20                                                          
         TM    FVIIND,FVITHIS      TEST FOR THIS TIME INPUT                     
         BZ    VRUPD20                                                          
         GOTO1 =V(NUMVAL),APPARM,IDSIDN,(X'01',0),RR=APRELO                     
         CLI   0(R1),X'FF'                                                      
         BE    EFNN                                                             
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    EIIF                                                             
         CLM   R1,3,=XL2'FFFF'                                                  
         BH    EIIF                                                             
         STCM  R1,3,IDNUM                                                       
         MVC   CTIKNUM,IDNUM                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDD+IOCONFIL+IO3                                           
         JL    *+2                                                              
         BE    EIIF                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   EIIF                                                             
         TM    IOERR,IOERNF                                                     
         BNZ   VRUPD50                                                          
         DC    H'0'                                                             
*                                                                               
VRUPD20  MVC   CTIKNUM,IDNUM                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDD+IOCONFIL+IO3                                           
         JL    *+2                                                              
         BE    VRUPD30                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   VRUPD30                                                          
         TM    IOERR,IOERNF                                                     
         BNZ   VRUPD40                                                          
         DC    H'0'                                                             
VRUPD30  SR    RF,RF                                                            
         ICM   RF,3,IDNUM                                                       
         LA    RF,1(RF)                                                         
         STCM  RF,3,IDNUM                                                       
         LTR   RF,RF                                                            
         BNZ   VRUPD20                                                          
         DC    H'0'                                                             
*                                                                               
VRUPD40  L     R2,AIOAREA2                                                      
         XC    CTIREC(256),CTIREC                                               
         MVI   CTIKTYP,CTIKTYPQ                                                 
         LA    R0,CTIDATA+1-CTIKEY                                              
         STCM  R0,3,CTILEN                                                      
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,X'04'                                                   
         MVC   APELEM+2(2),IDNUM                                                
         GOTO1 AADDELN,CTIREC                                                   
         JNE   *+2                                                              
         GOTO1 ASETACN,CTIREC                                                   
         JNE   *+2                                                              
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         JNE   *+2                                                              
*                                  ADD NEW ID RECORD                            
VRUPD50  L     R2,AIOAREA1                                                      
         MVC   IOKEY,KEYSAVE                                                    
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,X'04'                                                   
         MVC   APELEM+2(2),IDNUM                                                
         GOTO1 AADDELN,CTIREC                                                   
         JNE   *+2                                                              
         GOTO1 AIO,IOADD+IOCONFIL+IO1                                           
         JNE   *+2                                                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,IDNUM                                                    
*                                  ADD ID# PASSIVE RECORD                       
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTIREC                                                   
         MVI   APELEM,X'02'                                                     
         MVI   APELEM+1,X'0C'                                                   
         MVC   APELEM+2(10),KEYSAVE+CTIKID-CTIKEY                               
         GOTO1 AADDELN,CTIREC                                                   
         JNE   *+2                                                              
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOADD+IOCONFIL+IO1                                           
         JNE   *+2                                                              
*                                                                               
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    VALRECX                                                          
         USING CTAGYD,R1                                                        
         MVC   SAVNAGY,CTAGYID     GET AGENCY FOR THE KEY                       
         DROP  R1,R2                                                            
*                                                                               
         L     R3,AIOAREA2         ADD AG-UID# PASSIVE RECORD                   
         USING CT9BREC,R3          COPY ONLY 01 AND 01 ELEMENTS                 
         MVC   0(CT9BDATA+1-CT9BKEY+X'05'+X'0C',R3),0(R2)                       
         MVI   CT9BDATA-CT9BKEY+X'05'+X'0C'(R3),X'00'                           
         MVI   CT9BLEN,X'00'                                                    
         MVI   CT9BLEN+1,CT9BDATA+1-CT9BKEY+X'05'+X'0C'                         
*                                                                               
         XC    CT9BKEY,CT9BKEY                                                  
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01   SUB TYPE FOR AG-UID# PASSIVE RECORD          
         MVC   CT9BKNUM,IDNUM                                                   
         MVC   CT9BKAGY,SAVNAGY                                                 
*                                                                               
         MVC   IOKEY(L'CT9BKEY),CT9BKEY                                         
         GOTO1 AIO,IOADD+IOCONFIL+IO2                                           
         JNE   *+2                                                              
         DROP  R3                                                               
*                                                                               
VALRECX  MVC   FVMSGNO,=AL2(FVFOK) EXIT RECORD VALIDATION AND UPDATE OK         
         CLI   LIDFNDX,0           CHECK COMPATIBLE ID DUPE MESSAGE             
         BE    DISREC                                                           
         MVC   FVXTRA(15),=C'- Duplicate CID'                                   
         B     DISREC              REDISPLAY RECORD                             
*                                                                               
VALRECER B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOCATE SELIST ENTRY FOR SYSTEM AND SAVE AT ASE                      *         
***********************************************************************         
GETSOV   NTR1                                                                   
         L     R3,ASYSFACS                                                      
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
*                                                                               
         CLC   SYSTEM,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         ST    R3,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         XIT1                                                                   
         DROP  R3                                                               
                                                                                
***********************************************************************         
* SET PROGRAM NUMBER FROM PROGRAM NAME                                *         
* R4=A(SCANNER BLOCK ENTRY)                                           *         
***********************************************************************         
GETPGN   NTR1                                                                   
         LLC   R1,0(R4)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R3,APGM                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GPGN10   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R4)                                                
         BE    *+12                                                             
GPGN20   BXLE  R3,RE,GPGN10                                                     
         B     NO                  NOT FOUND                                    
         CLC   PGMCTRY,IDCTRY      CHECK COUNTRY CODE                           
         BE    *+12                                                             
         CLI   PGMCTRY,0           ELSE USE DEFAULT                             
         BNE   GPGN20                                                           
         MVC   PROGRAM,PGMNUM                                                   
         B     YES                                                              
         DROP  R3                                                               
                                                                                
* SET PROGRAM NUMBER FROM PROGRAM NAME                                          
* R4=A(SCANNER BLOCK ENTRY)                                                     
* USE ACCESS OVERIDE                                                            
*                                                                               
GETPAN   NTR1                                                                   
         LLC   R1,0(R4)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R3,APGM                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GPAN10   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R4)                                                
         BE    *+12                                                             
GPAN20   BXLE  R3,RE,GPAN10                                                     
         B     NO                  NOT FOUND                                    
         CLC   PGMCTRY,IDCTRY      CHECK PROGRAM COUNTRY CODE                   
         BE    *+12                                                             
         CLI   PGMCTRY,0           ELSE USE DEFAULT                             
         BNE   GPAN20                                                           
         MVC   PROGRAM,PGMNUM                                                   
         CLI   PGMALNUM,0          USE ACCESS OVERRIDE IF SET                   
         BE    *+10                                                             
         MVC   PROGRAM,PGMALNUM                                                 
         B     YES                                                              
         DROP  R3                                                               
                                                                                
* CONVERT THE X'21' ELEMENT TO THE NEW FORMAT                                   
*                                                                               
CNVELM21 NTR1                                                                   
         LA    R9,BLOCK2           POINT FROM WHERE TO COPY                     
         USING CTSYSD,R9                                                        
         LA    R3,APELEM+CTSYSPGM-CTSYSD  WHERE TO COPY                         
         LA    R1,1                FIRST PROGRAM                                
         LA    R0,16               LENGTH IS HEADER FIRST                       
         LA    R4,CTSYSPGM                                                      
CNV21LP  CLC   0(2,R4),CTSYSALL    DEFAULT?                                     
         BE    CNV21NX             YES, SKIP TO NEXT ONE                        
         STC   R1,0(R3)            STORE THE PROGRAM NUMBER                     
         MVC   1(2,R3),0(R4)       AND ITS AUTHORIZATION CODE                   
         AHI   R0,3                LENGTH IS CHANGED BY 3                       
         LA    R3,3(R3)            NEXT POSTION FOR NEXT PROGRAM                
CNV21NX  LA    R1,1(R1)            NEXT PROGRAM NUMBER                          
         LA    R4,2(R4)            NEXT AUTHORIZATION CODE                      
         CHI   R1,255              DID WE DO ALL 255 PROGRAMS?                  
         BNH   CNV21LP             NO, CONTINUE UNTIL WE'RE DONE                
         LA    R9,APELEM           STORE THE NEW LENGTH OF THE ELEMENT          
         STC   R0,CTSYSLEN                                                      
         XIT1                                                                   
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MERGE SYSTEM INFO FROM ACCESS RECORD                     *         
***********************************************************************         
MRGSYS   NTR1                                                                   
         USING CTIREC,R2                                                        
         LA    R3,CTIDATA                                                       
MRL1     CLI   0(R3),0             READ EACH ELEMENT IN ID RECORD               
         BE    MRGSYSX                                                          
         CLI   0(R3),X'21'         SYSTEM ELEMENT FOUND                         
         BNE   MRL1A                                                            
         L     R4,AIOAREA3                                                      
         LA    R4,CT5DATA-CT5REC(R4)                                            
MRL2     CLI   0(R4),0             READ EACH ELEMENT IN ACCESS RECORD           
         BE    MRL2A                                                            
         CLI   0(R4),X'21'         SYSTEM ELEMENT FOUND                         
         BNE   MRL2B                                                            
         CLC   CTSYSNUM-CTSYSD(1,R3),CTSYSNUM-CTSYSD(R4)                        
         BNE   MRL2B                                                            
         MVC   CTSYSSE-CTSYSD(2,R3),CTSYSSE-CTSYSD(R4)                          
         B     MRL1A                                                            
*                                                                               
MRL2A    MVI   0(R3),X'99'         SET TEMP ELEM CODE FOR DELETE                
         B     MRL1A                                                            
*                                                                               
MRL2B    LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     MRL2                                                             
*                                                                               
MRL1A    LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     MRL1                                                             
*                                                                               
MRGSYSX  GOTO1 VHELLO,APPARM,(C'D',CTFILE),(X'99',(R2)),0                       
         CLI   12(R1),0                                                         
         BE    MRGSYSX1                                                         
         CLI   12(R1),X'06'                                                     
         BE    MRGSYSX1                                                         
         DC    H'0'                                                             
MRGSYSX1 XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ID TO COMPATIBLE ID SAVE LIST IF IT IS NOT THERE     *         
* AND THEN REPORT IF DUPLICATE FOUND                                  *         
***********************************************************************         
ADDIDLST LR    R0,RE                                                            
         L     RE,AIDLIST          CURRENT AVALIABLE ENTRY                      
         L     RF,ATIA             BEGINNING OF THE TABLE                       
*                                                                               
AIDL10   CR    RF,RE                                                            
         BNL   AIDL30                                                           
         CLC   0(10,RF),LIDSAVE    FIND A MATCH?                                
         BE    AIDL20              YES - ALREADY THERE                          
         LA    RF,10(RF)                                                        
         B     AIDL10                                                           
*                                                                               
AIDL20   MVC   LIDFNDX,FVINDX                                                   
         MVC   LIDFADDR,FVADDR                                                  
         B     AIDLX                                                            
*                                                                               
AIDL30   C     RE,ATWA             WILL WE OVERFLOW TIA?                        
         BNL   EFTM                YES - TOO MANY USER IDS                      
         MVC   0(10,RE),LIDSAVE    SAVE THIS UID                                
         LA    RE,10(RE)                                                        
         ST    RE,AIDLIST          BUMP TABLE POINTER                           
*                                                                               
AIDLX    LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
* ROUTINE TO CHECK FOR WILD CARD STYLE USERID IN COMPATIBLE ID LIST             
* R4=A(SCANNER CONTROL BLOCK)                                                   
* IF WILDCARD (I.E. STRING ENDS WITH '*') RETURN IN LIDSAVE                     
* AND RETURN CC .EQ.                                                            
*                                                                               
WILDCARD NTR1                                                                   
         LLC   RF,0(R4)                                                         
         LA    R1,11(RF,R4)                                                     
WCAR010  CLI   0(R1),C'*'                                                       
         BE    WCAR020                                                          
         CLI   0(R1),C' '                                                       
         BE    WCAR012                                                          
         B     WCARNO                                                           
WCAR012  BCTR  R1,0                                                             
         BCT   RF,WCAR010                                                       
         B     WCARNO                                                           
WCAR020  BCTR  RF,0                                                             
         STC   RF,WILDCLEN                                                      
         B     WCAROK                                                           
WCAROK   SR    RC,RC                                                            
WCARNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A USER ID RECORD                                  *         
***********************************************************************         
DELREC   L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         GOTO1 ASETACN,CTIREC                                                   
         BNE   DELRECX             RECORD TOO BIG                               
         OI    CTISTAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         JNE   *+2                                                              
*                                                                               
         XC    APELEM,APELEM       DELETE PASSIVE USER ID RECORD                
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    DELRECX                                                          
         L     R2,AIOAREA2                                                      
         XC    CTIKEY,CTIKEY       UPDATE PASSIVE USER ID RECORD                
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,2(R3)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JNE   *+2                                                              
         GOTO1 ASETACN,CTIREC                                                   
         BNE   DELRECX             RECORD TOO BIG                               
         OI    CTISTAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         JNE   *+2                                                              
*                                                                               
         XC    SAVNAGY,SAVNAGY                                                  
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    DELRECX                                                          
         USING CTAGYD,R1                                                        
         MVC   SAVNAGY,CTAGYID     GET AGENCY FOR THE KEY                       
         DROP  R1,R2                                                            
*                                                                               
         L     R3,AIOAREA3         BUILD AG-UID# KEY                            
         USING CT9BREC,R3                                                       
         XC    CT9BKEY,CT9BKEY                                                  
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01   SUB TYPE FOR AG-UID# PASSIVE RECORD          
         MVC   CT9BKNUM,CTIKNUM-CTIKEY(R2)                                      
         MVC   CT9BKAGY,SAVNAGY                                                 
*                                  DELETE PASSIVE AG-UID# RECORD                
         MVC   IOKEY(L'CT9BKEY),CT9BKEY                                         
         GOTO1 AIO,IORDUPD+IOCONFIL+IO3                                         
         BE    DELREC2                                                          
         CLI   IOERR,IOERNF        UNLESS NOT THERE ANYWAY                      
         BE    DELRECX                                                          
         DC    H'0'                                                             
*                                                                               
DELREC2  GOTO1 ASETACN,CT9BREC                                                  
         BNE   DELRECX             RECORD TOO BIG                               
         OI    CT9BSTAT,X'80'                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO3                                         
         JNE   *+2                                                              
         DROP  R3                                                               
*                                                                               
DELRECX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED USER ID GROUP                          *         
***********************************************************************         
RESREC   L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         GOTO1 ASETACN,CTIREC                                                   
         BNE   RESRECX             RECORD TOO BIG                               
         NI    CTISTAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         JNE   *+2                                                              
*                                                                               
         XC    APELEM,APELEM       RESTORE PASSIVE USER ID RECORD               
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    RESRECX                                                          
         L     R2,AIOAREA2                                                      
         XC    CTIKEY,CTIKEY       UPDATE PASSIVE TERM NUMBER RECORD            
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,2(R3)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         JL    *+2                                                              
         TM    IOERR,IOEDEL                                                     
         JZ    *+2                                                              
         GOTO1 ASETACN,CTIREC                                                   
         BNE   RESRECX             RECORD TOO BIG                               
         NI    CTISTAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         JNE   *+2                                                              
*                                                                               
         XC    SAVNAGY,SAVNAGY                                                  
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    DELRECX                                                          
         USING CTAGYD,R1                                                        
         MVC   SAVNAGY,CTAGYID     GET AGENCY FOR THE KEY                       
         DROP  R1,R2                                                            
*                                  BUILD AG-UID# KEY                            
         L     R3,AIOAREA3                                                      
         USING CT9BREC,R3                                                       
         XC    CT9BKEY,CT9BKEY                                                  
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01   SUB TYPE FOR AG-UID# PASSIVE RECORD          
         MVC   CT9BKNUM,CTIKNUM-CTIKEY(R2)                                      
         MVC   CT9BKAGY,SAVNAGY                                                 
*                                  RESTORE PASSIVE AG-UID# RECORD               
         MVC   IOKEY(L'CT9BKEY),CT9BKEY                                         
         GOTO1 AIO,IORDUPD+IOCONFIL+IO3                                         
         JL    *+2                                                              
         TM    IOERR,IOEDEL                                                     
         JZ    *+2                                                              
         GOTO1 ASETACN,CT9BREC                                                  
         BNE   RESRECX             RECORD TOO BIG                               
         NI    CT9BSTAT,X'FF'-X'80'                                             
         GOTO1 AIO,IOWRITE+IOCONFIL+IO3                                         
         JNE   *+2                                                              
*                                                                               
RESRECX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF USER ID RECORD                            *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         USING CTIREC,R2                                                        
         MVC   IDSID,CTIKID                                                     
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY USER ID RECORD                                   *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         TWAXC IDSAGYAH                                                         
         XC    IDSSEAG,IDSSEAG     CLEAR SENAME/AGENCY# PROT. FIELD             
         OI    IDSSEAGH+6,X'80'                                                 
         XC    IDSIDSY,IDSIDSY                                                  
         OI    IDSIDSYH+6,X'80'                                                 
         MVC   IDSUSED,SPACES                                                   
         OI    IDSUSEDH+6,X'80'                                                 
*                                                                               
DISREC2  SR    RE,RE               DISPLAY %RECORD USED                         
         SR    RF,RF                                                            
         ICM   RF,3,CTILEN         FROM RECORD SIZE                             
         OC    CTIKID(8),CTIKID    ACCOUNT FOR EXTRA LENGTH OF PASSIVE          
         BZ    *+8                 POINTER                                      
         AHI   RF,8                                                             
         D     RE,=F'10'                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  IDSUSED+09(2),DUB                                                
         MVI   IDSUSED+11,C'%'                                                  
*                                                                               
DISREC4  MVC   FULL(1),CTISTAT     STATUS BYTE HAS LAST USED MONTH              
         NI    FULL,X'7F'                                                       
         SR    RE,RE                                                            
         LLC   RF,FULL             RF=RELATIVE MONTH 01=JAN2009                 
         AHI   RF,-1                                                            
         BNM   *+14                                                             
         MVC   FULL(3),=X'640100'  IF NOT USED SET MONTH TO JAN2000             
         B     DISREC5                                                          
         D     RE,=F'12'                                                        
         AHI   RE,1                RE=MONTH                                     
         AHI   RF,109              RF=YEAR                                      
         STC   RF,FULL                                                          
         STC   RE,FULL+1                                                        
         MVI   FULL+2,0                                                         
DISREC5  GOTO1 VDATCON,APPARM,(3,FULL),(22,DATEFLD)                             
         MVC   IDSUSED(8),DATEFLD                                               
         CLC   FULL(3),=X'640100'                                               
         BNE   *+10                                                             
         MVC   IDSUSED(8),=C'...     '                                          
*                                                                               
DISREC6  XC    XXCNT(XXCNTL),XXCNT ZERO ALL COUNTERS                            
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTIDOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    DREC10                                                           
         USING CTIDOEL,R1                                                       
         CLI   CTIDOLEN,CTIDOLNQ                                                
         BNE   DREC10                                                           
         TM    CTIDOFL1,CTIDOFSN                                                
         BZ    DREC10                                                           
         MVI   IDSPID,C'='                                                      
         MVC   IDSPID+1(8),CTIDOSID                                             
         B     DRDAT100                                                         
         DROP  R1                                                               
*                                                                               
DREC10   CLI   SYSTEM,0                                                         
         BE    DREC20                                                           
         GOTO1 ADISSE,SYSTEM       GET A(SELIST) ENTRY INTO ASE                 
         OC    APPARM,APPARM                                                    
         JZ    *+2                                                              
         MVC   ASE,APPARM                                                       
*                                                                               
DREC20   MVC   IDSAGYA,=C'??'      FUDGE FOR CONVERSION PROBLEM                 
         XC    ASYSEL,ASYSEL       SET A(SYSTEM ELEMENT)                        
         LA    R3,CTIDATA                                                       
*                                                                               
DRDAT20  CLI   0(R3),0                                                          
         BE    DRDAT50                                                          
         CLI   0(R3),X'03'         PRINCIPAL ID                                 
         BE    DRPID                                                            
         CLI   0(R3),X'20'         ID                                           
         BE    DRID                                                             
         CLI   0(R3),X'23'         PROGRAM EXCEPTION                            
         BE    DRPEX                                                            
         CLI   0(R3),X'21'         SYSTEM                                       
         BE    DRSYS                                                            
         CLI   0(R3),X'07'         ID OPTIONS ELEMENT                           
         BE    DROPT                                                            
         CLI   0(R3),X'06'         AGENCY ID                                    
         BE    DRAGY                                                            
         CLI   0(R3),CTTOUELQ      TIME OUT ELEMENT                             
         BE    DRTOU                                                            
*&&UK                                                                           
         CLI   0(R3),CTLNIELQ      LOTUS NOTES ADMIN ID ELEMENT                 
         BE    DRLNI                                                            
*&&                                                                             
         CLI   0(R3),CTAFPELQ      AFP ID ELEMENT                               
         BE    DRAFP                                                            
         CLI   0(R3),X'02'         ID# POINTER                                  
         BNE   DRDAT40                                                          
         CLI   1(R3),X'04'         CHECK NUMBER POINTER                         
         BNE   *+10                                                             
         MVC   IDNUM,2(R3)                                                      
         XC    IDSIDN,IDSIDN                                                    
         OI    IDSIDNH+6,X'80'                                                  
         EDIT  (B2,IDNUM),(5,IDSIDN),ALIGN=LEFT                                 
*                                                                               
DRDAT40  LLC   R4,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R4                                                            
         B     DRDAT20                                                          
         EJECT                                                                  
***********************************************************************         
* PUT BLOCKS TO TWA                                                   *         
***********************************************************************         
DRDAT50  CLI   IDCNT,0             VALID ID'S                                   
         BE    DRDAT60                                                          
         LLC   R0,IDCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(3,IDSID1H),(20,BLOCK1),(R0),        *        
               RR=APRELO                                                        
*                                                                               
DRDAT60  CLI   EXCNT,0             PROGRAM EXCEPTIONS                           
         BE    DRDAT70                                                          
         LLC   R0,EXCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(2,IDSPE1H),(20,BLOCK2),(R0),        *        
               RR=APRELO                                                        
*                                                                               
DRDAT70  OC    ASYSEL,ASYSEL       PROGRAM ACCESS                               
         BZ    DRDAT100                                                         
         L     RF,=A(DISPSYS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         L     R3,ASYSEL                                                        
         USING CTSYSD,R3                                                        
         MVC   AGNUM,CTSYSAGB                                                   
         BAS   RE,DISPSEAG         DISPLAY SENAME/AGENCY BINARY#                
*                                                                               
         TM    CTSYSIND,CTSYSIYP                                                
         BZ    DRDAT80                                                          
         MVI   IDSSPWD,C'Y'                                                     
         B     DRDAT90                                                          
DRDAT80  TM    CTSYSIND,CTSYSINP                                                
         BZ    DRDAT90                                                          
         MVI   IDSSPWD,C'N'                                                     
*                                                                               
DRDAT90  GOTO1 ADISLACC,APPARM,(CTSYSNUM,CTSYSLMT),AGAID                        
         MVC   IDSACCS,APWORK                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,CTSYSPRI                                                    
         EDIT  (R0),(4,IDSPRI),ALIGN=LEFT                                       
         OI    IDSPRI,X'F0'                                                     
         LLC   R0,PGCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(3,IDSPA1H),(20,BLOCK1),(R0),        *        
               RR=APRELO                                                        
*                                                                               
DRDAT100 MVI   IDSSHD,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   IDSSHD+1(L'IDSSHD-1),IDSSHD                                      
         CLI   SYCNT,0                                                          
         BE    DRDAT110                                                         
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DRDAT110                                                         
         LA    R1,L'IDSSHD                                                      
         SR    R1,RF                                                            
         BNP   DRDAT110                                                         
         SRL   R1,1                                                             
         LA    RE,IDSSHD(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DRDAT110 OI    IDSSHDH+6,X'80'                                                  
*                                                                               
DRDATX   MVC   SAVSYS,SYSTEM       SAVE LAST DISPLAYED SYSTEM                   
         CLI   LIDFNDX,0           CHECK COMPATIBLE ID DUPE MESSAGE             
         BNE   EXIT                                                             
         GOTO1 ADISACT,CTIREC                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD AN ID ELEMENT TO ID BLOCK (BLOCK1)                              *         
***********************************************************************         
DRID     LLC   R1,IDCNT            BUMP BLOCK COUNT                             
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IDCNT                                                         
         MHI   R4,20                                                            
         LA    R4,BLOCK1(R4)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R4),C' '          AND CLEAR IT                                 
         MVC   1(19,R4),0(R4)                                                   
         IC    R1,1(R3)                                                         
         OC    2(2,R3),2(R3)                                                    
         BNZ   *+10                                                             
         MVC   2(2,R3),=C'L='                                                   
*                                                                               
         CLC   2(2,R3),=X'0001'                                                 
         BNZ   *+10                                                             
         MVC   2(2,R3),=C'A='                                                   
*                                                                               
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R3)       MOVE IN ID                                   
         B     DRDAT40                                                          
         EJECT                                                                  
***********************************************************************         
* ADD A PROGRAM EXCEPTION ELEMENT TO BLOCK (BLOCK2)                   *         
***********************************************************************         
         USING CTPRGD,R3                                                        
DRPEX    MVC   APWORK(4),CTPRGRAM  IF NOT IN SYSTEM MODE DISPLAY TSPP           
         CLI   SYSTEM,0                                                         
         BE    DRPEX2                                                           
         MVC   DUB(4),CTPRGRAM                                                  
         MVI   DUB,C'0'                                                         
         GOTO1 VHEXIN,APPARM,DUB,DUB+4,4                                        
         CLC   SYSTEM,DUB+4        EXCEPTION FOR THIS SYSTEM                    
         BNE   DRDAT40                                                          
         MVC   PROGRAM,DUB+5       YES - GET PROGRAM NAME                       
         GOTO1 ADISPGM,APPARM,(SYSTEM,PROGRAM)                                  
         OC    APPARM,APPARM       CHECK FOUND                                  
         BZ    DRDAT40                                                          
*                                                                               
DRPEX2   LLC   R1,EXCNT            BUMP BLOCK COUNT                             
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,EXCNT                                                         
         MHI   R4,20                                                            
         LA    R4,BLOCK2(R4)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R4),C' '          AND CLEAR                                    
         MVC   1(19,R4),0(R4)                                                   
         MVC   0(4,R4),APWORK      FIRST HALF IS NAME (TSPP OR XXXX)            
         LA    R4,4(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'='                                                       
         MVC   2(1,R4),CTPRGTST    MOVE TEST LEVEL                              
         B     DRDAT40                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE A(SYSTEM ELEMENT) IF IT'S THE ONE REQUESTED                    *         
***********************************************************************         
DRSYS    CLI   SYSTEM,0                                                         
         BE    DRSYS10                                                          
         USING CTSYSD,R3                                                        
         CLC   CTSYSNUM,SYSTEM                                                  
         BNE   *+8                                                              
         ST    R3,ASYSEL                                                        
DRSYS10  MVC   SYSNUMS,CTSYSNUM                                                 
         L     RF,=A(GETSEN)       GET SE NAME AND ADD TO DISPLAY LIST          
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     DRDAT40                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
* DISPLAY PRINCIPAL ID                                                *         
***********************************************************************         
DRPID    EDIT  (B2,2(R3)),(5,IDSPID),FILL=0                                     
         L     R4,AIOAREA2         SWITCH IO AREAS                              
         DROP  R2                                                               
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,2(R3)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNE   DRPIDX                                                           
         MVC   IOKEY,APRECKEY                                                   
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
DRPID20  CLI   0(R1),0             FIND POINTER ELEMENT AND DISPLAY ID          
         BE    DRPIDX                                                           
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DRPID20                                                          
         MVC   IDSPID,2(R1)                                                     
DRPIDX   B     DRDAT40                                                          
         DROP  R4                                                               
         USING CTIREC,R2                                                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY ID OPTIONS                                                  *         
***********************************************************************         
         USING CTIDOEL,R3                                                       
DROPT    MVC   IDOPTS(1),CTIDOFL1  PROCESS USERID OPTIONS ELEMENT               
         MVC   IDOPTS+1(1),CTIDOFL2                                             
*                                                                               
DROP010  MVI   IDSACCR,C'N'        TEST FOR PERSONAL PASSWORD REQD              
         TM    IDOPTS,CTIDOFPW                                                  
         BZ    DROP012                                                          
         MVI   IDSACCR,C'Y'                                                     
*                                                                               
DROP012  MVI   IDSACCR+1,C'N'      TEST FOR PQ ACCESS PASSWORD REQD             
         TM    IDOPTS,CTIDOFPP                                                  
         BZ    DROP020                                                          
         MVI   IDSACCR+1,C'Y'                                                   
*                                                                               
DROP020  TM    IDOPTS,CTIDOFGN     TEST FOR GEBERIC USER-ID                     
*&&US*&& BZ    DROP030                                                          
*&&UK*&& BZ    DROP022                                                          
         MVC   IDSPID(7),=C'GENERIC'                                            
*&&UK                                                                           
DROP022  EQU   *                   TEST FOR MASTER/PARTNER FLAG                 
         TM    IDOPTS+1,CTIDOFMA+CTIDOFPA                                       
         BNO   DROP023                                                          
         MVI   IDSMPF,C'B'                                                      
         B     DROP030                                                          
*                                                                               
DROP023  TM    IDOPTS+1,CTIDOFMA                                                
         BZ    DROP024                                                          
         MVI   IDSMPF,C'M'                                                      
         B     DROP030                                                          
*                                                                               
DROP024  TM    IDOPTS+1,CTIDOFPA                                                
         BZ    DROP030                                                          
         MVI   IDSMPF,C'P'                                                      
*&&                                                                             
DROP030  CLI   CTIDOLEN,CTIDOLNQ                                                
         BNE   DROPX                                                            
         MVC   PQGID,CTIDOPQI                                                   
         MVC   SYNNUM,CTIDOSNU                                                  
         MVC   SYNID,CTIDOSID                                                   
         MVI   IDSCWC,C'N'                                                      
         TM    IDOPTS,CTIDOFWC                                                  
         BZ    DROP032                                                          
         MVI   IDSCWC,C'Y'                                                      
DROP032  MVI   IDSPPU,C'N'                                                      
         TM    IDOPTS,CTIDOPPU                                                  
         BZ    DROP040                                                          
         MVI   IDSPPU,C'Y'                                                      
*                                                                               
DROP040  TM    IDOPTS,CTIDOFSN     TEST FOR USERID IS SYNONYM                   
         BZ    DROP050                                                          
         MVI   IDSPID,C'='                                                      
         MVC   IDSPID+1(8),SYNID                                                
         MVC   IDSAGYA,=C'  '      CLEAR AGENCY ID FIELD                        
*                                                                               
DROP050  TM    IDOPTS,CTIDOFSP     TEST FOR SYNONYM POINTER                     
         BZ    DROP060                                                          
         XC    IDSIDSY,IDSIDSY                                                  
         OI    IDSIDSYH+6,X'80'                                                 
         MVC   IDSIDSY(8),=CL8'SYNONYM='                                        
         MVC   IDSIDSY+8(10),SYNID                                              
*                                                                               
DROP060  TM    IDOPTS,CTIDOFPI     TEST FOR MASTER PQ GROUP ID                  
         BZ    DROP070                                                          
         EDIT  (B2,PQGID),(5,IDSPQI),FILL=0                                     
         L     R4,AIOAREA2         SWITCH IO AREAS                              
         DROP  R2                                                               
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PQGID                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNE   DROP070                                                          
         MVC   IOKEY,APRECKEY                                                   
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
DROP062  CLI   0(R1),0             FIND POINTER ELEMENT AND DISPLAY ID          
         BE    DROP070                                                          
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DROP062                                                          
         MVC   IDSPQI,2(R1)        DISPLAY MASTER PQ GROUP USERID               
*                                                                               
DROP070  B     DROPX                                                            
*                                                                               
DROPX    B     DRDAT40                                                          
         DROP  R3,R4                                                            
         USING CTIREC,R2                                                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY AGENCY ALPHA AND LANGUAGE                                   *         
***********************************************************************         
         USING CTAGYD,R3                                                        
DRAGY    MVC   IDSAGYA,CTAGYID                                                  
         MVC   AGAID,CTAGYID                                                    
         L     RF,=A(GETACC)       GET ACCESS RECORD INTO IOAREA3               
         A     RF,APRELO           AND SAVE ID COUNTRY CODE                     
         BASR  RE,RF                                                            
         BNE   EADL                                                             
         L     R1,ALANG                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING LANGTABD,R1         R1=A(LANGUAGE TABLE)                         
         CLC   CTAGYLNG,LANGCODE   MATCH CODE TO TABLE                          
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   IDSLANG,LANGFUL                                                  
***********************************************************************         
* DISPLAY ID TYPE CODE                                                *         
***********************************************************************         
DRIDT    LARL  RE,TYPTAB                                                        
         CLI   CTAGYIDT,0                                                       
         BE    DRDAT40                                                          
DRIDT10  CLI   0(RE),0                                                          
         BE    DRIDT20                                                          
         CLC   CTAGYIDT,0(RE)                                                   
         BE    DRIDT30                                                          
         LA    RE,L'TYPTAB(RE)                                                  
         B     DRIDT10                                                          
DRIDT20  MVI   IDSIDT,C'?'                                                      
         B     DRDAT40                                                          
DRIDT30  MVC   IDSIDT(L'TYPTAB-1),1(RE)                                         
         B     DRDAT40                                                          
         DROP  R1,R3                                                            
                                                                                
***********************************************************************         
* DISPLAY TIME OUT ELEMENT DATA                                       *         
***********************************************************************         
         USING CTTOUD,R3                                                        
DRTOU    EDIT  CTTOUADV,(4,IDSATOU),WRK=APWORK,DUB=APDUB,ALIGN=LEFT             
         B     DRDAT40                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
* DISPLAY LOTUS NOTES ADMIN USER ID                                   *         
***********************************************************************         
*&&UK                                                                           
         USING CTLNID,R3                                                        
DRLNI    EDIT  (B2,CTLNINUM),(5,IDSLNI),FILL=0                                  
         L     R4,AIOAREA2         SWITCH IO AREAS                              
         DROP  R2                                                               
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CTLNINUM                                                 
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNE   DRLN100                                                          
         MVC   IOKEY,APRECKEY                                                   
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
*                                                                               
DRLN010  CLI   0(R1),0             FIND POINTER ELEMENT AND DISPLAY ID          
         BE    DRLN100                                                          
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DRLN010                                                          
         MVC   IDSLNI,2(R1)        DISPLAY LOTUS NOTES ADMIN USER ID            
*                                                                               
DRLN100  B     DRDAT40                                                          
         DROP  R3,R4                                                            
*&&                                                                             
         USING CTIREC,R2                                                        
                                                                                
***********************************************************************         
* DISPLAY AFP USER ID                                                 *         
***********************************************************************         
         USING CTAFPD,R3                                                        
DRAFP    EDIT  (B2,CTAFPNUM),(5,IDSAFP),FILL=0                                  
         L     R4,AIOAREA2         SWITCH IO AREAS                              
         DROP  R2                                                               
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CTAFPNUM                                                 
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNE   DRAF100                                                          
         MVC   IOKEY,APRECKEY                                                   
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
*                                                                               
DRAF010  CLI   0(R1),0             FIND POINTER ELEMENT AND DISPLAY ID          
         BE    DRAF100                                                          
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DRAF010                                                          
         MVC   IDSAFP,2(R1)        DISPLAY AFP USER ID                          
*                                                                               
DRAF100  B     DRDAT40                                                          
         DROP  R3,R4                                                            
         USING CTIREC,R2                                                        
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY SENAME AND AGENCY BINARY NUMBER IN ONE FIELD     *         
***********************************************************************         
DISPSEAG NTR1                                                                   
         XC    IDSSEAG,IDSSEAG                                                  
         OI    IDSSEAGH+6,X'80'                                                 
         L     RF,ASE                                                           
         USING SELISTD,RF                                                       
         MVC   IDSSEAG(L'SENAME),SENAME                                         
         LA    R0,L'SENAME                                                      
         DROP  RF                                                               
         LA    R4,IDSSEAG                                                       
DSEAG10  CLI   0(R4),0                                                          
         BE    DSEAG20                                                          
         CLI   0(R4),C' '                                                       
         BE    DSEAG20                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,DSEAG10                                                       
*                                                                               
DSEAG20  OC    AGNUM,AGNUM                                                      
         BZ    DSEAGX                                                           
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VHEXOUT,APPARM,AGNUM,(R4),1,=C'TOG'                              
*                                                                               
DSEAGX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
VALSEL   LA    R2,APRECKEY                                                      
         USING CTIREC,R2                                                        
         XC    SELDATA,SELDATA                                                  
*                                                                               
         LA    R4,LSTIDH                                                        
         L     RF,=A(VALPARS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF               GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
         XC    APPARM(24),APPARM                                                
*                                                                               
         XC    CTIKEY,CTIKEY       BUILD AN INITIAL KEY                         
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID(L'SELID),SELID                                            
*                                                                               
         MVI   GETSEQF,0           INTERNAL READ SEQUENCE FLAG                  
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         MVC   APPARM+4(2),=H'0'   SET NUMBER OF LIST LINES                     
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
GETSEL   LA    R2,IOKEY            READ NEXT LIST RECORD                        
         USING CTIREC,R2                                                        
         MVC   CTIKEY,APRECKEY                                                  
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
         USING CTIREC,R2                                                        
DISSEL   CLI   APACTN,ACTLST       FOR LIST ONLY                                
         BNE   DISSEL2                                                          
         OI    LSTHDLNH+6,X'80'                                                 
         MVC   LSTHDLN+24(18),=C'Systems Authorised'                            
         MVC   LSTHDLN+44(8),SPACES                                             
         OI    LSTHDLUH+6,X'80'                                                 
         MVC   LSTHDLU+44(8),SPACES                                             
*&&UK                                                                           
         CLI   OPTTES,C'Y'         TEST ALTDIS OPTION                           
         BNE   DISSEL1                                                          
         MVC   LSTHDLN+24(18),=C'Limit Access data '                            
         B     DISSEL2                                                          
*&&                                                                             
DISSEL1  CLI   LSTLUD,0            TEST LAST USED DATE ENTERED                  
         BE    DISSEL2                                                          
         OI    LSTHDLNH+6,X'80'                                                 
         MVC   LSTHDLN+44(8),=C'LastUsed'                                       
         OI    LSTHDLUH+6,X'80'                                                 
         MVC   LSTHDLU+44(8),=C'--------'                                       
*                                                                               
DISSEL2  L     R4,APPARM           BUILD A LINE OF USER ID REC DATA             
         L     RF,=A(LINE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
DISSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TWA                                           *         
***********************************************************************         
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
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
VQEXF    MVI   EXFMT,0                                                          
         GOTO1 AFVAL,REPEXFMH      SEE IF EXTENDED FORMAT REQUESTED             
         BNE   VQEXFX                                                           
         CLI   FVIFLD,C'X'         EXTENDED                                     
         BE    VQEX010                                                          
         CLI   FVIFLD,C'S'         SYSTEM                                       
         BE    VQEX010                                                          
         CLI   FVIFLD,C'L'         LOTUS NOTES                                  
         BE    VQEX010                                                          
         B     EIIF                                                             
*                                                                               
VQEX010  CLI   INWHEN,MIXIOKN      TEST FOR NOW REPORT                          
         BE    ENOW                'NOW' REPORT CAN'T USE THIS FILTER           
         MVC   EXFMT,FVIFLD                                                     
VQEXFX   EQU   *                                                                
*                                                                               
         LA    R4,REPIDH                                                        
         L     RF,=A(VALPARS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF               GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
         XC    APPARM(24),APPARM                                                
*                                                                               
         LA    R2,APRECKEY         BUILD AN INITIAL KEY                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID(L'SELID),SELID                                            
*                                                                               
VQDESC   CLI   EXFMT,C'L'                                                       
         BE    VQDE010                                                          
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSI,REPMSPAC                                                
         MVI   REPFOOTN,0                                                       
         LAY   R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         B     VQDESCX                                                          
VQDE010  B     VQDESCX                                                          
*                                                                               
VQDESCX  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT USER ID LIST                                       *         
***********************************************************************         
PRTREP   L     R9,AREP                                                          
         CLI   EXFMT,C' '          TEST FOR DEFAULT REPORT FORMAT               
         BE    PREC100                                                          
         CLI   EXFMT,C'S'          TEST FOR SYSTEM REPORT FORMAT                
         BE    PREC010                                                          
         CLI   EXFMT,C'X'          TEST FOR EXTENDED REPORT FORMAT              
         BE    PREC010                                                          
         CLI   EXFMT,C'L'          TEST FOR LOTUS NOTES REPORT FORMAT           
         BE    PREC020                                                          
         B     PREC100                                                          
*                                                                               
PREC010  LA    R0,LOCALD           INITIALISE FOR S/X REPORT FORMAT             
         AH    R0,=Y(AGYTAB-LOCALD)                                             
         ST    R0,AAGYTAB                                                       
         LA    R0,LOCALD                                                        
         AH    R0,=Y(AGYTABX-LOCALD)                                            
         ST    R0,AAGYTABX                                                      
*                                  CLEAR AGENCY COUNTRY TABLE                   
         L     RE,AAGYTAB                                                       
         L     RF,AAGYTABX                                                      
         SR    RF,RE                                                            
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,IOKEY            READ SYSTEM LIST RECORD                      
         USING CTWKEY,R2           INTO AIOAREA2                                
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         JNE   *+2                                                              
         DROP  R2                                                               
         B     PREC100                                                          
*                                                                               
PREC020  L     RF,=A(BLDACT)       INITIALISE FOR L REPORT FORMAT               
         A     RF,APRELO                                                        
         BASR  RE,RF               BUILD ACCESS RECORD TABLE                    
         L     RF,=A(BLDLNI)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF               BUILD LOTUS NOTES ID TABLE                   
*                                                                               
         ICM   RF,15,=V(DLFLD)                                                  
         A     RF,APRELO                                                        
         STCM  RF,15,VDLFLD                                                     
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   DLCBACT,DLCBSOR     START OF DOWNLOAD                            
         LA    R0,DLHOOK           A(HOOK LINE)                                 
         STCM  R0,15,DLCBAPR                                                    
         LA    R0,REPP1            A(PRINT LINE)                                
         STCM  R0,15,DLCBAPL                                                    
         GOTO1 VDLFLD                                                           
         LA    R1,DLCB                                                          
         DROP R1                                                                
*                                                                               
PREC100  LA    R2,IOKEY            READ FIRST ID RECORD                         
         USING CTIREC,R2                                                        
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
         BNE   PREC500                                                          
         B     PREC200                                                          
*                                                                               
PREC110  TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    PREC120                                                          
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     PREC130                                                          
PREC120  TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    PREC140                                                          
         NI    APINDS,X'FF'-APILRERD                                            
PREC130  LA    R2,IOKEY                                                         
         MVC   CTIKEY,APRECKEY                                                  
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    PREC140                                                          
         B     PREC500                                                          
*                                                                               
PREC140  LA    R1,IOSQ+IOCONFIL+IO1                                             
         L     RF,=A(GETREC)       GO GET NEXT REC                              
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   PREC500                                                          
*                                                                               
PREC200  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
         LA    R4,REPP1-14                                                      
         CLI   EXFMT,C'L'                                                       
         BE    PREC400                                                          
         CLI   EXFMT,0                                                          
         BNE   PREC300                                                          
         L     RF,=A(LINE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF               GO BUILD A PRINT LINE                        
*                                  SWITCH AGY ALPHA AND ID NAME IN LINE         
         USING LISTD,R4                                                         
         MVC   WORK(LISTNUM-LISTID),LISTID                                      
         MVC   LISTNUM-LISTID+WORK(LISTID-LISTAGYA),LISTAGYA                    
         MVC   LISTAGYA-LISTD(LISTNUM-LISTAGYA,R4),WORK                         
         DROP  R4                                                               
*                                                                               
         GOTO1 VREPORT,REPD                                                     
         B     PREC110                                                          
*                                                                               
PREC300  L     RF,=A(EXREP)                                                     
         A     RF,APRELO                                                        
         BASR  RE,RF               GO PROCESS EXTENDED FORMAT REPORT            
         B     PREC110                                                          
*                                                                               
PREC400  L     RF,=A(LNREP)                                                     
         A     RF,APRELO                                                        
         BASR  RE,RF               PROCESS LOTUS NOTES FORMAT REPORT            
         B     PREC110                                                          
*                                                                               
PREC500  CLI   EXFMT,C'L'          TEST FOR LOTUS NOTES FORMAT                  
         BNE   PRTRECX                                                          
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   DLCBACT,DLCBEOR     END OF DOWNLOAD                              
         GOTO1 VDLFLD                                                           
         DROP R1                                                                
*                                                                               
         L     R4,LACCTAB                                                       
         FREEMAIN RC,LV=(R4),A=AACCTAB                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         L     R4,LLNITAB                                                       
         FREEMAIN RC,LV=(R4),A=ALNITAB                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
PRTRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
*                                                                               
DLHOOK   LR    R0,RE               DOWNLOAD PRINT HOOK                          
         GOTO1 VREPORT,REPD                                                     
         MVI   REPPRNTN,1                                                       
         MVI   REPLINE,1                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GETTXT MESSAGE NUMBER ERROR EXITS                                   *         
***********************************************************************         
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
EFTM     MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     NO                  TOO MANY INPUT FIELDS                        
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
EUAT     MVC   FVMSGNO,=AL2(112)                                                
         B     NO                  CAN'T END IN UAT                             
EUAM     MVC   FVMSGNO,=AL2(111)                                                
         B     NO                  User ID not UAT compatible                   
ERTB     MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD TO BIG                                
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
EATO     MVC   FVMSGNO,=AL2(CE#ADTOU)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  ADV TIMEOUT FIELD ERROR                      
ENOW     MVC   FVMSGNO,=AL2(CE#NONOW)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  CANT PRINT IN NOW MODE                       
EADL     MVC   FVMSGNO,=AL2(CE#ACDEL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,IDSAGYAH         SET CURSOR                                   
         ST    R1,FVADDR                                                        
         B     NO                  ACCESS RECORD IS DELETED                     
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
REPDESCL DC    C'USER ID LIST'                                                  
CTFILE   DC    C'CTFILE '                                                       
XAUTH    DC    XL2'FFFF'                                                        
YAUTH    DC    XL2'000F'                                                        
NAUTH    DC    XL2'0000'                                                        
SPACES   DC    16C' '                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         DROP  RB,RA,R8                                                         
*                                                                               
REPSPEC  DS    0H                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'USER ID LIST'                                            
         SPEC  H2,57,C'------------'                                            
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ ACCESS RECORD INTO IOAREA3                          *         
* NTRY - AGAID=AGENCY ALPHA ID                                        *         
* EXIT - IDCTRY=COUNTRY CODE                                          *         
***********************************************************************         
GETACC   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETACC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GAC'    INSERT NAME                                  
*                                                                               
         MVI   IDCTRY,0                                                         
         MVC   KEYSAVE,IOKEY                                                    
         L     R1,AIOAREA3                                                      
         USING CT5REC,R1                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGAID                                                   
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO3                                          
         MVC   IOKEY,KEYSAVE                                                    
         JL    *+2                                                              
         BH    GETACCNO                                                         
         L     R1,AIOAREA3                                                      
         LA    R1,CT5DATA-CT5REC(R1)                                            
         SR    RF,RF                                                            
*                                  SAVE AGENCY COUNTRY CODE                     
         USING CTAGDD,R1                                                        
GETACC4  CLI   CTAGDEL,0                                                        
         JE    *+2                                                              
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETACC4                                                          
         CLI   CTAGDLEN,CTAGDLNQ                                                
         BNH   GETACCOK                                                         
         MVC   IDCTRY,CTAGDCTY                                                  
         DROP  R1                                                               
         B     GETACCOK                                                         
*                                                                               
GETACCOK SR    RC,RC               RETURN CC EQUAL                              
GETACCNO LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD PROGRAM ACCESS LIST INTO BLOCK1 AND SET PGCNT TO NUMBER OF    *         
* ENTRIES IN BLOCK.                                                   *         
***********************************************************************         
DISPSYS  NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DISPSYS,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DSY'    INSERT NAME                                  
*                                                                               
         L     R3,ASYSEL                                                        
         USING CTSYSEL,R3                                                       
         GOTO1 ADISSE,CTSYSSE                                                   
         OC    APPARM,APPARM                                                    
         JZ    *+2                                                              
         MVC   ASE,APPARM                                                       
         L     RE,ASE                                                           
         MVC   APGM,SEPGMS-SELISTD(RE)                                          
         LA    R4,CTSYSPGM         R4=A(AUTHS)                                  
         LLC   R9,CTSYSLEN         R9=LENGTH                                    
         MVI   PGCNT,0             SET COUNT                                    
         CLI   CTSYSLEN,16         CHECK FOR ALL=VALUE ONLY                     
         BE    DISPSYS8                                                         
*                                                                               
DISPSYS2 CHI   R9,16                                                            
         BNH   DISPSYS8                                                         
         MVC   PROGRAM,0(R4)                                                    
         LA    R4,1(R4)            POINT TO THE AUTHORIZATION CODE              
*                                  GET PROGRAM NAME                             
         L     RF,=A(GETPGAN)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'       CHECK IF NOT FOUND                           
         BE    DISPSYS6                                                         
*                                                                               
DISPSYS4 LLC   R1,PGCNT            BUMP BLOCK COUNT                             
         LA    RE,1(R1)                                                         
         STC   RE,PGCNT                                                         
         MHI   R1,20                                                            
         LA    R1,BLOCK1(R1)       GET A(BLOCK ENTRY)                           
         MVI   0(R1),C' '                                                       
         MVC   1(19,R1),0(R1)                                                   
         LR    R8,R1                                                            
*                                                                               
         LHI   RF,4                DEFAULT: 4-CHAR PROGRAM NAME                 
         CLI   CTSYSNUM,SYSACCQ    ACC?                                         
         BNE   DISPSYS5                                                         
         CLI   PROGRAM,X'ED'       PROGRAM=TIMEOFF?                             
         BNE   DISPSYS5                                                         
         LHI   RF,5                5-CHAR PROGRAM NAME FOR ACC/TIMEOFF          
DISPSYS5 BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),PGNAME                                                   
         AHI   RF,1                                                             
         LA    R8,0(R8,RF)                                                      
*                                                                               
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C'='          AUTH IS Y N OR XXXX                          
         MVI   2(R8),C'Y'          AUTH IS Y N OR XXXX                          
         CLC   0(2,R4),=XL2'000F'                                               
         BE    DISPSYS6                                                         
         MVI   2(R8),C'N'                                                       
         CLC   0(2,R4),=XL2'0000'                                               
         BE    DISPSYS6                                                         
         GOTO1 VHEXOUT,APPARM,(R4),2(R8),2,=C'TOG'                              
*                                                                               
DISPSYS6 LA    R8,CTSYSALL         EXIT IF ALL=VALUE JUST DONE                  
         CR    R8,R4                                                            
         BE    DISPSYSX                                                         
         LA    R4,2(R4)            NEXT PROGRAM                                 
         AHI   R9,-3                                                            
         B     DISPSYS2                                                         
*                                                                               
DISPSYS8 LA    R4,CTSYSALL                                                      
         MVC   PGNAME,=CL8'ALL'                                                 
         B     DISPSYS4                                                         
*                                  DISPLAY REST OF ELEMENT                      
DISPSYSX XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET PROGRAM NAME FROM PROGRAM ACCESS NUMBER                         *         
***********************************************************************         
GETPGAN  NTR1  BASE=*                                                           
         L     R3,APGM                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GETPGAN2 CLC   PGMNUM,PROGRAM      MATCH ON PROGRAM NUMBER                      
         BE    GETPGANY                                                         
         CLI   PGMALNUM,0                                                       
         BE    *+14                                                             
         CLC   PGMALNUM,PROGRAM    OR ACCESS OVERRIDE (IF SET)                  
         BE    GETPGANY                                                         
GETPGAN4 BXLE  R3,RE,GETPGAN2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGANX                                                         
GETPGANY CLC   PGMCTRY,IDCTRY                                                   
         BE    *+12                                                             
         CLI   PGMCTRY,0                                                        
         BNE   GETPGAN4                                                         
         MVC   PGNAME,PGMNAME                                                   
GETPGANX XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF SYSTEM SHORT NAMES IN DISPLAY HEADER BUFFER           *         
***********************************************************************         
GETSEN   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETSEN,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GSN'    INSERT NAME                                  
*                                                                               
GETSEN1  CLI   SYCNT,0             TEST FIRST CALL - NUMBER OF ITEMS            
         BNE   GETSEN2                                                          
         MVI   SYSNAMS,C' '                                                     
         MVC   SYSNAMS+1(L'SYSNAMS-1),SYSNAMS                                   
         MVC   SYSNAMS(7),=C'USER ID'                                           
         NC    SYSNAMS+1(3),=8X'BF'                                             
         MVC   SYSNAMS+9(7),=C'SYSTEMS'                                         
         NC    SYSNAMS+10(6),=8X'BF'                                            
         LA    RE,SYSNAMS+18       SET A(NEXT ENTRY)                            
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
         ICM   R1,1,SYCNT          TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYCNT            BUMP ITEM COUNT                              
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
***********************************************************************         
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                             *         
* R4=A(FIRST FIELD HEADER IN STANDARD DISPLAY)                        *         
* APPLICABLE TO BOTH LIST AND REPORT SCREEN FIELD OFFSETS             *         
***********************************************************************         
         USING CTIREC,R2                                                        
         USING LSTIDH,R4                                                        
VALPARS  NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALPARS,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VPA'    INSERT NAME                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VPID     GOTO1 AFVAL,LSTIDH        STORE USER ID IF ENTERED                     
         BNE   VPIDX                                                            
         LLC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VPID1    CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VPID2               FOR KEY COMPARE IN GETREC                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,VPID1                                                         
VPID2    STC   RE,SELKEYCL                                                      
         MVC   SELID,FVIFLD                                                     
         MVC   SELIDL,FVILEN                                                    
         MVC   SELIDSP,0(RF)                                                    
VPIDX    EQU   *                                                                
*                                                                               
VPSYS    GOTO1 AFVAL,LSTSYSH       VALIDATE SYSTEM                              
         BNE   VPSYSX                                                           
         L     RF,ASYSLST          LOOK UP NAME IN SYSLST                       
         LA    RF,6(RF)                                                         
         USING SYSLSTD,RF                                                       
         LLC   RE,FVILEN                                                        
         BCTR  RE,0                                                             
VPSYS3   CLI   SYSLNUM,0                                                        
         BE    VPSYS1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD                                               
         BE    VPSYS4                                                           
         LA    RF,SYSLLEN(RF)                                                   
         B     VPSYS3                                                           
VPSYS4   MVC   SELSYS,SYSLNUM                                                   
         B     VPSYSX                                                           
*                                                                               
VPSYS1   CLI   FVILEN,L'SENAME                                                  
         BH    VPSYSE                                                           
         MVC   APWORK(L'SENAME),FVIFLD                                          
         GOTO1 GETSE               GET SYSTEM LIST INFO                         
         BNE   VALPARSX                                                         
         MVC   SELSEN,APWORK                                                    
         B     VPSYSX                                                           
VPSYSE   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPSYSX   EQU   *                                                                
*                                                                               
VPPGM    GOTO1 AFVAL,LSTPGMH       VALIDATE PROGRAM                             
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
VPAGY    GOTO1 AFVAL,LSTAGYAH      VALIDATE AGENCY ALPHA ID                     
         BNE   VPAGYX                                                           
         USING CT5REC,R1                                                        
         LA    R1,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         JL    *+2                                                              
         BE    VPAGY2                                                           
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALPARSX            ACCESS RECORD NOT FOUND                      
VPAGY2   MVC   SELAGY,FVIFLD                                                    
         B     VPAGYX                                                           
VPAGY1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPAGYX   EQU   *                                                                
*                                                                               
VPPWD    GOTO1 AFVAL,LSTPWDH       VALIDATE PASSWORD REQUIRED                   
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
VPCID    GOTO1 AFVAL,LSTCIDH       VALIDATE COMPATIBLE USER ID                  
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
         JL    *+2                                                              
         BNE   VPCID1                                                           
         MVC   IOKEY,APRECKEY                                                   
         MVC   SELCID,CTIKID                                                    
         B     VPCIDX                                                           
VPCID1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VPCIDX   EQU   *                                                                
*                                                                               
VPULD    GOTO1 AFVAL,LSTLUDH       VALIDATE LAST USED DATE SMMMYY               
         BNE   VPULDX                                                           
*&&UK*&& CLI   OPTTES,C'Y'         NOT VALID FOR ALTDIS OPTION                  
*&&UK*&& BE    VPULD3                                                           
         CLI   FVIFLD,C'<'         FIRST BYTE IS COMPARE ACTION                 
         BE    VPULD1                                                           
         CLI   FVIFLD,C'='                                                      
         BE    VPULD1                                                           
         CLI   FVIFLD,C'>'                                                      
         BNE   VPULD3                                                           
VPULD1   GOTO1 VDATVAL,APPARM,(2,FVIFLD+1),(0,DATEFLD)                          
         CLC   DATEFLD(2),=C'00'                                                
         BE    VPULD3                                                           
         GOTO1 VDATCON,APPARM,(0,DATEFLD),(3,FULL)                              
         LLC   RE,FULL                                                          
         AHI   RE,-109             USERID START YEAR 2009                       
         BM    VPULD3                                                           
         MHI   RE,12                                                            
         LLC   RF,FULL+1                                                        
         LA    RE,1(RE,RF)         RELATIVE MONTH NUM FROM START YEAR           
         AHI   RE,-1               USERID START MONTH JAN                       
         BP    *+6                                                              
VPULD2   SR    RE,RE                                                            
         STC   RE,SELLUDM          RELATIVE MONTH NUMBER SINCE START            
         MVC   SELLUDA,FVIFLD                                                   
         B     VPULDX                                                           
VPULD3   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPULDX   EQU   *                                                                
*                                                                               
VPSYN    GOTO1 AFVAL,LSTSYNH       VALIDATE SYNONYM Y/N                         
         BNE   VPSYNX                                                           
         CLI   FVIFLD,C'Y'                                                      
         BE    VPSYN2                                                           
         CLI   FVIFLD,C'N'                                                      
         BNE   VPSYN1                                                           
VPSYN2   MVC   SELSYN,FVIFLD                                                    
         B     VPSYNX                                                           
VPSYN1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPSYNX   EQU   *                                                                
*                                                                               
VPIDT    GOTO1 AFVAL,LSTIDTH       VALIDATE ID TYPE                             
         BNE   VPIDTX                                                           
         LLC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL9'NONE     '                                        
         BNE   VPIDT02                                                          
         MVC   LSTIDT(L'TYPTAB-1),=CL9'NONE'                                    
         OI    LSTIDTH+6,X'80'                                                  
         MVI   SELIDT,C'-'                                                      
         B     VPIDTX                                                           
VPIDT02  CLI   FVIFLD,C'-'                                                      
         BNE   VPIDT04                                                          
         MVC   LSTIDT(L'TYPTAB-1),=CL9'NONE'                                    
         OI    LSTIDTH+6,X'80'                                                  
         MVI   SELIDT,C'-'                                                      
         B     VPIDTX                                                           
VPIDT04  LARL  RE,TYPTAB                                                        
VPIDT10  CLI   0(RE),0                                                          
         BE    VPIDT30                                                          
         LLC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),1(RE)                                                  
         BE    VPIDT20                                                          
         LA    RE,L'TYPTAB(RE)                                                  
         B     VPIDT10                                                          
VPIDT20  MVC   LSTIDT(L'TYPTAB-1),1(RE)                                         
         OI    LSTIDTH+6,X'80'                                                  
         MVC   SELIDT,0(RE)                                                     
         B     VPIDTX                                                           
VPIDT30  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPIDTX   EQU   *                                                                
*                                                                               
VPUSE    GOTO1 AFVAL,LSTUSEDH      VALIDATE %USED                               
         BNE   VPUSEX                                                           
         GOTO1 =V(NUMVAL),APPARM,LSTUSED,(X'01',0),RR=APRELO                    
         CLI   0(R1),X'FF'                                                      
         BE    VPUSE1                                                           
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    VPUSE1                                                           
         C     R1,=F'100'                                                       
         BNL   VPUSE1                                                           
         MHI   R1,10                                                            
         AHI   R1,-8                                                            
         STCM  R1,3,SELUSED                                                     
         B     VPUSEX                                                           
VPUSE1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPUSEX   EQU   *                                                                
*                                                                               
VPLMT    GOTO1 AFVAL,LSTLMTH       VALIDATE LIMIT ACCESS                        
         BNE   VPLMTX                                                           
         OC    SELSYS,SELSYS       INVALID WITHOUT SYS                          
         BZ    VPLMT1                                                           
         MVC   SELLMT,FVIFLD                                                    
         B     VPLMTX                                                           
*                                                                               
VPLMT1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPLMTX   EQU   *                                                                
*                                                                               
VALPARSX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM EXECUTIVE LIST VALUES                                    *         
***********************************************************************         
GETSE    NTR1                                                                   
         L     R1,ASYSFACS         GET SYSTEM LIST INFO                         
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SENAME,APWORK                                                    
         BE    GETSEOK                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
         LA    R0,1                                                             
         B     GETSEX                                                           
*                                                                               
GETSEOK  MVC   APWORK(2),SESYS                                                  
         SR    R0,R0                                                            
*                                                                               
GETSEX   LTR   R0,R0                                                            
         XIT1                                                                   
         DROP  R1                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS        *         
***********************************************************************         
         USING CTIREC,R2                                                        
GETREC   NTR1  LABEL=NO                                                         
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
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(CTIKID-CTIKEY),CTIKEY                                   
         BNE   GETRECN                                                          
         OC    CTIKID-CTIKEY(10,R2),CTIKID-CTIKEY(R2)                           
         BZ    GETRECN                                                          
*                                                                               
GRID     CLI   SELID,C'>'          TEST USER ID >N FOR LENGTH COMPARE           
         BNE   GRID2                                                            
         CLI   SELID+1,C'0'                                                     
         BL    GRID2                                                            
         CLI   SELID+1,C'9'                                                     
         BH    GRID2                                                            
         MVC   DUB(1),SELID+1                                                   
         NI    DUB,X'0F'                                                        
         LLC   R1,DUB                                                           
         LA    RE,CTIKID(R1)                                                    
         CLI   0(RE),C' '                                                       
         BH    GRIDX                                                            
         B     GETRECRD                                                         
GRID2    CLI   SELIDSP,C' '       USER ID - FILTER ONLY IF IT                   
         BNH   GRIDX                 CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GRID3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTIKID(0),SELID                                                  
         BH    GETRECN             (NO MORE RELEVENT RECORDS)                   
GRID3    GOTO1 ATXTFLT,APPARM,(SELIDL,SELID),(8,CTIKID)                         
         BNE   GETRECRD            READ NEXT RECORD                             
GRIDX    EQU   *                                                                
*                                                                               
GRSYN    OC    SELSYN,SELSYN       FILTER SYNONYM                               
         BZ    GRSYNX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'07'        GET IDOPTS ELEMENT                           
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         USING CTIDOD,R3                                                        
         CLI   SELSYN,C'Y'                                                      
         BNE   GRSYN10                                                          
         TM    CTIDOFL1,CTIDOFSN                                                
         BZ    GETRECRD                                                         
         B     GRSYNX                                                           
GRSYN10  TM    CTIDOFL1,CTIDOFSN                                                
         BNZ   GETRECRD                                                         
GRSYNX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GRSYS    OC    SELSYS,SELSYS       FILTER ON SYSTEM                             
         BZ    GRSYSX                                                           
         LR    R3,R2                                                            
         MVI   APELEM,X'21'        GET SYSTEM ELEMS                             
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         B     GRSYS3                                                           
GRSYS2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECRD            READ NEXT RECORD                             
         CLI   0(R3),X'21'                                                      
         BNE   GRSYS2                                                           
         USING CTSYSD,R3                                                        
GRSYS3   CLC   CTSYSNUM,SELSYS                                                  
         BNE   GRSYS2                                                           
         B     GRPGM                                                            
GRSYSX   EQU   *                                                                
*                                                                               
GRSEN    OC    SELSEN,SELSEN       FILTER ON SE NUMBER                          
         BZ    GRPGMX                                                           
         LR    R3,R2                                                            
         MVI   APELEM,X'21'        GET SYSTEM ELEMS                             
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         B     GRSEN3                                                           
GRSEN2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECRD            READ NEXT RECORD                             
         CLI   0(R3),X'21'                                                      
         BNE   GRSEN2                                                           
         USING CTSYSD,R3                                                        
GRSEN3   CLC   CTSYSSE,SELSEN                                                   
         BNE   GRSEN2                                                           
GRSENX   EQU   *                                                                
*                                                                               
GRPGM    OC    SELPGM,SELPGM       FILTER ON PROGRAM                            
         BZ    GRPGMX                                                           
         MVC   PROGRAM,SELPGM                                                   
         LA    R1,CTSYSPGM         POINT TO SYSTEM ELEMENT                      
         LLC   RE,CTSYSLEN                                                      
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
GRLMT    OC    SELLMT,SELLMT       FILTER ON LIMIT ACCESS                       
         BZ    GRLMTX                                                           
*&&UK*&& CLI   SELSYS,4            MEDIA                                        
*&&UK*&& BE    GRLM040                                                          
*&&UK*&& CLI   SELSYS,6            ACCOUNTING                                   
*&&UK*&& BNE   GRLMTX                                                           
         CLI   SELLMT,C'*'                                                      
         BNE   GRLM010                                                          
         CLI   CTSYSLMT,C'*'                                                    
         BNE   GETRECRD                                                         
         CLI   SELLMT+1,C'?'                                                    
         BNE   GRLM020                                                          
         CLI   CTSYSLMT+1,C' '                                                  
         BE    GETRECRD                                                         
         CLI   CTSYSLMT+1,0                                                     
         BE    GETRECRD                                                         
*                                                                               
         CLI   SELLMT+2,C'?'                                                    
         BNE   GRLM030                                                          
         CLI   CTSYSLMT+2,C' '                                                  
         BE    GETRECRD                                                         
         CLI   CTSYSLMT+2,0                                                     
         BE    GETRECRD                                                         
         B     GRLMTX                                                           
*                                                                               
GRLM010  CLC   CTSYSLMT,SELLMT                                                  
         BNE   GETRECRD                                                         
         B     GRLMTX                                                           
GRLM020  CLC   CTSYSLMT+1(3),SELLMT+1                                           
         BNE   GETRECRD                                                         
         B     GRLMTX                                                           
GRLM030  CLC   CTSYSLMT+2(2),SELLMT+2                                           
         BNE   GETRECRD                                                         
         B     GRLMTX                                                           
*&&UK                                                                           
GRLM040  CLI   SELLMT,C'1'         MEDIA SYSTEM                                 
         BL    GRLMTX                                                           
         CLI   SELLMT,C'4'                                                      
         BH    GRLMTX                                                           
         LLC   RE,SELLMT                                                        
         SHI   RE,X'F1'                                                         
         LA    RE,CTSYSLMT(RE)                                                  
         CLI   SELLMT+2,C'0'                                                    
         BL    GRLMTX                                                           
         CLI   SELLMT+2,C'9'                                                    
         BH    GRLMTX                                                           
         MVC   HALF,=C'00'                                                      
         MVC   HALF+1(1),SELLMT+2                                               
         CLI   SELLMT+3,C' '                                                    
         BE    GRLM042                                                          
         CLI   SELLMT+3,C'0'                                                    
         BL    GRLMTX                                                           
         CLI   SELLMT+3,C'9'                                                    
         BH    GRLMTX                                                           
         MVC   HALF,SELLMT+2                                                    
GRLM042  PACK  DUB,HALF                                                         
         CVB   RF,DUB                                                           
         STC   RF,HALF                                                          
         CLI   SELLMT+1,C'='                                                    
         BNE   GRLM050                                                          
         CLC   HALF(1),0(RE)                                                    
         BNE   GETRECRD                                                         
         B     GRLMTX                                                           
GRLM050  CLI   SELLMT+1,C'#'                                                    
         BNE   GRLMTX                                                           
         CLC   HALF(1),0(RE)                                                    
         BE    GETRECRD                                                         
         B     GRLMTX                                                           
*&&                                                                             
GRLMTX   EQU   *                                                                
*                                                                               
GRAGY    OC    SELAGY,SELAGY       FILTER ON AGENCY ALPHA ID                    
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
GRIDT    OC    SELIDT,SELIDT       FILTER ON ID TYPE                            
         BZ    GRIDTX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ     GET ID TYPE ELEMENT                          
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GRIDT10                                                          
         USING CTAGYD,R3                                                        
         CLI   CTAGYIDT,0                                                       
         BE    GRIDT10                                                          
         CLC   SELIDT,CTAGYIDT                                                  
         BNE   GETRECRD            READ NEXT RECORD                             
         B     GRIDTX                                                           
GRIDT10  CLI   SELIDT,C'-'                                                      
         BNE   GETRECRD            READ NEXT RECORD                             
GRIDTX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GRPWD    OC    SELPWD,SELPWD       FILTER ON PASSWORD REQUIRED                  
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
GRCID    OC    SELCID,SELCID       FILTER ON COMPATIBLE USER ID                 
         BZ    GRCIDX                                                           
*                                  BUILD COMPATIBLE ID TABLE                    
         OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         GOTO1 VGETIDS,APPARM,(C'C',(R2)),0,VDMGR                               
         CLI   0(R1),X'FF'         CHECK IO ERROR                               
         JE    *+2                                                              
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
GRLUD    CLI   SELLUDA,0           FILTER ON LAST USED DATE                     
         BE    GRLUDX                                                           
         MVC   BYTE,CTISTAT        STATUS BYTE HAS LAST RELATIVE MONTH          
         NI    BYTE,X'7F'          RELATIVE MONTH 01=JAN2009                    
GRLUD1   CLC   BYTE,SELLUDM                                                     
         BL    GRLUD2                                                           
         BH    GRLUD3                                                           
         CLI   SELLUDA,C'='                                                     
         BNE   GETRECRD                                                         
         B     GRLUDX                                                           
GRLUD2   CLI   SELLUDA,C'<'                                                     
         BNE   GETRECRD                                                         
         B     GRLUDX                                                           
GRLUD3   CLI   SELLUDA,C'>'                                                     
         BNE   GETRECRD                                                         
GRLUDX   EQU   *                                                                
*                                                                               
GRUSE    OC    SELUSED,SELUSED     FILTER ON %USED                              
         BZ    GRUSEX                                                           
         CLC   CTILEN,SELUSED                                                   
         BL    GETRECRD                                                         
GRUSEX   EQU   *                                                                
*                                                                               
GETRECY  SR    RC,RC               RETURN CC EQUAL RECORD OK                    
GETRECN  LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD A LINE OF USER ID RECORD DATA                                 *         
***********************************************************************         
         USING CTIREC,R2                                                        
LINE     NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING LINE,RB                                                          
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+LIN'    INSERT NAME                                  
*                                                                               
         L     R2,AIOAREA1                                                      
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTID,CTIKID                                                    
*                                                                               
LNNUM    MVI   APELEM,X'02'        GET ID# ELEMENT                              
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNNUMX                                                           
         LA    R3,2(R3)                                                         
         LA    R8,LISTNUM                                                       
         EDIT  (B2,(R3)),(5,0(R8)),ALIGN=LEFT                                   
LNNUMX   EQU   *                                                                
*                                                                               
LNAGYA   CLI   APMODE,APMREPP      AGENCY ALPHA ID                              
         BNE   LNAGYAX                                                          
         MVI   APELEM,CTAGYELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNAGYAX                                                          
         USING CTAGYD,R3                                                        
         MVC   LISTAGYA,CTAGYID                                                 
         DROP  R3                                                               
LNAGYAX  EQU   *                                                                
*                                                                               
LNOPT    MVI   APELEM,X'07'        ID OPTIONS ELEMENT                           
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNOPTX                                                           
         USING CTIDOD,R3                                                        
         TM    CTIDOFL1,CTIDOFSN                                                
         BZ    LNOPT1                                                           
*                                  PROCESS SYNONYM ID                           
         MVC   LISTSID(8),=CL8'SYNONYM='                                        
         MVC   LISTSID+8(10),CTIDOSID                                           
         B     LINEX                                                            
*                                                                               
LNOPT1   MVI   LISTPWD,C'N'                                                     
         TM    CTIDOFL1,CTIDOFPW                                                
         BZ    LNOP010                                                          
         MVI   LISTPWD,C'Y'                                                     
LNOP010  MVI   LISTPWD+1,C'N'                                                   
         TM    CTIDOFL1,CTIDOFPP                                                
         BZ    LNOPTX                                                           
         MVI   LISTPWD+1,C'Y'                                                   
LNOPTX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
LNIDT    MVI   APELEM,CTAGYELQ     ID TYPE                                      
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNIDTX                                                           
         USING CTAGYD,R3                                                        
*                                                                               
         LARL  RE,TYPTAB                                                        
         CLI   CTAGYIDT,0                                                       
         BE    LNIDTX                                                           
LNIDT10  CLI   0(RE),0                                                          
         BE    LNIDT20                                                          
         CLC   CTAGYIDT,0(RE)                                                   
         BE    LNIDT30                                                          
         LA    RE,L'TYPTAB(RE)                                                  
         B     LNIDT10                                                          
LNIDT20  MVI   LISTIDT,C'?'                                                     
         B     LNIDTX                                                           
LNIDT30  MVC   LISTIDT,0(RE)                                                    
LNIDTX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
LNSYS    EQU   *                   SYSTEMS AUTHORISED                           
*&&UK*&& CLI   OPTTES,C'Y'         TEST ALTDIS OPTION                           
*&&UK*&& BE    LNLIM                                                            
         LA    R8,APWORK                                                        
         MVI   0(R8),C' '                                                       
         MVC   1(63,R8),0(R8)                                                   
         LA    R3,CTIDATA                                                       
LNSYS1   CLI   0(R3),X'21'                                                      
         BE    LNSYS2A                                                          
LNSYS2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNSYS5                                                           
         B     LNSYS1                                                           
         USING CTSYSD,R3                                                        
LNSYS2A  L     RF,ASYSLST          LOOK UP NAME IN SYSLST                       
         LA    RF,6(RF)                                                         
         USING SYSLSTD,RF                                                       
LNSYS3   CLI   SYSLNUM,0                                                        
         BE    LNSYS2                                                           
         CLC   SYSLNUM,CTSYSNUM                                                 
         BE    LNSYS4                                                           
         LA    RF,SYSLLEN(RF)                                                   
         B     LNSYS3                                                           
LNSYS4   MVC   0(3,R8),SYSLNAME                                                 
         LA    R8,4(R8)                                                         
         B     LNSYS2                                                           
         DROP  RF                                                               
LNSYS5   GOTO1 =V(SQUASHER),APPARM,APWORK,64,RR=RB                              
         GOTO1 =V(CHOPPER),APPARM,(64,APWORK),(27,LISTSYS),1,RR=RB              
         CLI   SELLUDA,0                                                        
         BE    LNIDS                                                            
LNSYS6   MVC   FULL(1),CTISTAT     STATUS BYTE HAS LAST USED MONTH              
         NI    FULL,X'7F'                                                       
         SR    RE,RE                                                            
         LLC   RF,FULL             RF=RELATIVE MONTH 01=JAN2009                 
         AHI   RF,-1                                                            
         BNM   *+14                                                             
         MVC   FULL(3),=X'640100'  IF NOT USED SET MONTH TO JAN2000             
         B     LNSYS7                                                           
         D     RE,=F'12'                                                        
         AHI   RE,1                RE=MONTH                                     
         AHI   RF,109              RF=YEAR                                      
         STC   RF,FULL                                                          
         STC   RE,FULL+1                                                        
         MVI   FULL+2,0                                                         
LNSYS7   GOTO1 VDATCON,APPARM,(3,FULL),(22,DATEFLD)                             
         MVC   LISTSYS+20(8),DATEFLD                                            
         CLC   FULL(3),=X'640100'                                               
         BNE   *+10                                                             
         MVC   LISTSYS+20(8),=C'...     '                                       
LNSYSX   B     LNIDS                                                            
*&&UK                                                                           
LNLIM    LA    R3,CTIDATA                                                       
LNLIM1   CLI   0(R3),CTSYSELQ                                                   
         BE    LNLIM3                                                           
LNLIM2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNIDS                                                            
         B     LNLIM1                                                           
         USING CTSYSD,R3                                                        
LNLIM3   GOTO1 ADISLACC,APPARM,(CTSYSNUM,CTSYSLMT),AGAID                        
         CLI   APWORK,X'41'                                                     
         BL    LNLIM2                                                           
         CLI   CTSYSNUM,6          ACCOUNTING                                   
         BE    LNLIM4                                                           
         CLI   CTSYSNUM,4          MEDIA                                        
         BNE   LNLIM2                                                           
         MVC   LISTLIMM,APWORK                                                  
         MVC   LISTLIXM,=C'M='                                                  
         B     LNLIM2                                                           
LNLIM4   MVC   LISTLIMA,APWORK                                                  
         MVC   LISTLIXA,=C'A='                                                  
         B     LNLIM2                                                           
         DROP  R3                                                               
*&&                                                                             
LNIDS    LA    R8,APWORK           ID LIST                                      
         MVI   0(R8),C' '                                                       
         MVC   1(79,R8),0(R8)                                                   
         LA    R9,7                                                             
         LR    R3,R2                                                            
         MVI   APELEM,X'1F'        GET PRINCIPAL ID ELEM                        
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNIDS1                                                           
         USING CTPID,R3                                                         
         MVC   0(10,R8),CTPID                                                   
         LA    R8,11(R8)                                                        
LNIDS1   LR    R3,R2                                                            
         MVI   APELEM,X'20'        GET ORDINARY ID ELEMS                        
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNIDS5                                                           
         B     LNIDS4                                                           
LNIDS2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNIDS5                                                           
         CLI   0(R3),X'20'                                                      
         BNE   LNIDS2                                                           
         USING CTIDD,R3                                                         
LNIDS4   CLC   CTID(2),=X'0000'    L=LISTNAME ELEMENT                           
         BNE   LNIDS4A                                                          
         MVC   0(2,R8),=C'L='                                                   
         MVC   2(8,R8),CTID+2                                                   
         B     LNIDS4C                                                          
LNIDS4A  CLC   CTID(2),=X'0001'    A=AG ELEMENT                                 
         BNE   LNIDS4B                                                          
         MVC   0(2,R8),=C'A='                                                   
         MVC   2(2,R8),CTIDAGY                                                  
         B     LNIDS4C                                                          
LNIDS4B  MVC   0(10,R8),CTID                                                    
LNIDS4C  LA    R8,11(R8)                                                        
         BCT   R9,LNIDS2                                                        
LNIDS5   GOTO1 =V(SQUASHER),APPARM,APWORK,80,RR=RB                              
         GOTO1 =V(CHOPPER),APPARM,(80,APWORK),(23,LISTIDS),1,RR=RB              
*                                                                               
LINEX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD FOR REPORT IN EXTENDED FORMAT MODE                   *         
* THIS CODE WAS DERIVED FROM OLD CON/REQ/50 REPORT CTREP5002          *         
***********************************************************************         
EXREP    NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING EXREP,RB                                                         
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+EXR'    INSERT NAME                                  
         USING CTIREC,R2                                                        
*                                  USER ID NAME LINE                            
ERNA     MVC   REPP1(20),=CL20'IDENTIFICATION'                                  
         MVC   REPP1+19(10),CTIKID                                              
         GOTO1 VREPORT,REPD                                                     
ERNAX    EQU   *                                                                
*                                  ACTIVITY AND ID NUMBER                       
ERIN     MVC   REPP1(20),=CL20'ID NUMBER'                                       
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'01'        GET ACTIVITY ELEMENT                         
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    ERIN010                                                          
         USING CTACTD,R3                                                        
         GOTO1 VDATCON,APPARM,(3,CTACTDT),(8,REPP1+94)                          
         DROP  R3                                                               
ERIN010  XC    APELEM,APELEM                                                    
         MVI   APELEM,X'02'        GET ID NUMBER ELEMENT                        
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    ERINX                                                            
         MVC   REPP1+19(13),ERSPACES                                            
         USING CTDSCD,R3                                                        
         EDIT  (2,CTDSC),(4,REPP1+19),ALIGN=LEFT                                
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
         DROP  R3                                                               
ERINX    EQU   *                                                                
*                                                                               
ERAG     MVC   AGAID,ERSPACES      AGENCY ELEMENT                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'06'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    ERAGX                                                            
         MVC   AGAID,2(R3)                                                      
ERAGX    EQU   *                                                                
*                                                                               
ERSY     CLI   EXFMT,C'S'          SYSTEM ELEMENTS                              
         BNE   ERCL                                                             
         BAS   RE,GETCTRY                                                       
         MVC   REPP1(40),=C'SYSTEM   SYSTEM   S.E.    AGENCY  AGENCY'           
         MVC   REPP2(40),=C'------   NUMBER  NUMBER   BINARY  ALPHA.'           
         MVC   REPP1+40(38),=C'  ACCESS   PROGRAM AUTHORIZATION LIST'           
         MVC   REPP2+40(38),=C'  LIMIT    --------------------------'           
         GOTO1 VREPORT,REPD                                                     
         LA    R3,CTIDATA                                                       
ERSY010  CLI   0(R3),0                                                          
         BE    ERSYX                                                            
         CLI   0(R3),X'21'                                                      
         BE    ERSY030                                                          
ERSY020  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     ERSY010                                                          
ERSY030  BAS   RE,CLEAR                                                         
         BAS   RE,AUTH                                                          
         B     ERSY020                                                          
*                                                                               
ERSYX    OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
*                                                                               
ERCL     BAS   RE,CLEAR            COMPATIBLE USER ID LIST                      
         LA    R3,CTIDATA                                                       
ERCL010  CLI   0(R3),0                                                          
         BE    ERCL040                                                          
         CLI   0(R3),X'20'                                                      
         BE    ERCL030                                                          
ERCL020  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     ERCL010                                                          
ERCL030  BAS   RE,IDLIST                                                        
         B     ERCL020                                                          
ERCL040  CLI   AREA,C' '                                                        
         BE    ERCLX                                                            
         MVC   REPP1(7),=C'ID LIST'                                             
         MVC   REPP2(7),=40C'-'                                                 
         GOTO1 =V(SQUASHER),APPARM,AREA,1000,RR=APRELO                          
         GOTO1 =V(CHOPPER),APPARM,(250,AREA),(60,REPP1+41),(C'P',4),   +        
               RR=APRELO                                                        
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
ERCLX    EQU   *                                                                
*                                                                               
ERDL     BAS   RE,CLEAR            DESTINATION USER ID LIST                     
         LA    R3,CTIDATA                                                       
ERDL010  CLI   0(R3),0                                                          
         BE    ERDL040                                                          
         CLI   0(R3),X'34'                                                      
         BE    ERDL030                                                          
ERDL020  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     ERDL010                                                          
ERDL030  BAS   RE,VALDEST                                                       
         B     ERDL020                                                          
ERDL040  CLI   AREA,C' '                                                        
         BE    ERDLX                                                            
         MVC   REPP1(26),=C'LIST OF VALID DESTINATIONS'                         
         MVC   REPP2(26),=40C'-'                                                
         GOTO1 =V(SQUASHER),APPARM,AREA,1000,RR=APRELO                          
         GOTO1 =V(CHOPPER),APPARM,(250,AREA),(60,REPP1+41),(C'P',4),   +        
               RR=APRELO                                                        
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
ERDLX    EQU   *                                                                
*                                                                               
ERPR     BAS   RE,CLEAR            VALID PRINTER LIST                           
         LA    R3,CTIDATA                                                       
ERPR010  CLI   0(R3),0                                                          
         BE    ERPR040                                                          
         CLI   0(R3),X'3A'                                                      
         BE    ERPR030                                                          
ERPR020  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     ERPR010                                                          
ERPR030  BAS   RE,VALPRINT                                                      
         B     ERPR020                                                          
ERPR040  CLI   AREA,C' '                                                        
         BE    ERPRX                                                            
         MVC   REPP1(22),=C'LIST OF VALID PRINTERS'                             
         MVC   REPP2(22),=40C'-'                                                
         GOTO1 =V(SQUASHER),APPARM,AREA,1000,RR=APRELO                          
         GOTO1 =V(CHOPPER),APPARM,(250,AREA),(60,REPP1+41),(C'P',4),   +        
               RR=APRELO                                                        
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
ERPRX    EQU   *                                                                
*                                                                               
EROR     XC    APELEM,APELEM       ORIGIN DETAILS                               
         MVI   APELEM,X'36'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    ERORX                                                            
         USING CTORGD,R3                                                        
         MVC   REPP1(33),=C'DETAILS OF THIS ORIGIN       NAME'                  
         MVC   REPP2(37),=C'----------------------       ADDRESS '              
         MVC   REPP1+41(33),CTORGNAM                                            
         MVC   REPP2+41(33),CTORGADD                                            
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
         DROP  R3                                                               
ERORX    EQU   *                                                                
*                                                                               
ERDD     XC    APELEM,APELEM       DESTINATION DETAILS                          
         MVI   APELEM,X'30'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    ERDDX                                                            
         USING CTDSTD,R3                                                        
         MVC   REPP1(33),=C'DETAILS OF THIS DESTINATION  NAME'                  
         MVC   REPP2(37),=C'---------------------------  ADDRESS '              
         MVC   REPP1+41(33),CTDSTNAM                                            
         MVC   REPP2+41(33),CTDSTADD                                            
         CLI   CTDSTLEN,166                                                     
         BL    ERDD010                                                          
         MVC   REPP3+41(33),CTDSTAD2                                            
         MVC   REPP4+41(33),CTDSTAD3                                            
*                                                                               
ERDD010  GOTO1 VREPORT,REPD                                                     
         MVC   REPP1+29(5),=C'LOGOS'                                            
         MVC   REPP2+29(10),=C'POWER CODE'                                      
         MVC   REPP1+41(7),CTDSTLG1                                             
         MVC   REPP1+49(7),CTDSTLG2                                             
         MVC   REPP2+41(L'CTDSTPOW),CTDSTPOW                                    
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
         DROP  R3                                                               
ERDDX    EQU   *                                                                
*                                                                               
ERPE     BAS   RE,CLEAR            PROGRAM EXCEPTION LIST                       
         LA    R4,AREA                                                          
         LA    R3,CTIDATA                                                       
ERPE010  CLI   0(R3),0                                                          
         BE    ERPE040                                                          
         CLI   0(R3),X'23'                                                      
         BE    ERPE030                                                          
ERPE020  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     ERPE010                                                          
*                                                                               
         USING CTPRGD,R3                                                        
ERPE030  MVC   0(4,R4),CTPRGRAM                                                 
         MVI   4(R4),C'='                                                       
         MVC   5(1,R4),CTPRGTST                                                 
         MVC   APWORK(10),CTIKID                                                
         LA    R1,APWORK                                                        
*                                                                               
ERPE032  CLI   0(R1),C' '                                                       
         BE    ERPE034                                                          
         LA    R1,1(R1)                                                         
         B     ERPE032                                                          
*                                                                               
ERPE034  MVI   0(R1),C'='                                                       
         MVC   1(1,R1),CTPRGTST                                                 
         LA    R4,7(R4)                                                         
         B     ERPE020                                                          
*                                                                               
ERPE040  MVC   REPP1(22),=C'PROGRAM EXCEPTION LIST'                             
         MVC   REPP2(22),=22C'-'                                                
         GOTO1 =V(SQUASHER),APPARM,AREA,250,RR=APRELO                           
         GOTO1 =V(CHOPPER),APPARM,(250,AREA),(60,REPP1+41),(C'P',4),   +        
               RR=APRELO                                                        
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
         B     ERPEX                                                            
ERPEX    EQU   *                                                                
*                                                                               
ERSH     XC    APELEM,APELEM       SHIPPING UNIT/ROUTE                          
         MVI   APELEM,X'4C'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    ERSHX                                                            
         USING CTSHPD,R3                                                        
         LLC   R4,CTSHPLEN                                                      
         AHI   R4,-6                                                            
         MVC   REPP1(23),=C'SHIPPING UNIT AND ROUTE'                            
         MVC   REPP2(23),=23C'-'                                                
         GOTO1 =V(CHOPPER),APPARM,((R4),CTSHPINS),(60,REPP1+41),       +        
               (C'P',4),RR=APRELO                                               
         DROP  R3                                                               
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
         B     ERSHX                                                            
ERSHX    EQU   *                                                                
*                                                                               
ERAT     SR    R5,R5               ATTENTION DETAILS                            
         LA    R3,CTIDATA                                                       
ERAT010  CLI   0(R3),0                                                          
         BE    ERAT040                                                          
         CLI   0(R3),X'31'                                                      
         BE    ERAT030                                                          
ERAT020  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     ERAT010                                                          
*                                                                               
         USING CTATTND,R3                                                       
ERAT030  MVC   REPP1+41(3),CTATTTYP                                             
         LA    R4,REPP1+43                                                      
         CLI   0(R4),C' '                                                       
         BNE   ERAT032                                                          
         BCTR  R4,0                                                             
         CLI   0(R4),C' '                                                       
         BNE   ERAT032                                                          
         BCTR  R4,0                                                             
*                                                                               
ERAT032  MVI   1(R4),C'='          SHOW EXPANSION OF ATTENTION TYPE             
         LLC   RF,CTATTLEN         XXX=AAAAA ETC                                
         AHI   RF,-6                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),CTATTDET                                                 
         LA    R5,1(R5)                                                         
         CHI   R5,2                                                             
         BH    ERAT036                                                          
         BE    ERAT034                                                          
         MVC   REPP1(25),=C'ATTENTION CODE EXPANSIONS'                          
         B     ERAT036                                                          
*                                                                               
ERAT034  MVC   REPP1(25),=25C'-'                                                
*                                                                               
ERAT036  GOTO1 VREPORT,REPD                                                     
         B     ERAT020                                                          
*                                                                               
ERAT040  CHI   R5,1                                                             
         BL    ERATX                                                            
         BE    ERAT050                                                          
         GOTO1 VREPORT,REPD                                                     
         B     ERATX                                                            
*                                                                               
ERAT050  MVC   REPP1(25),=25C'-'                                                
         GOTO1 VREPORT,REPD                                                     
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
*                                                                               
ERATX    OI    REPHEADI,REPHFRCE   FORCE NEW PAGE NEXT TIME                     
         NI    REPHEADI,X'FF'-(REPHSPAC)                                        
         XIT1                                                                   
                                                                                
* ROUTINE TO CLEAR AND ADD IDS                                                  
*                                                                               
CLEAR    MVI   AREA,C' '                                                        
         MVC   AREA+1(249),AREA                                                 
         MVC   AREA+250(250),AREA+000                                           
         MVC   AREA+500(250),AREA+250                                           
         MVC   AREA+750(250),AREA+500                                           
         BR    RE                                                               
*                                                                               
IDLIST   NTR1                                                                   
         LA    R4,AREA                                                          
*                                                                               
ID2      CLC   0(10,R4),ERSPACES                                                
         BE    ID4                                                              
         LA    R4,11(R4)                                                        
         B     ID2                                                              
*                                                                               
         USING CTIDD,R3                                                         
ID4      MVC   0(10,R4),CTID                                                    
         XIT1                                                                   
         DROP  R3                                                               
***********************************************************************         
* ROUTINE TO ADD TO DESTINATION LIST                                  *         
***********************************************************************         
VALDEST  NTR1                                                                   
         LA    R4,AREA                                                          
VD2      CLC   0(16,R4),ERSPACES                                                
         BE    VD4                                                              
         LA    R4,17(R4)                                                        
         B     VD2                                                              
*                                                                               
         USING CTVALD,R3                                                        
VD4      MVC   0(10,R4),CTVALDST                                                
         XIT1                                                                   
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO ADD TO PRINTER LIST                                      *         
***********************************************************************         
VALPRINT NTR1                                                                   
         LA    R4,AREA                                                          
VP2      CLC   0(15,R4),ERSPACES                                                
         BE    VP4                                                              
         LA    R4,17(R4)                                                        
         B     VP2                                                              
*                                                                               
         USING CTPRND,R3                                                        
VP4      MVC   0(4,R4),CTPRNLIN                                                 
         CLI   3(R4),C' '                                                       
         BNE   *+8                                                              
         AHI   R4,-1                                                            
         MVC   5(4,R4),CTPRNADD                                                 
         MVI   4(R4),C'-'                                                       
         EDIT  (1,CTPRNNUM),(5,9(R4)),BRACKET=YES                               
         CLI   12(R4),C' '                                                      
         BNE   *+8                                                              
         MVI   12(R4),0                                                         
         XIT1                                                                   
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE AUTHORIZATIONS                                    *         
***********************************************************************         
AUTH     NTR1                                                                   
         USING CTSYSD,R3                                                        
         GOTO1 ADISSE,CTSYSSE                                                   
         MVC   REPP1(7),APWORK                                                  
         OC    APPARM,APPARM                                                    
         BZ    AUTH7                                                            
         MVC   ASE,APPARM                                                       
         L     RE,ASE                                                           
         MVC   APGM,SEPGMS-SELISTD(RE)                                          
         GOTO1 VHEXOUT,APPARM,CTSYSNUM,REPP1+11,1,=C'TOG'                       
         GOTO1 VHEXOUT,APPARM,CTSYSSE,REPP1+19                                  
         BAS   RE,GETFNAME         EXPAND FILE NAME                             
         GOTO1 VHEXOUT,APPARM,CTSYSAGB,REPP1+28                                 
         MVI   REPP1+11,C' '                                                    
         MVC   REPP1+35(2),AGAID                                                
         GOTO1 ADISLACC,APPARM,(CTSYSNUM,CTSYSLMT),AGAID                        
*&&UK*&& MVC   REPP1+42(6),CTSYSLMT                                             
*&&US*&& MVC   REPP1+42(6),APWORK                                               
         MVC   AREA(15),=C'DEFAULT=X''0000'''                                   
         GOTO1 VHEXOUT,APPARM,CTSYSALL,AREA+10,2                                
         LA    R4,CTSYSPGM                                                      
         LA    R8,AREA+16                                                       
         LLC   R5,CTSYSLEN                                                      
         AHI   R5,-16                                                           
         BZ    AUTH6                                                            
*                                                                               
AUTH2    MVC   PROGRAM,0(R4)                                                    
         MVC   PGNAME,=C'UNKNOWN'                                               
         L     RF,=A(GETPGAN)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         CLC   PGNAME,=C'UNKNOWN'                                               
         BE    AUTH4                                                            
         MVC   0(4,R8),PGNAME                                                   
         MVC   4(8,R8),=C'=X''0000'''                                           
         GOTO1 VHEXOUT,APPARM,1(R4),7(R8),2                                     
         CLC   4(8,R8),=C'=X''0000'''                                           
         BNE   *+10                                                             
         MVC   4(8,R8),=C'=N      '                                             
         LA    R8,13(R8)                                                        
*                                                                               
AUTH4    LA    R4,3(R4)            3 BYTES PER NON-DEFAULT PROGRAM              
         AHI   R5,-3                                                            
         BNZ   AUTH2                                                            
*                                                                               
AUTH6    GOTO1 =V(CHOPPER),APPARM,(252,AREA),(60,REPP1+51),(C'P',4),   *        
               RR=APRELO                                                        
*                                                                               
AUTH7    GOTO1 VREPORT,REPD                                                     
         XIT1                                                                   
                                                                                
***********************************************************************         
* ROUTINE TO EXPAND FILE NAME FROM CTSYSSE                                      
***********************************************************************         
GETFNAME NTR1                                                                   
         USING CTSYSD,R3                                                        
         L     R4,AIOAREA2                                                      
         USING CTWREC,R4                                                        
         LA    R4,CTWDATA                                                       
*                                                                               
GETFN2   CLI   0(R4),X'A4'                                                      
         BNE   GETFN4                                                           
         CLI   0(R4),0                                                          
         BE    GETFNX                                                           
         CLC   CTSYSSE,11(R4)      CHECK MATCH ON NUMBER                        
         BNE   GETFN4                                                           
         MVC   REPP1+17(7),3(R4)   FOUND DISPLAY NAME                           
         B     GETFNX                                                           
*                                                                               
GETFN4   LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    GETFNX                                                           
         B     GETFN2                                                           
*                                                                               
GETFNX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ID COUNTRY CODE FROM AGENCY ALPHA ID                 *         
***********************************************************************         
GETCTRY  NTR1                                                                   
         MVI   IDCTRY,0                                                         
         L     R4,AAGYTAB                                                       
*                                                                               
GETC010  CLI   0(R4),0                                                          
         BE    GETC020                                                          
         CLC   0(2,R4),AGAID                                                    
         BE    GETC040                                                          
         LA    R4,3(R4)                                                         
         CLM   R4,15,AAGYTABX                                                   
         BL    GETC010                                                          
         B     GETC030                                                          
*                                                                               
GETC020  OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         L     RF,=A(GETACC)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETCX                                                            
         MVC   0(2,R4),AGAID                                                    
         MVC   2(1,R4),IDCTRY                                                   
         B     GETCX                                                            
*                                                                               
GETC030  OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         L     RF,=A(GETACC)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETCX                                                            
         B     GETCX                                                            
*                                                                               
GETC040  MVC   IDCTRY,2(R4)                                                     
         B     GETCX                                                            
*                                                                               
GETCX    EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
ERSPACES DC    80C' '                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD ACCESS RECORD DATA TABLE FOR LOTUS NOTES FORMAT REPORT        *         
***********************************************************************         
BLDACT   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING BLDACT,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+BAC'    INSERT NAME                                  
*                                  CLEAR ACCESS RECORD DATA TABLE               
         ICM   R4,15,=AL4((ACCTMAX+1)*(ACCTABL))                                
         GETMAIN RC,LV=(R4),BNDRY=PAGE                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,AACCTAB          SAVE A(GETMAIN REGION)                       
         ST    R0,LACCTAB          SAVE LENGTH(GETMAIN REGION)                  
         AR    R1,R0                                                            
         ST    R1,AACCTABX                                                      
*                                                                               
         L     RE,AACCTAB          CLEAR ACCESS RECORD DATA TABLE               
         L     RF,AACCTABX                                                      
         SR    RF,RE                                                            
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         L     R4,AACCTAB                                                       
         USING ACCTABD,R4                                                       
         LA    R2,IOKEY            READ AGENCY ACCESS RECORDS                   
         USING CT5KEY,R2           INTO AIOAREA2                                
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTO1 AIO,IOHIGH+IOCONFIL+IO2                                          
         B     BACT020                                                          
BACT010  GOTO1 AIO,IOSQ+IOCONFIL+IO2                                            
BACT020  JNE   *+2                                                              
         L     R2,AIOAREA2                                                      
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   BACT200                                                          
         MVC   ACCTALPH,CT5KALPH                                                
*                                                                               
         XC    ACCTACTD,ACCTACTD                                                
         LA    R1,CT5DATA                                                       
         DROP  R2                                                               
         SR    R0,R0                                                            
BACT100  CLI   0(R1),0                                                          
         BE    BACT120                                                          
         CLI   0(R1),X'01'                                                      
         BNE   *+14                                                             
         MVC   ACCTACTD,2(R1)                                                   
         B     BACT110                                                          
         CLI   0(R1),CTDSCELQ                                                   
         BNE   *+14                                                             
         MVC   ACCTPRNO,CTDSC-CTDSCD(R1)                                        
         B     BACT110                                                          
         CLI   0(R1),CTAGDELQ                                                   
         BNE   BACT110                                                          
         MVC   ACCTHONO,CTAGDHG-CTAGDD(R1)                                      
         MVC   ACCTGRNO,CTAGDAG-CTAGDD(R1)                                      
BACT110  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BACT100                                                          
*                                                                               
BACT120  OC    ACCTHONO,ACCTHONO   SET DEFAULT VALUES                           
         BNZ   *+10                                                             
         MVC   ACCTHONO,ACCTPRNO                                                
         OC    ACCTGRNO,ACCTGRNO                                                
         BNZ   *+10                                                             
         MVC   ACCTGRNO,ACCTHONO                                                
         LA    R4,ACCTABL(R4)                                                   
         CLM   R4,15,AACCTABX                                                   
         BL    BACT010                                                          
         DC    H'0'                                                             
*                                                                               
BACT200  L     R4,AACCTAB          SET USER NAMES                               
BACT210  OC    ACCTALPH,ACCTALPH                                                
         BZ    BACT300                                                          
         GOTO1 BACTID,ACCTPRNO                                                  
         MVC   ACCTPRID,INUSRID                                                 
         CLC   ACCTHONO,ACCTPRNO                                                
         BNE   BACT220                                                          
         MVC   ACCTHOID,ACCTPRID                                                
         B     BACT230                                                          
BACT220  GOTO1 BACTID,ACCTHONO                                                  
         MVC   ACCTHOID,INUSRID                                                 
BACT230  CLC   ACCTGRNO,ACCTPRNO                                                
         BNE   BACT240                                                          
         MVC   ACCTGRID,ACCTPRID                                                
         B     BACT260                                                          
BACT240  GOTO1 BACTID,ACCTGRNO                                                  
         MVC   ACCTGRID,INUSRID                                                 
BACT260  LA    R4,ACCTABL(R4)                                                   
         CLM   R4,15,AACCTABX                                                   
         BL    BACT210                                                          
         DC    H'0'                                                             
*                                                                               
BACT300  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* READ AGENCY ACCESS RECORDS                                          *         
***********************************************************************         
BACTID   NTR1                                                                   
         MVI   INUSRNM,C'?'                                                     
         MVC   INUSRNM+1(L'INUSRNM-1),INUSRNM                                   
         MVC   INUSRID,BASPACES                                                 
         SR    R0,R0                                                            
         ICM   R0,3,0(R1)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   INUSRID+0(3),=C'ID='                                             
         UNPK  INUSRID+3(5),DUB                                                 
         LA    R2,IOKEY            READ AGENCY ACCESS RECORDS                   
         USING CTIKEY,R2           INTO AIOAREA2                                
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R1)                                                    
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   BACTIDX                                                          
         L     R2,AIOAREA2                                                      
         LA    R1,CTIDATA                                                       
         DROP  R2                                                               
         SR    R0,R0                                                            
BACTID2  CLI   0(R1),0                                                          
         BE    BACTIDX                                                          
         CLI   0(R1),X'02'                                                      
         BNE   *+14                                                             
         MVC   INUSRID,CTDSC-CTDSCD(R1)                                         
         B     BACTID4                                                          
         CLI   0(R1),X'06'                                                      
         BNE   *+14                                                             
         MVC   INUSRAL,CTAGYID-CTAGYD(R1)                                       
         B     BACTID4                                                          
         CLI   0(R1),X'30'                                                      
         BNE   BACTID4                                                          
         MVC   INUSRNM,CTDSTNAM-CTDSTD(R1)                                      
         B     BACTID4                                                          
BACTID4  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BACTID2                                                          
BACTIDX  XIT1                                                                   
*                                                                               
BASPACES DC    80C' '                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD LOTUS NOTES ID TABLE FOR LOTUS NOTES FORMAT REPORT            *         
***********************************************************************         
BLDLNI   NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING BLDLNI,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+BLN'    INSERT NAME                                  
*                                  CLEAR ACCESS RECORD DATA TABLE               
         ICM   R4,15,=AL4((LNITMAX+1)*(LNITABL))                                
         GETMAIN RC,LV=(R4),BNDRY=PAGE                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ALNITAB          SAVE A(GETMAIN REGION)                       
         ST    R0,LLNITAB          SAVE LENGTH(GETMAIN REGION)                  
         AR    R1,R0                                                            
         ST    R1,ALNITABX                                                      
*                                  CLEAR ACCESS RECORD DATA TABLE               
         L     RE,ALNITAB                                                       
         L     RF,ALNITABX                                                      
         SR    RF,RE                                                            
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,IOKEY            READ USER ID RECORDS                         
         USING CTIKEY,R2           INTO AIOAREA2                                
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         GOTO1 AIO,IOHIGH+IOCONFIL+IO2                                          
         B     BLNI020                                                          
BLNI010  GOTO1 AIO,IOSQ+IOCONFIL+IO2                                            
BLNI020  JNE   *+2                                                              
         L     R2,AIOAREA2                                                      
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BNE   BLNI200                                                          
*                                                                               
         LA    R3,CTIDATA                                                       
         DROP  R2                                                               
         SR    R0,R0                                                            
BLNI100  CLI   0(R3),0                                                          
         BE    BLNI010                                                          
         CLI   0(R3),CTLNIELQ                                                   
         BE    BLNI110                                                          
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BLNI100                                                          
*                                                                               
         USING CTLNID,R3                                                        
BLNI110  L     R4,ALNITAB                                                       
         USING LNITABD,R4                                                       
BLNI120  OC    LNITNUM,LNITNUM                                                  
         BZ    BLNI130                                                          
         CLC   LNITNUM,CTLNINUM                                                 
         BE    BLNI010                                                          
         B     BLNI140                                                          
BLNI130  MVC   LNITNUM,CTLNINUM                                                 
         B     BLNI010                                                          
BLNI140  LA    R4,LNITABL(R4)                                                   
         CLM   R4,15,ALNITABX                                                   
         BL    BLNI120                                                          
         DC    H'0'                                                             
*                                                                               
BLNI200  L     R4,ALNITAB          SET USER NAMES                               
BLNI210  OC    LNITNUM,LNITNUM                                                  
         BZ    BLNI300                                                          
         GOTO1 BLNIID,LNITNUM                                                   
         MVC   LNITCOD,INUSRID                                                  
         LA    R4,LNITABL(R4)                                                   
         CLM   R4,15,ALNITABX                                                   
         BL    BLNI210                                                          
         DC    H'0'                                                             
*                                                                               
BLNI300  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET LOTIS NOTES DOO DAHS                                 *         
***********************************************************************         
BLNIID   NTR1                                                                   
         MVC   INUSRID,BLSPACES                                                 
         SR    R0,R0                                                            
         ICM   R0,3,0(R1)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   INUSRID+0(3),=C'ID='                                             
         UNPK  INUSRID+3(5),DUB                                                 
         LA    R2,IOKEY            READ AGENCY ACCESS RECORDS                   
         USING CTIKEY,R2           INTO AIOAREA2                                
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R1)                                                    
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   BLNIIDX                                                          
         L     R2,AIOAREA2                                                      
         LA    R1,CTIDATA                                                       
         DROP  R2                                                               
         SR    R0,R0                                                            
BLNIID2  CLI   0(R1),0                                                          
         BE    BLNIIDX                                                          
         CLI   0(R1),X'02'                                                      
         BNE   *+14                                                             
         MVC   INUSRID,CTDSC-CTDSCD(R1)                                         
         B     BLNIID4                                                          
BLNIID4  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BLNIID2                                                          
BLNIIDX  XIT1                                                                   
*                                                                               
BLSPACES DC    80C' '                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD FOR REPORT IN LOTUS NOTES DOWNLOAD FORMAT            *         
***********************************************************************         
LNREP    NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING LNREP,RB                                                         
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+LNR'    INSERT NAME                                  
         USING CTIREC,R2                                                        
         LA    R4,DLCB                                                          
         USING DLCBD,R4                                                         
*                                                                               
         MVI   REPP1,C' '          CLEAR D/L PRINTLINE                          
         MVC   REPP1+1(L'REPP1-1),REPP1                                         
*                                  USER ID                                      
LNRUID   MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(L'CTIKID),CTIKID                                         
         GOTO1 VDLFLD,(R4)                                                      
LNRUIDX  EQU   *                                                                
*                                                                               
LNRUAD   XC    UIDACTD,UIDACTD     SAVE USER ID ACTIVITY DATE                   
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'01'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNRUADX                                                          
         MVC   UIDACTD,2(R3)                                                    
LNRUADX  EQU   *                                                                
*                                                                               
LNRUIN   XC    APELEM,APELEM       USER ID NUMBER                               
         MVI   APELEM,X'02'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNUI100                                                          
         USING CTDSCD,R3                                                        
         EDIT  (2,CTDSC),(4,DUB),ALIGN=LEFT                                     
         MVI   DLCBTYP,DLCBNUM                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(4),DUB                                                   
         GOTO1 VDLFLD,(R4)                                                      
         B     LNRUINX                                                          
LNUI100  MVI   DLCBTYP,DLCBNUM                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         XC    DUB,DUB                                                          
         MVC   DLCBFLD(4),DUB                                                   
         GOTO1 VDLFLD,(R4)                                                      
         B     LNRUINX                                                          
LNRUINX  EQU   *                                                                
*                                                                               
LNRLNI   XC    APELEM,APELEM       LOTUS NOTES ADMIN USER ID                    
         MVI   APELEM,CTLNIELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNLN100                                                          
         USING CTLNID,R3                                                        
         MVC   IDNUM,CTLNINUM                                                   
         L     R3,ALNITAB                                                       
         USING LNITABD,R3                                                       
LNLN010  OC    LNITNUM,LNITNUM                                                  
         BZ    LNLN100                                                          
         CLC   LNITNUM,IDNUM                                                    
         BE    LNLN020                                                          
         LA    R3,LNITABL(R3)                                                   
         CLM   R3,15,ALNITABX                                                   
         BL    LNLN010                                                          
         DC    H'0'                                                             
LNLN020  MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),LNITCOD                                              
         GOTO1 VDLFLD,(R4)                                                      
         B     LNRLNIX                                                          
LNLN100  MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),LNSPACES                                             
         GOTO1 VDLFLD,(R4)                                                      
         B     LNRLNIX                                                          
LNRLNIX  EQU   *                                                                
*                                                                               
LNRAGY   XC    AGAID,AGAID         AGENCY ELEMENT                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'06'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNAG100                                                          
         USING CTAGYD,R3                                                        
         MVC   AGAID,CTAGYID                                                    
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(2),CTAGYID                                               
         GOTO1 VDLFLD,(R4)                                                      
         L     R3,AACCTAB                                                       
         USING ACCTABD,R3                                                       
LNAG010  CLM   R3,15,AACCTABX                                                   
         BNL   LNAG200                                                          
         OC    ACCTALPH,ACCTALPH                                                
         BZ    LNAG200                                                          
         CLC   ACCTALPH,AGAID                                                   
         BE    *+12                                                             
         LA    R3,ACCTABL(R3)                                                   
         B     LNAG010                                                          
         CLC   ACCTACTD,UIDACTD                                                 
         BNH   *+10                                                             
         MVC   UIDACTD,ACCTACTD                                                 
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),ACCTPRID                                             
         GOTO1 VDLFLD,(R4)                                                      
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),ACCTHOID                                             
         GOTO1 VDLFLD,(R4)                                                      
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),ACCTGRID                                             
         GOTO1 VDLFLD,(R4)                                                      
         B     LNRAGYX                                                          
LNAG100  MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(2),LNSPACES                                              
         GOTO1 VDLFLD,(R4)                                                      
LNAG200  MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),LNSPACES                                             
         GOTO1 VDLFLD,(R4)                                                      
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),LNSPACES                                             
         GOTO1 VDLFLD,(R4)                                                      
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),LNSPACES                                             
         GOTO1 VDLFLD,(R4)                                                      
LNRAGYX  EQU   *                                                                
*                                                                               
LNRACD   OC    UIDACTD,UIDACTD     RESOLVE ACTIVITY DATE                        
         BZ    LNAC100                                                          
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDATCON,APPARM,(3,UIDACTD),(21,DLCBFLD)                          
         GOTO1 VDLFLD,(R4)                                                      
         B     LNRACDX                                                          
LNAC100  MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(10),LNSPACES                                             
         GOTO1 VDLFLD,(R4)                                                      
LNRACDX  EQU   *                                                                
*                                                                               
LNRDST   XC    APELEM,APELEM       DESTINATION DETAILS                          
         MVI   APELEM,X'30'                                                     
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNDS100                                                          
         USING CTDSTD,R3                                                        
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(33),CTDSTNAM                                             
         OC    DLCBFLD(33),LNSPACES                                             
         GOTO1 VDLFLD,(R4)                                                      
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(7),CTDSTLG2                                              
         OC    DLCBFLD(7),LNSPACES                                              
         GOTO1 VDLFLD,(R4)                                                      
         B     LNRDSTX                                                          
LNDS100  MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(33),LNSPACES                                             
         GOTO1 VDLFLD,(R4)                                                      
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(7),LNSPACES                                              
         GOTO1 VDLFLD,(R4)                                                      
         B     LNRDSTX                                                          
*                                                                               
LNRDSTX  EQU   *                                                                
*                                                                               
LNRSYS   XC    LNRSENT,LNRSENT     SYSTEMS                                      
         LA    R3,CTIDATA                                                       
         USING CTSYSD,R3                                                        
LNSY010  CLI   0(R3),0                                                          
         BE    LNSY100                                                          
         CLI   0(R3),X'21'                                                      
         BE    LNSY030                                                          
LNSY020  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     LNSY010                                                          
LNSY030  LA    R1,LNRSYSN                                                       
         LA    RE,LNRSYST                                                       
         LA    RF,LNRSENT                                                       
LNSY032  CLC   0(1,RE),CTSYSNUM                                                 
         BE    LNSY034                                                          
         LA    RE,1(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R1,LNSY032                                                       
         B     LNSY020                                                          
LNSY034  L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         LA    RE,SYSLLEN(RE)      GO PAST SERVICE SYSTEM ENTRY                 
         USING SYSLSTD,RE                                                       
LNSY036  CLI   0(RE),X'FF'                                                      
         BE    LNSY020                                                          
         CLC   SYSLNUM,CTSYSNUM                                                 
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)                                                   
         B     LNSY036                                                          
         MVC   0(1,RF),CTSYSSE                                                  
         TM    SYSLIND1,X'40'                                                   
         BZ    *+10                                                             
         MVC   1(1,RF),CTSYSAGB                                                 
         TM    SYSLIND1,X'80'                                                   
         BZ    *+10                                                             
         MVC   1(2,RF),AGAID                                                    
         B     LNSY020                                                          
*                                                                               
LNSY100  LA    R3,LNRSENT                                                       
         LA    R8,LNRSYSN                                                       
LNSY110  CLI   0(R3),0                                                          
         BNE   LNSY120                                                          
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(7),LNSPACES                                              
         GOTO1 VDLFLD,(R4)                                                      
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(2),LNSPACES                                              
         GOTO1 VDLFLD,(R4)                                                      
         B     LNSY200                                                          
LNSY120  GOTO1 ADISSE,0(R3)                                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(7),APWORK                                                
         GOTO1 VDLFLD,(R4)                                                      
         MVC   AGYBIN,LNSPACES                                                  
         CLI   1(R3),0                                                          
         BE    LNSY130                                                          
         MVC   AGYBIN,1(R3)                                                     
         CLI   2(R3),0                                                          
         BNE   LNSY130                                                          
         XOUT  1(R3),AGYBIN,1                                                   
LNSY130  MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBFLD(2),AGYBIN                                                
         GOTO1 VDLFLD,(R4)                                                      
LNSY200  LA    R3,3(R3)                                                         
         BCT   R8,LNSY110                                                       
*                                                                               
LNRSYSX  EQU   *                                                                
         MVI   DLCBACT,DLCBEOL     END OF DOWNLOAD LINE                         
         GOTO1 VDLFLD,(R4)                                                      
*                                                                               
LNRX     EQU   *                                                                
         XIT1                                                                   
LNRSYSN  EQU   7                                                                
LNRSYST  DC    X'04050607090A0B'                                                
*                                                                               
LNSPACES DC    80C' '                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK ID RECORD AND ITS PASSIVE WILL NOT BE TOO BIG BEFORE UPDATE   *         
* R2=A(USER ID RECORD)                                                *         
***********************************************************************         
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
***********************************************************************         
* ID TYPE TABLE                                                       *         
***********************************************************************         
         DS    0F                                                               
TYPTAB   DS    0CL10                                                            
         DC    C'A',C'ACTUALS  '                                                
         DC    C'C',C'CLITEST  '                                                
         DC    C'D',C'DORMANT  '                                                
         DC    C'E',C'ESTIMATES'                                                
         DC    C'M',C'MEDIA    '                                                
         DC    C'O',C'OFFICE   '                                                
         DC    C'R',C'ARCHIVE  '                                                
*&&UK*&& DC    C'T',C'TEST     '                                                
*&&US*&& DC    C'T',C'TRAFFIC  '                                                
         DC    C'X',C'CROSSFILE'                                                
TYPTABX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TEMP W/S                                             *         
***********************************************************************         
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
* DDDLCB                                                                        
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SCREENS AND SAVED STORAGE                            *         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
* CTGENFAD                                                                      
       ++INCLUDE CTGENFAD                                                       
         ORG   GENTABH                                                          
* CTGENDAD                                                                      
       ++INCLUDE CTGENDAD                                                       
         ORG   GENTABH                                                          
* CTGENBAD                                                                      
       ++INCLUDE CTGENBAD                                                       
         ORG                                                                    
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 LAST SYSTEM DISPLAYED                        
SAVKEY   DS    XL(L'CTIKEY)        SAVE LAST RECORD KEY READ FOR COPY           
SAVOAGY  DS    CL2                 OLD AGENCY ALPHA-ID                          
SAVNAGY  DS    CL2                 NEW AGENCY ALPHA-ID                          
SAVCLRL  EQU   *-SAVOVER                                                        
*                                                                               
ACCTABD  DSECT                     ACCESS RECORD DATA TABLE                     
ACCTALPH DS    CL2                 AGENCY ALPHA-ID                              
ACCTACTD DS    XL3                 ACTIVITY DATE                                
ACCTPRNO DS    XL2                 PRINCIPLE ID #                               
ACCTPRID DS    CL10                PRINCIPLE ID CODE                            
ACCTHONO DS    XL2                 HOLDING COMPANY-ID #                         
ACCTHOID DS    CL10                HOLDING COMPANY-ID CODE                      
ACCTGRNO DS    XL2                 AGENCY GROUP ID #                            
ACCTGRID DS    CL10                AGENCY GROUP-ID CODE                         
ACCTABL  EQU   *-ACCTABD                                                        
*                                                                               
LNITABD  DSECT                     LOTUS NOTES ADMIN USER ID TABLE              
LNITNUM  DS    XL2                 LNI NUMBER                                   
LNITCOD  DS    CL10                LNI CODE                                     
LNITABL  EQU   *-LNITABD                                                        
*                                                                               
LISTD    DSECT                     LIST/SELECT LINE LAYOUT                      
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
         ORG   *-3                                                              
LISTAGYA DS    CL2                                                              
         ORG                                                                    
LISTID   DS    CL8                                                              
         DS    CL1                                                              
LISTNUM  DS    CL5                                                              
         DS    CL1                                                              
LISTPWD  DS    CL2                                                              
         DS    CL1                                                              
LISTIDT  DS    CL1                                                              
         DS    CL1                                                              
LISTSYS  DS    CL30                                                             
         ORG   LISTSYS                                                          
LISTLIXA DS    CL2                                                              
LISTLIMA DS    CL8                                                              
         DS    CL1                                                              
LISTLIXM DS    CL2                                                              
LISTLIMM DS    CL17                                                             
         DS    CL1                                                              
LISTIDS  DS    CL23                                                             
         ORG   LISTPWD                                                          
LISTSID  DS    CL30                                                             
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER LOCAL W/S                                            *         
***********************************************************************         
LOCALD   DSECT                                                                  
DUB      DS    D                                                                
RETURN   DS    F                                                                
ASYSEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
VDLFLD   DS    V                                                                
WORK     DS    CL(L'APWORK)                                                     
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
DATEFLD  DS    CL12                                                             
*                                                                               
SELDATA  DS    0XL(SELDATAL)                                                    
SELID    DS    CL8                 USER ID                                      
SELIDSP  DS    CL1                 1ST SPECIAL CHAR                             
SELIDL   DS    CL1                 L'DATA ENTERED                               
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELSYS   DS    XL1                 SYSTEM SEOV NUMBER                           
SELSEN   DS    XL1                 SYSTEM SE NUMBER                             
SELPGM   DS    XL1                 PROGRAM NUMBER                               
SELAGY   DS    XL2                 AGENCY ALPHA ID                              
SELPWD   DS    XL1                 PASSWORD REQUIRED                            
SELCID   DS    XL10                COMPATIBLE USER ID                           
SELSYN   DS    CL1                 SYNONYM Y/N?                                 
SELIDT   DS    CL1                 ID TYPE                                      
SELUSED  DS    XL2                 %USED                                        
SELLMT   DS    CL4                 LIMIT ACCESS                                 
SELLUDA  DS    CL1                 LAST USED ACTION = > <                       
SELLUDM  DS    XL1                 LAST USED MONTHS SINCE JAN/2009              
SELDATAL EQU   *-SELID                                                          
*                                                                               
FLDCNT   DS    XL1                                                              
FCOUNT   DS    XL1                                                              
*                                                                               
AUTHCHK  DS    XL1                                                              
*                                                                               
EXFMT    DS    CL1                                                              
*                                                                               
SYSTEM   DS    CL1                                                              
SFLAGS   DS    CL1                                                              
PROGRAM  DS    CL1                                                              
PGNAME   DS    CL8                                                              
*                                                                               
INUSRAL  DS    CL2                                                              
INUSRID  DS    CL10                                                             
INUSRNM  DS    CL33                                                             
*                                                                               
LNRSENT  DS    XL(3*LNRSYSN)                                                    
*                                                                               
IDNUM    DS    XL(L'CTIKNUM)                                                    
AGNUM    DS    XL(L'CTSYSAGB)                                                   
AGAID    DS    XL(L'CT5KALPH)                                                   
SECAID   DS    XL(L'CT5KALPH)                                                   
AGYBIN   DS    CL2                                                              
IDOPTS   DS    XL2                                                              
PQGID    DS    XL2                                                              
SYNNUM   DS    XL2                                                              
SYNID    DS    CL10                                                             
LASTIDOS DS    XL2                                                              
LASTSYN  DS    XL2                                                              
LASTSID  DS    CL10                                                             
IDCTRY   DS    XL1                 COUNTRY CODE                                 
WILDCLEN DS    XL1                                                              
UIDACTD  DS    XL3                                                              
*                                                                               
XXCNT    DS    0C                                                               
IDCNT    DS    CL1                                                              
EXCNT    DS    CL1                                                              
PGCNT    DS    CL1                                                              
APCNT    DS    CL1                                                              
ATCNT    DS    CL1                                                              
SYCNT    DS    CL1                                                              
XXCNTL   EQU   *-XXCNT                                                          
*                                                                               
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
TERMNUM  DS    H                                                                
*                                                                               
GETSEQF  DS    XL1                                                              
IOCOUNT  DS    H                                                                
FLAG     DS    XL1                                                              
*                                                                               
AIDLIST  DS    A                                                                
LIDSAVE  DS    CL10                                                             
LIDFNDX  DS    XL1                                                              
LIDFADDR DS    A                                                                
*                                                                               
SYSEL    DS    XL12                                                             
*                                                                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
*                                                                               
DLCB     DS    CL(DLCBXL)                                                       
*                                                                               
DELKEY   DS    CL(L'IOKEY)                                                      
KEYSAVE  DS    CL(L'IOKEY)                                                      
*                                                                               
LACCTAB  DS    F                                                                
AACCTAB  DS    A                                                                
AACCTABX DS    A                                                                
ACCTMAX  EQU   1000                                                             
*                                                                               
LLNITAB  DS    F                                                                
ALNITAB  DS    A                                                                
ALNITABX DS    A                                                                
LNITMAX  EQU   10000                                                            
*                                                                               
AAGYTAB  DS    A                                                                
AAGYTABX DS    A                                                                
AREA     DS    1000XL1                                                          
AGYTAB   DS    1000XL3                                                          
AGYTABX  DS    0D                                                               
*                                                                               
LOCALX   EQU   *                                                                
*                                                                               
       ++INCLUDE FASYSEQUS                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTGEN05   02/01/18'                                      
         END                                                                    
